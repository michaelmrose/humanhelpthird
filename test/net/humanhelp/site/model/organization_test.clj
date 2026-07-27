(ns net.humanhelp.site.model.organization-test
  "Public-boundary coverage for the HumanHelp Organization model.

   These tests treat net.humanhelp.site.model.organization.core as the
   Organization model's supported interface. They intentionally do not require
   Organization domain, schema, Graph, or FX implementation namespaces and do
   not reach into private vars.

   The fixture runs the real Graph resolvers, gesso.model transaction handler,
   Gesso Live transaction path, and XTDB2 node. Only prerequisite root and User
   authorization documents are seeded directly because those creation flows are
   outside the currently exposed Organization API."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.graph :as graph]
   [gesso.live.core :as live]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [malli.registry :as mr]
   [net.humanhelp.schema :as schema]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user]
   [xtdb.api :as xt]
   [xtdb.node :as xtn])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed test identities
;; =============================================================================

(def organization-id
  (UUID/fromString
   "81000000-0000-0000-0000-000000000001"))

(def other-organization-id
  (UUID/fromString
   "81000000-0000-0000-0000-000000000002"))

(def missing-organization-id
  (UUID/fromString
   "81000000-0000-0000-0000-000000000099"))

(def admin-user-id
  (UUID/fromString
   "82000000-0000-0000-0000-000000000001"))

(def outsider-user-id
  (UUID/fromString
   "82000000-0000-0000-0000-000000000002"))

(def admin-membership-id
  (UUID/fromString
   "83000000-0000-0000-0000-000000000001"))

(def admin-role-id
  (UUID/fromString
   "84000000-0000-0000-0000-000000000001"))

(def fixture-time
  (Instant/parse
   "2026-07-24T12:00:00Z"))

;; =============================================================================
;; Real test runtime
;; =============================================================================

(defonce ^:private !runtime
  (atom nil))

(defn- runtime
  []
  (or
   @!runtime
   (throw
    (ex-info
     "Organization test runtime is not initialized."
     {}))))

(def model-modules
  [user/module
   organization/module])

(def malli-opts
  {:registry
   (mr/composite-registry
    m/default-registry
    ;; The current application registry may predate this Organization rewrite.
    ;; Merge the public Organization registry in explicitly so the boundary
    ;; test validates the model being exercised without depending on app wiring
    ;; having already been migrated.
    (merge
     schema/schema
     organization/schema))})

(defn- live-rules
  []
  [{:when-topic :organization
    :expand
    (fn [_ctx change]
      [change])}

   {:when-topic :organization-group
    :expand
    (fn [_ctx change]
      [change])}

   {:when-topic :location
    :expand
    (fn [_ctx change]
      [change])}])

(defn- root-organization-document
  [id name]
  {:xt/id id
   :organization/name name
   :organization/status :active
   :organization/revision 0
   :organization/created-at fixture-time
   :organization/updated-at fixture-time})

(defn- user-document
  [id email display-name]
  {:xt/id id
   :user/email email
   :user/email-verified-at fixture-time
   :user/display-name display-name
   :user/status :active
   :user/revision 0
   :user/created-at fixture-time
   :user/updated-at fixture-time})

(defn- admin-membership-document
  []
  {:xt/id admin-membership-id
   :membership/user admin-user-id
   :membership/organization organization-id
   :membership/skills #{}
   :membership/status :active
   :membership/revision 0
   :membership/created-at fixture-time
   :membership/updated-at fixture-time})

(defn- admin-role-document
  []
  {:xt/id admin-role-id
   :role-assignment/membership admin-membership-id
   :role-assignment/organization organization-id
   :role-assignment/role :admin
   :role-assignment/scope-type :organization
   :role-assignment/scope-id organization-id
   :role-assignment/status :active
   :role-assignment/revision 0
   :role-assignment/created-at fixture-time
   :role-assignment/updated-at fixture-time})

(defn- seed-prerequisites!
  [node]
  (xt/execute-tx
   node
   [[:put-docs
     :organization
     (root-organization-document
      organization-id
      "Boundary Test Organization")]

    [:put-docs
     :organization
     (root-organization-document
      other-organization-id
      "Other Organization")]

    [:put-docs
     :user
     (user-document
      admin-user-id
      "organization-admin@example.com"
      "Organization Admin")]

    [:put-docs
     :user
     (user-document
      outsider-user-id
      "organization-outsider@example.com"
      "Organization Outsider")]

    [:put-docs
     :membership
     (admin-membership-document)]

    [:put-docs
     :role-assignment
     (admin-role-document)]]))

(defn- with-runtime
  [f]
  (with-open [node (xtn/start-node {})]
    (let [live-system
          (live/create
           {:rules
            (live-rules)

            :dispatch-options
            {:threads 1
             :queue-size 64}})

          ctx
          {:biff/node node
           :biff/conn node
           :xtdb/node node

           :biff/modules
           model-modules

           :biff.fx/handlers
           (merge
            graph/fx-handlers
            model.tx/handlers)

           :biff/malli-opts
           malli-opts

           :gesso.live/system
           live-system}]

      (seed-prerequisites!
       node)

      (reset!
       !runtime
       {:node node
        :live-system live-system
        :ctx ctx})

      (try
        (f)
        (finally
          (reset! !runtime nil)
          (live/close! live-system))))))

(use-fixtures
 :each
 with-runtime)

;; =============================================================================
;; Test helpers
;; =============================================================================

(defn- base-ctx
  []
  (:ctx
   (runtime)))

(defn- admin-ctx
  []
  (assoc
   (base-ctx)
   :current-user/id
   admin-user-id))

(defn- outsider-ctx
  []
  (assoc
   (base-ctx)
   :current-user/id
   outsider-user-id))

(defn- error-type
  [f]
  (try
    (f)
    ::did-not-throw
    (catch Throwable error
      (loop [error error]
        (when error
          (or
           (:error/type
            (ex-data error))
           (recur
            (ex-cause error))))))))

(defn- committed?
  [result]
  (=
   :committed
   (get-in
    result
    [:transaction
     :commit/status])))

(defn- ctx-after
  [fallback-ctx result]
  (if-let [consistent-ctx
           (get-in
            result
            [:transaction
             :ctx])]
    (merge
     fallback-ctx
     consistent-ctx)
    fallback-ctx))

(defn- create-group!
  [ctx parent-scope name]
  (let [result
        (organization/create-organization-group
         ctx
         {:organization-id organization-id
          :parent-scope parent-scope
          :name name})]
    (is
     (committed? result))
    {:ctx
     (ctx-after ctx result)
     :document
     (:organization-group result)
     :result
     result}))

(defn- create-location!
  [ctx parent-scope name]
  (let [result
        (organization/create-location
         ctx
         {:organization-id organization-id
          :parent-scope parent-scope
          :name name})]
    (is
     (committed? result))
    {:ctx
     (ctx-after ctx result)
     :document
     (:location result)
     :result
     result}))

(defn- document-ids
  [documents]
  (mapv
   :xt/id
   documents))

;; =============================================================================
;; Public facade contract
;; =============================================================================

(deftest public-facade-contract-test
  (testing "module registration is exposed entirely through the public facade"
    (is
     (=
      organization/schema
      (:schema organization/module)))
    (is
     (identical?
      organization/resolvers
      (:biff.graph/resolvers organization/module))))

  (testing "the supported operation registry contains only public Organization operations"
    (is
     (=
      #{:organization/create-group
        :organization/create-location
        :organization/rename
        :organization/suspend
        :organization/reactivate
        :organization-group/rename
        :organization-group/move
        :organization-group/suspend
        :organization-group/reactivate
        :location/rename
        :location/move
        :location/suspend
        :location/reactivate}
      (set
       (keys
        organization/operations))))
    (is
     (every?
      var?
      (vals
       organization/operations))))

  (testing "public vocabulary and scope constructors remain coherent"
    (let [root-scope
          (organization/organization-scope
           organization-id)

          group-id
          (UUID/fromString
           "85000000-0000-0000-0000-000000000001")

          group-scope
          (organization/organization-group-scope
           group-id)]
      (is
       (=
        :organization
        organization/organization-entity-type))
      (is
       (=
        :organization-group
        organization/organization-group-entity-type))
      (is
       (=
        :location
        organization/location-entity-type))
      (is
       (=
        #{:active :suspended :closed}
        organization/statuses))
      (is
       (=
        "North Store"
        (organization/normalize-name
         "  North Store  ")))
      (is
       (organization/name?
        "North Store"))
      (is
       (organization/scope-reference?
        root-scope))
      (is
       (organization/organization-scope?
        root-scope))
      (is
       (organization/organization-group-scope?
        group-scope))
      (is
       (organization/can-transition-status?
        :active
        :suspend))
      (is
       (false?
        (organization/can-transition-status?
         :closed
         :reactivate))))))

;; =============================================================================
;; Public reads
;; =============================================================================

(deftest public-root-read-test
  (let [ctx
        (base-ctx)

        root-scope
        (organization/organization-scope
         organization-id)

        facts
        (organization/organization-facts
         ctx
         organization-id)

        document
        (organization/require-organization
         ctx
         organization-id)

        context
        (organization/require-organization-context
         ctx
         organization-id)]

    (testing "raw public facts expose the documented found/document envelope"
      (is
       (true?
        (:organization/found? facts)))
      (is
       (organization/organization-document?
        (:organization/doc facts))))

    (testing "the required read returns the persisted Organization through public predicates"
      (is
       (organization/organization-document?
        document))
      (is
       (=
        organization-id
        (organization/organization-id document)))
      (is
       (=
        "Boundary Test Organization"
        (organization/organization-name document)))
      (is
       (organization/organization-active?
        document)))

    (testing "the normalized root context is authoritative and operational"
      (is
       (=
        document
        (:organization context)))
      (is
       (true?
        (:operational? context)))
      (is
       (=
        {:organization/id organization-id
         :scope/target root-scope
         :scope/applicable [root-scope]
         :scope/operational? true}
        (:scope-context context)))
      (is
       (organization/scope-context?
        (:scope-context context))))

    (testing "invalid and missing Organizations fail through the public contract"
      (is
       (=
        :organization.core/invalid-organization-id
        (error-type
         #(organization/require-organization
           ctx
           "not-a-uuid"))))
      (is
       (=
        :organization/not-found
        (error-type
         #(organization/require-organization
           ctx
           missing-organization-id)))))))

;; =============================================================================
;; Public create/read hierarchy workflow
;; =============================================================================

(deftest public-hierarchy-create-and-read-test
  (let [ctx0
        (admin-ctx)

        root-scope
        (organization/organization-scope
         organization-id)

        {ctx1 :ctx
         group :document}
        (create-group!
         ctx0
         root-scope
         "  Operations  ")

        group-id
        (organization/organization-group-id
         group)

        group-scope
        (organization/organization-group-scope
         group-id)

        {ctx2 :ctx
         nested-group :document}
        (create-group!
         ctx1
         group-scope
         "Receiving")

        nested-group-id
        (organization/organization-group-id
         nested-group)

        nested-group-scope
        (organization/organization-group-scope
         nested-group-id)

        {ctx3 :ctx
         location :document}
        (create-location!
         ctx2
         nested-group-scope
         "North Store")

        location-id
        (organization/location-id
         location)

        group-context
        (organization/require-organization-group-context
         ctx3
         {:organization-id organization-id
          :organization-group-id group-id})

        nested-context
        (organization/require-organization-group-context
         ctx3
         {:organization-id organization-id
          :organization-group-id nested-group-id})

        location-context
        (organization/require-location-context
         ctx3
         {:organization-id organization-id
          :location-id location-id})]

    (testing "create results are valid public documents with normalized names"
      (is
       (organization/organization-group-document?
        group))
      (is
       (=
        "Operations"
        (organization/organization-group-name group)))
      (is
       (organization/organization-group-direct-child-of?
        group
        root-scope))
      (is
       (organization/location-document?
        location))
      (is
       (=
        nested-group-scope
        (organization/location-parent-scope location))))

    (testing "a root child has no ancestor groups"
      (is
       (empty?
        (:ancestor-groups group-context)))
      (is
       (=
        [group-scope
         root-scope]
        (get-in
         group-context
         [:scope-context
          :scope/applicable])))
      (is
       (true?
        (:operational? group-context))))

    (testing "nested group ancestry is target-first"
      (is
       (=
        [group-id]
        (document-ids
         (:ancestor-groups nested-context))))
      (is
       (=
        [nested-group-scope
         group-scope
         root-scope]
        (get-in
         nested-context
         [:scope-context
          :scope/applicable]))))

    (testing "Location ancestry and scope order follow the complete hierarchy"
      (is
       (=
        [nested-group-id
         group-id]
        (document-ids
         (:ancestor-groups location-context))))
      (is
       (=
        [(organization/location-scope location-id)
         nested-group-scope
         group-scope
         root-scope]
        (get-in
         location-context
         [:scope-context
          :scope/applicable])))
      (is
       (true?
        (:operational? location-context)))
      (is
       (organization/location-active?
        (:location location-context))))

    (testing "the normalized read enforces Organization ownership"
      (is
       (=
        :organization-group/organization-mismatch
        (error-type
         #(organization/require-organization-group-context
           ctx3
           {:organization-id other-organization-id
            :organization-group-id group-id})))))))

;; =============================================================================
;; Public rename and lifecycle behavior
;; =============================================================================

(deftest public-rename-and-lifecycle-test
  (let [ctx0
        (admin-ctx)

        root-scope
        (organization/organization-scope
         organization-id)

        {ctx1 :ctx
         group :document}
        (create-group!
         ctx0
         root-scope
         "Sales Floor")

        group-id
        (organization/organization-group-id
         group)

        group-scope
        (organization/organization-group-scope
         group-id)

        {ctx2 :ctx
         location :document}
        (create-location!
         ctx1
         group-scope
         "North Store")

        location-id
        (organization/location-id
         location)

        rename-org
        (organization/rename-organization
         ctx2
         {:organization-id organization-id
          :name "  Renamed Organization  "})

        ctx3
        (ctx-after ctx2 rename-org)

        rename-group
        (organization/rename-organization-group
         ctx3
         {:organization-id organization-id
          :organization-group-id group-id
          :name "  Customer Service  "})

        ctx4
        (ctx-after ctx3 rename-group)

        rename-location
        (organization/rename-location
         ctx4
         {:organization-id organization-id
          :location-id location-id
          :name "  Downtown Store  "})

        ctx5
        (ctx-after ctx4 rename-location)]

    (testing "renames commit and are visible through required public reads"
      (is
       (committed? rename-org))
      (is
       (committed? rename-group))
      (is
       (committed? rename-location))
      (is
       (=
        "Renamed Organization"
        (organization/organization-name
         (organization/require-organization
          ctx5
          organization-id))))
      (is
       (=
        "Customer Service"
        (organization/organization-group-name
         (:organization-group
          (organization/require-organization-group-context
           ctx5
           {:organization-id organization-id
            :organization-group-id group-id})))))
      (is
       (=
        "Downtown Store"
        (organization/location-name
         (:location
          (organization/require-location-context
           ctx5
           {:organization-id organization-id
            :location-id location-id}))))))

    (let [suspend-group
          (organization/suspend-organization-group
           ctx5
           {:organization-id organization-id
            :organization-group-id group-id})

          ctx6
          (ctx-after ctx5 suspend-group)

          suspended-group-context
          (organization/require-organization-group-context
           ctx6
           {:organization-id organization-id
            :organization-group-id group-id})

          location-under-suspended-group
          (organization/require-location-context
           ctx6
           {:organization-id organization-id
            :location-id location-id})]

      (testing "suspending a group changes local group state without rewriting its Location"
        (is
         (committed? suspend-group))
        (is
         (organization/organization-group-suspended?
          (:organization-group suspended-group-context)))
        (is
         (false?
          (:operational? suspended-group-context)))
        (is
         (organization/location-active?
          (:location location-under-suspended-group)))
        (is
         (false?
          (:operational? location-under-suspended-group))))

      (let [reactivate-group
            (organization/reactivate-organization-group
             ctx6
             {:organization-id organization-id
              :organization-group-id group-id})

            ctx7
            (ctx-after ctx6 reactivate-group)]

        (testing "reactivating the group restores descendant operational state"
          (is
           (committed? reactivate-group))
          (is
           (true?
            (:operational?
             (organization/require-location-context
              ctx7
              {:organization-id organization-id
               :location-id location-id})))))

        (let [suspend-org
              (organization/suspend-organization
               ctx7
               {:organization-id organization-id})

              ctx8
              (ctx-after ctx7 suspend-org)]

          (testing "Organization suspension makes the descendant hierarchy non-operational"
            (is
             (committed? suspend-org))
            (is
             (organization/organization-suspended?
              (:organization
               (organization/require-organization-context
                ctx8
                organization-id))))
            (is
             (false?
              (:operational?
               (organization/require-location-context
                ctx8
                {:organization-id organization-id
                 :location-id location-id})))))

          (let [reactivate-org
                (organization/reactivate-organization
                 ctx8
                 {:organization-id organization-id})

                ctx9
                (ctx-after ctx8 reactivate-org)

                suspend-location
                (organization/suspend-location
                 ctx9
                 {:organization-id organization-id
                  :location-id location-id})

                ctx10
                (ctx-after ctx9 suspend-location)

                suspended-location-context
                (organization/require-location-context
                 ctx10
                 {:organization-id organization-id
                  :location-id location-id})]

            (testing "local Location lifecycle remains independently visible"
              (is
               (committed? reactivate-org))
              (is
               (committed? suspend-location))
              (is
               (organization/location-suspended?
                (:location suspended-location-context)))
              (is
               (false?
                (:operational? suspended-location-context))))

            (let [reactivate-location
                  (organization/reactivate-location
                   ctx10
                   {:organization-id organization-id
                    :location-id location-id})

                  ctx11
                  (ctx-after ctx10 reactivate-location)]

              (testing "reactivating the Location restores a fully active hierarchy"
                (is
                 (committed? reactivate-location))
                (is
                 (true?
                  (:operational?
                   (organization/require-location-context
                    ctx11
                    {:organization-id organization-id
                     :location-id location-id}))))))))))))

;; =============================================================================
;; Public moves and cycle prevention
;; =============================================================================

(deftest public-move-and-cycle-test
  (let [ctx0
        (admin-ctx)

        root-scope
        (organization/organization-scope
         organization-id)

        {ctx1 :ctx
         group-a :document}
        (create-group!
         ctx0
         root-scope
         "Group A")

        group-a-id
        (organization/organization-group-id
         group-a)

        group-a-scope
        (organization/organization-group-scope
         group-a-id)

        {ctx2 :ctx
         group-b :document}
        (create-group!
         ctx1
         group-a-scope
         "Group B")

        group-b-id
        (organization/organization-group-id
         group-b)

        group-b-scope
        (organization/organization-group-scope
         group-b-id)

        {ctx3 :ctx
         group-c :document}
        (create-group!
         ctx2
         root-scope
         "Group C")

        group-c-id
        (organization/organization-group-id
         group-c)

        group-c-scope
        (organization/organization-group-scope
         group-c-id)

        {ctx4 :ctx
         location :document}
        (create-location!
         ctx3
         group-b-scope
         "Movable Store")

        location-id
        (organization/location-id
         location)

        move-location
        (organization/move-location
         ctx4
         {:organization-id organization-id
          :location-id location-id
          :parent-scope group-c-scope})

        ctx5
        (ctx-after ctx4 move-location)

        moved-location-context
        (organization/require-location-context
         ctx5
         {:organization-id organization-id
          :location-id location-id})]

    (testing "a Location can be moved to another authorized operational branch"
      (is
       (committed? move-location))
      (is
       (=
        group-c-scope
        (organization/location-parent-scope
         (:location moved-location-context))))
      (is
       (=
        [group-c-id]
        (document-ids
         (:ancestor-groups moved-location-context))))
      (is
       (=
        [(organization/location-scope location-id)
         group-c-scope
         root-scope]
        (get-in
         moved-location-context
         [:scope-context
          :scope/applicable]))))

    (testing "moving a group beneath its own descendant is rejected"
      (is
       (=
        :organization-group/cycle
        (error-type
         #(organization/move-organization-group
           ctx5
           {:organization-id organization-id
            :organization-group-id group-a-id
            :parent-scope group-b-scope}))))

      ;; The failed public operation must leave the hierarchy unchanged.
      (let [group-a-context
            (organization/require-organization-group-context
             ctx5
             {:organization-id organization-id
              :organization-group-id group-a-id})

            group-b-context
            (organization/require-organization-group-context
             ctx5
             {:organization-id organization-id
              :organization-group-id group-b-id})]
        (is
         (=
          root-scope
          (organization/organization-group-parent-scope
           (:organization-group group-a-context))))
        (is
         (=
          group-a-scope
          (organization/organization-group-parent-scope
           (:organization-group group-b-context))))))))

;; =============================================================================
;; Public authorization boundary
;; =============================================================================

(deftest public-authorization-boundary-test
  (let [root-scope
        (organization/organization-scope
         organization-id)]

    (testing "a signed-in user is required for mutations"
      (is
       (=
        :organization/not-authenticated
        (error-type
         #(organization/create-organization-group
           (base-ctx)
           {:organization-id organization-id
            :parent-scope root-scope
            :name "Unauthorized Group"})))))

    (testing "authentication alone does not grant administrator authority"
      (is
       (=
        :user/not-authorized
        (error-type
         #(organization/create-organization-group
           (outsider-ctx)
           {:organization-id organization-id
            :parent-scope root-scope
            :name "Unauthorized Group"})))))

    (testing "failed authorization leaves the public hierarchy unchanged"
      (let [context
            (organization/require-organization-context
             (base-ctx)
             organization-id)]
        (is
         (organization/organization-active?
          (:organization context)))
        (is
         (true?
          (:operational? context)))))))
