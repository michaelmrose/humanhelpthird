(ns net.humanhelp.site.model.organization-test
  "Coverage for the rewritten HumanHelp Organization model.

   This namespace tests the five Organization layers together without depending
   on User, Membership, XTDB, or Gesso Live runtime plumbing.

   Persisted reads are represented by an in-memory document store through the
   public gesso.model/load-by-id seam. Everything above that seam is real:
   Organization domain rules, Malli schemas, hierarchy traversal, scope
   contexts, generic guards, transaction fragments, planners, and the public
   core boundary."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.organization.domain :as domain]
   [net.humanhelp.site.model.organization.schema :as organization.schema])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed values
;; =============================================================================

(def organization-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def other-organization-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000002"))

(def missing-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000099"))

(def root-group-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000001"))

(def child-group-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000002"))

(def destination-group-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000003"))

(def cycle-a-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000004"))

(def cycle-b-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000005"))

(def other-group-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000006"))

(def location-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000001"))

(def new-organization-id
  (UUID/fromString
   "40000000-0000-7000-8000-000000000001"))

(def new-group-id
  (UUID/fromString
   "40000000-0000-7000-8000-000000000002"))

(def new-location-id
  (UUID/fromString
   "40000000-0000-7000-8000-000000000003"))

(def actor-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000001"))

(def t-before
  (Instant/parse
   "2026-06-30T23:59:00Z"))

(def t0
  (Instant/parse
   "2026-07-01T00:00:00Z"))

(def t1
  (Instant/parse
   "2026-07-01T00:01:00Z"))

(def t2
  (Instant/parse
   "2026-07-01T00:02:00Z"))

(def organization-scope
  (organization/organization-scope
   organization-id))

(def other-organization-scope
  (organization/organization-scope
   other-organization-id))

(def root-group-scope
  (organization/organization-group-scope
   root-group-id))

(def child-group-scope
  (organization/organization-group-scope
   child-group-id))

(def destination-group-scope
  (organization/organization-group-scope
   destination-group-id))

(def location-scope
  (organization/location-scope
   location-id))

;; =============================================================================
;; Document builders
;; =============================================================================

(defn- after
  [model-command]
  (command/after
   model-command))

(defn- organization-document
  ([]
   (organization-document
    organization-id
    "Human Help"))
  ([id name]
   (after
    (domain/create-organization-command
     {:id   id
      :name name
      :now  t0}))))

(defn- group-document
  ([id parent-scope name]
   (group-document
    organization-id
    id
    parent-scope
    name))
  ([organization-id id parent-scope name]
   (after
    (domain/create-organization-group-command
     {:id              id
      :organization-id organization-id
      :parent-scope    parent-scope
      :name            name
      :now             t0}))))

(defn- location-document
  ([parent-scope]
   (location-document
    organization-id
    location-id
    parent-scope
    "Downtown"))
  ([organization-id id parent-scope name]
   (after
    (domain/create-location-command
     {:id              id
      :organization-id organization-id
      :parent-scope    parent-scope
      :name            name
      :now             t0}))))

(defn- ordinary-hierarchy
  []
  [(organization-document)
   (group-document
    root-group-id
    organization-scope
    "Operations")
   (group-document
    child-group-id
    root-group-scope
    "North Region")
   (group-document
    destination-group-id
    organization-scope
    "South Region")
   (location-document
    child-group-scope)])

(defn- document-key
  [document]
  (cond
    (contains?
     document
     :location/organization)
    [:location
     (:xt/id document)]

    (contains?
     document
     :organization-group/organization)
    [:organization-group
     (:xt/id document)]

    (contains?
     document
     :organization/name)
    [:organization
     (:xt/id document)]

    :else
    (throw
     (ex-info
      "Unknown Organization test document."
      {:document document}))))

(defn- store
  [documents]
  (into
   {}
   (map
    (juxt
     document-key
     identity))
   documents))

(defn- with-store*
  [documents f]
  (let [documents
        (store documents)]
    (with-redefs
     [model/load-by-id
      (fn [descriptor _ctx id]
        (get
         documents
         [(:entity-type descriptor)
          id]))]
      (f))))

(defmacro with-store
  [documents & body]
  `(with-store*
     ~documents
     (fn []
       ~@body)))

;; =============================================================================
;; Test helpers
;; =============================================================================

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

(defn- plan-fragment
  [plan]
  (:transaction-fragment
   plan))

(defn- plan-command
  [plan]
  (first
   (:commands
    (plan-fragment plan))))

(defn- plan-change
  [plan]
  (first
   (:changes
    (plan-fragment plan))))

(defn- guard-targets
  [fragment]
  (mapv
   command/guard-target
   (:guards fragment)))

(defn- normalized-plan
  [plan]
  (model.tx/normalize-plan
   (merge
    (plan-fragment plan)
    (:transaction-options plan))))

(defn- effective-guard-targets
  [plan]
  (let [normalized
        (normalized-plan plan)]
    (guard-targets
     {:guards
      (model.tx/effective-guards
       (:commands normalized)
       (:guards normalized))})))

;; =============================================================================
;; Scope values and context semantics
;; =============================================================================

(deftest scope-values-test
  (testing "scope constructors produce the complete structural vocabulary"
    (is
     (=
      {:scope/type :organization
       :scope/id   organization-id}
      organization-scope))

    (is
     (=
      {:scope/type :organization-group
       :scope/id   root-group-id}
      root-group-scope))

    (is
     (=
      {:scope/type :location
       :scope/id   location-id}
      location-scope)))

  (testing "scope predicates distinguish target and parent scopes"
    (is
     (organization/scope?
      organization-scope))

    (is
     (organization/scope?
      location-scope))

    (is
     (organization/parent-scope?
      organization-scope))

    (is
     (organization/parent-scope?
      root-group-scope))

    (is
     (false?
      (organization/parent-scope?
       location-scope)))

    (is
     (false?
      (organization/scope?
       {:scope/type :unknown
        :scope/id   organization-id}))))

  (testing "scope identity is type plus id"
    (is
     (organization/same-scope?
      root-group-scope
      (organization/organization-group-scope
       root-group-id)))

    (is
     (false?
      (organization/same-scope?
       root-group-scope
       child-group-scope)))))

(deftest scope-context-value-test
  (let [context
        {:organization/id
         organization-id

         :scope/target
         location-scope

         :scope/applicable
         [location-scope
          child-group-scope
          root-group-scope
          organization-scope]

         :scope/operational?
         true}]

    (is
     (organization/scope-context?
      context))

    (is
     (=
      organization-id
      (organization/scope-context-organization-id
       context)))

    (is
     (=
      location-scope
      (organization/scope-context-target
       context)))

    (is
     (organization/scope-context-operational?
      context))

    (is
     (organization/scope-applies?
      context
      root-group-scope))

    (is
     (organization/scope-applies?
      context
      organization-scope))

    (is
     (false?
      (organization/scope-applies?
       context
       destination-group-scope))))

  (testing "context shape rejects the wrong target ordering and wrong root"
    (is
     (false?
      (organization/scope-context?
       {:organization/id    organization-id
        :scope/target       location-scope
        :scope/applicable
        [child-group-scope
         location-scope
         organization-scope]
        :scope/operational? true})))

    (is
     (false?
      (organization/scope-context?
       {:organization/id    organization-id
        :scope/target       location-scope
        :scope/applicable
        [location-scope
         other-organization-scope]
        :scope/operational? true})))))

;; =============================================================================
;; Domain construction, schema, and lifecycle
;; =============================================================================

(deftest persisted-document-schema-test
  (let [organization-document
        (organization-document)

        group
        (group-document
         root-group-id
         organization-scope
         "  Operations  ")

        location
        (location-document
         root-group-scope)]

    (testing "creation normalizes names and produces consistent versioned documents"
      (is
       (=
        "Operations"
        (:organization-group/name group)))

      (is
       (=
        0
        (:organization-group/revision group)))

      (is
       (=
        t0
        (:organization-group/created-at group)))

      (is
       (=
        t0
        (:organization-group/updated-at group)))

      (is
       (domain/organization-document-consistent?
        organization-document))

      (is
       (domain/organization-group-document-consistent?
        group))

      (is
       (domain/location-document-consistent?
        location)))

    (testing "Malli persisted schemas agree with the domain invariants"
      (is
       (m/validate
        organization.schema/organization-document-schema
        organization-document))

      (is
       (m/validate
        organization.schema/organization-group-document-schema
        group))

      (is
       (m/validate
        organization.schema/location-document-schema
        location))

      (is
       (false?
        (m/validate
         organization.schema/location-document-schema
         (assoc
          location
          :unexpected/value
          true)))))

    (testing "invalid lifecycle/version combinations are rejected"
      (is
       (false?
        (domain/organization-document-consistent?
         (assoc
          organization-document
          :organization/status
          :suspended))))

      (is
       (false?
        (domain/location-document-consistent?
         (assoc
          location
          :location/updated-at
          t-before)))))))

(deftest rename-and-versioning-test
  (let [before
        (organization-document)

        renamed
        (domain/rename-organization
         before
         {:name
          "  Human Help Cooperative  "

          :now
          t1})]

    (is
     (=
      "Human Help Cooperative"
      (:organization/name renamed)))

    (is
     (=
      1
      (:organization/revision renamed)))

    (is
     (=
      t1
      (:organization/updated-at renamed)))

    (is
     (=
      :organization/unchanged
      (error-type
       #(domain/rename-organization
         before
         {:name "Human Help"
          :now  t1}))))

    (is
     (=
      :organization/invalid-time
      (error-type
       #(domain/rename-organization
         before
         {:name "Earlier"
          :now  t-before}))))))

(deftest move-transitions-test
  (let [group
        (group-document
         child-group-id
         root-group-scope
         "North Region")

        moved-group
        (domain/move-organization-group
         group
         {:parent-scope
          destination-group-scope

          :now
          t1

          :actor-id
          actor-id

          :reason
          :test/reorganization})

        location
        (location-document
         child-group-scope)

        moved-location
        (domain/move-location
         location
         {:parent-scope
          destination-group-scope

          :now
          t1})]

    (testing "moves update structural parent, audit data, and version"
      (is
       (=
        destination-group-scope
        (organization/organization-group-parent-scope
         moved-group)))

      (is
       (=
        actor-id
        (:organization-group/moved-by
         moved-group)))

      (is
       (=
        :test/reorganization
        (:organization-group/move-reason
         moved-group)))

      (is
       (=
        1
        (:organization-group/revision
         moved-group)))

      (is
       (=
        destination-group-scope
        (organization/location-parent-scope
         moved-location))))

    (testing "local move rules reject no-ops and self-parenting"
      (is
       (=
        :organization-group/parent-unchanged
        (error-type
         #(domain/move-organization-group
           group
           {:parent-scope
            root-group-scope

            :now
            t1}))))

      (is
       (=
        :organization-group/cycle
        (error-type
         #(domain/move-organization-group
           group
           {:parent-scope
            child-group-scope

            :now
            t1})))))))

(deftest lifecycle-transitions-test
  (let [active
        (organization-document)

        suspended
        (domain/suspend-organization
         active
         {:now
          t1

          :actor-id
          actor-id

          :reason
          :test/maintenance})

        reactivated
        (domain/reactivate-organization
         suspended
         {:now t2})

        closed
        (domain/close-organization
         suspended
         {:now
          t2

          :actor-id
          actor-id

          :reason
          :test/closure})]

    (testing "suspend/reactivate maintains lifecycle audit invariants"
      (is
       (organization/organization-suspended?
        suspended))

      (is
       (=
        t1
        (:organization/suspended-at
         suspended)))

      (is
       (organization/organization-active?
        reactivated))

      (is
       (not
        (contains?
         reactivated
         :organization/suspended-at)))

      (is
       (=
        2
        (:organization/revision
         reactivated))))

    (testing "close is terminal and clears suspension state"
      (is
       (organization/organization-closed?
        closed))

      (is
       (=
        t2
        (:organization/closed-at
         closed)))

      (is
       (not
        (contains?
         closed
         :organization/suspended-at)))

      (is
       (=
        :organization/closed
        (error-type
         #(domain/rename-organization
           closed
           {:name
            "Cannot Rename"

            :now
            (Instant/parse
             "2026-07-01T00:03:00Z")})))))))

;; =============================================================================
;; Module and public entity reads
;; =============================================================================

(deftest module-contract-test
  (is
   (map?
    organization/module))

  (is
   (identical?
    organization/schema
    (:schema
     organization/module)))

  (is
   (identical?
    organization/resolvers
    (:biff.graph/resolvers
     organization/module)))

  (doseq [schema-key
          [:organization
           :organization-group
           :location
           :organization/scope-context
           :organization-group/scope-context
           :location/scope-context]]
    (is
     (contains?
      organization/schema
      schema-key))))

(deftest public-entity-read-test
  (let [documents
        (ordinary-hierarchy)]

    (with-store
      documents

      (testing "nullable and required reads use the expected entity"
        (is
         (=
          organization-id
          (organization/organization-id
           (organization/require-organization
            {}
            organization-id))))

        (is
         (=
          root-group-id
          (organization/organization-group-id
           (organization/require-organization-group
            {}
            root-group-id))))

        (is
         (=
          location-id
          (organization/location-id
           (organization/require-location
            {}
            location-id))))

        (is
         (nil?
          (organization/location
           {}
           missing-id))))

      (testing "public reads reject malformed ids and distinguish missing ids"
        (is
         (=
          :organization.core/invalid-location-id
          (error-type
           #(organization/location
             {}
             :not-a-uuid))))

        (is
         (=
          :location/not-found
          (error-type
           #(organization/require-location
             {}
             missing-id))))))))

;; =============================================================================
;; Hierarchy traversal and operational state
;; =============================================================================

(deftest hierarchy-scope-context-test
  (with-store
    (ordinary-hierarchy)

    (let [root-context
          (organization/require-scope-context
           {}
           organization-scope)

          group-context
          (organization/require-scope-context
           {}
           child-group-scope)

          location-context
          (organization/require-scope-context
           {}
           location-scope)]

      (testing "root context contains only the root scope"
        (is
         (=
          [organization-scope]
          (:scope/applicable
           root-context)))

        (is
         (organization/scope-context-operational?
          root-context)))

      (testing "group context is target-first through its parent chain"
        (is
         (=
          [child-group-scope
           root-group-scope
           organization-scope]
          (:scope/applicable
           group-context))))

      (testing "Location context includes every structural ancestor"
        (is
         (=
          [location-scope
           child-group-scope
           root-group-scope
           organization-scope]
          (:scope/applicable
           location-context)))

        (is
         (organization/scope-applies?
          location-context
          root-group-scope))

        (is
         (organization/scope-context-operational?
          location-context))))))

(deftest local-active-versus-effective-operational-test
  (let [organization-document
        (organization-document)

        root-group
        (group-document
         root-group-id
         organization-scope
         "Operations")

        suspended-root
        (domain/suspend-organization-group
         root-group
         {:now t1})

        child
        (group-document
         child-group-id
         root-group-scope
         "North Region")

        location
        (location-document
         child-group-scope)]

    (with-store
      [organization-document
       suspended-root
       child
       location]

      (let [location-context
            (organization/require-scope-context
             {}
             location-scope)]

        (is
         (organization/location-active?
          location))

        (is
         (false?
          (organization/scope-context-operational?
           location-context)))))))

(deftest hierarchy-corruption-test
  (let [organization-document
        (organization-document)

        cycle-a
        (group-document
         cycle-a-id
         (organization/organization-group-scope
          cycle-b-id)
         "Cycle A")

        cycle-b
        (group-document
         cycle-b-id
         (organization/organization-group-scope
          cycle-a-id)
         "Cycle B")]

    (with-store
      [organization-document
       cycle-a
       cycle-b]

      (is
       (=
        :organization.graph/hierarchy-cycle
        (error-type
         #(organization/require-scope-context
           {}
           (organization/organization-group-scope
            cycle-a-id)))))))

  (let [current-organization
        (organization-document)

        other-organization
        (organization-document
         other-organization-id
         "Other Organization")

        foreign-parent
        (group-document
         other-organization-id
         other-group-id
         other-organization-scope
         "Foreign Parent")

        child
        (group-document
         child-group-id
         (organization/organization-group-scope
          other-group-id)
         "Wrong Parent Organization")]

    (with-store
      [current-organization
       other-organization
       foreign-parent
       child]

      (is
       (=
        :organization.graph/cross-organization-parent
        (error-type
         #(organization/require-scope-context
           {}
           child-group-scope)))))))

(deftest missing-scope-test
  (with-store
    []

    (is
     (nil?
      (organization/scope-context
       {}
       (organization/location-scope
        missing-id))))

    (is
     (=
      :organization/scope-not-found
      (error-type
       #(organization/require-scope-context
         {}
         (organization/location-scope
          missing-id)))))))

;; =============================================================================
;; Guarded scope dependencies
;; =============================================================================

(deftest scope-dependency-test
  (with-store
    (ordinary-hierarchy)

    (let [{:keys
           [scope-context
            transaction-fragment]}
          (organization/require-scope-dependency
           {}
           location-scope)]

      (testing "dependency returns the same semantic context as the ordinary read"
        (is
         (=
          (organization/require-scope-context
           {}
           location-scope)
          scope-context)))

      (testing "every persisted document used by the hierarchy is guarded"
        (is
         (=
          [[:location location-id]
           [:organization-group child-group-id]
           [:organization-group root-group-id]
           [:organization organization-id]]
          (guard-targets
           transaction-fragment)))

        (is
         (every?
          command/guard?
          (:guards
           transaction-fragment)))

        (is
         (empty?
          (:commands
           transaction-fragment)))

        (is
         (empty?
          (:changes
           transaction-fragment))))))

  (with-store
    []

    (is
     (nil?
      (organization/scope-dependency
       {}
       (organization/location-scope
        missing-id))))

    (is
     (=
      :organization/scope-not-found
      (error-type
       #(organization/require-scope-dependency
         {}
         (organization/location-scope
          missing-id)))))))

;; =============================================================================
;; Mutation planning
;; =============================================================================

(deftest create-organization-plan-test
  (with-redefs
   [fx/uuid7
    (fn [_seed _now]
      [new-organization-id])]

    (let [plan
          (organization/plan-create-organization
           {:biff.fx/seed 1
            :biff.fx/now  t1}
           {:name
            "  New Organization  "})

          model-command
          (plan-command plan)

          document
          (command/after model-command)]

      (is
       (command/create?
        model-command))

      (is
       (=
        [:organization
         new-organization-id]
        (command/target
         model-command)))

      (is
       (=
        "New Organization"
        (:organization/name document)))

      (is
       (empty?
        (:guards
         (plan-fragment plan))))

      (is
       (=
        {:topic                  :organization
         :id                     new-organization-id
         :change/kind            :created
         :organization/operation :create
         :organization/id        new-organization-id
         :organization/status    :active
         :organization/revision  0}
        (plan-change plan))))))

(deftest create-child-plan-test
  (with-store
    [(organization-document)]

    (with-redefs
     [fx/uuid7
      (fn [_seed _now]
        [new-group-id])]

      (let [plan
            (organization/plan-create-organization-group
             {:biff.fx/seed 2
              :biff.fx/now  t1}
             {:parent-scope
              organization-scope

              :name
              "  Receiving  "})

            model-command
            (plan-command plan)

            document
            (command/after
             model-command)]

        (is
         (command/create?
          model-command))

        (is
         (=
          organization-id
          (:organization-group/organization
           document)))

        (is
         (=
          organization-scope
          (organization/organization-group-parent-scope
           document)))

        (is
         (=
          [[:organization organization-id]]
          (guard-targets
           (plan-fragment plan))))

        (is
         (=
          [[:organization organization-id]]
          (guard-targets
           (normalized-plan plan))))))))

(deftest update-plan-test
  (with-store
    (ordinary-hierarchy)

    (let [plan
          (organization/plan-rename-location
           {:biff.fx/now t1}
           {:location-id location-id
            :name        "  Downtown Store  "
            :actor-id    actor-id})

          model-command
          (plan-command plan)

          raw-fragment
          (plan-fragment plan)

          normalized
          (normalized-plan plan)]

      (testing "planner returns one canonical update command and semantic change"
        (is
         (command/update?
          model-command))

        (is
         (=
          :rename
          (command/operation
           model-command)))

        (is
         (=
          "Downtown Store"
          (:location/name
           (command/after
            model-command))))

        (is
         (=
          :location
          (:topic
           (plan-change plan))))

        (is
         (=
          location-id
          (:id
           (plan-change plan)))))

      (testing "raw planning guards every Organization read dependency"
        (is
         (=
          [[:location location-id]
           [:organization-group child-group-id]
           [:organization-group root-group-id]
           [:organization organization-id]]
          (guard-targets
           raw-fragment))))

      (testing "normalization validates and canonicalizes guards without applying command redundancy"
        (is
         (=
          [[:location location-id]
           [:organization-group child-group-id]
           [:organization-group root-group-id]
           [:organization organization-id]]
          (guard-targets
           normalized))))

      (testing "effective guards remove the dependency already enforced by the update command"
        (is
         (=
          [[:organization-group child-group-id]
           [:organization-group root-group-id]
           [:organization organization-id]]
          (effective-guard-targets
           plan))))

      (testing "transaction-wide dispatch metadata stays outside the fragment"
        (is
         (not
          (contains?
           raw-fragment
           :entry-fn)))

        (is
         (=
          {:coalesce-key
           [:location location-id]}
          ((:entry-fn
            (:transaction-options plan))
           (plan-change plan))))))))

(deftest close-plans-exist-test
  (with-store
    (ordinary-hierarchy)

    (doseq [[planner input entity-type]
            [[organization/plan-close-organization
              {:organization-id organization-id
               :actor-id        actor-id}
              :organization]

             [organization/plan-close-organization-group
              {:organization-group-id child-group-id
               :actor-id              actor-id}
              :organization-group]

             [organization/plan-close-location
              {:location-id location-id
               :actor-id    actor-id}
              :location]]]

      (let [plan
            (planner
             {:biff.fx/now t1}
             input)

            model-command
            (plan-command plan)]

        (is
         (=
          :close
          (command/operation
           model-command)))

        (is
         (=
          entity-type
          (:model/entity-type
           model-command)))))))

;; =============================================================================
;; Move planning and hierarchy safety
;; =============================================================================

(deftest move-plan-test
  (with-store
    (ordinary-hierarchy)

    (let [plan
          (organization/plan-move-location
           {:biff.fx/now t1}
           {:location-id
            location-id

            :parent-scope
            destination-group-scope

            :actor-id
            actor-id

            :reason
            :test/reorganization})

          model-command
          (plan-command plan)

          moved
          (command/after
           model-command)

          raw-targets
          (guard-targets
           (plan-fragment plan))

          normalized-targets
          (guard-targets
           (normalized-plan plan))

          effective-targets
          (effective-guard-targets
           plan)]

      (is
       (=
        destination-group-scope
        (organization/location-parent-scope
         moved)))

      (testing "move depends on both current and destination hierarchy snapshots"
        (is
         (=
          #{[:location location-id]
            [:organization-group child-group-id]
            [:organization-group root-group-id]
            [:organization-group destination-group-id]
            [:organization organization-id]}
          (set raw-targets))))

      (testing "normalization collapses repeated shared guards but retains the command target guard"
        (is
         (=
          #{[:location location-id]
            [:organization-group child-group-id]
            [:organization-group root-group-id]
            [:organization-group destination-group-id]
            [:organization organization-id]}
          (set normalized-targets)))

        (is
         (=
          (count normalized-targets)
          (count
           (set normalized-targets)))))

      (testing "effective guards remove the dependency already enforced by the move command"
        (is
         (=
          #{[:organization-group child-group-id]
            [:organization-group root-group-id]
            [:organization-group destination-group-id]
            [:organization organization-id]}
          (set effective-targets)))))))

(deftest move-rejects-descendant-cycle-test
  (let [organization-document
        (organization-document)

        root
        (group-document
         root-group-id
         organization-scope
         "Root")

        child
        (group-document
         child-group-id
         root-group-scope
         "Child")]

    (with-store
      [organization-document
       root
       child]

      (is
       (=
        :organization-group/cycle
        (error-type
         #(organization/plan-move-organization-group
           {:biff.fx/now t1}
           {:organization-group-id
            root-group-id

            :parent-scope
            child-group-scope})))))))

(deftest move-rejects-other-organization-test
  (let [current-organization
        (organization-document)

        other-organization
        (organization-document
         other-organization-id
         "Other Organization")

        group
        (group-document
         root-group-id
         organization-scope
         "Operations")

        other-group
        (group-document
         other-organization-id
         other-group-id
         other-organization-scope
         "Other Group")]

    (with-store
      [current-organization
       other-organization
       group
       other-group]

      (is
       (=
        :organization/ownership-mismatch
        (error-type
         #(organization/plan-move-organization-group
           {:biff.fx/now t1}
           {:organization-group-id
            root-group-id

            :parent-scope
            (organization/organization-group-scope
             other-group-id)})))))))

(deftest create-and-move-require-operational-destination-test
  (let [organization-document
        (organization-document)

        suspended
        (domain/suspend-organization-group
         (group-document
          root-group-id
          organization-scope
          "Suspended")
         {:now t1})]

    (with-store
      [organization-document
       suspended]

      (with-redefs
       [fx/uuid7
        (fn [_seed _now]
          [new-location-id])]

        (is
         (=
          :organization/scope-not-operational
          (error-type
           #(organization/plan-create-location
             {:biff.fx/seed 3
              :biff.fx/now  t2}
             {:parent-scope
              root-group-scope

              :name
              "Blocked"}))))))))

;; =============================================================================
;; Planner validation
;; =============================================================================

(deftest planner-requires-existing-targets-test
  (with-store
    []

    (is
     (=
      :organization/scope-not-found
      (error-type
       #(organization/plan-rename-location
         {:biff.fx/now t1}
         {:location-id missing-id
          :name        "Missing"}))))

    (is
     (=
      :organization/scope-not-found
      (error-type
       #(organization/plan-create-location
         {:biff.fx/seed 4
          :biff.fx/now  t1}
         {:parent-scope
          (organization/organization-group-scope
           missing-id)

          :name
          "Missing Parent"}))))))

(deftest planner-requires-time-and-seed-test
  (is
   (=
    :organization.fx/missing-now
    (error-type
     #(organization/plan-create-organization
       {:biff.fx/seed 1}
       {:name "No Time"}))))

  (is
   (=
    :organization.fx/missing-seed
    (error-type
     #(organization/plan-create-organization
       {:biff.fx/now t1}
       {:name "No Seed"})))))
