(ns net.humanhelp.site.model.request-test
  "Tests for the rewritten HumanHelp Request model.

   This suite targets the current five-namespace Request model:

     request.domain
     request.schema
     request.graph
     request.fx
     request.core

   RequestAssignment is tested as a Request-owned persisted entity rather than
   as a separate top-level model.

   Most tests use an in-memory Request persistence seam by redefining only
   gesso.model/Biff query reads. Cross-model authorization is stubbed strictly
   through User, Organization, and Membership public core APIs. Real XTDB/Gesso
   Live commit behavior belongs to request-integration-test."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.fx :as fx]
   [com.biffweb.xtdb :as biff.xtdb]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain :as domain]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.request.schema :as request.schema]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed identities and times
;; =============================================================================

(def request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def other-request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000002"))

(def third-request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000003"))

(def missing-request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000099"))

(def organization-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000001"))

(def other-organization-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000002"))

(def location-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000001"))

(def other-location-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000002"))

(def requestor-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000001"))

(def helper-a-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000002"))

(def helper-b-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000003"))

(def manager-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000004"))

(def capability-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000005"))

(def primary-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000001"))

(def collaborator-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000002"))

(def other-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000003"))

(def generated-request-id
  (UUID/fromString
   "70000000-0000-7000-8000-000000000001"))

(def generated-assignment-id
  (UUID/fromString
   "70000000-0000-7000-8000-000000000002"))

(def generated-other-assignment-id
  (UUID/fromString
   "70000000-0000-7000-8000-000000000003"))

(def helper-membership-id
  (UUID/fromString
   "80000000-0000-0000-0000-000000000001"))

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

(def t3
  (Instant/parse
   "2026-07-01T00:03:00Z"))

(def canonical-content
  {:title
   "Need help"

   :details
   "Please help me find the right item."

   :location-detail
   "Near the front desk"})

;; =============================================================================
;; Test helpers
;; =============================================================================

(defn- after
  [model-command]
  (command/after
   model-command))

(defn- error-type
  [f]
  (try
    (f)
    ::did-not-throw

    (catch Throwable error
      (loop [error
             error]
        (when
         error
          (or
           (:error/type
            (ex-data
             error))

           (recur
            (ex-cause
             error))))))))

(defn- plan-fragment
  [plan]
  (:transaction-fragment
   plan))

(defn- plan-commands
  [plan]
  (:commands
   (plan-fragment
    plan)))

(defn- plan-command
  [plan]
  (first
   (plan-commands
    plan)))

(defn- plan-changes
  [plan]
  (:changes
   (plan-fragment
    plan)))

(defn- plan-change
  [plan]
  (first
   (plan-changes
    plan)))

(defn- plan-assertions
  [plan]
  (:assertions
   (plan-fragment
    plan)))

(defn- normalized-plan
  [plan]
  (model.tx/normalize-plan
   (merge
    (plan-fragment
     plan)

    (:transaction-options
     plan))))

(defn- guard-targets
  [fragment]
  (mapv
   command/guard-target
   (:guards
    fragment)))

;; =============================================================================
;; Canonical Request and RequestAssignment fixtures
;; =============================================================================

(defn- request-document
  ([]
   (request-document
    {}))

  ([overrides]
   (let [id
         (get
          overrides
          :id
          request-id)

         org-id
         (get
          overrides
          :organization-id
          organization-id)

         loc-id
         (get
          overrides
          :location-id
          location-id)

         requestor
         (get
          overrides
          :requestor
          (domain/user-requestor
           requestor-id))

         content
         (get
          overrides
          :content
          canonical-content)

         now
         (get
          overrides
          :now
          t0)

         document
         (after
          (domain/create-request-command
           {:id
            id

            :organization-id
            org-id

            :location-id
            loc-id

            :requestor
            requestor

            :content
            content

            :now
            now}))

         status
         (:status
          overrides)]

     (case
      status

       nil
       document

       :open
       document

       :claimed
       (after
        (domain/claim-request-command
         document
         {:now
          (get
           overrides
           :transition-time
           t1)}))

       :on-the-way
       (let [claimed
             (after
              (domain/claim-request-command
               document
               {:now
                (get
                 overrides
                 :claim-time
                 t1)}))]
         (after
          (domain/mark-on-the-way-command
           claimed
           {:now
            (get
             overrides
             :transition-time
             t2)})))

       :done
       (let [claimed
             (after
              (domain/claim-request-command
               document
               {:now
                (get
                 overrides
                 :claim-time
                 t1)}))]
         (after
          (domain/complete-request-command
           claimed
           {:now
            (get
             overrides
             :transition-time
             t2)})))

       :cancelled
       (after
        (domain/cancel-request-command
         document
         {:now
          (get
           overrides
           :transition-time
           t1)

          :reason
          (get
           overrides
           :reason
           :test/cancelled)}))))))

(defn- assignment-document
  ([]
   (assignment-document
    {}))

  ([overrides]
   (let [document
         (after
          (domain/create-assignment-command
           {:id
            (get
             overrides
             :id
             primary-assignment-id)

            :request-id
            (get
             overrides
             :request-id
             request-id)

            :helper-id
            (get
             overrides
             :helper-id
             helper-a-id)

            :role
            (get
             overrides
             :role
             :primary)

            :source
            (get
             overrides
             :source
             :test/assignment)

            :actor-id
            (get
             overrides
             :actor-id
             manager-id)

            :now
            (get
             overrides
             :now
             t1)}))]

     (if
      (:ended?
       overrides)
       (after
        (domain/end-assignment-command
         document
         {:actor-id
          (get
           overrides
           :ended-by
           manager-id)

          :reason
          (get
           overrides
           :end-reason
           :test/ended)

          :now
          (get
           overrides
           :ended-at
           t2)}))
       document))))

;; =============================================================================
;; In-memory Request persistence seam
;; =============================================================================

(defn- document-entity-type
  [document]
  (cond
    (contains?
     document
     :request/status)
    :request

    (contains?
     document
     :request-assignment/status)
    :request-assignment

    :else
    nil))

(defn- predicate-match?
  [document predicate]
  (if
   (nil?
    predicate)
    true

    (let [[operator
           & arguments]
          predicate]

      (case
       operator

        :=
        (let [[field
               expected]
              arguments]
          (=
           expected
           (get
            document
            field)))

        :and
        (every?
         #(predicate-match?
           document
           %)
         arguments)

        :or
        (some
         #(predicate-match?
           document
           %)
         arguments)

        (throw
         (ex-info
          "Unsupported test query predicate."
          {:predicate
           predicate}))))))

(defn- with-request-store*
  [documents f]
  (let [documents
        (vec
         documents)]

    (with-redefs
     [model/load-by-id
      (fn [descriptor _ctx id]
        (let [entity-type
              (:entity-type
               descriptor)]
          (some
           (fn [document]
             (when
              (and
               (=
                entity-type
                (document-entity-type
                 document))

               (=
                id
                (:xt/id
                 document)))
               document))
           documents)))

      biff.xtdb/q
      (fn [_ctx query]
        (let [entity-type
              (first
               (:from
                query))

              where
              (:where
               query)]
          (->>
           documents

           (filter
            #(=
              entity-type
              (document-entity-type
               %)))

           (filter
            #(predicate-match?
              %
              where))

           vec)))]

      (f))))

(defmacro with-request-store
  [documents & body]
  `(with-request-store*
     ~documents
     (fn []
       ~@body)))

;; =============================================================================
;; Foreign-model dependency seam
;; =============================================================================

(defn- scope-context
  [{:keys
    [scope-organization-id
     scope-target
     operational?]
    :or
    {scope-organization-id
     organization-id

     operational?
     true}}]
  (let [target
        (or
         scope-target
         (organization/location-scope
          location-id))]
    {:organization/id
     scope-organization-id

     :scope/target
     target

     :scope/applicable
     [target
      (organization/organization-scope
       scope-organization-id)]

     :scope/operational?
     operational?}))

(defn- user-dependency
  [user-id active?]
  {:user
   {:xt/id
    user-id

    :user/status
    (if
     active?
      :active
      :suspended)}

   :transaction-fragment
   model.tx/empty-fragment})

(defn- helper-dependency
  [helper-id skills]
  {:membership
   {:xt/id
    helper-membership-id

    :membership/user
    helper-id

    :membership/organization
    organization-id

    :membership/status
    :active

    :membership/skills
    (set
     skills)}

   :transaction-fragment
   model.tx/empty-fragment})

(defn- with-authorization*
  [{:keys
    [generated-id
     operational?
     scope-organization-id
     scope-target
     active-user?
     helper-ids
     helper-skills
     manager-role]
    :or
    {generated-id
     generated-assignment-id

     operational?
     true

     active-user?
     true

     helper-ids
     #{helper-a-id
       helper-b-id}

     helper-skills
     {}

     manager-role
     nil}}
   f]
  (with-redefs
   [fx/uuid7
    (fn [_seed _now]
      [generated-id])

    organization/require-scope-dependency
    (fn [_ctx scope]
      {:scope-context
       (scope-context
        {:scope-organization-id
         (or
          scope-organization-id
          organization-id)

         :scope-target
         (or
          scope-target
          scope)

         :operational?
         operational?})

       :transaction-fragment
       model.tx/empty-fragment})

    user/require-user-dependency
    (fn [_ctx user-id]
      (user-dependency
       user-id
       active-user?))

    membership/require-helper-dependency
    (fn [_ctx helper-id _scope]
      (if
       (contains?
        helper-ids
        helper-id)
        (helper-dependency
         helper-id
         (get
          helper-skills
          helper-id
          #{}))

        (throw
         (ex-info
          "Helper authority missing."
          {:error/type
           :membership/helper-required

           :user/id
           helper-id}))))

    membership/supervisor-dependency
    (fn [_ctx _actor-id _scope]
      (when
       (=
        :supervisor
        manager-role)
        {:transaction-fragment
         model.tx/empty-fragment}))

    membership/admin-dependency
    (fn [_ctx _actor-id _scope]
      (when
       (=
        :admin
        manager-role)
        {:transaction-fragment
         model.tx/empty-fragment}))

    membership/membership-has-skill?
    (fn [membership-document skill]
      (contains?
       (:membership/skills
        membership-document)
       (membership/normalize-skill
        skill)))]

    (f)))

(defmacro with-authorization
  [options & body]
  `(with-authorization*
     ~options
     (fn []
       ~@body)))

;; =============================================================================
;; Requestor and content values
;; =============================================================================

(deftest requestor-value-test
  (let [user-requestor
        (request/user-requestor
         requestor-id)

        capability-requestor
        (request/capability-requestor
         capability-id)]

    (is
     (request/requestor-reference?
      user-requestor))

    (is
     (request/user-requestor?
      user-requestor))

    (is
     (request/capability-requestor?
      capability-requestor))

    (is
     (false?
      (request/requestor-reference?
       {:requestor/type
        :user})))

    (is
     (false?
      (request/requestor-reference?
       {:requestor/type
        :unknown

        :requestor/id
        requestor-id})))))

(deftest content-normalization-and-validation-test
  (is
   (=
    {:title
     "Need help"

     :details
     nil

     :location-detail
     "Aisle 4"}
    (request/normalize-content
     {:title
      "  Need help  "

      :details
      "   "

      :location-detail
      "  Aisle 4  "})))

  (is
   (request/valid-content?
    canonical-content))

  (is
   (=
    #{:title}
    (set
     (keys
      (request/content-errors
       {:title
        "   "})))))

  (is
   (contains?
    (request/content-errors
     {:title
      "Valid"

      :details
      (apply
       str
       (repeat
        (inc
         request/details-max)
        "x"))})
    :details)))

;; =============================================================================
;; Request domain and schema
;; =============================================================================

(deftest request-create-and-schema-test
  (let [document
        (request-document)]

    (is
     (=
      request-id
      (request/request-id
       document)))

    (is
     (=
      organization-id
      (request/organization-id
       document)))

    (is
     (=
      location-id
      (request/location-id
       document)))

    (is
     (request/requested-by-user?
      document
      requestor-id))

    (is
     (request/open?
      document))

    (is
     (request/active?
      document))

    (is
     (false?
      (request/terminal?
       document)))

    (is
     (=
      0
      (request/revision
       document)))

    (is
     (=
      t0
      (request/created-at
       document)))

    (is
     (request/request-document?
      document))

    (is
     (m/validate
      request.schema/request-document-schema
      document))

    (is
     (false?
      (m/validate
       request.schema/request-document-schema
       (assoc
        document
        :unexpected/value
        true))))))

(deftest request-create-validation-test
  (doseq
   [input
    [{:organization-id
      organization-id

      :location-id
      location-id

      :requestor
      (domain/user-requestor
       requestor-id)

      :content
      canonical-content

      :now
      t0}

     {:id
      request-id

      :organization-id
      organization-id

      :location-id
      location-id

      :requestor
      {:requestor/type
       :bad

       :requestor/id
       requestor-id}

      :content
      canonical-content

      :now
      t0}

     {:id
      request-id

      :organization-id
      organization-id

      :location-id
      location-id

      :requestor
      (domain/user-requestor
       requestor-id)

      :content
      {:title
       " "}

      :now
      t0}

     {:id
      request-id

      :organization-id
      organization-id

      :location-id
      location-id

      :requestor
      (domain/user-requestor
       requestor-id)

      :content
      canonical-content}]]

    (is
     (=
      :request/invalid-create-input
      (error-type
       #(domain/create-request-command
         input))))))

(deftest request-edit-command-test
  (let [original
        (request-document)

        model-command
        (domain/edit-request-command
         original
         {:content
          {:title
           "  Updated title  "

           :details
           " "

           :location-detail
           "  Customer service  "}

          :now
          t1})

        changed
        (after
         model-command)]

    (is
     (command/update?
      model-command))

    (is
     (=
      :edit
      (command/operation
       model-command)))

    (is
     (=
      {:title
       "Updated title"

       :details
       nil

       :location-detail
       "Customer service"}
      (request/content
       changed)))

    (is
     (=
      1
      (request/revision
       changed)))

    (is
     (=
      t1
      (request/updated-at
       changed)))

    (is
     (=
      :request/unchanged
      (error-type
       #(domain/edit-request-command
         original
         {:content
          canonical-content

          :now
          t1}))))

    (is
     (=
      :request/invalid-time
      (error-type
       #(domain/edit-request-command
         original
         {:content
          {:title
           "Changed"}

          :now
          t-before}))))))

(deftest request-lifecycle-command-test
  (let [open
        (request-document)

        claim-command
        (domain/claim-request-command
         open
         {:now
          t1})

        claimed
        (after
         claim-command)

        on-the-way-command
        (domain/mark-on-the-way-command
         claimed
         {:now
          t2})

        on-the-way
        (after
         on-the-way-command)

        complete-command
        (domain/complete-request-command
         on-the-way
         {:now
          t3})

        done
        (after
         complete-command)

        unclaimed
        (after
         (domain/unclaim-request-command
          claimed
          {:now
           t2}))

        cancelled
        (after
         (domain/cancel-request-command
          open
          {:now
           t1

           :reason
           :test/no-longer-needed}))]

    (testing
     "claim establishes the assignment-requiring lifecycle"
      (is
       (request/claimed?
        claimed))

      (is
       (request/lifecycle-expects-primary-assignment?
        claimed))

      (is
       (=
        t1
        (:request/claimed-at
         claimed))))

    (testing
     "on-the-way preserves claim history"
      (is
       (request/on-the-way?
        on-the-way))

      (is
       (=
        t1
        (:request/claimed-at
         on-the-way)))

      (is
       (=
        t2
        (:request/on-the-way-at
         on-the-way))))

    (testing
     "completion is terminal"
      (is
       (request/done?
        done))

      (is
       (request/terminal?
        done))

      (is
       (=
        t3
        (:request/completed-at
         done))))

    (testing
     "unclaim returns to a clean open state"
      (is
       (request/open?
        unclaimed))

      (is
       (nil?
        (:request/claimed-at
         unclaimed)))

      (is
       (nil?
        (:request/on-the-way-at
         unclaimed))))

    (testing
     "open cancellation records optional reason"
      (is
       (request/cancelled?
        cancelled))

      (is
       (=
        :test/no-longer-needed
        (:request/cancellation-reason
         cancelled))))

    (testing
     "invalid transitions are rejected"
      (is
       (=
        :request/invalid-transition
        (error-type
         #(domain/complete-request-command
           open
           {:now
            t1}))))

      (is
       (=
        :request/invalid-transition
        (error-type
         #(domain/claim-request-command
           claimed
           {:now
            t2})))))))

(deftest request-document-corruption-test
  (let [open
        (request-document)

        claimed
        (request-document
         {:status
          :claimed})]

    (doseq
     [corrupt
      [(assoc
        open
        :request/status
        :claimed)

       (assoc
        open
        :request/completed-at
        t0)

       (assoc
        claimed
        :request/on-the-way-at
        t0)

       (assoc
        open
        :request/revision
        -1)

       (assoc
        open
        :request/updated-at
        t-before)]]

      (is
       (false?
        (request/request-document?
         corrupt))))))

;; =============================================================================
;; RequestAssignment domain and schema
;; =============================================================================

(deftest assignment-create-and-schema-test
  (let [assignment
        (assignment-document)]

    (is
     (=
      primary-assignment-id
      (request/assignment-id
       assignment)))

    (is
     (=
      request-id
      (request/assignment-request-id
       assignment)))

    (is
     (=
      helper-a-id
      (request/assignment-helper-id
       assignment)))

    (is
     (request/active-primary-assignment?
      assignment))

    (is
     (=
      t1
      (request/assignment-assigned-at
       assignment)))

    (is
     (=
      (request/assignment-created-at
       assignment)
      (request/assignment-assigned-at
       assignment)))

    (is
     (request/assignment-document?
      assignment))

    (is
     (m/validate
      request.schema/request-assignment-document-schema
      assignment))

    (is
     (not
      (contains?
       assignment
       :request-assignment/assigned-at)))))

(deftest assignment-end-command-test
  (let [active
        (assignment-document)

        model-command
        (domain/end-assignment-command
         active
         {:actor-id
          manager-id

          :reason
          :test/reassigned

          :now
          t2})

        ended
        (after
         model-command)]

    (is
     (command/update?
      model-command))

    (is
     (=
      :end
      (command/operation
       model-command)))

    (is
     (request/assignment-ended?
      ended))

    (is
     (=
      manager-id
      (request/assignment-ended-by
       ended)))

    (is
     (=
      :test/reassigned
      (request/assignment-end-reason
       ended)))

    (is
     (=
      t2
      (request/assignment-ended-at
       ended)))

    (is
     (=
      1
      (request/assignment-revision
       ended)))

    (is
     (=
      :request-assignment/already-ended
      (error-type
       #(domain/end-assignment-command
         ended
         {:reason
          :test/again

          :now
          t3}))))

    (is
     (=
      :request-assignment/invalid-end-input
      (error-type
       #(domain/end-assignment-command
         active
         {:reason
          :not-qualified

          :now
          t2}))))))

(deftest assignment-collection-facts-test
  (let [primary
        (assignment-document)

        collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator})

        ended
        (assignment-document
         {:id
          other-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator

          :ended?
          true})

        assignments
        [primary
         collaborator
         ended]]

    (is
     (=
      [primary
       collaborator]
      (request/active-assignments
       assignments)))

    (is
     (=
      [ended]
      (request/ended-assignments
       assignments)))

    (is
     (=
      primary
      (request/active-primary-assignment
       assignments)))

    (is
     (=
      collaborator
      (domain/active-assignment-for-helper
       assignments
       helper-b-id)))

    (is
     (=
      #{helper-a-id
        helper-b-id}
      (request/active-helper-ids
       assignments)))

    (is
     (=
      #{helper-b-id}
      (request/active-collaborator-helper-ids
       assignments)))))

(deftest assignment-collection-corruption-test
  (let [primary-a
        (assignment-document)

        primary-b
        (assignment-document
         {:id
          other-assignment-id

          :helper-id
          helper-b-id

          :role
          :primary})

        collaborator-same-helper
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-a-id

          :role
          :collaborator})]

    (is
     (=
      :request-assignment/ambiguous-primary
      (error-type
       #(request/active-primary-assignment
         [primary-a
          primary-b]))))

    (is
     (=
      :request-assignment/ambiguous-helper
      (error-type
       #(domain/active-assignment-for-helper
         [primary-a
          collaborator-same-helper]
         helper-a-id))))))

;; =============================================================================
;; Descriptors, generated schema, and module
;; =============================================================================

(deftest descriptor-and-module-test
  (testing
   "both persisted Request entities are valid gesso.model descriptors"
    (is
     (model/descriptor?
      request.schema/request-descriptor))

    (is
     (model/descriptor?
      request.schema/request-assignment-descriptor))

    (is
     (=
      [:request
       :request-assignment]
      (mapv
       :entity-type
       request.schema/descriptors))))

  (testing
   "public schema contains generated and Request-specific Graph values"
    (doseq
     [key
      [:request
       :request/id
       :request/doc
       :request/found?
       :request/organization-id
       :request/location-id
       :request/status
       :request/open?
       :request/active-helper-ids
       :request-assignment
       :request-assignment/id
       :request-assignment/doc
       :request-assignment/found?
       :request-assignment/helper-id
       :request-assignment/assigned-at
       :request-assignment/active?]]

      (is
       (contains?
        request/schema
        key))))

  (testing
   "Request contributes no generated mutation FX handlers"
    (is
     (false?
      (contains?
       request/module
       :biff.fx/handlers))))

  (testing
   "module contains four generated entity resolvers plus three custom resolvers"
    (let [expected-generated
          #{(model/by-id-resolver-id
             request.schema/request-descriptor)

            (model/fields-resolver-id
             request.schema/request-descriptor)

            (model/by-id-resolver-id
             request.schema/request-assignment-descriptor)

            (model/fields-resolver-id
             request.schema/request-assignment-descriptor)}

          actual
          (set
           (map
            :biff.graph/id
            request/resolvers))]

      (is
       (every?
        actual
        expected-generated))

      (is
       (contains?
        actual
        (:biff.graph/id
         request.graph/request-lifecycle-resolver)))

      (is
       (contains?
        actual
        (:biff.graph/id
         request.graph/request-assignment-derived-resolver)))

      (is
       (contains?
        actual
        (:biff.graph/id
         request.graph/request-assignment-summary-resolver)))

      (is
       (=
        7
        (count
         request/resolvers))))))

;; =============================================================================
;; Public reads and Request-owned Graph collections
;; =============================================================================

(deftest core-read-api-test
  (let [request-document
        (request-document)

        assignment
        (assignment-document)]

    (with-request-store
      [request-document
       assignment]

      (is
       (=
        request-document
        (request/request
         {}
         request-id)))

      (is
       (=
        request-document
        (request/require-request
         {}
         request-id)))

      (is
       (nil?
        (request/request
         {}
         missing-request-id)))

      (is
       (=
        :request/not-found
        (error-type
         #(request/require-request
           {}
           missing-request-id))))

      (is
       (=
        assignment
        (request/assignment
         {}
         primary-assignment-id)))

      (is
       (=
        :request-assignment/not-found
        (error-type
         #(request/require-assignment
           {}
           other-assignment-id)))))))

(deftest location-request-collection-test
  (let [old-open
        (request-document
         {:id
          request-id

          :now
          t0})

        newer-open
        (request-document
         {:id
          other-request-id

          :now
          t1})

        terminal
        (request-document
         {:id
          third-request-id

          :now
          t0

          :status
          :cancelled

          :transition-time
          t2})

        foreign
        (request-document
         {:id
          missing-request-id

          :organization-id
          other-organization-id

          :location-id
          other-location-id

          :now
          t0})]

    (with-request-store
      [old-open
       newer-open
       terminal
       foreign]

      (is
       (=
        [other-request-id
         request-id]
        (mapv
         request/request-id
         (request/requests-for-location
          {:biff.xtdb/node
           ::test-node}
          {:organization-id
           organization-id

           :location-id
           location-id}))))

      (is
       (=
        #{request-id
          other-request-id
          third-request-id}
        (set
         (map
          request/request-id
          (request/requests-for-location
<<<<<<< HEAD
           {}
=======
           {:biff.xtdb/node
            ::test-node}
>>>>>>> biff2-migration
           {:organization-id
            organization-id

            :location-id
            location-id

            :include-terminal?
            true}))))))))

(deftest assignment-read-collection-test
  (let [primary
        (assignment-document
         {:now
          t1})

        collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator

          :now
          t2})

        ended
        (assignment-document
         {:id
          other-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator

          :now
          t0

          :ended?
          true

          :ended-at
          t1})]

    (with-request-store
      [primary
       collaborator
       ended]

      (is
       (=
        [other-assignment-id
         primary-assignment-id
         collaborator-assignment-id]
        (mapv
         request/assignment-id
         (request/assignments-for-request
<<<<<<< HEAD
          {}
=======
          {:biff.xtdb/node
           ::test-node}
>>>>>>> biff2-migration
          request-id))))

      (is
       (=
        [primary-assignment-id
         collaborator-assignment-id]
        (mapv
         request/assignment-id
         (request/active-assignments-for-request
<<<<<<< HEAD
          {}
=======
          {:biff.xtdb/node
           ::test-node}
>>>>>>> biff2-migration
          request-id))))

      (is
       (=
        collaborator
        (request/active-assignment-for-helper
<<<<<<< HEAD
         {}
=======
         {:biff.xtdb/node
          ::test-node}
>>>>>>> biff2-migration
         request-id
         helper-b-id)))

      (is
       (=
        primary
        (request/active-primary-assignment-for-request
<<<<<<< HEAD
         {}
=======
         {:biff.xtdb/node
          ::test-node}
>>>>>>> biff2-migration
         request-id))))))

;; =============================================================================
;; Request aggregate snapshots and corruption
;; =============================================================================

(deftest request-snapshot-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)

        collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator})]

    (with-request-store
      [claimed
       primary
       collaborator]

      (let [snapshot
            (request/require-request-snapshot
<<<<<<< HEAD
             {}
=======
             {:biff.xtdb/node
              ::test-node}
>>>>>>> biff2-migration
             request-id)]

        (is
         (=
          claimed
          (:request
           snapshot)))

        (is
         (=
          primary
          (:primary-assignment
           snapshot)))

        (is
         (=
          #{helper-a-id
            helper-b-id}
          (:active-helper-ids
           snapshot)))

        (is
         (=
          #{helper-b-id}
          (:active-collaborator-helper-ids
           snapshot)))))))

(deftest request-snapshot-lifecycle-corruption-test
  (testing
   "claimed Request requires a primary"
    (with-request-store
      [(request-document
        {:status
         :claimed})]

      (is
       (=
        :request.graph/missing-active-primary
        (error-type
         #(request/require-request-snapshot
<<<<<<< HEAD
           {}
=======
           {:biff.xtdb/node
            ::test-node}
>>>>>>> biff2-migration
           request-id))))))

  (testing
   "open Request may not retain active assignments"
    (with-request-store
      [(request-document)
       (assignment-document)]

      (is
       (=
        :request.graph/open-request-has-active-assignments
        (error-type
         #(request/require-request-snapshot
<<<<<<< HEAD
           {}
=======
           {:biff.xtdb/node
            ::test-node}
>>>>>>> biff2-migration
           request-id))))))

  (testing
   "terminal Request may not retain active assignments"
    (with-request-store
      [(request-document
        {:status
         :done})
       (assignment-document)]

      (is
       (=
        :request.graph/terminal-request-has-active-assignments
        (error-type
         #(request/require-request-snapshot
<<<<<<< HEAD
           {}
=======
           {:biff.xtdb/node
            ::test-node}
>>>>>>> biff2-migration
           request-id)))))))

(deftest request-snapshot-assignment-corruption-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)

        duplicate-helper
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-a-id

          :role
          :collaborator})

        duplicate-primary
        (assignment-document
         {:id
          other-assignment-id

          :helper-id
          helper-b-id

          :role
          :primary})]

    (testing
     "the same helper may not have two active assignments"
      (with-request-store
        [claimed
         primary
         duplicate-helper]

        (is
         (=
          :request.graph/duplicate-active-helper
          (error-type
           #(request/require-request-snapshot
<<<<<<< HEAD
             {}
=======
             {:biff.xtdb/node
              ::test-node}
>>>>>>> biff2-migration
             request-id))))))

    (testing
     "direct active-primary lookup detects persisted duplicate primaries"
      (with-request-store
        [claimed
         primary
         duplicate-primary]

        (is
         (=
          :request.graph/non-unique-active-primary
          (error-type
           #(request/active-primary-assignment-for-request
<<<<<<< HEAD
             {}
=======
             {:biff.xtdb/node
              ::test-node}
>>>>>>> biff2-migration
             request-id))))))))

;; =============================================================================
;; Request guarded dependencies
;; =============================================================================

(deftest request-dependency-test
  (let [document
        (request-document)]

    (with-request-store
      [document]

      (let [{loaded
             :request

             fragment
             :transaction-fragment}
            (request/require-request-dependency
             {}
             request-id)]

        (is
         (=
          document
          loaded))

        (is
         (=
          [[:request
            request-id]]
          (guard-targets
           fragment)))

        (is
         (empty?
          (:commands
           fragment)))

        (is
         (empty?
          (:changes
           fragment))))

      (is
       (nil?
        (request/request-dependency
         {}
         missing-request-id)))

      (is
       (=
        :request/not-found
        (error-type
         #(request/require-request-dependency
           {}
           missing-request-id)))))))

(deftest request-snapshot-dependency-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)

        collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator})]

    (with-request-store
      [claimed
       primary
       collaborator]

      (let [dependency
            (request/require-request-snapshot-dependency
<<<<<<< HEAD
             {}
=======
             {:biff.xtdb/node
              ::test-node}
>>>>>>> biff2-migration
             request-id)]

        (is
         (=
          #{[:request
             request-id]

            [:request-assignment
             primary-assignment-id]

            [:request-assignment
             collaborator-assignment-id]}
          (set
           (guard-targets
            (:transaction-fragment
             dependency)))))

        (is
         (=
          primary
          (:primary-assignment
           dependency)))))))

;; =============================================================================
;; Request creation planning
;; =============================================================================

(deftest create-request-plan-test
  (with-authorization
    {:generated-id
     generated-request-id}

    (let [plan
          (request/plan-create-request
           {:current-user/id
            requestor-id

            :biff.fx/seed
            9

            :biff.fx/now
            t1}

           {:organization-id
            organization-id

            :location-id
            location-id

            :content
            {:title
             "  Need help  "

             :details
             "  Details  "}})

          model-command
          (plan-command
           plan)

          document
          (after
           model-command)

          change
          (plan-change
           plan)]

      (is
       (command/create?
        model-command))

      (is
       (=
        [:request
         generated-request-id]
        (command/target
         model-command)))

      (is
       (=
        requestor-id
        (request/requestor-id
         document)))

      (is
       (request/open?
        document))

      (is
       (=
        "Need help"
        (:request/title
         document)))

      (is
       (=
        document
        (get-in
         plan
         [:result
          :request])))

      (is
       (empty?
        (plan-assertions
         plan)))

      (is
       (=
        {:topic
         :request

         :id
         generated-request-id

         :change/kind
         :created

         :request/operation
         :create

         :request/id
         generated-request-id

         :request/organization-id
         organization-id

         :request/location-id
         location-id

         :request/requestor-type
         :user

         :request/requestor-id
         requestor-id

         :request/status
         :open

         :request/revision
         0}
        change))

      (is
       (=
        {:coalesce-key
         [:request
          generated-request-id]}
        ((:entry-fn
          (:transaction-options
           plan))
         change)))

      (is
       (map?
        (normalized-plan
         plan))))))

(deftest create-request-planner-validation-test
  (is
   (=
    :request.fx/missing-now
    (error-type
     #(request/plan-create-request
       {:current-user/id
        requestor-id

        :biff.fx/seed
        1}

       {:organization-id
        organization-id

        :location-id
        location-id

        :content
        canonical-content}))))

  (with-authorization
    {}

    (is
     (=
      :request.fx/missing-seed
      (error-type
       #(request/plan-create-request
         {:current-user/id
          requestor-id

          :biff.fx/now
          t1}

         {:organization-id
          organization-id

          :location-id
          location-id

          :content
          canonical-content})))))

  (with-authorization
    {:active-user?
     false

     :generated-id
     generated-request-id}

    (is
     (=
      :user/not-active
      (error-type
       #(request/plan-create-request
         {:current-user/id
          requestor-id

          :biff.fx/seed
          1

          :biff.fx/now
          t1}

         {:organization-id
          organization-id

          :location-id
          location-id

          :content
          canonical-content})))))

  (with-authorization
    {:operational?
     false

     :generated-id
     generated-request-id}

    (is
     (=
      :request/location-not-operational
      (error-type
       #(request/plan-create-request
         {:current-user/id
          requestor-id

          :biff.fx/seed
          1

          :biff.fx/now
          t1}

         {:organization-id
          organization-id

          :location-id
          location-id

          :content
          canonical-content}))))))

;; =============================================================================
;; Owner planning
;; =============================================================================

(deftest edit-request-plan-test
  (let [document
        (request-document)]

    (with-request-store
      [document]

      (with-authorization
        {}

        (let [plan
              (request/plan-edit-request
               {:current-user/id
                requestor-id

                :biff.fx/now
                t1}

               {:request-id
                request-id

                :content
                {:title
                 "  Edited  "}})

              model-command
              (plan-command
               plan)

              changed
              (after
               model-command)]

          (is
           (=
            :edit
            (command/operation
             model-command)))

          (is
           (=
            "Edited"
            (:request/title
             changed)))

          (is
           (=
            [[:request
              request-id]]
            (guard-targets
             (plan-fragment
              plan))))

          (is
           (=
            :edit
            (:request/operation
             (plan-change
              plan)))))))))

(deftest edit-request-authorization-test
  (let [document
        (request-document)]

    (with-request-store
      [document]

      (with-authorization
        {}

        (is
         (=
          :request/not-authorized
          (error-type
           #(request/plan-edit-request
             {:current-user/id
              helper-a-id

              :biff.fx/now
              t1}

             {:request-id
              request-id

              :content
              {:title
               "No"}})))))))

  (let [capability-owned
        (request-document
         {:requestor
          (domain/capability-requestor
           capability-id)})]

    (with-request-store
      [capability-owned]

      (with-authorization
        {}

        (is
         (=
          :request/capability-authorization-unavailable
          (error-type
           #(request/plan-edit-request
             {:current-user/id
              requestor-id

              :biff.fx/now
              t1}

             {:request-id
              request-id

              :content
              {:title
               "No"}}))))))))

;; =============================================================================
;; Claim planning
;; =============================================================================

(deftest self-claim-plan-test
  (let [open
        (request-document)]

    (with-request-store
      [open]

      (with-authorization
        {:generated-id
         generated-assignment-id

         :helper-ids
         #{helper-a-id}}

        (let [plan
              (request/plan-claim-request
<<<<<<< HEAD
               {:current-user/id
                helper-a-id

                :biff.fx/seed
                1

                :biff.fx/now
                t1}

               {:request-id
                request-id})

              [request-command
               assignment-command]
              (plan-commands
               plan)

              claimed
              (after
               request-command)

              primary
              (after
               assignment-command)]

          (is
           (=
            2
            (count
             (plan-commands
              plan))))

          (is
           (request/claimed?
            claimed))

          (is
           (request/active-primary-assignment?
            primary))

          (is
           (=
            helper-a-id
            (request/assignment-helper-id
             primary)))

          (is
           (=
            :request/claim
            (request/assignment-source
=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                helper-a-id

                :biff.fx/seed
                1

                :biff.fx/now
                t1}

               {:request-id
                request-id})

              [request-command
               assignment-command]
              (plan-commands
               plan)

              claimed
              (after
               request-command)

              primary
              (after
               assignment-command)]

          (is
           (=
            2
            (count
             (plan-commands
              plan))))

          (is
           (request/claimed?
            claimed))

          (is
           (request/active-primary-assignment?
            primary))

          (is
           (=
            helper-a-id
            (request/assignment-helper-id
>>>>>>> biff2-migration
             primary)))

          (is
           (=
<<<<<<< HEAD
            helper-a-id
            (request/assignment-assigned-by
=======
            :request/claim
            (request/assignment-source
>>>>>>> biff2-migration
             primary)))

          (is
           (=
<<<<<<< HEAD
            [(model.tx/assert-none
              :request-assignment
              [:and
               [:=
                :request-assignment/request
                request-id]

               [:=
                :request-assignment/status
                :active]])]
            (plan-assertions
             plan)))

=======
            helper-a-id
            (request/assignment-assigned-by
             primary)))

          (is
           (=
            [(model.tx/assert-none
              :request-assignment
              [:and
               [:=
                :request-assignment/request
                request-id]

               [:=
                :request-assignment/status
                :active]])]
            (plan-assertions
             plan)))

>>>>>>> biff2-migration
          (is
           (=
            [:claim
             :claim]
            (mapv
             :request/operation
             (plan-changes
              plan)))))))))

(deftest manager-claim-plan-test
  (let [open
        (request-document)]

    (with-request-store
      [open]

      (with-authorization
        {:generated-id
         generated-assignment-id

         :helper-ids
         #{helper-b-id}

         :manager-role
         :admin}

        (let [plan
              (request/plan-claim-request
<<<<<<< HEAD
               {:current-user/id
=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
>>>>>>> biff2-migration
                manager-id

                :biff.fx/seed
                2

                :biff.fx/now
                t1}

               {:request-id
                request-id

                :helper-id
                helper-b-id})

              primary
              (get-in
               plan
               [:result
                :primary-assignment])]

          (is
           (=
            helper-b-id
            (request/assignment-helper-id
             primary)))

          (is
           (=
            :request/manager-claim
            (request/assignment-source
             primary)))

          (is
           (=
            manager-id
            (request/assignment-assigned-by
             primary))))))))

(deftest manager-claim-authorization-test
  (let [open
        (request-document)]

    (with-request-store
      [open]

      (with-authorization
        {:generated-id
         generated-assignment-id

         :helper-ids
         #{helper-b-id}

         :manager-role
         nil}

        (is
         (=
          :request/not-authorized
          (error-type
           #(request/plan-claim-request
<<<<<<< HEAD
             {:current-user/id
=======
             {:biff.xtdb/node
              ::test-node

              :current-user/id
>>>>>>> biff2-migration
              manager-id

              :biff.fx/seed
              2

              :biff.fx/now
              t1}

             {:request-id
              request-id

              :helper-id
              helper-b-id}))))))))

;; =============================================================================
;; Primary-helper lifecycle planning
;; =============================================================================

(deftest unclaim-ends-all-active-assignments-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)

        collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator})]

    (with-request-store
      [claimed
       primary
       collaborator]

      (with-authorization
        {:operational?
         false}

        (let [plan
              (request/plan-unclaim-request
<<<<<<< HEAD
               {:current-user/id
                helper-a-id

                :biff.fx/now
                t2}

               {:request-id
                request-id})

              [request-command
               & assignment-commands]
              (plan-commands
               plan)]

          (is
           (request/open?
            (after
             request-command)))

          (is
           (=
            2
            (count
             assignment-commands)))

          (is
           (every?
            request/assignment-ended?
            (map
             after
=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                helper-a-id

                :biff.fx/now
                t2}

               {:request-id
                request-id})

              [request-command
               & assignment-commands]
              (plan-commands
               plan)]

          (is
           (request/open?
            (after
             request-command)))

          (is
           (=
            2
            (count
>>>>>>> biff2-migration
             assignment-commands)))

          (is
           (every?
<<<<<<< HEAD
=======
            request/assignment-ended?
            (map
             after
             assignment-commands)))

          (is
           (every?
>>>>>>> biff2-migration
            #(=
              :request/unclaimed
              (request/assignment-end-reason
               %))
            (map
             after
             assignment-commands))))))))

(deftest mark-on-the-way-plan-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)]

    (with-request-store
      [claimed
       primary]

      (with-authorization
        {:helper-ids
         #{helper-a-id}}

        (let [plan
              (request/plan-mark-request-on-the-way
<<<<<<< HEAD
               {:current-user/id
                helper-a-id

                :biff.fx/now
                t2}

               {:request-id
                request-id})]

          (is
           (request/on-the-way?
            (get-in
             plan
             [:result
              :request])))

          (is
=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                helper-a-id

                :biff.fx/now
                t2}

               {:request-id
                request-id})]

          (is
           (request/on-the-way?
            (get-in
             plan
             [:result
              :request])))

          (is
>>>>>>> biff2-migration
           (=
            1
            (count
             (plan-commands
              plan)))))))))

(deftest complete-ends-all-active-assignments-test
  (let [on-the-way
        (request-document
         {:status
          :on-the-way})

        primary
        (assignment-document)

        collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator})]

    (with-request-store
      [on-the-way
       primary
       collaborator]

      (with-authorization
        {:helper-ids
         #{helper-a-id}}

        (let [plan
              (request/plan-complete-request
<<<<<<< HEAD
               {:current-user/id
                helper-a-id

                :biff.fx/now
                t3}

               {:request-id
                request-id})]

          (is
           (request/done?
            (get-in
             plan
             [:result
              :request])))

          (is
           (=
            3
            (count
             (plan-commands
              plan))))

          (is
=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                helper-a-id

                :biff.fx/now
                t3}

               {:request-id
                request-id})]

          (is
           (request/done?
            (get-in
             plan
             [:result
              :request])))

          (is
           (=
            3
            (count
             (plan-commands
              plan))))

          (is
>>>>>>> biff2-migration
           (every?
            #(=
              :request/completed
              (request/assignment-end-reason
               %))
            (get-in
             plan
             [:result
              :assignments]))))))))

;; =============================================================================
;; Owner cancellation
;; =============================================================================

(deftest cancel-request-plan-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)]

    (with-request-store
      [claimed
       primary]

      (with-authorization
        {:operational?
         false}

        (let [plan
              (request/plan-cancel-request
<<<<<<< HEAD
               {:current-user/id
                requestor-id

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :reason
                :test/customer-cancelled})]

=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                requestor-id

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :reason
                :test/customer-cancelled})]

>>>>>>> biff2-migration
          (is
           (request/cancelled?
            (get-in
             plan
             [:result
              :request])))

          (is
           (=
            :test/customer-cancelled
            (get-in
             plan
             [:result
              :request
              :request/cancellation-reason])))

          (is
           (=
            :request/cancelled
            (request/assignment-end-reason
             (first
              (get-in
               plan
               [:result
                :assignments]))))))))))

;; =============================================================================
;; Collaborator planning
;; =============================================================================

(deftest add-collaborator-plan-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)]

    (with-request-store
      [claimed
       primary]

      (with-authorization
        {:generated-id
         generated-assignment-id

         :helper-ids
         #{helper-a-id
           helper-b-id}}

        (let [plan
              (request/plan-add-collaborator
<<<<<<< HEAD
               {:current-user/id
                helper-a-id

                :biff.fx/seed
                3

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :helper-id
                helper-b-id})

              [request-command
               collaborator-command]
              (plan-commands
               plan)

              touched
              (after
               request-command)

              collaborator
              (after
               collaborator-command)]

=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                helper-a-id

                :biff.fx/seed
                3

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :helper-id
                helper-b-id})

              [request-command
               collaborator-command]
              (plan-commands
               plan)

              touched
              (after
               request-command)

              collaborator
              (after
               collaborator-command)]

>>>>>>> biff2-migration
          (testing
           "assignment-only mutation serializes through Request revision"
            (is
             (request/claimed?
              touched))

            (is
             (=
              2
              (request/revision
               touched))))

          (testing
           "new collaborator is Request-owned participation"
            (is
             (request/active-collaborator-assignment?
              collaborator))

            (is
             (=
              helper-b-id
              (request/assignment-helper-id
               collaborator)))

            (is
             (=
              :request/collaboration
              (request/assignment-source
               collaborator))))

          (is
           (=
            [(model.tx/assert-none
              :request-assignment
              [:and
               [:=
                :request-assignment/request
                request-id]

               [:=
                :request-assignment/helper
                helper-b-id]

               [:=
                :request-assignment/status
                :active]])]
            (plan-assertions
             plan))))))))

(deftest add-collaborator-skill-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)]

    (with-request-store
      [claimed
       primary]

      (with-authorization
        {:generated-id
         generated-assignment-id

         :helper-ids
         #{helper-a-id
           helper-b-id}

         :helper-skills
         {helper-b-id
          #{"forklift"}}}

        (let [plan
              (request/plan-add-collaborator
<<<<<<< HEAD
               {:current-user/id
=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
>>>>>>> biff2-migration
                helper-a-id

                :biff.fx/seed
                3

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :helper-id
                helper-b-id

                :skill
                "  FORKLIFT  "})]

          (is
           (=
            "forklift"
            (:required-skill
             (:result
              plan)))))))

    (with-request-store
      [claimed
       primary]

      (with-authorization
        {:generated-id
         generated-assignment-id

         :helper-ids
         #{helper-a-id
           helper-b-id}

         :helper-skills
         {helper-b-id
          #{}}}

        (is
         (=
          :request/helper-missing-skill
          (error-type
           #(request/plan-add-collaborator
<<<<<<< HEAD
             {:current-user/id
=======
             {:biff.xtdb/node
              ::test-node

              :current-user/id
>>>>>>> biff2-migration
              helper-a-id

              :biff.fx/seed
              3

              :biff.fx/now
              t2}

             {:request-id
              request-id

              :helper-id
              helper-b-id

              :skill
              "forklift"}))))))))

(deftest remove-collaborator-plan-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)

        collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator})]

    (with-request-store
      [claimed
       primary
       collaborator]

      (with-authorization
        {:operational?
         false

       ;; Current helper authority is intentionally irrelevant here.
         :helper-ids
         #{}}

        (let [plan
              (request/plan-remove-collaborator
<<<<<<< HEAD
               {:current-user/id
                helper-a-id

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :helper-id
                helper-b-id})

              ended
              (get-in
               plan
               [:result
                :collaborator-assignment])]

          (is
           (request/assignment-ended?
            ended))

          (is
           (=
            :request/collaborator-removed
            (request/assignment-end-reason
             ended)))

          (is
           (=
=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                helper-a-id

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :helper-id
                helper-b-id})

              ended
              (get-in
               plan
               [:result
                :collaborator-assignment])]

          (is
           (request/assignment-ended?
            ended))

          (is
           (=
            :request/collaborator-removed
            (request/assignment-end-reason
             ended)))

          (is
           (=
>>>>>>> biff2-migration
            2
            (request/revision
             (get-in
              plan
              [:result
               :request])))))))))

;; =============================================================================
;; Reassignment planning
;; =============================================================================

(deftest reassign-request-plan-test
  (let [claimed
        (request-document
         {:status
          :claimed})

        primary
        (assignment-document)

        target-collaborator
        (assignment-document
         {:id
          collaborator-assignment-id

          :helper-id
          helper-b-id

          :role
          :collaborator})]

    (with-request-store
      [claimed
       primary
       target-collaborator]

      (with-authorization
        {:generated-id
         generated-other-assignment-id

         :helper-ids
         #{helper-b-id}

         :manager-role
         :supervisor}

        (let [plan
              (request/plan-reassign-request
<<<<<<< HEAD
               {:current-user/id
                manager-id

                :biff.fx/seed
                4

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :helper-id
                helper-b-id})

              commands
              (plan-commands
               plan)

=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                manager-id

                :biff.fx/seed
                4

                :biff.fx/now
                t2}

               {:request-id
                request-id

                :helper-id
                helper-b-id})

              commands
              (plan-commands
               plan)

>>>>>>> biff2-migration
              new-primary
              (get-in
               plan
               [:result
                :primary-assignment])]

          (is
           (=
            4
            (count
             commands)))

          (is
           (=
            2
            (request/revision
             (get-in
              plan
              [:result
               :request]))))

          (is
           (request/assignment-ended?
            (get-in
             plan
             [:result
              :previous-primary-assignment])))

          (is
           (request/assignment-ended?
            (get-in
             plan
             [:result
              :previous-collaborator-assignment])))

          (is
           (request/active-primary-assignment?
            new-primary))

          (is
           (=
            helper-b-id
            (request/assignment-helper-id
             new-primary)))

          (is
           (=
            :request/reassignment
            (request/assignment-source
             new-primary))))))))

(deftest reassign-request-validation-test
  (testing
   "on-the-way Request is deliberately not manager-reassignable"
    (let [on-the-way
          (request-document
           {:status
            :on-the-way})

          primary
          (assignment-document)]

      (with-request-store
        [on-the-way
         primary]

        (with-authorization
          {:generated-id
           generated-other-assignment-id

           :helper-ids
           #{helper-b-id}

           :manager-role
           :admin}

          (is
           (=
            :request/not-reassignable
            (error-type
             #(request/plan-reassign-request
<<<<<<< HEAD
               {:current-user/id
                manager-id

                :biff.fx/seed
                4

                :biff.fx/now
                t3}

               {:request-id
                request-id

=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                manager-id

                :biff.fx/seed
                4

                :biff.fx/now
                t3}

               {:request-id
                request-id

>>>>>>> biff2-migration
                :helper-id
                helper-b-id}))))))))

  (testing
   "current primary cannot be selected as replacement"
    (let [claimed
          (request-document
           {:status
            :claimed})

          primary
          (assignment-document)]

      (with-request-store
        [claimed
         primary]

        (with-authorization
          {:generated-id
           generated-other-assignment-id

           :helper-ids
           #{helper-a-id}

           :manager-role
           :admin}

          (is
           (=
            :request/helper-already-primary
            (error-type
             #(request/plan-reassign-request
<<<<<<< HEAD
               {:current-user/id
                manager-id

                :biff.fx/seed
                4

                :biff.fx/now
                t2}

               {:request-id
                request-id

=======
               {:biff.xtdb/node
                ::test-node

                :current-user/id
                manager-id

                :biff.fx/seed
                4

                :biff.fx/now
                t2}

               {:request-id
                request-id

>>>>>>> biff2-migration
                :helper-id
                helper-a-id})))))))))

;; =============================================================================
;; Location and actor authorization boundaries
;; =============================================================================

(deftest location-ownership-validation-test
  (with-authorization
    {:generated-id
     generated-request-id

     :scope-organization-id
     other-organization-id}

    (is
     (=
      :request/location-organization-mismatch
      (error-type
       #(request/plan-create-request
         {:current-user/id
          requestor-id

          :biff.fx/seed
          1

          :biff.fx/now
          t1}

         {:organization-id
          organization-id

          :location-id
          location-id

          :content
          canonical-content}))))))

(deftest planner-requires-authenticated-actor-test
  (with-authorization
    {:generated-id
     generated-request-id}

    (is
     (=
      :request/not-authenticated
      (error-type
       #(request/plan-create-request
         {:biff.fx/seed
          1

          :biff.fx/now
          t1}

         {:organization-id
          organization-id

          :location-id
          location-id

          :content
          canonical-content}))))))

;; =============================================================================
;; Public facade facts
;; =============================================================================

(deftest public-facade-facts-test
  (let [document
        (request-document)

        assignment
        (assignment-document
         {:role
          :collaborator})]

    (is
     (=
      request-id
      (request/request-id
       document)))

    (is
     (request/belongs-to-location?
      document
      organization-id
      location-id))

    (is
     (request/controlled-by?
      document
      {:user-id
       requestor-id}))

    (is
     (false?
      (request/controlled-by?
       document
       {:user-id
        helper-a-id})))

    (is
     (request/collaborator-assignment?
      assignment))

    (is
     (request/assignment-for-request?
      assignment
      request-id))

    (is
     (request/assignment-for-helper?
      assignment
      helper-a-id))))
