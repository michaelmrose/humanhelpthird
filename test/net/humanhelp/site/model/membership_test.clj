(ns net.humanhelp.site.model.membership-test
  "Replacement tests for the HumanHelp Membership model.

   Membership owns:

   - User ↔ Organization Memberships;
   - organization-local skills;
   - RoleAssignments;
   - Membership-side role applicability over Organization scopes;
   - convenient read-side effective-role queries;
   - guarded authorization dependencies for atomic mutations.

   The public authorization surfaces are deliberately distinct:

   Pure evaluation over already-loaded values:
     effective-role-assignments-for-membership
     effective-roles-for-membership
     membership-has-role?

   Ordinary read-side queries:
     effective-role-state
     effective-roles
     has-role?
     helper?
     supervisor?
     admin?
     staff?

   Atomic mutation authorization:
     role-dependency
     require-role-dependency
     helper-dependency
     require-helper-dependency
     supervisor-dependency
     require-supervisor-dependency
     admin-dependency
     require-admin-dependency."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.experimental :as biffx]
   [gesso.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.membership.domain :as domain]
   [net.humanhelp.site.model.membership.graph :as membership.graph]
   [net.humanhelp.site.model.membership.schema :as membership.schema]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed values
;; =============================================================================

(def user-id
  (UUID/fromString
   "00000000-0000-0000-0000-000000000001"))

(def other-user-id
  (UUID/fromString
   "00000000-0000-0000-0000-000000000002"))

(def organization-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def other-organization-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000002"))

(def group-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000001"))

(def location-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000001"))

(def other-location-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000002"))

(def membership-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000001"))

(def other-membership-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000002"))

(def missing-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000099"))

(def helper-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000001"))

(def supervisor-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000002"))

(def admin-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000003"))

(def generated-membership-id
  (UUID/fromString
   "70000000-0000-7000-8000-000000000001"))

(def generated-role-assignment-id
  (UUID/fromString
   "70000000-0000-7000-8000-000000000002"))

(def actor-id
  (UUID/fromString
   "60000000-0000-0000-0000-000000000001"))

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

;; =============================================================================
;; Organization scopes and contexts
;; =============================================================================

(def organization-scope
  (organization/organization-scope
   organization-id))

(def group-scope
  (organization/organization-group-scope
   group-id))

(def location-scope
  (organization/location-scope
   location-id))

(def other-location-scope
  (organization/location-scope
   other-location-id))

(def other-organization-scope
  (organization/organization-scope
   other-organization-id))

(def location-context
  {:organization/id
   organization-id

   :scope/target
   location-scope

   :scope/applicable
   [location-scope
    group-scope
    organization-scope]

   :scope/operational?
   true})

(def group-context
  {:organization/id
   organization-id

   :scope/target
   group-scope

   :scope/applicable
   [group-scope
    organization-scope]

   :scope/operational?
   true})

(def organization-context
  {:organization/id
   organization-id

   :scope/target
   organization-scope

   :scope/applicable
   [organization-scope]

   :scope/operational?
   true})

(def non-operational-location-context
  (assoc
   location-context
   :scope/operational?
   false))

(def other-organization-context
  {:organization/id
   other-organization-id

   :scope/target
   other-organization-scope

   :scope/applicable
   [other-organization-scope]

   :scope/operational?
   true})

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

(defn- after
  [model-command]
  (command/after
   model-command))

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

(defn- plan-assertions
  [plan]
  (:assertions
   (plan-fragment plan)))

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

(defn- membership-document
  ([]
   (membership-document
    {}))

  ([overrides]
   (after
    (domain/create-membership-command
     (merge
      {:id
       membership-id

       :user-id
       user-id

       :organization-id
       organization-id

       :now
       t0}
      overrides)))))

(defn- role-assignment-document
  ([]
   (role-assignment-document
    (membership-document)
    {}))

  ([overrides]
   (role-assignment-document
    (membership-document)
    overrides))

  ([membership-document overrides]
   (after
    (domain/create-role-assignment-command
     membership-document
     (merge
      {:id
       helper-assignment-id

       :role
       :helper

       :scope
       location-scope

       :now
       t0}
      overrides)))))

(defn- suspended-membership
  []
  (after
   (domain/suspend-membership-command
    (membership-document)
    {:actor-id
     actor-id

     :reason
     :test/suspended

     :now
     t1})))

(defn- revoked-membership
  []
  (after
   (domain/revoke-membership-command
    (membership-document)
    {:actor-id
     actor-id

     :reason
     :test/revoked

     :now
     t1})))

(defn- revoked-role-assignment
  [role-assignment]
  (after
   (domain/revoke-role-assignment-command
    role-assignment
    {:actor-id
     actor-id

     :reason
     :test/revoked

     :now
     t1})))

(defn- fake-user
  ([]
   (fake-user
    :active))

  ([status]
   {:xt/id
    user-id

    :user/status
    status

    :user/revision
    0

    :user/created-at
    t0

    :user/updated-at
    t0}))

(def user-version
  {:revision-key
   :user/revision

   :created-at-key
   :user/created-at

   :updated-at-key
   :user/updated-at})

(def fake-organization
  {:xt/id
   organization-id

   :organization/revision
   0

   :organization/created-at
   t0

   :organization/updated-at
   t0})

(def organization-version
  {:revision-key
   :organization/revision

   :created-at-key
   :organization/created-at

   :updated-at-key
   :organization/updated-at})

(defn- user-dependency-value
  ([]
   (user-dependency-value
    (fake-user)))

  ([user-document]
   {:user
    user-document

    :transaction-fragment
    (model.tx/guards-fragment
     (command/guard
      :user
      user-document
      user-version))}))

(defn- scope-dependency-value
  ([]
   (scope-dependency-value
    location-context))

  ([scope-context]
   {:scope-context
    scope-context

    :transaction-fragment
    (model.tx/guards-fragment
     (command/guard
      :organization
      fake-organization
      organization-version))}))

(defn- with-model-documents*
  [documents f]
  (let [documents
        (vec documents)

        by-id
        (into
         {}
         (map
          (juxt
           :xt/id
           identity))
         documents)]
    (with-redefs
     [model/load-by-id
      (fn [_descriptor _ctx id]
        (get
         by-id
         id))]
      (f))))

(defmacro with-model-documents
  [documents & body]
  `(with-model-documents*
     ~documents
     (fn []
       ~@body)))

(defmacro with-active-cross-model-dependencies
  [& body]
  `(with-redefs
    [user/require-user-dependency
     (fn [_ctx# requested-user-id#]
       (is
        (=
         user-id
         requested-user-id#))

       (user-dependency-value))

     organization/require-scope-dependency
     (fn [_ctx# requested-scope#]
       (cond
         (organization/same-scope?
          requested-scope#
          organization-scope)
         (scope-dependency-value
          organization-context)

         (organization/same-scope?
          requested-scope#
          group-scope)
         (scope-dependency-value
          group-context)

         :else
         (scope-dependency-value
          location-context)))]
     ~@body))

;; =============================================================================
;; Skill and role values
;; =============================================================================

(deftest skill-value-test
  (testing "skills are canonical case-insensitive organization-local labels"
    (is
     (=
      "customer service"
      (membership/normalize-skill
       "  Customer Service  ")))

    (is
     (nil?
      (membership/normalize-skill
       "   ")))

    (is
     (membership/skill?
      "customer service"))

    (is
     (false?
      (membership/skill?
       "Customer Service")))

    (is
     (false?
      (membership/skill?
       ""))))

  (testing "skill collections normalize to canonical sets"
    (is
     (=
      #{"customer service"
        "paint"}
      (membership/normalize-skills
       [" Customer Service "
        "PAINT"
        "paint"])))

    (is
     (membership/skills?
      #{"customer service"
        "paint"}))

    (is
     (false?
      (membership/skills?
       ["paint"]))))

  (testing "malformed non-string input is not silently erased"
    (is
     (=
      42
      (membership/normalize-skill
       42)))

    (is
     (false?
      (membership/skill?
       42)))))

(deftest role-value-test
  (doseq [role
          [:helper
           :supervisor
           :admin]]
    (is
     (membership/role?
      role)))

  (is
   (false?
    (membership/role?
     :customer)))

  (is
   (false?
    (membership/role?
     :owner))))

;; =============================================================================
;; Membership construction and schema
;; =============================================================================

(deftest membership-create-test
  (let [document
        (membership-document
         {:skills
          [" Paint "
           "CUSTOMER SERVICE"
           "paint"]})]
    (is
     (=
      membership-id
      (membership/membership-id
       document)))

    (is
     (=
      user-id
      (membership/membership-user-id
       document)))

    (is
     (=
      organization-id
      (membership/membership-organization-id
       document)))

    (is
     (=
      #{"paint"
        "customer service"}
      (membership/membership-skills
       document)))

    (is
     (membership/membership-active?
      document))

    (is
     (=
      0
      (:membership/revision
       document)))

    (is
     (=
      t0
      (:membership/created-at
       document)))

    (is
     (=
      t0
      (:membership/updated-at
       document)))

    (is
     (domain/membership-document-consistent?
      document))

    (is
     (m/validate
      membership.schema/membership-document-schema
      document))))

(deftest membership-create-validation-test
  (doseq [input
          [{:user-id
            user-id

            :organization-id
            organization-id

            :now
            t0}

           {:id
            membership-id

            :organization-id
            organization-id

            :now
            t0}

           {:id
            membership-id

            :user-id
            user-id

            :now
            t0}

           {:id
            membership-id

            :user-id
            user-id

            :organization-id
            organization-id}

           {:id
            membership-id

            :user-id
            user-id

            :organization-id
            organization-id

            :skills
            [""]

            :now
            t0}]]
    (is
     (=
      :membership/invalid-create-input
      (error-type
       #(domain/create-membership-command
         input))))))

(deftest membership-schema-is-closed-test
  (is
   (false?
    (m/validate
     membership.schema/membership-document-schema
     (assoc
      (membership-document)
      :membership/random
      true)))))

;; =============================================================================
;; Membership skill commands
;; =============================================================================

(deftest membership-skill-command-test
  (let [original
        (membership-document
         {:skills
          #{"paint"}})

        add-command
        (domain/add-skill-command
         original
         {:skill
          " CUSTOMER SERVICE "

          :now
          t1})

        added
        (after
         add-command)

        remove-command
        (domain/remove-skill-command
         added
         {:skill
          "PAINT"

          :now
          t2})

        removed
        (after
         remove-command)]
    (is
     (command/update?
      add-command))

    (is
     (=
      :add-skill
      (command/operation
       add-command)))

    (is
     (=
      #{"paint"
        "customer service"}
      (membership/membership-skills
       added)))

    (is
     (=
      1
      (:membership/revision
       added)))

    (is
     (=
      #{"customer service"}
      (membership/membership-skills
       removed)))

    (is
     (=
      2
      (:membership/revision
       removed)))

    (is
     (=
      :membership/skill-already-present
      (error-type
       #(domain/add-skill-command
         original
         {:skill
          "PAINT"

          :now
          t1}))))

    (is
     (=
      :membership/skill-missing
      (error-type
       #(domain/remove-skill-command
         original
         {:skill
          "customer service"

          :now
          t1}))))))

;; =============================================================================
;; Membership lifecycle
;; =============================================================================

(deftest membership-lifecycle-test
  (let [original
        (membership-document)

        suspend-command
        (domain/suspend-membership-command
         original
         {:actor-id
          actor-id

          :reason
          :test/suspended

          :now
          t1})

        suspended
        (after
         suspend-command)

        reactivate-command
        (domain/reactivate-membership-command
         suspended
         {:now
          t2})

        reactivated
        (after
         reactivate-command)

        revoke-command
        (domain/revoke-membership-command
         suspended
         {:actor-id
          actor-id

          :reason
          :test/revoked

          :now
          t2})

        revoked
        (after
         revoke-command)]
    (testing "suspension owns its audit"
      (is
       (membership/membership-suspended?
        suspended))

      (is
       (=
        t1
        (:membership/suspended-at
         suspended)))

      (is
       (=
        actor-id
        (:membership/suspended-by
         suspended)))

      (is
       (=
        :test/suspended
        (:membership/suspension-reason
         suspended))))

    (testing "reactivation clears suspension audit"
      (is
       (membership/membership-active?
        reactivated))

      (is
       (nil?
        (:membership/suspended-at
         reactivated)))

      (is
       (nil?
        (:membership/suspended-by
         reactivated)))

      (is
       (nil?
        (:membership/suspension-reason
         reactivated))))

    (testing "revocation is terminal"
      (is
       (membership/membership-revoked?
        revoked))

      (is
       (=
        t2
        (:membership/revoked-at
         revoked)))

      (is
       (=
        actor-id
        (:membership/revoked-by
         revoked)))

      (is
       (=
        :test/revoked
        (:membership/revocation-reason
         revoked)))

      (is
       (nil?
        (:membership/suspended-at
         revoked)))

      (is
       (domain/membership-document-consistent?
        revoked)))

    (testing "invalid transitions fail"
      (is
       (=
        :membership/already-suspended
        (error-type
         #(domain/suspend-membership-command
           suspended
           {:now
            t2}))))

      (is
       (=
        :membership/already-active
        (error-type
         #(domain/reactivate-membership-command
           original
           {:now
            t1}))))

      (is
       (=
        :membership/revoked
        (error-type
         #(domain/reactivate-membership-command
           revoked
           {:now
            t3}))))

      (is
       (=
        :membership/revoked
        (error-type
         #(domain/add-skill-command
           revoked
           {:skill
            "paint"

            :now
            t3})))))))

(deftest membership-update-time-test
  (is
   (=
    :membership/invalid-time
    (error-type
     #(domain/add-skill-command
       (membership-document)
       {:skill
        "paint"

        :now
        t-before})))))

;; =============================================================================
;; RoleAssignment construction and schema
;; =============================================================================

(deftest role-assignment-create-test
  (let [membership-document
        (membership-document)

        model-command
        (domain/create-role-assignment-command
         membership-document
         {:id
          helper-assignment-id

          :role
          :helper

          :scope
          group-scope

          :actor-id
          actor-id

          :reason
          :test/granted

          :now
          t1})

        assignment
        (after
         model-command)]
    (is
     (command/create?
      model-command))

    (is
     (=
      membership-id
      (membership/role-assignment-membership-id
       assignment)))

    (is
     (=
      :helper
      (membership/assigned-role
       assignment)))

    (is
     (=
      group-scope
      (membership/role-assignment-scope
       assignment)))

    (is
     (membership/role-assignment-active?
      assignment))

    (is
     (=
      actor-id
      (:role-assignment/assigned-by
       assignment)))

    (is
     (=
      :test/granted
      (:role-assignment/assignment-reason
       assignment)))

    (testing "Organization is not redundantly persisted"
      (is
       (false?
        (contains?
         assignment
         :role-assignment/organization))))

    (is
     (domain/role-assignment-document-consistent?
      assignment))

    (is
     (m/validate
      membership.schema/role-assignment-document-schema
      assignment))))

(deftest role-assignment-create-validation-test
  (let [active
        (membership-document)

        suspended
        (suspended-membership)]
    (doseq [input
            [{:id
              helper-assignment-id

              :role
              :customer

              :scope
              location-scope

              :now
              t1}

             {:id
              helper-assignment-id

              :role
              :helper

              :scope
              {:scope/type
               :unknown

               :scope/id
               location-id}

              :now
              t1}

             {:role
              :helper

              :scope
              location-scope

              :now
              t1}

             {:id
              helper-assignment-id

              :role
              :helper

              :scope
              location-scope}]]
      (is
       (=
        :role-assignment/invalid-create-input
        (error-type
         #(domain/create-role-assignment-command
           active
           input)))))

    (testing "roles can only be granted to active Memberships"
      (is
       (=
        :role-assignment/invalid-create-input
        (error-type
         #(domain/create-role-assignment-command
           suspended
           {:id
            helper-assignment-id

            :role
            :helper

            :scope
            location-scope

            :now
            t2})))))))

(deftest role-assignment-schema-is-closed-test
  (is
   (false?
    (m/validate
     membership.schema/role-assignment-document-schema
     (assoc
      (role-assignment-document)
      :role-assignment/organization
      organization-id)))))

;; =============================================================================
;; RoleAssignment lifecycle
;; =============================================================================

(deftest role-assignment-revocation-test
  (let [original
        (role-assignment-document)

        model-command
        (domain/revoke-role-assignment-command
         original
         {:actor-id
          actor-id

          :reason
          :test/revoked

          :now
          t1})

        revoked
        (after
         model-command)]
    (is
     (command/update?
      model-command))

    (is
     (=
      :revoke
      (command/operation
       model-command)))

    (is
     (membership/role-assignment-revoked?
      revoked))

    (is
     (=
      t1
      (:role-assignment/revoked-at
       revoked)))

    (is
     (=
      actor-id
      (:role-assignment/revoked-by
       revoked)))

    (is
     (=
      :test/revoked
      (:role-assignment/revocation-reason
       revoked)))

    (is
     (=
      1
      (:role-assignment/revision
       revoked)))

    (is
     (=
      :role-assignment/revoked
      (error-type
       #(domain/revoke-role-assignment-command
         revoked
         {:now
          t2}))))))

;; =============================================================================
;; Pure Membership-side authorization
;; =============================================================================

(deftest exact-role-semantics-test
  (let [membership-document
        (membership-document)

        helper
        (role-assignment-document
         membership-document
         {:id
          helper-assignment-id

          :role
          :helper

          :scope
          location-scope})

        supervisor
        (role-assignment-document
         membership-document
         {:id
          supervisor-assignment-id

          :role
          :supervisor

          :scope
          location-scope})]
    (is
     (domain/role-assignment-grants?
      helper
      membership-id
      :helper
      location-scope))

    (is
     (false?
      (domain/role-assignment-grants?
       supervisor
       membership-id
       :helper
       location-scope)))

    (testing "roles are exact, not hierarchical"
      (is
       (=
        #{:supervisor}
        (membership/effective-roles-for-membership
         membership-document
         [supervisor]
         location-context)))

      (is
       (false?
        (membership/membership-has-role?
         membership-document
         [supervisor]
         location-context
         :helper)))

      (is
       (membership/membership-has-role?
        membership-document
        [supervisor]
        location-context
        :supervisor)))))

(deftest pure-effective-role-assignment-filter-test
  (let [membership-document
        (membership-document)

        helper
        (role-assignment-document
         membership-document
         {:id
          helper-assignment-id

          :role
          :helper

          :scope
          organization-scope})

        supervisor
        (role-assignment-document
         membership-document
         {:id
          supervisor-assignment-id

          :role
          :supervisor

          :scope
          group-scope})

        admin
        (role-assignment-document
         membership-document
         {:id
          admin-assignment-id

          :role
          :admin

          :scope
          other-location-scope})]
    (is
     (=
      #{helper
        supervisor}
      (set
       (membership/effective-role-assignments-for-membership
        membership-document
        [helper
         supervisor
         admin]
        location-context))))))

(deftest hierarchical-scope-authorization-test
  (let [membership-document
        (membership-document)

        organization-helper
        (role-assignment-document
         membership-document
         {:id
          helper-assignment-id

          :role
          :helper

          :scope
          organization-scope})

        group-supervisor
        (role-assignment-document
         membership-document
         {:id
          supervisor-assignment-id

          :role
          :supervisor

          :scope
          group-scope})

        location-admin
        (role-assignment-document
         membership-document
         {:id
          admin-assignment-id

          :role
          :admin

          :scope
          location-scope})

        assignments
        [organization-helper
         group-supervisor
         location-admin]]
    (testing "ancestor grants apply to descendant scope"
      (is
       (=
        #{:helper
          :supervisor
          :admin}
        (membership/effective-roles-for-membership
         membership-document
         assignments
         location-context))))

    (testing "descendant grants do not apply upward"
      (is
       (=
        #{:helper}
        (membership/effective-roles-for-membership
         membership-document
         assignments
         organization-context))))

    (testing "pure predicates use already-loaded values"
      (is
       (membership/membership-has-role?
        membership-document
        assignments
        location-context
        :helper))

      (is
       (membership/membership-has-role?
        membership-document
        assignments
        location-context
        :supervisor))

      (is
       (membership/membership-has-role?
        membership-document
        assignments
        location-context
        :admin)))))

(deftest pure-authorization-requires-active-membership-test
  (let [active
        (membership-document)

        suspended
        (suspended-membership)

        revoked
        (revoked-membership)

        assignment
        (role-assignment-document
         active
         {:scope
          organization-scope})]
    (is
     (membership/membership-has-role?
      active
      [assignment]
      location-context
      :helper))

    (is
     (false?
      (membership/membership-has-role?
       suspended
       [assignment]
       location-context
       :helper)))

    (is
     (false?
      (membership/membership-has-role?
       revoked
       [assignment]
       location-context
       :helper)))))

(deftest pure-authorization-requires-operational-matching-scope-context-test
  (let [membership-document
        (membership-document)

        assignment
        (role-assignment-document
         membership-document
         {:scope
          organization-scope})]
    (is
     (false?
      (membership/membership-has-role?
       membership-document
       [assignment]
       non-operational-location-context
       :helper)))

    (is
     (false?
      (membership/membership-has-role?
       membership-document
       [assignment]
       other-organization-context
       :helper)))))

(deftest revoked-assignment-does-not-authorize-test
  (let [membership-document
        (membership-document)

        assignment
        (role-assignment-document
         membership-document
         {:scope
          organization-scope})

        revoked
        (revoked-role-assignment
         assignment)]
    (is
     (membership/membership-has-role?
      membership-document
      [assignment]
      location-context
      :helper))

    (is
     (false?
      (membership/membership-has-role?
       membership-document
       [revoked]
       location-context
       :helper)))))

;; =============================================================================
;; Descriptor and generated module
;; =============================================================================

(deftest descriptor-test
  (is
   (model/descriptor?
    membership.schema/membership-descriptor))

  (is
   (model/descriptor?
    membership.schema/role-assignment-descriptor))

  (is
   (=
    :membership
    (:entity-type
     membership.schema/membership-descriptor)))

  (is
   (=
    :role-assignment
    (:entity-type
     membership.schema/role-assignment-descriptor)))

  (is
   (=
    domain/membership-version
    (:version
     membership.schema/membership-descriptor)))

  (is
   (=
    domain/role-assignment-version
    (:version
     membership.schema/role-assignment-descriptor))))

(deftest module-contract-test
  (is
   (map?
    membership/module))

  (is
   (identical?
    membership/schema
    (:schema
     membership/module)))

  (is
   (identical?
    membership/resolvers
    (:biff.graph/resolvers
     membership/module)))

  (doseq [schema-key
          [:membership
           :membership/id
           :membership/user-id
           :membership/organization-id
           :membership/skills
           :membership/status

           :role-assignment
           :role-assignment/id
           :role-assignment/membership-id
           :role-assignment/role
           :role-assignment/scope-type
           :role-assignment/scope-id
           :role-assignment/status]]
    (is
     (contains?
      membership/schema
      schema-key)))

  (testing "each descriptor contributes by-id and field resolvers"
    (is
     (=
      4
      (count
       membership/resolvers)))

    (is
     (=
      #{(model/by-id-resolver-id
         membership.schema/membership-descriptor)

        (model/fields-resolver-id
         membership.schema/membership-descriptor)

        (model/by-id-resolver-id
         membership.schema/role-assignment-descriptor)

        (model/fields-resolver-id
         membership.schema/role-assignment-descriptor)}
      (set
       (map
        :biff.graph/id
        membership/resolvers))))))

;; =============================================================================
;; Graph relationship reads
;; =============================================================================

(deftest memberships-for-user-query-test
  (let [first-membership
        (membership-document)

        second-membership
        (membership-document
         {:id
          other-membership-id

          :organization-id
          other-organization-id})

        captured
        (atom nil)]
    (with-redefs
     [biffx/q
      (fn [connection query]
        (reset!
         captured
         [connection
          query])

        [second-membership
         first-membership])]

      (is
       (=
        [first-membership
         second-membership]
        (membership.graph/memberships-for-user
         {:biff/conn
          :connection}
         user-id))))

    (let [[connection query]
          @captured]
      (is
       (=
        :connection
        connection))

      (is
       (=
        :membership
        (:from
         query)))

      (is
       (=
        [:=
         :membership/user
         user-id]
        (:where
         query)))

      (is
       (=
        (model/document-columns
         membership.schema/membership-descriptor)
        (:select
         query))))))

(deftest current-membership-query-semantics-test
  (let [active
        (membership-document)

        suspended
        (suspended-membership)

        revoked
        (revoked-membership)]
    (testing "active Membership is current"
      (with-redefs
       [biffx/q
        (fn [& _]
          [active])]

        (is
         (=
          active
          (membership.graph/current-membership
           {:biff/conn
            :connection}
           user-id
           organization-id)))))

    (testing "suspended Membership remains the current relationship"
      (with-redefs
       [biffx/q
        (fn [& _]
          [suspended])]

        (is
         (=
          suspended
          (membership.graph/current-membership
           {:biff/conn
            :connection}
           user-id
           organization-id)))))

    (testing "revoked Membership is historical"
      (with-redefs
       [biffx/q
        (fn [& _]
          [revoked])]

        (is
         (nil?
          (membership.graph/current-membership
           {:biff/conn
            :connection}
           user-id
           organization-id)))))

    (testing "multiple non-revoked Memberships are persisted corruption"
      (with-redefs
       [biffx/q
        (fn [& _]
          [active
           (membership-document
            {:id
             other-membership-id})])]

        (is
         (=
          :membership.graph/non-unique-current-membership
          (error-type
           #(membership.graph/current-membership
             {:biff/conn
              :connection}
             user-id
             organization-id))))))))

(deftest role-assignment-query-test
  (let [membership-document
        (membership-document)

        helper
        (role-assignment-document
         membership-document
         {:id
          helper-assignment-id

          :role
          :helper})

        supervisor
        (role-assignment-document
         membership-document
         {:id
          supervisor-assignment-id

          :role
          :supervisor})

        captured
        (atom nil)]
    (with-redefs
     [biffx/q
      (fn [connection query]
        (reset!
         captured
         [connection
          query])

        [supervisor
         helper])]

      (is
       (=
        [helper
         supervisor]
        (membership.graph/role-assignments-for-membership
         {:biff/conn
          :connection}
         membership-id))))

    (is
     (=
      [:=
       :role-assignment/membership
       membership-id]
      (get-in
       @captured
       [1 :where])))))

;; =============================================================================
;; Stable core entity reads
;; =============================================================================

(deftest core-by-id-read-test
  (let [membership-document
        (membership-document)

        assignment
        (role-assignment-document)]
    (with-model-documents
      [membership-document
       assignment]

      (is
       (=
        membership-document
        (membership/membership
         {}
         membership-id)))

      (is
       (=
        membership-document
        (membership/require-membership
         {}
         membership-id)))

      (is
       (=
        assignment
        (membership/role-assignment
         {}
         helper-assignment-id)))

      (is
       (nil?
        (membership/membership
         {}
         missing-id)))

      (is
       (=
        :membership/not-found
        (error-type
         #(membership/require-membership
           {}
           missing-id))))

      (is
       (=
        :membership.core/invalid-membership-id
        (error-type
         #(membership/membership
           {}
           :bad-id))))

      (is
       (=
        :membership.core/invalid-role-assignment-id
        (error-type
         #(membership/role-assignment
           {}
           :bad-id)))))))

;; =============================================================================
;; Ordinary read-side authorization
;; =============================================================================

(deftest read-side-effective-role-state-test
  (let [user-document
        (fake-user)

        membership-document
        (membership-document)

        helper
        (role-assignment-document
         membership-document
         {:id
          helper-assignment-id

          :role
          :helper

          :scope
          organization-scope})

        supervisor
        (role-assignment-document
         membership-document
         {:id
          supervisor-assignment-id

          :role
          :supervisor

          :scope
          group-scope})

        assignments
        [helper
         supervisor]]
    (with-redefs
     [organization/require-scope-context
      (fn [_ctx requested-scope]
        (is
         (organization/same-scope?
          location-scope
          requested-scope))

        location-context)

      user/user
      (fn [_ctx requested-user-id]
        (is
         (=
          user-id
          requested-user-id))

        user-document)

      membership.graph/current-membership
      (fn [_ctx requested-user-id requested-organization-id]
        (is
         (=
          user-id
          requested-user-id))

        (is
         (=
          organization-id
          requested-organization-id))

        membership-document)

      membership.graph/active-role-assignments-for-membership
      (fn [_ctx requested-membership-id]
        (is
         (=
          membership-id
          requested-membership-id))

        assignments)]

      (let [state
            (membership/effective-role-state
             {}
             user-id
             location-scope)]
        (is
         (=
          user-document
          (:user state)))

        (is
         (=
          membership-document
          (:membership state)))

        (is
         (=
          location-context
          (:scope-context state)))

        (is
         (=
          assignments
          (:role-assignments state)))

        (is
         (=
          #{:helper
            :supervisor}
          (:roles state))))

      (is
       (=
        #{:helper
          :supervisor}
        (membership/effective-roles
         {}
         user-id
         location-scope)))

      (is
       (membership/has-role?
        {}
        user-id
        location-scope
        :helper))

      (is
       (membership/helper?
        {}
        user-id
        location-scope))

      (is
       (membership/supervisor?
        {}
        user-id
        location-scope))

      (is
       (false?
        (membership/admin?
         {}
         user-id
         location-scope)))

      (is
       (membership/staff?
        {}
        user-id
        location-scope)))))

(deftest read-side-authorization-rejects-inactive-user-test
  (let [membership-read?
        (atom false)

        assignment-read?
        (atom false)]
    (with-redefs
     [organization/require-scope-context
      (fn [& _]
        location-context)

      user/user
      (fn [& _]
        (fake-user
         :suspended))

      membership.graph/current-membership
      (fn [& _]
        (reset!
         membership-read?
         true)

        (membership-document))

      membership.graph/active-role-assignments-for-membership
      (fn [& _]
        (reset!
         assignment-read?
         true)

        [(role-assignment-document)])]

      (is
       (nil?
        (membership/effective-role-state
         {}
         user-id
         location-scope)))

      (is
       (=
        #{}
        (membership/effective-roles
         {}
         user-id
         location-scope)))

      (is
       (false?
        (membership/helper?
         {}
         user-id
         location-scope)))

      (is
       (false?
        (membership/staff?
         {}
         user-id
         location-scope)))

      (testing "Membership state is not consulted once User is inactive"
        (is
         (false?
          @membership-read?))

        (is
         (false?
          @assignment-read?))))))

(deftest read-side-authorization-rejects-missing-user-test
  (with-redefs
   [organization/require-scope-context
    (fn [& _]
      location-context)

    user/user
    (fn [& _]
      nil)

    membership.graph/current-membership
    (fn [& _]
      (throw
       (ex-info
        "Membership query should not run for missing User."
        {})))]

    (is
     (nil?
      (membership/effective-role-state
       {}
       user-id
       location-scope)))

    (is
     (=
      #{}
      (membership/effective-roles
       {}
       user-id
       location-scope)))

    (is
     (false?
      (membership/helper?
       {}
       user-id
       location-scope)))))

(deftest read-side-authorization-rejects-missing-membership-test
  (with-redefs
   [organization/require-scope-context
    (fn [& _]
      location-context)

    user/user
    (fn [& _]
      (fake-user))

    membership.graph/current-membership
    (fn [& _]
      nil)

    membership.graph/active-role-assignments-for-membership
    (fn [& _]
      (throw
       (ex-info
        "RoleAssignment query should not run without Membership."
        {})))]

    (is
     (nil?
      (membership/effective-role-state
       {}
       user-id
       location-scope)))

    (is
     (=
      #{}
      (membership/effective-roles
       {}
       user-id
       location-scope)))

    (is
     (false?
      (membership/staff?
       {}
       user-id
       location-scope)))))

(deftest read-side-authorization-respects-suspended-membership-test
  (let [suspended
        (suspended-membership)

        assignment
        (role-assignment-document
         (membership-document)
         {:scope
          organization-scope})]
    (with-redefs
     [organization/require-scope-context
      (fn [& _]
        location-context)

      user/user
      (fn [& _]
        (fake-user))

      membership.graph/current-membership
      (fn [& _]
        suspended)

      membership.graph/active-role-assignments-for-membership
      (fn [& _]
        [assignment])]

      (let [state
            (membership/effective-role-state
             {}
             user-id
             location-scope)]
        (is
         (=
          suspended
          (:membership state)))

        (is
         (=
          #{}
          (:roles state))))

      (is
       (false?
        (membership/helper?
         {}
         user-id
         location-scope)))

      (is
       (false?
        (membership/staff?
         {}
         user-id
         location-scope))))))

(deftest read-side-role-semantics-remain-exact-test
  (let [membership-document
        (membership-document)

        admin
        (role-assignment-document
         membership-document
         {:id
          admin-assignment-id

          :role
          :admin

          :scope
          organization-scope})]
    (with-redefs
     [organization/require-scope-context
      (fn [& _]
        location-context)

      user/user
      (fn [& _]
        (fake-user))

      membership.graph/current-membership
      (fn [& _]
        membership-document)

      membership.graph/active-role-assignments-for-membership
      (fn [& _]
        [admin])]

      (is
       (=
        #{:admin}
        (membership/effective-roles
         {}
         user-id
         location-scope)))

      (is
       (membership/admin?
        {}
        user-id
        location-scope))

      (is
       (false?
        (membership/supervisor?
         {}
         user-id
         location-scope)))

      (is
       (false?
        (membership/helper?
         {}
         user-id
         location-scope)))

      (is
       (membership/staff?
        {}
        user-id
        location-scope)))))

;; =============================================================================
;; Membership dependency
;; =============================================================================

(deftest membership-dependency-test
  (let [document
        (membership-document)]
    (with-model-documents
      [document]

      (let [{:keys
             [membership
              transaction-fragment]}
            (membership/require-membership-dependency
             {}
             membership-id)]
        (is
         (=
          document
          membership))

        (is
         (=
          [[:membership
            membership-id]]
          (guard-targets
           transaction-fragment))))

      (is
       (nil?
        (membership/membership-dependency
         {}
         missing-id)))

      (is
       (=
        :membership/not-found
        (error-type
         #(membership/require-membership-dependency
           {}
           missing-id)))))))

(deftest current-membership-dependency-test
  (let [document
        (membership-document)]
    (with-redefs
     [membership.graph/current-membership
      (fn [_ctx requested-user-id requested-organization-id]
        (is
         (=
          user-id
          requested-user-id))

        (is
         (=
          organization-id
          requested-organization-id))

        document)]

      (let [{:keys
             [membership
              transaction-fragment]}
            (membership/current-membership-dependency
             {}
             user-id
             organization-id)]
        (is
         (=
          document
          membership))

        (is
         (=
          [[:membership
            membership-id]]
          (guard-targets
           transaction-fragment)))))))

;; =============================================================================
;; Membership creation planning
;; =============================================================================

(deftest create-membership-plan-test
  (with-active-cross-model-dependencies
    (with-redefs
     [fx/uuid7
      (fn [_seed _now]
        [generated-membership-id])]

      (let [plan
            (membership/plan-create-membership
             {:biff.fx/seed
              7

              :biff.fx/now
              t1}
             {:user-id
              user-id

              :organization-id
              organization-id

              :skills
              [" Paint "]})

            model-command
            (plan-command
             plan)

            document
            (after
             model-command)

            normalized
            (normalized-plan
             plan)]
        (is
         (command/create?
          model-command))

        (is
         (=
          generated-membership-id
          (:xt/id
           document)))

        (is
         (=
          #{"paint"}
          (:membership/skills
           document)))

        (testing "User and Organization facts are guarded"
          (is
           (=
            #{[:user
               user-id]

              [:organization
               organization-id]}
            (set
             (guard-targets
              normalized)))))

        (testing "current Membership uniqueness is atomic"
          (is
           (=
            1
            (count
             (plan-assertions
              plan)))))

        (is
         (=
          :membership
          (:topic
           (plan-change
            plan))))

        (is
         (=
          :created
          (:change/kind
           (plan-change
            plan))))

        (is
         (=
          :create
          (:membership/operation
           (plan-change
            plan))))))))

;; =============================================================================
;; Ordinary Membership planners
;; =============================================================================

(deftest membership-update-planner-test
  (let [active
        (membership-document
         {:skills
          #{"paint"}})

        suspended
        (suspended-membership)]
    (with-model-documents
      [active]

      (let [add-plan
            (membership/plan-add-skill
             {:biff.fx/now
              t1}
             {:membership-id
              membership-id

              :skill
              "customer service"})

            remove-plan
            (membership/plan-remove-skill
             {:biff.fx/now
              t1}
             {:membership-id
              membership-id

              :skill
              "paint"})

            suspend-plan
            (membership/plan-suspend-membership
             {:biff.fx/now
              t1}
             {:membership-id
              membership-id})]
        (is
         (=
          :add-skill
          (command/operation
           (plan-command
            add-plan))))

        (is
         (=
          :remove-skill
          (command/operation
           (plan-command
            remove-plan))))

        (is
         (=
          :suspend
          (command/operation
           (plan-command
            suspend-plan))))

        (is
         (empty?
          (:guards
           (plan-fragment
            add-plan))))))

    (with-model-documents
      [suspended]

      (with-active-cross-model-dependencies
        (let [plan
              (membership/plan-reactivate-membership
               {:biff.fx/now
                t2}
               {:membership-id
                membership-id})

              normalized
              (normalized-plan
               plan)]
          (is
           (=
            :reactivate
            (command/operation
             (plan-command
              plan))))

          (is
           (=
            #{[:user
               user-id]

              [:organization
               organization-id]}
            (set
             (guard-targets
              normalized)))))))))

(deftest revoke-membership-does-not-cascade-role-assignments-test
  (with-model-documents
    [(membership-document)]

    (let [plan
          (membership/plan-revoke-membership
           {:biff.fx/now
            t1}
           {:membership-id
            membership-id})

          commands
          (:commands
           (plan-fragment
            plan))]
      (is
       (=
        1
        (count
         commands)))

      (is
       (=
        :membership
        (:model/entity-type
         (first
          commands))))

      (is
       (=
        :revoke
        (command/operation
         (first
          commands)))))))

;; =============================================================================
;; RoleAssignment creation planning
;; =============================================================================

(deftest create-role-assignment-plan-test
  (with-model-documents
    [(membership-document)]

    (with-active-cross-model-dependencies
      (with-redefs
       [fx/uuid7
        (fn [_seed _now]
          [generated-role-assignment-id])]

        (let [plan
              (membership/plan-create-role-assignment
               {:biff.fx/seed
                7

                :biff.fx/now
                t1}
               {:membership-id
                membership-id

                :role
                :helper

                :scope
                location-scope

                :actor-id
                actor-id

                :reason
                :test/granted})

              model-command
              (plan-command
               plan)

              document
              (after
               model-command)

              normalized
              (normalized-plan
               plan)]
          (is
           (command/create?
            model-command))

          (is
           (=
            generated-role-assignment-id
            (:xt/id
             document)))

          (is
           (=
            membership-id
            (:role-assignment/membership
             document)))

          (is
           (=
            location-scope
            (membership/role-assignment-scope
             document)))

          (testing "Membership, User, and Organization are dependencies"
            (is
             (=
              #{[:membership
                 membership-id]

                [:user
                 user-id]

                [:organization
                 organization-id]}
              (set
               (guard-targets
                normalized)))))

          (testing "duplicate exact active grant is prevented atomically"
            (is
             (=
              1
              (count
               (plan-assertions
                plan)))))

          (is
           (=
            :role-assignment
            (:topic
             (plan-change
              plan))))

          (is
           (=
            :create
            (:role-assignment/operation
             (plan-change
              plan)))))))))

(deftest role-assignment-scope-ownership-test
  (with-model-documents
    [(membership-document)]

    (with-redefs
     [user/require-user-dependency
      (fn [& _]
        (user-dependency-value))

      organization/require-scope-dependency
      (fn [& _]
        (scope-dependency-value
         other-organization-context))

      fx/uuid7
      (fn [& _]
        [generated-role-assignment-id])]

      (is
       (=
        :membership/scope-ownership-mismatch
        (error-type
         #(membership/plan-create-role-assignment
           {:biff.fx/seed
            7

            :biff.fx/now
            t1}
           {:membership-id
            membership-id

            :role
            :helper

            :scope
            other-organization-scope})))))))

;; =============================================================================
;; RoleAssignment revocation planning
;; =============================================================================

(deftest revoke-role-assignment-plan-test
  (let [assignment
        (role-assignment-document)]
    (with-model-documents
      [assignment]

      (let [plan
            (membership/plan-revoke-role-assignment
             {:biff.fx/now
              t1}
             {:role-assignment-id
              helper-assignment-id

              :actor-id
              actor-id

              :reason
              :test/revoked})

            model-command
            (plan-command
             plan)]
        (is
         (command/update?
          model-command))

        (is
         (=
          :revoke
          (command/operation
           model-command)))

        (is
         (membership/role-assignment-revoked?
          (after
           model-command)))))))

;; =============================================================================
;; Positive atomic authorization proof
;; =============================================================================

(deftest role-dependency-test
  (let [membership-document
        (membership-document)

        granting-assignment
        (role-assignment-document
         membership-document
         {:scope
          organization-scope})]
    (with-redefs
     [membership.graph/current-membership
      (fn [_ctx requested-user-id requested-organization-id]
        (is
         (=
          user-id
          requested-user-id))

        (is
         (=
          organization-id
          requested-organization-id))

        membership-document)

      membership.graph/active-role-assignments-for-membership
      (fn [_ctx requested-membership-id]
        (is
         (=
          membership-id
          requested-membership-id))

        [granting-assignment])

      user/require-user-dependency
      (fn [& _]
        (user-dependency-value))

      organization/require-scope-dependency
      (fn [_ctx requested-scope]
        (is
         (organization/same-scope?
          location-scope
          requested-scope))

        (scope-dependency-value
         location-context))]

      (let [{:keys
             [user
              membership
              role-assignment
              scope-context
              role
              transaction-fragment]}
            (membership/require-helper-dependency
             {}
             user-id
             location-scope)]
        (is
         (=
          user-id
          (:xt/id
           user)))

        (is
         (=
          membership-document
          membership))

        (is
         (=
          granting-assignment
          role-assignment))

        (is
         (=
          location-context
          scope-context))

        (is
         (=
          :helper
          role))

        (is
         (=
          #{[:user
             user-id]

            [:membership
             membership-id]

            [:role-assignment
             helper-assignment-id]

            [:organization
             organization-id]}
          (set
           (guard-targets
            transaction-fragment))))))))

(deftest role-dependency-denial-test
  (let [membership-document
        (membership-document)

        supervisor
        (role-assignment-document
         membership-document
         {:role
          :supervisor

          :scope
          organization-scope})]
    (with-redefs
     [membership.graph/current-membership
      (fn [& _]
        membership-document)

      membership.graph/active-role-assignments-for-membership
      (fn [& _]
        [supervisor])

      user/require-user-dependency
      (fn [& _]
        (user-dependency-value))

      organization/require-scope-dependency
      (fn [& _]
        (scope-dependency-value
         location-context))]

      (is
       (nil?
        (membership/helper-dependency
         {}
         user-id
         location-scope)))

      (is
       (=
        :membership/access-denied
        (error-type
         #(membership/require-helper-dependency
           {}
           user-id
           location-scope)))))))

(deftest role-dependency-rejects-inactive-user-test
  (let [membership-document
        (membership-document)

        assignment
        (role-assignment-document
         membership-document
         {:scope
          organization-scope})]
    (with-redefs
     [membership.graph/current-membership
      (fn [& _]
        membership-document)

      membership.graph/active-role-assignments-for-membership
      (fn [& _]
        [assignment])

      user/require-user-dependency
      (fn [& _]
        (user-dependency-value
         (fake-user
          :suspended)))

      organization/require-scope-dependency
      (fn [& _]
        (scope-dependency-value
         location-context))]

      (is
       (=
        :membership/user-not-active
        (error-type
         #(membership/helper-dependency
           {}
           user-id
           location-scope)))))))

(deftest role-dependency-rejects-suspended-membership-test
  (let [suspended
        (suspended-membership)

        assignment
        (role-assignment-document
         (membership-document)
         {:scope
          organization-scope})]
    (with-redefs
     [membership.graph/current-membership
      (fn [& _]
        suspended)

      membership.graph/active-role-assignments-for-membership
      (fn [& _]
        [assignment])

      user/require-user-dependency
      (fn [& _]
        (user-dependency-value))

      organization/require-scope-dependency
      (fn [& _]
        (scope-dependency-value
         location-context))]

      (is
       (nil?
        (membership/helper-dependency
         {}
         user-id
         location-scope)))

      (is
       (=
        :membership/access-denied
        (error-type
         #(membership/require-helper-dependency
           {}
           user-id
           location-scope)))))))

(deftest role-dependency-rejects-non-operational-scope-test
  (with-redefs
   [organization/require-scope-dependency
    (fn [& _]
      (scope-dependency-value
       non-operational-location-context))]

    (is
     (=
      :membership/scope-not-operational
      (error-type
       #(membership/helper-dependency
         {}
         user-id
         location-scope))))))

;; =============================================================================
;; Planner context failures
;; =============================================================================

(deftest planner-context-test
  (testing "creation requires framework seed"
    (with-active-cross-model-dependencies
      (is
       (=
        :membership.fx/missing-seed
        (error-type
         #(membership/plan-create-membership
           {:biff.fx/now
            t1}
           {:user-id
            user-id

            :organization-id
            organization-id}))))))

  (testing "updates require framework time"
    (with-model-documents
      [(membership-document)]

      (is
       (=
        :membership.fx/missing-now
        (error-type
         #(membership/plan-add-skill
           {}
           {:membership-id
            membership-id

            :skill
            "paint"}))))))

  (testing "missing Membership is distinct"
    (with-model-documents
      []

      (is
       (=
        :membership/not-found
        (error-type
         #(membership/plan-add-skill
           {:biff.fx/now
            t1}
           {:membership-id
            missing-id

            :skill
            "paint"})))))))

;; =============================================================================
;; Gesso plan compatibility
;; =============================================================================

(deftest membership-plans-normalize-test
  (with-model-documents
    [(membership-document
      {:skills
       #{"paint"}})]

    (doseq [plan
            [(membership/plan-add-skill
              {:biff.fx/now
               t1}
              {:membership-id
               membership-id

               :skill
               "customer service"})

             (membership/plan-remove-skill
              {:biff.fx/now
               t1}
              {:membership-id
               membership-id

               :skill
               "paint"})

             (membership/plan-suspend-membership
              {:biff.fx/now
               t1}
              {:membership-id
               membership-id})]]
      (let [normalized
            (normalized-plan
             plan)]
        (is
         (=
          1
          (count
           (:commands
            normalized))))

        (is
         (=
          1
          (count
           (:changes
            normalized))))

        (is
         (ifn?
          (:entry-fn
           normalized)))))))
