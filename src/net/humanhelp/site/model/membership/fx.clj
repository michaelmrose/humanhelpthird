(ns net.humanhelp.site.model.membership.fx
  "Membership-specific dependencies, authorization proofs, and transaction
   planning.

   This namespace is the integration layer between Membership's pure domain and
   the stable public APIs of User and Organization.

   It owns:

   - Membership and RoleAssignment persistence reads needed by mutations;
   - generated Membership and RoleAssignment IDs;
   - atomic uniqueness assertions;
   - User and Organization dependencies required by Membership decisions;
   - role-specific authorization dependencies for other models;
   - semantic Gesso Live changes;
   - composable gesso.model transaction plans.

   It does not commit transactions. Callers compose returned fragments and
   commit the complete plan through gesso.model.tx."
  (:require
   [com.biffweb.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.membership.domain :as membership]
   [net.humanhelp.site.model.membership.graph :as membership.graph]
   [net.humanhelp.site.model.membership.schema :as membership.schema]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Errors and FX context
;; =============================================================================

(defn- fail!
  ([type message]
   (fail! type message nil))
  ([type message details]
   (throw
    (ex-info
     message
     (cond->
      {:error/type type}

       (some? details)
       (assoc
        :error/details
        details))))))

(defn- now!
  [ctx]
  (or
   (:biff.fx/now ctx)

   (fail!
    :membership.fx/missing-now
    "Membership planning requires :biff.fx/now.")))

(defn- seed!
  [ctx]
  (or
   (:biff.fx/seed ctx)

   (fail!
    :membership.fx/missing-seed
    "Membership creation requires :biff.fx/seed.")))

(defn- generated-id
  [ctx]
  (first
   (fx/uuid7
    (seed! ctx)
    (now! ctx))))

(defn- require-uuid!
  [value type label details-key]
  (when-not
   (uuid? value)
    (fail!
     type
     (str label " must be a UUID.")
     {details-key value}))
  value)

(defn- require-membership-id!
  [membership-id]
  (require-uuid!
   membership-id
   :membership/invalid-membership-id
   "Membership ID"
   :membership/id))

(defn- require-role-assignment-id!
  [role-assignment-id]
  (require-uuid!
   role-assignment-id
   :role-assignment/invalid-role-assignment-id
   "RoleAssignment ID"
   :role-assignment/id))

;; =============================================================================
;; Conventional persisted reads
;; =============================================================================

(defn- load-membership
  [ctx membership-id]
  (model/load-by-id
   membership.schema/membership-descriptor
   ctx
   (require-membership-id!
    membership-id)))

(defn- require-membership!
  [ctx membership-id]
  (or
   (load-membership
    ctx
    membership-id)

   (fail!
    :membership/not-found
    "The Membership does not exist."
    {:membership/id membership-id})))

(defn- load-role-assignment
  [ctx role-assignment-id]
  (model/load-by-id
   membership.schema/role-assignment-descriptor
   ctx
   (require-role-assignment-id!
    role-assignment-id)))

(defn- require-role-assignment!
  [ctx role-assignment-id]
  (or
   (load-role-assignment
    ctx
    role-assignment-id)

   (fail!
    :role-assignment/not-found
    "The RoleAssignment does not exist."
    {:role-assignment/id
     role-assignment-id})))

;; =============================================================================
;; Generic Membership-owned dependency guards
;; =============================================================================

(defn- membership-guard
  [membership-document]
  (command/guard
   membership/membership-entity-type
   membership-document
   membership/membership-version))

(defn- role-assignment-guard
  [role-assignment]
  (command/guard
   membership/role-assignment-entity-type
   role-assignment
   membership/role-assignment-version))

(defn membership-dependency
  "Returns Membership plus a guard-only transaction fragment.

   Use this from another top-level model when a decision depends on the current
   Membership document.

   Returns nil when Membership does not exist."
  [ctx membership-id]
  (when-let [membership-document
             (load-membership
              ctx
              membership-id)]
    {:membership
     membership-document

     :transaction-fragment
     (model.tx/guards-fragment
      (membership-guard
       membership-document))}))

(defn require-membership-dependency
  "Returns membership-dependency or throws when Membership does not exist."
  [ctx membership-id]
  (or
   (membership-dependency
    ctx
    membership-id)

   (fail!
    :membership/not-found
    "The Membership does not exist."
    {:membership/id membership-id})))

(defn current-membership-dependency
  "Returns the current non-revoked Membership between User and Organization plus
   a guard-only transaction fragment.

   Suspended Memberships are current relationships and are therefore returned.

   Returns nil when there is no current Membership."
  [ctx user-id organization-id]
  (when-let [membership-document
             (membership.graph/current-membership
              ctx
              user-id
              organization-id)]
    {:membership
     membership-document

     :transaction-fragment
     (model.tx/guards-fragment
      (membership-guard
       membership-document))}))

;; =============================================================================
;; Cross-model requirements
;; =============================================================================

(defn- require-active-user-dependency!
  [ctx user-id]
  (let [{user-document
         :user

         :as dependency}
        (user/require-user-dependency
         ctx
         user-id)]
    (when-not
     (user/active?
      user-document)
      (fail!
       :membership/user-not-active
       "The Membership operation requires an active User."
       {:user/id
        user-id

        :user/status
        (user/user-status
         user-document)}))

    dependency))

(defn- require-operational-scope-dependency!
  [ctx scope]
  (let [{:keys
         [scope-context]
         :as   dependency}
        (organization/require-scope-dependency
         ctx
         scope)]
    (when-not
     (organization/scope-context-operational?
      scope-context)
      (fail!
       :membership/scope-not-operational
       "The Membership operation requires an operational Organization scope."
       {:scope
        (organization/scope-context-target
         scope-context)

        :organization/id
        (organization/scope-context-organization-id
         scope-context)}))

    dependency))

(defn- require-membership-scope-ownership!
  [membership-document scope-context]
  (let [membership-organization-id
        (membership/membership-organization-id
         membership-document)

        scope-organization-id
        (organization/scope-context-organization-id
         scope-context)]
    (when-not
     (=
      membership-organization-id
      scope-organization-id)
      (fail!
       :membership/scope-ownership-mismatch
       "The Organization scope belongs to a different Organization than the Membership."
       {:membership/id
        (membership/membership-id
         membership-document)

        :membership/organization
        membership-organization-id

        :scope
        (organization/scope-context-target
         scope-context)

        :scope/organization
        scope-organization-id}))

    scope-context))

;; =============================================================================
;; Atomic cardinality assertions
;; =============================================================================

(defn- assert-no-current-membership
  [user-id organization-id]
  (model.tx/assert-none
   membership/membership-entity-type
   [:and
    [:=
     :membership/user
     user-id]

    [:=
     :membership/organization
     organization-id]

    [:or
     [:=
      :membership/status
      :active]

     [:=
      :membership/status
      :suspended]]]))

(defn- assert-no-active-role-assignment
  [membership-id role scope]
  (model.tx/assert-none
   membership/role-assignment-entity-type
   [:and
    [:=
     :role-assignment/membership
     membership-id]

    [:=
     :role-assignment/role
     role]

    [:=
     :role-assignment/scope-type
     (organization/scope-type
      scope)]

    [:=
     :role-assignment/scope-id
     (organization/scope-id
      scope)]

    [:=
     :role-assignment/status
     :active]]))

;; =============================================================================
;; Semantic changes
;; =============================================================================

(defn- change-entry
  [{:keys
    [topic
     id]}]
  {:coalesce-key
   [topic id]})

(def ^:private transaction-options
  {:entry-fn
   change-entry})

(defn- membership-change
  [model-command]
  (let [document
        (command/after
         model-command)]
    {:topic
     :membership

     :id
     (:xt/id document)

     :change/kind
     (if
      (command/create?
       model-command)
       :created
       :updated)

     :membership/operation
     (command/operation
      model-command)

     :membership/id
     (:xt/id document)

     :user/id
     (:membership/user document)

     :organization/id
     (:membership/organization document)

     :membership/status
     (:membership/status document)

     :membership/revision
     (:membership/revision document)}))

(defn- role-assignment-change
  [model-command]
  (let [document
        (command/after
         model-command)]
    {:topic
     :role-assignment

     :id
     (:xt/id document)

     :change/kind
     (if
      (command/create?
       model-command)
       :created
       :updated)

     :role-assignment/operation
     (command/operation
      model-command)

     :role-assignment/id
     (:xt/id document)

     :membership/id
     (:role-assignment/membership
      document)

     :role
     (:role-assignment/role
      document)

     :scope
     (membership/role-assignment-scope
      document)

     :role-assignment/status
     (:role-assignment/status
      document)

     :role-assignment/revision
     (:role-assignment/revision
      document)}))

;; =============================================================================
;; Generic plan construction
;; =============================================================================

(defn- membership-mutation-fragment
  ([model-command]
   (membership-mutation-fragment
    model-command
    []))

  ([model-command assertions]
   (model.tx/fragment
    {:commands
     [model-command]

     :assertions
     assertions

     :changes
     [(membership-change
       model-command)]})))

(defn- role-assignment-mutation-fragment
  ([model-command]
   (role-assignment-mutation-fragment
    model-command
    []))

  ([model-command assertions]
   (model.tx/fragment
    {:commands
     [model-command]

     :assertions
     assertions

     :changes
     [(role-assignment-change
       model-command)]})))

(defn- membership-plan
  ([model-command]
   (membership-plan
    model-command
    model.tx/empty-fragment
    []))

  ([model-command dependency-fragment]
   (membership-plan
    model-command
    dependency-fragment
    []))

  ([model-command dependency-fragment assertions]
   {:result
    {:membership
     (command/after
      model-command)}

    :transaction-fragment
    (model.tx/compose
     dependency-fragment
     (membership-mutation-fragment
      model-command
      assertions))

    :transaction-options
    transaction-options}))

(defn- role-assignment-plan
  ([model-command]
   (role-assignment-plan
    model-command
    model.tx/empty-fragment
    []))

  ([model-command dependency-fragment]
   (role-assignment-plan
    model-command
    dependency-fragment
    []))

  ([model-command dependency-fragment assertions]
   {:result
    {:role-assignment
     (command/after
      model-command)}

    :transaction-fragment
    (model.tx/compose
     dependency-fragment
     (role-assignment-mutation-fragment
      model-command
      assertions))

    :transaction-options
    transaction-options}))

;; =============================================================================
;; Membership creation
;; =============================================================================

(defn plan-create-membership
  "Plans one durable User ↔ Organization Membership.

   Creation requires:

   - an active User;
   - an operational Organization;
   - no existing active or suspended Membership for the pair.

   The User and Organization dependencies are guarded in the same transaction
   as Membership creation."
  [ctx {:keys
        [user-id
         organization-id
         skills]}]
  (let [user-dependency
        (require-active-user-dependency!
         ctx
         user-id)

        organization-scope
        (organization/organization-scope
         organization-id)

        organization-dependency
        (require-operational-scope-dependency!
         ctx
         organization-scope)

        model-command
        (membership/create-membership-command
         {:id
          (generated-id ctx)

          :user-id
          user-id

          :organization-id
          organization-id

          :skills
          (or
           skills
           #{})

          :now
          (now! ctx)})]
    (membership-plan
     model-command

     (model.tx/compose
      (:transaction-fragment
       user-dependency)

      (:transaction-fragment
       organization-dependency))

     [(assert-no-current-membership
       user-id
       organization-id)])))

;; =============================================================================
;; Ordinary Membership mutations
;; =============================================================================

(defn- plan-membership-update
  [ctx membership-id command-fn input]
  (let [membership-document
        (require-membership!
         ctx
         membership-id)

        model-command
        (command-fn
         membership-document
         (assoc
          input
          :now
          (now! ctx)))]
    (membership-plan
     model-command)))

(defn plan-add-skill
  [ctx {:keys
        [membership-id]
        :as   input}]
  (plan-membership-update
   ctx
   membership-id
   membership/add-skill-command
   {:skill
    (:skill input)}))

(defn plan-remove-skill
  [ctx {:keys
        [membership-id]
        :as   input}]
  (plan-membership-update
   ctx
   membership-id
   membership/remove-skill-command
   {:skill
    (:skill input)}))

(defn plan-suspend-membership
  [ctx {:keys
        [membership-id]
        :as   input}]
  (plan-membership-update
   ctx
   membership-id
   membership/suspend-membership-command
   {:actor-id
    (:actor-id input)

    :reason
    (:reason input)}))

(defn plan-revoke-membership
  "Plans terminal revocation of Membership.

   RoleAssignments do not need to be rewritten when their Membership is
   revoked. An assignment is effective only while its owning Membership is
   active, and revoked Memberships cannot be reactivated. Keeping assignment
   history intact avoids a redundant cascading mutation and avoids pretending
   that a query-derived cascade can protect against concurrent phantom inserts."
  [ctx {:keys
        [membership-id]
        :as   input}]
  (plan-membership-update
   ctx
   membership-id
   membership/revoke-membership-command
   {:actor-id
    (:actor-id input)

    :reason
    (:reason input)}))

;; =============================================================================
;; Membership reactivation
;; =============================================================================

(defn plan-reactivate-membership
  "Reactivation re-establishes current organizational authority and therefore
   revalidates both the User and Organization.

   The Membership update command supplies its own version precondition. User
   and Organization are separate guarded dependencies."
  [ctx {:keys
        [membership-id]}]
  (let [membership-document
        (require-membership!
         ctx
         membership-id)

        user-dependency
        (require-active-user-dependency!
         ctx
         (membership/membership-user-id
          membership-document))

        organization-dependency
        (require-operational-scope-dependency!
         ctx
         (organization/organization-scope
          (membership/membership-organization-id
           membership-document)))

        model-command
        (membership/reactivate-membership-command
         membership-document
         {:now
          (now! ctx)})]
    (membership-plan
     model-command

     (model.tx/compose
      (:transaction-fragment
       user-dependency)

      (:transaction-fragment
       organization-dependency)))))

;; =============================================================================
;; RoleAssignment creation
;; =============================================================================

(defn plan-create-role-assignment
  "Plans one role grant to an active Membership at an operational Organization
   scope.

   The Membership, User, and complete Organization scope derivation are guarded.
   A transaction assertion prevents concurrent duplicate active grants for the
   same Membership, role, and exact scope."
  [ctx {:keys
        [membership-id
         role
         scope
         actor-id
         reason]}]
  (let [membership-dependency
        (require-membership-dependency
         ctx
         membership-id)

        membership-document
        (:membership
         membership-dependency)

        user-dependency
        (require-active-user-dependency!
         ctx
         (membership/membership-user-id
          membership-document))

        scope-dependency
        (require-operational-scope-dependency!
         ctx
         scope)

        scope-context
        (:scope-context
         scope-dependency)

        _
        (require-membership-scope-ownership!
         membership-document
         scope-context)

        model-command
        (membership/create-role-assignment-command
         membership-document
         {:id
          (generated-id ctx)

          :role
          role

          :scope
          scope

          :actor-id
          actor-id

          :reason
          reason

          :now
          (now! ctx)})]
    (role-assignment-plan
     model-command

     (model.tx/compose
      (:transaction-fragment
       membership-dependency)

      (:transaction-fragment
       user-dependency)

      (:transaction-fragment
       scope-dependency))

     [(assert-no-active-role-assignment
       membership-id
       role
       scope)])))

;; =============================================================================
;; RoleAssignment revocation
;; =============================================================================

(defn plan-revoke-role-assignment
  [ctx {:keys
        [role-assignment-id
         actor-id
         reason]}]
  (let [role-assignment
        (require-role-assignment!
         ctx
         role-assignment-id)

        model-command
        (membership/revoke-role-assignment-command
         role-assignment
         {:actor-id
          actor-id

          :reason
          reason

          :now
          (now! ctx)})]
    (role-assignment-plan
     model-command)))

;; =============================================================================
;; Role-specific atomic authorization dependencies
;; =============================================================================

(defn- authorization-state
  [ctx user-id scope role]
  (when-not
   (membership/role?
    role)
    (fail!
     :membership/invalid-role
     "Authorization requires a valid Membership role."
     {:role role}))

  (let [scope-dependency
        (require-operational-scope-dependency!
         ctx
         scope)

        scope-context
        (:scope-context
         scope-dependency)

        organization-id
        (organization/scope-context-organization-id
         scope-context)

        membership-document
        (membership.graph/current-membership
         ctx
         user-id
         organization-id)]

    (when
     membership-document

      (let [user-dependency
            (require-active-user-dependency!
             ctx
             user-id)

            role-assignments
            (membership.graph/active-role-assignments-for-membership
             ctx
             (membership/membership-id
              membership-document))

            granting-assignment
            (membership/effective-role-assignment
             membership-document
             role-assignments
             scope-context
             role)]

        (when
         granting-assignment
          {:user
           (:user
            user-dependency)

           :membership
           membership-document

           :role-assignment
           granting-assignment

           :scope-context
           scope-context

           :role
           role

           :transaction-fragment
           (model.tx/compose
            (:transaction-fragment
             user-dependency)

            (model.tx/guards-fragment
             (membership-guard
              membership-document)

             (role-assignment-guard
              granting-assignment))

            (:transaction-fragment
             scope-dependency))})))))

(defn role-dependency
  "Returns an atomic proof that User currently holds role at scope.

   The returned transaction fragment guards exactly the persisted facts used by
   the successful authorization decision:

   - the active User;
   - the active Membership;
   - one granting active RoleAssignment;
   - every Organization document used to establish scope ancestry and
     operational state.

   Returns nil when the User has no effective grant.

   This API intentionally proves one positive role requirement rather than
   promising an exact stable snapshot of every role assignment."
  [ctx user-id scope role]
  (authorization-state
   ctx
   user-id
   scope
   role))

(defn require-role-dependency
  "Returns role-dependency or throws when User lacks the requested authority."
  [ctx user-id scope role]
  (or
   (role-dependency
    ctx
    user-id
    scope
    role)

   (fail!
    :membership/access-denied
    "The User does not hold the required role at this Organization scope."
    {:user/id
     user-id

     :scope
     scope

     :required-role
     role})))

(defn helper-dependency
  [ctx user-id scope]
  (role-dependency
   ctx
   user-id
   scope
   :helper))

(defn require-helper-dependency
  [ctx user-id scope]
  (require-role-dependency
   ctx
   user-id
   scope
   :helper))

(defn supervisor-dependency
  [ctx user-id scope]
  (role-dependency
   ctx
   user-id
   scope
   :supervisor))

(defn require-supervisor-dependency
  [ctx user-id scope]
  (require-role-dependency
   ctx
   user-id
   scope
   :supervisor))

(defn admin-dependency
  [ctx user-id scope]
  (role-dependency
   ctx
   user-id
   scope
   :admin))

(defn require-admin-dependency
  [ctx user-id scope]
  (require-role-dependency
   ctx
   user-id
   scope
   :admin))

;; =============================================================================
;; Non-transactional effective-role reads
;; =============================================================================

(defn effective-role-state
  "Returns Membership authorization state for display/read decisions.

   Unlike role-dependency, this function does not claim atomic stability and
   therefore returns no transaction fragment.

   Returns nil when User has no current Membership in the scope's Organization."
  [ctx user-id scope]
  (let [scope-context
        (organization/require-scope-context
         ctx
         scope)

        organization-id
        (organization/scope-context-organization-id
         scope-context)]

    (when-let [membership-document
               (membership.graph/current-membership
                ctx
                user-id
                organization-id)]

      (let [role-assignments
            (membership.graph/active-role-assignments-for-membership
             ctx
             (membership/membership-id
              membership-document))]

        {:membership
         membership-document

         :scope-context
         scope-context

         :role-assignments
         role-assignments

         :roles
         (membership/effective-roles
          membership-document
          role-assignments
          scope-context)}))))
