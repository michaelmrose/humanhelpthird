(ns net.humanhelp.site.model.membership.core
  "Stable public boundary for the HumanHelp Membership model.

   Code outside net.humanhelp.site.model.membership should depend on this
   namespace rather than Membership domain, schema, Graph, or FX internals.

   Membership owns:

   - the durable User ↔ Organization relationship;
   - organization-local skills;
   - RoleAssignments;
   - Membership-side role applicability over Organization scopes;
   - convenient read-side effective-role queries;
   - guarded authorization dependencies for atomic cross-model decisions.

   User owns global identity and account lifecycle.
   Organization owns hierarchy and scope structure.

   There are deliberately three distinct authorization surfaces:

   1. Pure Membership evaluation over already-loaded values:

        effective-role-assignments-for-membership
        effective-roles-for-membership
        membership-has-role?

   2. Ordinary read-side queries:

        effective-role-state
        effective-roles
        has-role?
        helper?
        supervisor?
        admin?
        staff?

   3. Atomic authorization dependencies for mutations:

        role-dependency
        require-role-dependency
        helper-dependency
        require-helper-dependency
        ...

   Read-side authorization is convenient but is not an atomic proof.
   Mutation authorization should use the dependency APIs."
  (:require
   [gesso.model.core :as model]
   [net.humanhelp.site.model.membership.domain :as domain]
   [net.humanhelp.site.model.membership.fx :as membership.fx]
   [net.humanhelp.site.model.membership.graph :as membership.graph]
   [net.humanhelp.site.model.membership.schema :as membership.schema]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Model registration
;; =============================================================================

(def module
  "Membership's Biff module contribution.

   gesso.model supplies conventional persisted schema and entity resolvers for
   Membership and RoleAssignment.

   Install gesso.model.tx/module once separately at the application level."
  (model/build-module
   membership.schema/descriptors
   {:schema
    membership.schema/custom-schema

    :resolvers
    membership.graph/custom-resolvers}))

(def schema
  (:schema module))

(def resolvers
  (:biff.graph/resolvers module))

;; =============================================================================
;; Errors
;; =============================================================================

(defn- fail!
  ([type message]
   (fail!
    type
    message
    nil))

  ([type message details]
   (throw
    (ex-info
     message
     (cond->
      {:error/type
       type}

       (some? details)
       (assoc
        :error/details
        details))))))

(defn- require-membership-id!
  [membership-id]
  (when-not
   (uuid?
    membership-id)
    (fail!
     :membership.core/invalid-membership-id
     "Membership ID must be a UUID."
     {:membership/id
      membership-id}))

  membership-id)

(defn- require-role-assignment-id!
  [role-assignment-id]
  (when-not
   (uuid?
    role-assignment-id)
    (fail!
     :membership.core/invalid-role-assignment-id
     "RoleAssignment ID must be a UUID."
     {:role-assignment/id
      role-assignment-id}))

  role-assignment-id)

;; =============================================================================
;; Membership reads
;; =============================================================================

(defn membership
  "Returns Membership by UUID, or nil when absent."
  [ctx membership-id]
  (model/load-by-id
   membership.schema/membership-descriptor
   ctx
   (require-membership-id!
    membership-id)))

(defn require-membership
  "Returns Membership by UUID or throws when absent."
  [ctx membership-id]
  (or
   (membership
    ctx
    membership-id)

   (fail!
    :membership/not-found
    "The Membership does not exist."
    {:membership/id
     membership-id})))

(defn memberships-for-user
  "Returns all historical Memberships belonging to User."
  [ctx user-id]
  (membership.graph/memberships-for-user
   ctx
   user-id))

(defn memberships-for-organization
  "Returns all historical Memberships belonging to Organization."
  [ctx organization-id]
  (membership.graph/memberships-for-organization
   ctx
   organization-id))

(defn memberships-for-user-and-organization
  "Returns every historical Membership linking User and Organization."
  [ctx user-id organization-id]
  (membership.graph/memberships-for-user-and-organization
   ctx
   user-id
   organization-id))

(defn current-membership
  "Returns the one current non-revoked Membership linking User to Organization.

   A suspended Membership remains the current relationship, although it grants
   no current role authority.

   Returns nil when no current Membership exists."
  [ctx user-id organization-id]
  (membership.graph/current-membership
   ctx
   user-id
   organization-id))

(defn require-current-membership
  "Returns User's current Membership in Organization or throws when absent."
  [ctx user-id organization-id]
  (or
   (current-membership
    ctx
    user-id
    organization-id)

   (fail!
    :membership/not-found
    "The User has no current Membership in this Organization."
    {:user/id
     user-id

     :organization/id
     organization-id})))

(defn active-memberships-for-organization
  "Returns Organization's active Memberships."
  [ctx organization-id]
  (membership.graph/active-memberships-for-organization
   ctx
   organization-id))

;; =============================================================================
;; RoleAssignment reads
;; =============================================================================

(defn role-assignment
  "Returns RoleAssignment by UUID, or nil when absent."
  [ctx role-assignment-id]
  (model/load-by-id
   membership.schema/role-assignment-descriptor
   ctx
   (require-role-assignment-id!
    role-assignment-id)))

(defn require-role-assignment
  "Returns RoleAssignment by UUID or throws when absent."
  [ctx role-assignment-id]
  (or
   (role-assignment
    ctx
    role-assignment-id)

   (fail!
    :role-assignment/not-found
    "The RoleAssignment does not exist."
    {:role-assignment/id
     role-assignment-id})))

(defn role-assignments-for-membership
  "Returns all historical RoleAssignments owned by Membership."
  [ctx membership-id]
  (membership.graph/role-assignments-for-membership
   ctx
   membership-id))

(defn active-role-assignments-for-membership
  "Returns Membership's active RoleAssignments."
  [ctx membership-id]
  (membership.graph/active-role-assignments-for-membership
   ctx
   membership-id))

(defn role-assignments-at-scope
  "Returns all historical RoleAssignments at one exact Organization scope."
  [ctx scope]
  (membership.graph/role-assignments-at-scope
   ctx
   scope))

(defn active-role-assignments-at-scope
  "Returns active RoleAssignments at one exact Organization scope."
  [ctx scope]
  (membership.graph/active-role-assignments-at-scope
   ctx
   scope))

(defn active-role-assignments-for-membership-at-scope
  "Returns Membership's active RoleAssignments at one exact Organization scope."
  [ctx membership-id scope]
  (membership.graph/active-role-assignments-for-membership-at-scope
   ctx
   membership-id
   scope))

;; =============================================================================
;; Pure Membership-side authorization
;; =============================================================================

(defn effective-role-assignments-for-membership
  "Returns the supplied RoleAssignments that are effective for Membership at
   scope-context.

   This is a pure calculation over already-loaded values. It performs no
   persistence reads and does not establish User account state."
  [membership-document role-assignments scope-context]
  (domain/effective-role-assignments
   membership-document
   role-assignments
   scope-context))

(defn effective-roles-for-membership
  "Returns the exact roles Membership holds at scope-context.

   Roles are exact grants. Supervisor does not implicitly grant Helper, and
   Admin does not implicitly grant Supervisor or Helper.

   This is a pure calculation over already-loaded values."
  [membership-document role-assignments scope-context]
  (domain/effective-roles
   membership-document
   role-assignments
   scope-context))

(defn membership-has-role?
  "Returns whether already-loaded Membership state grants role at
   scope-context.

   This is a pure Membership-side predicate and does not establish User
   account state."
  [membership-document role-assignments scope-context role]
  (domain/has-role?
   membership-document
   role-assignments
   scope-context
   role))

;; =============================================================================
;; Ordinary read-side authorization
;; =============================================================================

(defn effective-role-state
  "Returns User's current effective Membership role state at scope.

   Returns nil when:

   - User does not exist;
   - User is not active;
   - User has no current Membership in the target Organization.

   A returned state contains the ordinary read values used to derive the
   result:

     :user
     :membership
     :scope-context
     :role-assignments
     :roles

   This is intentionally a read-side convenience API. It provides no
   transaction guards and must not be used as an atomic authorization proof for
   a mutation."
  [ctx user-id scope]
  (let [scope-context
        (organization/require-scope-context
         ctx
         scope)

        organization-id
        (organization/scope-context-organization-id
         scope-context)]

    (when-let [user-document
               (user/user
                ctx
                user-id)]

      (when
       (user/active?
        user-document)

        (when-let [membership-document
                   (membership.graph/current-membership
                    ctx
                    user-id
                    organization-id)]

          (let [role-assignments
                (membership.graph/active-role-assignments-for-membership
                 ctx
                 (domain/membership-id
                  membership-document))

                roles
                (domain/effective-roles
                 membership-document
                 role-assignments
                 scope-context)]

            {:user
             user-document

             :membership
             membership-document

             :scope-context
             scope-context

             :role-assignments
             role-assignments

             :roles
             roles}))))))

(defn effective-roles
  "Returns User's effective exact roles at scope.

   Returns the empty set when User has no effective Membership authority.

   This is an ordinary read. Use require-role-dependency or a role-specific
   dependency function when authorizing a mutation."
  [ctx user-id scope]
  (or
   (:roles
    (effective-role-state
     ctx
     user-id
     scope))

   #{}))

(defn has-role?
  "Returns whether User currently has role at scope.

   This is an ordinary read-side predicate, not an atomic authorization proof."
  [ctx user-id scope role]
  (contains?
   (effective-roles
    ctx
    user-id
    scope)
   role))

(defn helper?
  "Returns whether User currently has the exact :helper role at scope."
  [ctx user-id scope]
  (has-role?
   ctx
   user-id
   scope
   :helper))

(defn supervisor?
  "Returns whether User currently has the exact :supervisor role at scope."
  [ctx user-id scope]
  (has-role?
   ctx
   user-id
   scope
   :supervisor))

(defn admin?
  "Returns whether User currently has the exact :admin role at scope."
  [ctx user-id scope]
  (has-role?
   ctx
   user-id
   scope
   :admin))

(defn staff?
  "Returns whether User currently has at least one effective staff role at
   scope."
  [ctx user-id scope]
  (boolean
   (seq
    (effective-roles
     ctx
     user-id
     scope))))

;; =============================================================================
;; Guarded Membership dependencies
;; =============================================================================

(defn membership-dependency
  "Returns Membership plus a guard-only transaction fragment.

   Another top-level model should use this when an atomic decision depends on
   the current Membership document.

   Returns nil when Membership does not exist."
  [ctx membership-id]
  (membership.fx/membership-dependency
   ctx
   membership-id))

(defn require-membership-dependency
  "Returns membership-dependency or throws when Membership does not exist."
  [ctx membership-id]
  (membership.fx/require-membership-dependency
   ctx
   membership-id))

(defn current-membership-dependency
  "Returns User's current Membership in Organization plus its guard-only
   transaction fragment.

   Returns nil when User has no current Membership in Organization."
  [ctx user-id organization-id]
  (membership.fx/current-membership-dependency
   ctx
   user-id
   organization-id))

;; =============================================================================
;; Atomic role authorization dependencies
;; =============================================================================

(defn role-dependency
  "Returns an atomic positive authorization proof that User currently holds
   role at scope.

   The returned dependency contains the documents and guards needed to keep
   the successful authorization decision valid until commit.

   Returns nil when the requested role is not currently effective."
  [ctx user-id scope role]
  (membership.fx/role-dependency
   ctx
   user-id
   scope
   role))

(defn require-role-dependency
  "Returns role-dependency or throws when User lacks role at scope."
  [ctx user-id scope role]
  (membership.fx/require-role-dependency
   ctx
   user-id
   scope
   role))

(defn helper-dependency
  "Returns an atomic helper authorization proof, or nil."
  [ctx user-id scope]
  (membership.fx/helper-dependency
   ctx
   user-id
   scope))

(defn require-helper-dependency
  "Returns an atomic helper authorization proof or throws."
  [ctx user-id scope]
  (membership.fx/require-helper-dependency
   ctx
   user-id
   scope))

(defn supervisor-dependency
  "Returns an atomic supervisor authorization proof, or nil."
  [ctx user-id scope]
  (membership.fx/supervisor-dependency
   ctx
   user-id
   scope))

(defn require-supervisor-dependency
  "Returns an atomic supervisor authorization proof or throws."
  [ctx user-id scope]
  (membership.fx/require-supervisor-dependency
   ctx
   user-id
   scope))

(defn admin-dependency
  "Returns an atomic admin authorization proof, or nil."
  [ctx user-id scope]
  (membership.fx/admin-dependency
   ctx
   user-id
   scope))

(defn require-admin-dependency
  "Returns an atomic admin authorization proof or throws."
  [ctx user-id scope]
  (membership.fx/require-admin-dependency
   ctx
   user-id
   scope))

;; =============================================================================
;; Stable skill and role values
;; =============================================================================

(defn normalize-skill
  [value]
  (domain/normalize-skill
   value))

(defn skill?
  [value]
  (domain/skill?
   value))

(defn normalize-skills
  [values]
  (domain/normalize-skills
   values))

(defn skills?
  [value]
  (domain/skills?
   value))

(defn role?
  [value]
  (domain/role?
   value))

;; =============================================================================
;; Stable Membership document facts
;; =============================================================================

(defn membership-id
  [membership-document]
  (domain/membership-id
   membership-document))

(defn membership-user-id
  [membership-document]
  (domain/membership-user-id
   membership-document))

(defn membership-organization-id
  [membership-document]
  (domain/membership-organization-id
   membership-document))

(defn membership-status
  [membership-document]
  (domain/membership-status
   membership-document))

(defn membership-skills
  [membership-document]
  (domain/membership-skills
   membership-document))

(defn membership-active?
  [membership-document]
  (domain/membership-active?
   membership-document))

(defn membership-suspended?
  [membership-document]
  (domain/membership-suspended?
   membership-document))

(defn membership-revoked?
  [membership-document]
  (domain/membership-revoked?
   membership-document))

(defn membership-for-user?
  [membership-document user-id]
  (domain/membership-for-user?
   membership-document
   user-id))

(defn membership-for-organization?
  [membership-document organization-id]
  (domain/membership-for-organization?
   membership-document
   organization-id))

(defn membership-relates?
  [membership-document user-id organization-id]
  (domain/membership-relates?
   membership-document
   user-id
   organization-id))

(defn membership-has-skill?
  [membership-document skill]
  (domain/membership-has-skill?
   membership-document
   skill))

;; =============================================================================
;; Stable RoleAssignment document facts
;; =============================================================================

(defn role-assignment-id
  [role-assignment-document]
  (domain/role-assignment-id
   role-assignment-document))

(defn role-assignment-membership-id
  [role-assignment-document]
  (domain/role-assignment-membership-id
   role-assignment-document))

(defn assigned-role
  [role-assignment-document]
  (domain/assigned-role
   role-assignment-document))

(defn role-assignment-status
  [role-assignment-document]
  (domain/role-assignment-status
   role-assignment-document))

(defn role-assignment-scope
  [role-assignment-document]
  (domain/role-assignment-scope
   role-assignment-document))

(defn role-assignment-active?
  [role-assignment-document]
  (domain/role-assignment-active?
   role-assignment-document))

(defn role-assignment-revoked?
  [role-assignment-document]
  (domain/role-assignment-revoked?
   role-assignment-document))

(defn role-assignment-for-membership?
  [role-assignment-document membership-id]
  (domain/role-assignment-for-membership?
   role-assignment-document
   membership-id))

(defn role-assignment-grants-role?
  [role-assignment-document role]
  (domain/role-assignment-grants-role?
   role-assignment-document
   role))

(defn role-assignment-at-scope?
  [role-assignment-document scope]
  (domain/role-assignment-at-scope?
   role-assignment-document
   scope))

;; =============================================================================
;; Membership mutation planning
;; =============================================================================

(defn plan-create-membership
  [ctx input]
  (membership.fx/plan-create-membership
   ctx
   input))

(defn plan-add-skill
  [ctx input]
  (membership.fx/plan-add-skill
   ctx
   input))

(defn plan-remove-skill
  [ctx input]
  (membership.fx/plan-remove-skill
   ctx
   input))

(defn plan-suspend-membership
  [ctx input]
  (membership.fx/plan-suspend-membership
   ctx
   input))

(defn plan-reactivate-membership
  [ctx input]
  (membership.fx/plan-reactivate-membership
   ctx
   input))

(defn plan-revoke-membership
  [ctx input]
  (membership.fx/plan-revoke-membership
   ctx
   input))

;; =============================================================================
;; RoleAssignment mutation planning
;; =============================================================================

(defn plan-create-role-assignment
  [ctx input]
  (membership.fx/plan-create-role-assignment
   ctx
   input))

(defn plan-revoke-role-assignment
  [ctx input]
  (membership.fx/plan-revoke-role-assignment
   ctx
   input))
