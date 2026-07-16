(ns net.humanhelp.site.model.user.domain.access
  "Pure access facts composed from User identity, membership, and role-assignment
   documents.

   This namespace interprets already-loaded User model documents. It does not
   query XTDB, authorize mutations, inspect Organization documents, or decide
   which organization groups contain a location.

   Effective scoped access is evaluated against applicable-scopes supplied by
   the caller. Organization is responsible for deriving that collection. For a
   location, it will normally contain the location scope, every containing
   organization-group scope, and the organization scope."
  (:require
   [net.humanhelp.site.model.user.domain.common :as user.common]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.model.user.domain.membership :as membership]
   [net.humanhelp.site.model.user.domain.role :as role]))

;; =============================================================================
;; Membership composition
;; =============================================================================

(defn membership-for-user?
  "Returns true when membership is a valid membership document belonging to
   user."
  [user membership]
  (and
   (identity/document-consistent? user)
   (membership/document-consistent? membership)
   (membership/for-user?
    membership
    (:xt/id user))))

(defn current-membership?
  "Returns true when membership still represents an organization affiliation.

   Suspended memberships remain current affiliations. Revoked memberships do
   not."
  [membership]
  (and
   (membership/document-consistent? membership)
   (not
    (membership/revoked? membership))))

(defn current-membership-for-user?
  [user membership]
  (and
   (membership-for-user? user membership)
   (current-membership? membership)))

(defn access-enabled-membership?
  "Returns true when user and membership may currently participate in
   organization access decisions."
  [user membership]
  (and
   (membership-for-user? user membership)
   (identity/active? user)
   (membership/active? membership)))

(defn current-memberships-for-user
  [user memberships]
  (filterv
   #(current-membership-for-user? user %)
   memberships))

(defn active-memberships-for-user
  [user memberships]
  (filterv
   #(access-enabled-membership? user %)
   memberships))

(defn current-memberships-for-organization
  [user memberships organization-id]
  (filterv
   #(and
     (current-membership-for-user? user %)
     (membership/for-organization? % organization-id))
   memberships))

(defn active-memberships-for-organization
  [user memberships organization-id]
  (filterv
   #(and
     (access-enabled-membership? user %)
     (membership/for-organization? % organization-id))
   memberships))

(defn organization-affiliated?
  "Returns true when user has at least one non-revoked membership."
  [user memberships]
  (boolean
   (seq
    (current-memberships-for-user
     user
     memberships))))

(defn customer?
  "Returns true when an active user has no current organization affiliation.

   Customer is a derived condition, not a persisted role."
  [user memberships]
  (and
   (identity/document-consistent? user)
   (identity/active? user)
   (not
    (organization-affiliated?
     user
     memberships))))

;; =============================================================================
;; Role-assignment composition
;; =============================================================================

(defn assignment-for-membership?
  "Returns true when role-assignment belongs to membership and repeats the same
   organization relationship."
  [membership role-assignment]
  (and
   (membership/document-consistent? membership)
   (role/document-consistent? role-assignment)
   (role/for-membership?
    role-assignment
    (:xt/id membership))
   (role/for-organization?
    role-assignment
    (membership/organization-id membership))))

(defn applicable-scopes?
  "Returns true when every value is a structurally valid scope reference.

   This does not establish hierarchy or ownership. Organization must derive the
   collection from valid Organization data."
  [scopes]
  (and
   (coll? scopes)
   (every?
    user.common/scope-reference?
    scopes)))

(defn assignment-applies-at?
  "Returns true when the assignment's exact scope is among applicable-scopes.

   applicable-scopes should contain the target scope and every broader scope
   that Organization says covers it."
  [role-assignment applicable-scopes]
  (and
   (role/document-consistent? role-assignment)
   (applicable-scopes? applicable-scopes)
   (contains?
    (set applicable-scopes)
    (role/scope role-assignment))))

(defn effective-assignment?
  "Returns true when one role assignment grants access through membership at
   any supplied applicable scope.

   User identity is intentionally not accepted here. Use effective-assignments
   or has-role? when identity status must also be enforced."
  [membership role-assignment applicable-scopes]
  (and
   (membership/active? membership)
   (role/active? role-assignment)
   (assignment-for-membership?
    membership
    role-assignment)
   (assignment-applies-at?
    role-assignment
    applicable-scopes)))

(defn effective-assignments
  "Returns active assignments that currently grant user access through one
   active membership at any applicable scope."
  [user membership role-assignments applicable-scopes]
  (if
   (and
    (access-enabled-membership?
     user
     membership)
    (applicable-scopes?
     applicable-scopes))
    (filterv
     #(effective-assignment?
       membership
       %
       applicable-scopes)
     role-assignments)
    []))

(defn effective-roles
  "Returns the exact roles currently granted at any applicable scope.

   HumanHelp does not infer a numeric or automatic role hierarchy here."
  [user membership role-assignments applicable-scopes]
  (into
   #{}
   (map role/assigned-role)
   (effective-assignments
    user
    membership
    role-assignments
    applicable-scopes)))

(defn effective-assignment-for-role
  "Returns one effective assignment granting expected-role, or nil.

   Write workflows that rely on the assignment should retain the returned
   document and recheck its expected version at commit time."
  [user membership role-assignments applicable-scopes expected-role]
  (when
   (user.common/role?
    expected-role)
    (some
     #(when
       (= expected-role
          (role/assigned-role %))
        %)
     (effective-assignments
      user
      membership
      role-assignments
      applicable-scopes))))

(defn administrator-assignment
  "Returns one effective administrator assignment, or nil."
  [user membership role-assignments applicable-scopes]
  (effective-assignment-for-role
   user
   membership
   role-assignments
   applicable-scopes
   :admin))

(defn has-role?
  [user membership role-assignments applicable-scopes expected-role]
  (boolean
   (effective-assignment-for-role
    user
    membership
    role-assignments
    applicable-scopes
    expected-role)))

(defn helper?
  [user membership role-assignments applicable-scopes]
  (has-role?
   user
   membership
   role-assignments
   applicable-scopes
   :helper))

(defn supervisor?
  [user membership role-assignments applicable-scopes]
  (has-role?
   user
   membership
   role-assignments
   applicable-scopes
   :supervisor))

(defn admin?
  [user membership role-assignments applicable-scopes]
  (has-role?
   user
   membership
   role-assignments
   applicable-scopes
   :admin))

(defn staff?
  "Returns true when at least one exact HumanHelp role is effective."
  [user membership role-assignments applicable-scopes]
  (boolean
   (seq
    (effective-roles
     user
     membership
     role-assignments
     applicable-scopes))))

;; =============================================================================
;; Public access contexts and capabilities
;; =============================================================================

(def invite-helper-to-location-capability
  :user/invite-helper-to-location)

(def capabilities
  "Capabilities currently emitted in a public User access context."
  #{invite-helper-to-location-capability})

(defn capability?
  [value]
  (contains? capabilities value))

(defn- capabilities-for-roles
  [roles]
  (cond-> #{}
    (contains? roles :admin)
    (conj invite-helper-to-location-capability)))

(defn access-context
  "Returns a compact, consumer-facing access value for one organization scope.

   Unlike User Graph access facts, this result contains no User, Membership, or
   Role Assignment documents. It is safe for views and other models to consume
   without depending on User's internal Graph shape.

   `organization-id` and `applicable-scopes` must come from a trusted
   Organization read. Invalid or mismatched documents fail closed: the result
   contains no membership, roles, or capabilities."
  [user membership role-assignments applicable-scopes organization-id]
  (when
   (and
    (identity/document-consistent? user)
    (uuid? organization-id)
    (applicable-scopes? applicable-scopes))
    (let [organization-membership?
          (and
           (access-enabled-membership?
            user
            membership)
           (membership/for-organization?
            membership
            organization-id))

          effective-assignments
          (if
           organization-membership?
            (effective-assignments
             user
             membership
             role-assignments
             applicable-scopes)
            [])

          roles
          (into
           #{}
           (map role/assigned-role)
           effective-assignments)

          capability-set
          (capabilities-for-roles roles)]
      {:user/id (:xt/id user)
       :user/active? (identity/active? user)
       :organization/id organization-id
       :membership/id
       (when organization-membership?
         (:xt/id membership))
       :membership/active?
       (boolean organization-membership?)
       :user/effective-roles roles
       :user/capabilities capability-set
       :user/helper? (contains? roles :helper)
       :user/supervisor? (contains? roles :supervisor)
       :user/admin? (contains? roles :admin)
       :user/staff? (boolean (seq roles))})))

(defn access-context?
  "Returns true for the stable, document-free public access-context shape."
  [value]
  (and
   (map? value)
   (uuid? (:user/id value))
   (boolean? (:user/active? value))
   (uuid? (:organization/id value))
   (or
    (nil? (:membership/id value))
    (uuid? (:membership/id value)))
   (boolean? (:membership/active? value))
   (set? (:user/effective-roles value))
   (every?
    user.common/role?
    (:user/effective-roles value))
   (set? (:user/capabilities value))
   (every?
    capability?
    (:user/capabilities value))
   (boolean? (:user/helper? value))
   (boolean? (:user/supervisor? value))
   (boolean? (:user/admin? value))
   (boolean? (:user/staff? value))
   (= (:user/helper? value)
      (contains?
       (:user/effective-roles value)
       :helper))
   (= (:user/supervisor? value)
      (contains?
       (:user/effective-roles value)
       :supervisor))
   (= (:user/admin? value)
      (contains?
       (:user/effective-roles value)
       :admin))
   (= (:user/staff? value)
      (boolean
       (seq
        (:user/effective-roles value))))
   (= (:membership/active? value)
      (some?
       (:membership/id value)))
   (= (:user/capabilities value)
      (capabilities-for-roles
       (:user/effective-roles value)))))

(defn has-capability?
  [access-context expected-capability]
  (and
   (access-context? access-context)
   (capability? expected-capability)
   (contains?
    (:user/capabilities access-context)
    expected-capability)))

(defn can-invite-helper?
  "Returns true when this access context may display the helper-invitation UI.

   The write workflow must still reload and reauthorize against current data."
  [access-context]
  (has-capability?
   access-context
   invite-helper-to-location-capability))
