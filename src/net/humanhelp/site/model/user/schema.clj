(ns net.humanhelp.site.model.user.schema
  "Schemas for users and their organization relationships.

   A customer is a user without organization membership or staff authority.
   Customer is therefore a derived condition, not a persisted role.

   Staff authority is modeled as:

     user
       -> membership
         -> role assignment

   A membership connects a user to an organization. A role assignment grants
   helper, supervisor, or admin authority through that membership and may be
   scoped to a particular location."
  (:require
   [net.humanhelp.schema.common :as common]))

(def ?
  common/?)

(def schema
  {;; ==========================================================================
   ;; User
   ;; ==========================================================================

   :user/status
   [:enum
    :active
    :suspended
    :deleted]

   :user
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:user/email
     ?
     ::common/email]

    [:user/phone
     ?
     ::common/phone-digits]

    [:user/phone-display
     ?
     ::common/phone-display]

    [:user/display-name
     ?
     ::common/display-name]

    [:user/phone-verified-at
     ?
     ::common/zdt]

    [:user/status
     :user/status]

    [:user/revision
     ::common/revision]

    [:user/joined-at
     ::common/zdt]

    [:user/updated-at
     ::common/zdt]

    [:user/suspended-at
     ?
     ::common/zdt]

    [:user/deleted-at
     ?
     ::common/zdt]]

   ;; Graph-facing user attributes.

   :user/id
   ::common/id

   :user/doc
   :user

   :user/found?
   :boolean

   :user/active?
   :boolean

   ;; True when the user has no active organization memberships or roles.
   :user/customer?
   :boolean

   :current-user/id
   ::common/id

   ;; ==========================================================================
   ;; Organization membership
   ;; ==========================================================================

   :membership/status
   [:enum
    :active
    :suspended
    :revoked]

   :membership
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:membership/user
     ::common/id]

    [:membership/organization
     ::common/id]

    [:membership/status
     :membership/status]

    [:membership/revision
     ::common/revision]

    [:membership/created-at
     ::common/zdt]

    [:membership/updated-at
     ::common/zdt]

    [:membership/ended-at
     ?
     ::common/zdt]]

   ;; Graph-facing membership attributes.

   :membership/id
   ::common/id

   :membership/user-id
   ::common/id

   :membership/organization-id
   ::common/id

   :membership/doc
   :membership

   :membership/found?
   :boolean

   :membership/active?
   :boolean

   :user/memberships
   [:vector :membership]

   :user/active-memberships
   [:vector :membership]

   :current-membership/id
   ::common/id

   ;; ==========================================================================
   ;; Role assignment
   ;; ==========================================================================

   :role-assignment/role
   [:enum
    :helper
    :supervisor
    :admin]

   :role-assignment/status
   [:enum
    :active
    :revoked]

   :role-assignment
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:role-assignment/membership
     ::common/id]

    [:role-assignment/role
     :role-assignment/role]

    ;; Absence means that the role applies throughout the organization.
    [:role-assignment/location
     ?
     ::common/id]

    [:role-assignment/status
     :role-assignment/status]

    [:role-assignment/revision
     ::common/revision]

    [:role-assignment/created-at
     ::common/zdt]

    [:role-assignment/updated-at
     ::common/zdt]

    [:role-assignment/ended-at
     ?
     ::common/zdt]]

   ;; Graph-facing role-assignment attributes.

   :role-assignment/id
   ::common/id

   :role-assignment/membership-id
   ::common/id

   :role-assignment/location-id
   ::common/id

   :role-assignment/doc
   :role-assignment

   :role-assignment/found?
   :boolean

   :role-assignment/active?
   :boolean

   :role-assignment/organization-wide?
   :boolean

   :membership/role-assignments
   [:vector :role-assignment]

   :membership/active-role-assignments
   [:vector :role-assignment]

   :membership/roles
   [:set :role-assignment/role]

   :membership/location-ids
   [:set ::common/id]

   :current-role-assignment/id
   ::common/id

   ;; ==========================================================================
   ;; Staff invitation
   ;; ==========================================================================

   :invitation/status
   [:enum
    :pending
    :accepted
    :revoked
    :expired]

   :invitation
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:invitation/organization
     ::common/id]

    ;; Absence means that the invited role applies throughout the organization.
    [:invitation/location
     ?
     ::common/id]

    ;; An invitation must identify a recipient by phone, email, or both.
    [:invitation/phone
     ?
     ::common/phone-digits]

    [:invitation/email
     ?
     ::common/email]

    [:invitation/role
     :role-assignment/role]

    ;; Only the hash is persisted. The bearer token itself is never stored.
    [:invitation/token-hash
     ::common/token-hash]

    [:invitation/status
     :invitation/status]

    [:invitation/created-by
     ::common/id]

    [:invitation/accepted-by
     ?
     ::common/id]

    [:invitation/revision
     ::common/revision]

    [:invitation/created-at
     ::common/zdt]

    [:invitation/updated-at
     ::common/zdt]

    [:invitation/expires-at
     ::common/zdt]

    [:invitation/accepted-at
     ?
     ::common/zdt]

    [:invitation/revoked-at
     ?
     ::common/zdt]]

   ;; Graph-facing invitation attributes.

   :invitation/id
   ::common/id

   :invitation/organization-id
   ::common/id

   :invitation/location-id
   ::common/id

   :invitation/doc
   :invitation

   :invitation/found?
   :boolean

   :invitation/pending?
   :boolean

   :invitation/expired?
   :boolean

   ;; ==========================================================================
   ;; Request capability
   ;; ==========================================================================

   :request-capability/status
   [:enum
    :active
    :revoked
    :expired]

   :request-capability
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:request-capability/request
     ::common/id]

    ;; Optional because a capability may belong to a guest with no user record.
    [:request-capability/user
     ?
     ::common/id]

    ;; Only the hash is persisted. The bearer token itself is never stored.
    [:request-capability/token-hash
     ::common/token-hash]

    [:request-capability/status
     :request-capability/status]

    [:request-capability/revision
     ::common/revision]

    [:request-capability/created-at
     ::common/zdt]

    [:request-capability/updated-at
     ::common/zdt]

    [:request-capability/expires-at
     ::common/zdt]

    [:request-capability/last-used-at
     ?
     ::common/zdt]

    [:request-capability/revoked-at
     ?
     ::common/zdt]]

   ;; Graph-facing request-capability attributes.

   :request-capability/id
   ::common/id

   :request-capability/request-id
   ::common/id

   :request-capability/user-id
   ::common/id

   :request-capability/doc
   :request-capability

   :request-capability/found?
   :boolean

   :request-capability/active?
   :boolean

   :current-request-capability/id
   ::common/id})
