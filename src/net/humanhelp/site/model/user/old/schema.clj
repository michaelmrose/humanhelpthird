(ns net.humanhelp.site.model.user.schema
  "Schemas for users and their organization relationships.

   A customer is a user without active organization membership or staff
   authority. Customer is therefore a derived condition, not a persisted role.

   Staff authority is modeled as:

     user
       -> membership
         -> role assignment

   A membership connects a user to an organization. A role assignment grants
   helper, supervisor, or admin authority through that membership and may be
   scoped to a particular location.

   This registry includes:

   - persisted document schemas
   - standalone Graph attributes
   - FX command inputs
   - internal user-model FX working values"
  (:require
   [net.humanhelp.schema.common :as common]))

(def ?
  common/?)

(def schema
  {;; ==========================================================================
   ;; User attributes
   ;; ==========================================================================

   :user/status
   [:enum
    :active
    :suspended
    :deleted]

   :user/email
   ::common/email

   :user/phone
   ::common/phone-digits

   :user/phone-display
   ::common/phone-display

   :user/display-name
   ::common/display-name

   :user/phone-verified-at
   ::common/zdt

   :user/revision
   ::common/revision

   :user/joined-at
   ::common/zdt

   :user/updated-at
   ::common/zdt

   :user/suspended-at
   ::common/zdt

   :user/deleted-at
   ::common/zdt

   ;; ==========================================================================
   ;; Persisted user document
   ;; ==========================================================================

   :user
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:user/email
     ?
     :user/email]

    [:user/phone
     ?
     :user/phone]

    [:user/phone-display
     ?
     :user/phone-display]

    [:user/display-name
     ?
     :user/display-name]

    [:user/phone-verified-at
     ?
     :user/phone-verified-at]

    [:user/status
     :user/status]

    [:user/revision
     :user/revision]

    [:user/joined-at
     :user/joined-at]

    [:user/updated-at
     :user/updated-at]

    [:user/suspended-at
     ?
     :user/suspended-at]

    [:user/deleted-at
     ?
     :user/deleted-at]]

   ;; ==========================================================================
   ;; Graph-facing user attributes
   ;; ==========================================================================

   :user/id
   ::common/id

   :user/doc
   :user

   :user/found?
   :boolean

   :user/active?
   :boolean

   ;; True when the user has no current organizational affiliation.
   :user/customer?
   :boolean

   :current-user/id
   ::common/id

   ;; ==========================================================================
   ;; User command inputs
   ;; ==========================================================================

   ;; These are caller-facing inputs. Generated :id, :now, and verification
   ;; timestamps are added by FX and stored under :user-model/input.

   :user/create-input
   [:map
    {:closed true}

    [:email
     ?
     :string]

    [:phone
     ?
     :string]

    [:display-name
     ?
     :string]]

   :user/profile-input
   [:map
    {:closed true}

    [:display-name
     ?
     :string]]

   ;; ==========================================================================
   ;; Membership attributes
   ;; ==========================================================================

   :membership/status
   [:enum
    :active
    :suspended
    :revoked]

   :membership/user
   ::common/id

   :membership/organization
   ::common/id

   :membership/revision
   ::common/revision

   :membership/created-at
   ::common/zdt

   :membership/updated-at
   ::common/zdt

   :membership/ended-at
   ::common/zdt

   ;; ==========================================================================
   ;; Persisted membership document
   ;; ==========================================================================

   :membership
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:membership/user
     :membership/user]

    [:membership/organization
     :membership/organization]

    [:membership/status
     :membership/status]

    [:membership/revision
     :membership/revision]

    [:membership/created-at
     :membership/created-at]

    [:membership/updated-at
     :membership/updated-at]

    [:membership/ended-at
     ?
     :membership/ended-at]]

   ;; ==========================================================================
   ;; Graph-facing membership attributes
   ;; ==========================================================================

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
   [:vector
    :membership]

   :user/active-memberships
   [:vector
    :membership]

   ;; ==========================================================================
   ;; Membership command inputs
   ;; ==========================================================================

   :membership/create-input
   [:map
    {:closed true}

    [:user-id
     ::common/id]

    [:organization-id
     ::common/id]]

   ;; ==========================================================================
   ;; Role-assignment attributes
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

   :role-assignment/membership
   ::common/id

   :role-assignment/location
   ::common/id

   :role-assignment/revision
   ::common/revision

   :role-assignment/created-at
   ::common/zdt

   :role-assignment/updated-at
   ::common/zdt

   :role-assignment/ended-at
   ::common/zdt

   ;; ==========================================================================
   ;; Persisted role-assignment document
   ;; ==========================================================================

   :role-assignment
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:role-assignment/membership
     :role-assignment/membership]

    [:role-assignment/role
     :role-assignment/role]

    ;; Absence means that the role applies throughout the organization.
    [:role-assignment/location
     ?
     :role-assignment/location]

    [:role-assignment/status
     :role-assignment/status]

    [:role-assignment/revision
     :role-assignment/revision]

    [:role-assignment/created-at
     :role-assignment/created-at]

    [:role-assignment/updated-at
     :role-assignment/updated-at]

    [:role-assignment/ended-at
     ?
     :role-assignment/ended-at]]

   ;; ==========================================================================
   ;; Graph-facing role-assignment attributes
   ;; ==========================================================================

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
   [:vector
    :role-assignment]

   :membership/active-role-assignments
   [:vector
    :role-assignment]

   :membership/roles
   [:set
    :role-assignment/role]

   :membership/location-ids
   [:set
    ::common/id]

   ;; ==========================================================================
   ;; Role-assignment command inputs
   ;; ==========================================================================

   :role-assignment/create-input
   [:map
    {:closed true}

    [:membership-id
     ::common/id]

    [:role
     :role-assignment/role]

    [:location-id
     ?
     ::common/id]]

   ;; ==========================================================================
   ;; Invitation attributes
   ;; ==========================================================================

   :invitation/status
   [:enum
    :pending
    :accepted
    :revoked
    :expired]

   :invitation/organization
   ::common/id

   :invitation/location
   ::common/id

   :invitation/phone
   ::common/phone-digits

   :invitation/email
   ::common/email

   :invitation/role
   :role-assignment/role

   :invitation/token-hash
   ::common/token-hash

   :invitation/created-by
   ::common/id

   :invitation/accepted-by
   ::common/id

   :invitation/revision
   ::common/revision

   :invitation/created-at
   ::common/zdt

   :invitation/updated-at
   ::common/zdt

   :invitation/expires-at
   ::common/zdt

   :invitation/accepted-at
   ::common/zdt

   :invitation/revoked-at
   ::common/zdt

   ;; ==========================================================================
   ;; Persisted invitation document
   ;; ==========================================================================

   :invitation
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:invitation/organization
     :invitation/organization]

    ;; Absence means the invited role applies throughout the organization.
    [:invitation/location
     ?
     :invitation/location]

    ;; At least one recipient identifier is required by the domain.
    [:invitation/phone
     ?
     :invitation/phone]

    [:invitation/email
     ?
     :invitation/email]

    [:invitation/role
     :invitation/role]

    ;; Only the hash is persisted. The bearer token itself is never stored.
    [:invitation/token-hash
     :invitation/token-hash]

    [:invitation/status
     :invitation/status]

    [:invitation/created-by
     :invitation/created-by]

    [:invitation/accepted-by
     ?
     :invitation/accepted-by]

    [:invitation/revision
     :invitation/revision]

    [:invitation/created-at
     :invitation/created-at]

    [:invitation/updated-at
     :invitation/updated-at]

    [:invitation/expires-at
     :invitation/expires-at]

    [:invitation/accepted-at
     ?
     :invitation/accepted-at]

    [:invitation/revoked-at
     ?
     :invitation/revoked-at]]

   ;; ==========================================================================
   ;; Graph-facing invitation attributes
   ;; ==========================================================================

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

   ;; This currently describes the materialized :expired lifecycle status.
   :invitation/expired?
   :boolean

   ;; ==========================================================================
   ;; Invitation command inputs
   ;; ==========================================================================

   :invitation/create-input
   [:map
    {:closed true}

    [:organization-id
     ::common/id]

    [:location-id
     ?
     ::common/id]

    [:phone
     ?
     :string]

    [:email
     ?
     :string]

    [:role
     :role-assignment/role]

    [:token-hash
     ::common/token-hash]

    [:created-by
     ::common/id]

    [:expires-at
     ::common/zdt]]

   ;; ==========================================================================
   ;; Request-capability attributes
   ;; ==========================================================================

   :request-capability/status
   [:enum
    :active
    :revoked
    :expired]

   :request-capability/request
   ::common/id

   :request-capability/user
   ::common/id

   :request-capability/token-hash
   ::common/token-hash

   :request-capability/revision
   ::common/revision

   :request-capability/created-at
   ::common/zdt

   :request-capability/updated-at
   ::common/zdt

   :request-capability/expires-at
   ::common/zdt

   :request-capability/last-used-at
   ::common/zdt

   :request-capability/revoked-at
   ::common/zdt

   ;; ==========================================================================
   ;; Persisted request-capability document
   ;; ==========================================================================

   :request-capability
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:request-capability/request
     :request-capability/request]

    ;; Optional because a capability may belong to a guest with no user record.
    [:request-capability/user
     ?
     :request-capability/user]

    ;; Only the hash is persisted. The bearer token itself is never stored.
    [:request-capability/token-hash
     :request-capability/token-hash]

    [:request-capability/status
     :request-capability/status]

    [:request-capability/revision
     :request-capability/revision]

    [:request-capability/created-at
     :request-capability/created-at]

    [:request-capability/updated-at
     :request-capability/updated-at]

    [:request-capability/expires-at
     :request-capability/expires-at]

    [:request-capability/last-used-at
     ?
     :request-capability/last-used-at]

    [:request-capability/revoked-at
     ?
     :request-capability/revoked-at]]

   ;; ==========================================================================
   ;; Graph-facing request-capability attributes
   ;; ==========================================================================

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

   ;; ==========================================================================
   ;; Request-capability command inputs
   ;; ==========================================================================

   :request-capability/create-input
   [:map
    {:closed true}

    [:request-id
     ::common/id]

    [:user-id
     ?
     ::common/id]

    [:token-hash
     ::common/token-hash]

    [:expires-at
     ::common/zdt]]

   ;; ==========================================================================
   ;; Shared user-model FX documents
   ;; ==========================================================================

   :user-model/document
   [:or
    :user
    :membership
    :role-assignment
    :invitation
    :request-capability]

   ;; Prepared domain input after FX adds generated IDs and timestamps.
   ;; Individual domain namespaces perform the authoritative field validation.
   :user-model/input
   [:map-of
    :keyword
    :any]

   ;; Result returned by a Graph query effect.
   :user-model/facts
   [:map-of
    :keyword
    :any]

   :user-model/after
   :user-model/document

   :user-model/result-key
   :keyword

   ;; ==========================================================================
   ;; Shared model commands
   ;; ==========================================================================

   :user-model/command
   [:map
    {:closed true}

    [:model/entity-type
     :keyword]

    [:model/operation
     :keyword]

    [:model/id
     ::common/id]

    [:model/expected
     ?
     [:map-of
      :keyword
      :any]]

    [:model/before
     ?
     :user-model/document]

    [:model/after
     :user-model/document]]

   ;; ==========================================================================
   ;; Commit results
   ;; ==========================================================================

   :user-model/commit-result
   [:map
    {:closed true}

    [:ok?
     :boolean]

    [:value
     ?
     :any]

    [:error
     ?
     :keyword]

    [:errors
     ?
     [:map-of
      :keyword
      :any]]]})
