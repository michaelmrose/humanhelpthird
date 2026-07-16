(ns net.humanhelp.site.model.user.core
  "The supported public facade for the HumanHelp User model.

   Code outside net.humanhelp.site.model.user should require this namespace
   instead of depending directly on User domain, Graph, schema, or FX
   implementation namespaces.

   This facade exposes:

   - the User model's Biff module contribution;
   - stable Graph query contracts and named entity reads;
   - a compact, document-free scoped access context for consumers;
   - the currently supported effectful User operations;
   - selected pure User values and predicates.

   It deliberately does not expose domain command constructors, lifecycle
   transitions, workflow planners, token hashing, transaction assertions, raw
   access-proof Graph results, or resolver implementations."
  (:require
   [gesso.graph :as graph]
   [net.humanhelp.site.model.user.domain.access :as access]
   [net.humanhelp.site.model.user.domain.common :as user.common]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.model.user.domain.invitation :as invitation]
   [net.humanhelp.site.model.user.domain.membership :as membership]
   [net.humanhelp.site.model.user.domain.role :as role]
   [net.humanhelp.site.model.user.fx :as user.fx]
   [net.humanhelp.site.model.user.graph :as user.graph]
   [net.humanhelp.site.model.user.schema :as user.schema]))

;; =============================================================================
;; Errors
;; =============================================================================

(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type error-type}
       (some? details) (assoc :error/details details))))))

;; =============================================================================
;; Model registration
;; =============================================================================

(def schema
  "Malli schemas contributed by the User model."
  user.schema/schema)

(def resolvers
  "Gesso Graph resolvers contributed by the User model."
  user.graph/resolvers)

;; net.humanhelp.site.model.fx/module must be installed separately, exactly once
;; for the application. User FX uses that shared transaction handler and does
;; not contribute another transaction implementation.
(def module
  "Biff module contribution for the User model."
  {:schema schema
   :biff.graph/resolvers resolvers})

;; =============================================================================
;; Public Graph query contracts
;; =============================================================================

(def user-document-query
  user.graph/user-document-query)

(def membership-document-query
  user.graph/membership-document-query)

(def role-assignment-document-query
  user.graph/role-assignment-document-query)

(def invitation-document-query
  user.graph/invitation-document-query)

(def user-query
  "Loads a User identity lookup result with :user/found? and :user/doc."
  user.graph/user-command-query)

(def membership-query
  "Loads a membership lookup result with :membership/found? and
   :membership/doc."
  user.graph/membership-command-query)

(def role-assignment-query
  "Loads a role-assignment lookup result with :role-assignment/found? and
   :role-assignment/doc."
  user.graph/role-assignment-command-query)

(def invitation-query
  "Loads an invitation lookup result with :invitation/found? and
   :invitation/doc."
  user.graph/invitation-command-query)

(def customer-query
  "Loads one user, their memberships, and derived customer facts."
  user.graph/customer-query)

(def active-role-assignments-at-scope-query
  "Loads active assignments at one exact organization scope."
  user.graph/active-role-assignments-at-scope-query)

;; User's detailed scoped-access Graph query remains private. It contains full
;; identity, membership, and role-assignment documents needed to derive a safe
;; public access context. Consumers should call access-context instead.
(def ^:private access-proof-query
  user.graph/access-query)

;; =============================================================================
;; Named User-model reads
;; =============================================================================

(defn user-facts
  "Loads a User identity by one supported lookup.

   lookup may contain :user-id, :phone, or :email. The result follows
   user-query and contains :user/found? plus optional :user/doc."
  [ctx lookup]
  (graph/query
   ctx
   (user.graph/user-query-input lookup)
   user-query))

(defn membership-facts
  "Loads one membership by UUID."
  [ctx membership-id]
  (graph/query
   ctx
   (user.graph/membership-query-input
    {:membership-id membership-id})
   membership-query))

(defn role-assignment-facts
  "Loads one role assignment by UUID."
  [ctx role-assignment-id]
  (graph/query
   ctx
   (user.graph/role-assignment-query-input
    {:role-assignment-id role-assignment-id})
   role-assignment-query))

(defn invitation-facts
  "Loads one invitation by UUID.

   Raw-token lookup and hashing remain internal to accept-invitation."
  [ctx invitation-id]
  (graph/query
   ctx
   (user.graph/invitation-query-input
    {:invitation-id invitation-id})
   invitation-query))

(defn customer-facts
  "Loads customer/organization-affiliation facts for one user UUID."
  [ctx user-id]
  (graph/query
   ctx
   (user.graph/customer-query-input {:user-id user-id})
   customer-query))

(defn active-role-assignments-at-scope
  "Loads active role assignments at one exact scope in an organization."
  [ctx organization-id scope]
  (graph/query
   ctx
   (user.graph/scoped-role-assignment-query-input
    {:organization-id organization-id
     :scope scope})
   active-role-assignments-at-scope-query))

;; =============================================================================
;; Organization scope-context contract
;; =============================================================================

(defn scope-context?
  "Returns true for the Organization-owned scope context accepted by
   access-context.

   Expected shape:

     {:organization/id organization-id
      :scope/target    {:scope/type ... :scope/id ...}
      :scope/applicable [target-scope ... organization-scope]}

   Organization is responsible for establishing the actual hierarchy. User
   validates only the public shape and internal agreement of that result."
  [value]
  (let [organization-id (:organization/id value)
        target (:scope/target value)
        applicable (:scope/applicable value)
        organization-scope
        (when (uuid? organization-id)
          (role/organization-scope organization-id))]
    (boolean
     (and
      (map? value)
      (uuid? organization-id)
      (user.common/scope-reference? target)
      (access/applicable-scopes? applicable)
      (= (count applicable)
         (count (set applicable)))
      (some #(user.common/same-scope? target %)
            applicable)
      (some #(user.common/same-scope? organization-scope %)
            applicable)))))

(defn- require-scope-context!
  [scope-context]
  (when-not (scope-context? scope-context)
    (fail!
     :user/invalid-scope-context
     "User access requires a valid Organization scope context."
     {:scope-context scope-context}))
  {:organization-id (:organization/id scope-context)
   :target (:scope/target scope-context)
   :applicable-scopes (vec (:scope/applicable scope-context))})

(defn- access-proof-facts
  [ctx user-id organization-id applicable-scopes]
  (graph/query
   ctx
   (user.graph/access-query-input
    {:user-id user-id
     :organization-id organization-id
     :applicable-scopes applicable-scopes})
   access-proof-query))

(defn- current-membership-document
  [facts]
  (when (:user/current-membership-found? facts)
    (get-in facts
            [:user/current-membership
             :membership/doc])))

(defn- current-role-assignment-documents
  [facts]
  (mapv
   :role-assignment/doc
   (get-in facts
           [:user/current-membership
            :membership/role-assignments])))

(defn access-context
  "Loads a compact scoped User access value for views and other models.

   Input:

     {:user-id user-id
      :scope-context organization-scope-context}

   The scope context must be produced by Organization. The returned map contains
   no User, Membership, or Role Assignment documents and does not expose User's
   internal Graph result shape.

   The write operation must always reload and reauthorize current facts; this
   context is suitable for rendering and downstream policy composition, not as
   a mutation security boundary."
  [ctx {:keys [user-id scope-context]}]
  (when-not (uuid? user-id)
    (fail!
     :user/invalid-user-id
     "User access requires a UUID user ID."
     {:user-id user-id}))

  (let [{:keys [organization-id target applicable-scopes]}
        (require-scope-context! scope-context)

        facts
        (access-proof-facts
         ctx
         user-id
         organization-id
         applicable-scopes)]
    (when-not (:user/found? facts)
      (fail!
       :user/not-found
       "The requested user does not exist."
       {:user/id user-id}))

    (let [user (:user/doc facts)
          membership-document
          (current-membership-document facts)
          role-assignments
          (if membership-document
            (current-role-assignment-documents facts)
            [])
          context
          (access/access-context
           user
           membership-document
           role-assignments
           applicable-scopes
           organization-id)]
      (when-not context
        (fail!
         :user/inconsistent-access-facts
         "User Graph returned facts that cannot form a valid access context."
         {:user/id user-id
          :organization/id organization-id
          :scope/target target}))

      (assoc context
             :scope/target target
             :user/display-name (:user/display-name user)))))

;; =============================================================================
;; Public effectful operations
;; =============================================================================

(defn invite-helper-to-location
  "Invites a helper to one active location.

   input contains :organization-id, :location-id, and exactly one of :phone or
   :email. The caller is responsible for delivering the returned raw token."
  [ctx input]
  (user.fx/invite-helper-to-location ctx input))

(defn accept-invitation
  "Accepts a pending location-scoped helper invitation for the authenticated
   user. input is {:token raw-bearer-token}."
  [ctx input]
  (user.fx/accept-invitation ctx input))

(def operations
  "Public User operation registry. Entries point at this facade rather than
   the internal FX namespace."
  {:user/invite-helper-to-location #'invite-helper-to-location
   :user/accept-invitation #'accept-invitation})

;; =============================================================================
;; Shared User values
;; =============================================================================

(def user-entity-type
  identity/entity-type)

(def membership-entity-type
  membership/entity-type)

(def role-assignment-entity-type
  role/entity-type)

(def invitation-entity-type
  invitation/entity-type)

(def roles
  user.common/roles)

(def scope-types
  user.common/scope-types)

(def capabilities
  access/capabilities)

(def invite-helper-to-location-capability
  access/invite-helper-to-location-capability)

(defn role?
  [value]
  (user.common/role? value))

(defn scope-type?
  [value]
  (user.common/scope-type? value))

(defn scope-reference?
  [value]
  (user.common/scope-reference? value))

(defn same-scope?
  [a b]
  (user.common/same-scope? a b))

(defn capability?
  [value]
  (access/capability? value))

(defn organization-scope
  [organization-id]
  (role/organization-scope organization-id))

(defn organization-group-scope
  [organization-group-id]
  (role/organization-group-scope organization-group-id))

(defn location-scope
  [location-id]
  (role/location-scope location-id))

(defn organization-group-scope?
  [scope]
  (role/organization-group-scope? scope))

(defn location-scope?
  [scope]
  (role/location-scope? scope))

;; =============================================================================
;; Identity values and facts
;; =============================================================================

(defn normalize-phone
  [value]
  (user.common/normalize-phone value))

(defn phone?
  [value]
  (user.common/phone? value))

(defn normalize-email
  [value]
  (user.common/normalize-email value))

(defn email?
  [value]
  (user.common/email? value))

(defn normalize-display-name
  [value]
  (identity/normalize-display-name value))

(defn display-name?
  [value]
  (identity/display-name? value))

(defn user-active?
  [user]
  (identity/active? user))

(defn user-suspended?
  [user]
  (identity/suspended? user))

(defn user-deleted?
  [user]
  (identity/deleted? user))

(defn user-has-phone?
  [user]
  (identity/has-phone? user))

(defn user-has-email?
  [user]
  (identity/has-email? user))

(defn user-has-contact?
  [user]
  (identity/has-contact? user))

(defn user-phone-verified?
  [user]
  (identity/phone-verified? user))

(defn user-email-verified?
  [user]
  (identity/email-verified? user))

(defn user-has-verified-contact?
  [user]
  (identity/has-verified-contact? user))

;; =============================================================================
;; Membership facts
;; =============================================================================

(defn membership-active?
  [membership-document]
  (membership/active? membership-document))

(defn membership-suspended?
  [membership-document]
  (membership/suspended? membership-document))

(defn membership-revoked?
  [membership-document]
  (membership/revoked? membership-document))

(defn membership-user-id
  [membership-document]
  (membership/user-id membership-document))

(defn membership-organization-id
  [membership-document]
  (membership/organization-id membership-document))

(defn membership-for-user?
  [membership-document user-id]
  (membership/for-user? membership-document user-id))

(defn membership-for-organization?
  [membership-document organization-id]
  (membership/for-organization? membership-document organization-id))

(defn current-membership?
  [membership-document]
  (access/current-membership? membership-document))

(defn access-enabled-membership?
  [user membership-document]
  (access/access-enabled-membership? user membership-document))

;; =============================================================================
;; Role-assignment facts
;; =============================================================================

(defn role-assignment-active?
  [role-assignment]
  (role/active? role-assignment))

(defn role-assignment-revoked?
  [role-assignment]
  (role/revoked? role-assignment))

(defn role-assignment-membership-id
  [role-assignment]
  (role/membership-id role-assignment))

(defn role-assignment-organization-id
  [role-assignment]
  (role/organization-id role-assignment))

(defn assigned-role
  [role-assignment]
  (role/assigned-role role-assignment))

(defn role-assignment-scope
  [role-assignment]
  (role/scope role-assignment))

(defn role-assignment-grants?
  [role-assignment membership-id assigned-role scope]
  (role/grants?
   role-assignment
   membership-id
   assigned-role
   scope))

;; =============================================================================
;; Invitation facts
;; =============================================================================

(defn invitation-pending?
  [invitation-document]
  (invitation/pending? invitation-document))

(defn invitation-accepted?
  [invitation-document]
  (invitation/accepted? invitation-document))

(defn invitation-declined?
  [invitation-document]
  (invitation/declined? invitation-document))

(defn invitation-revoked?
  [invitation-document]
  (invitation/revoked? invitation-document))

(defn invitation-expired?
  [invitation-document]
  (invitation/expired? invitation-document))

(defn invitation-terminal?
  [invitation-document]
  (invitation/terminal? invitation-document))

(defn invitation-organization-id
  [invitation-document]
  (invitation/organization-id invitation-document))

(defn invitation-offered-role
  [invitation-document]
  (invitation/offered-role invitation-document))

(defn invitation-scope
  [invitation-document]
  (invitation/scope invitation-document))

(defn invitation-recipient-type
  [invitation-document]
  (invitation/recipient-type invitation-document))

(defn invitation-recipient-value
  [invitation-document]
  (invitation/recipient-value invitation-document))

(defn invitation-addressed-to?
  [invitation-document contact]
  (invitation/addressed-to? invitation-document contact))

(defn invitation-usable-at?
  [invitation-document now]
  (invitation/usable-at? invitation-document now))

;; =============================================================================
;; Composed access facts and capabilities
;; =============================================================================

(defn organization-affiliated?
  [user memberships]
  (access/organization-affiliated? user memberships))

(defn customer?
  [user memberships]
  (access/customer? user memberships))

(defn applicable-scopes?
  [scopes]
  (access/applicable-scopes? scopes))

(defn access-context?
  [value]
  (and
   (access/access-context? value)
   (user.common/scope-reference?
    (:scope/target value))))

(defn has-capability?
  [access-context expected-capability]
  (access/has-capability?
   access-context
   expected-capability))

(defn can-invite-helper?
  "Returns true when an access context may display the helper-invitation UI.

   invite-helper-to-location always reloads and reauthorizes current facts."
  [access-context]
  (access/can-invite-helper? access-context))

;; These lower-level composition helpers remain available for model code that
;; already owns the required documents. Views and request-board reads should
;; normally consume access-context instead.
(defn effective-assignments
  [user membership-document role-assignments applicable-scopes]
  (access/effective-assignments
   user
   membership-document
   role-assignments
   applicable-scopes))

(defn effective-roles
  [user membership-document role-assignments applicable-scopes]
  (access/effective-roles
   user
   membership-document
   role-assignments
   applicable-scopes))

(defn effective-assignment-for-role
  [user membership-document role-assignments applicable-scopes expected-role]
  (access/effective-assignment-for-role
   user
   membership-document
   role-assignments
   applicable-scopes
   expected-role))

(defn administrator-assignment
  [user membership-document role-assignments applicable-scopes]
  (access/administrator-assignment
   user
   membership-document
   role-assignments
   applicable-scopes))

(defn has-role?
  [user membership-document role-assignments applicable-scopes expected-role]
  (access/has-role?
   user
   membership-document
   role-assignments
   applicable-scopes
   expected-role))

(defn helper?
  [user membership-document role-assignments applicable-scopes]
  (access/helper?
   user
   membership-document
   role-assignments
   applicable-scopes))

(defn supervisor?
  [user membership-document role-assignments applicable-scopes]
  (access/supervisor?
   user
   membership-document
   role-assignments
   applicable-scopes))

(defn admin?
  [user membership-document role-assignments applicable-scopes]
  (access/admin?
   user
   membership-document
   role-assignments
   applicable-scopes))

(defn staff?
  [user membership-document role-assignments applicable-scopes]
  (access/staff?
   user
   membership-document
   role-assignments
   applicable-scopes))
