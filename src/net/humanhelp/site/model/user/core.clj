(ns net.humanhelp.site.model.user.core
  "The supported public facade for the HumanHelp User model.

   Code outside net.humanhelp.site.model.user should require this namespace
   instead of depending directly on User domain, Graph, schema, or FX
   implementation namespaces.

   This facade exposes:

   - the User model's Biff module contribution;
   - stable raw Graph query contracts and compatibility reads;
   - normalized required entity and aggregate reads;
   - a compact, document-free scoped access context for consumers;
   - organization-local Membership skill facts;
   - the currently supported effectful User operations;
   - selected pure User values and predicates.

   It deliberately does not expose domain command constructors, lifecycle
   transitions, workflow planners, token hashing, transaction assertions, raw
   access-proof Graph results, or resolver implementations."
  (:require
   [gesso.graph :as graph]
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
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
       (some? details)
       (assoc :error/details details))))))

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
  "Loads a Membership lookup result with :membership/found? and
   :membership/doc."
  user.graph/membership-command-query)

(def role-assignment-query
  "Loads a role-assignment lookup result with :role-assignment/found? and
   :role-assignment/doc."
  user.graph/role-assignment-command-query)

(def invitation-query
  "Loads an Invitation lookup result with :invitation/found? and
   :invitation/doc."
  user.graph/invitation-command-query)

(def customer-query
  "Loads one User, their Memberships, and derived customer facts."
  user.graph/customer-query)

(def active-role-assignments-at-scope-query
  "Loads active assignments at one exact Organization scope."
  user.graph/active-role-assignments-at-scope-query)

;; User's detailed scoped-access Graph query remains private. It contains full
;; identity, Membership, and role-assignment documents needed to derive a safe
;; public access context. Consumers should call access-context instead.
(def ^:private access-proof-query
  user.graph/access-query)

;; =============================================================================
;; Named User-model reads
;; =============================================================================

(defn user-facts
  "Loads a User identity by one supported lookup.

   lookup may contain exactly one of :user-id, :phone, or :email. The result
   follows user-query and contains :user/found? plus optional :user/doc."
  [ctx lookup]
  (graph/query
   ctx
   (user.graph/user-query-input lookup)
   user-query))

(defn membership-facts
  "Loads one Membership by UUID."
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
  "Loads one Invitation by UUID.

   Raw-token lookup and hashing remain internal to accept-invitation."
  [ctx invitation-id]
  (graph/query
   ctx
   (user.graph/invitation-query-input
    {:invitation-id invitation-id})
   invitation-query))

(defn customer-facts
  "Loads customer/Organization-affiliation facts for one User UUID."
  [ctx user-id]
  (graph/query
   ctx
   (user.graph/customer-query-input
    {:user-id user-id})
   customer-query))

(defn active-role-assignments-at-scope
  "Loads active role assignments at one exact scope in an Organization."
  [ctx organization-id scope]
  (graph/query
   ctx
   (user.graph/scoped-role-assignment-query-input
    {:organization-id organization-id
     :scope scope})
   active-role-assignments-at-scope-query))

;; =============================================================================
;; Normalized required reads
;; =============================================================================

(defn- require-uuid!
  [value error-type message details]
  (when-not
   (uuid? value)
    (fail!
     error-type
     message
     details))
  value)

(defn- require-document!
  [facts
   found-key
   document-key
   document-predicate
   error-type
   message
   details]
  (when-not
   (true?
    (get facts found-key))
    (fail!
     error-type
     message
     details))

  (let [document
        (get facts document-key)]
    (when-not
     (document-predicate document)
      (fail!
       :user.core/invalid-read-result
       "User Graph returned a found entity without a valid document."
       (assoc
        details
        :found-key found-key
        :document-key document-key
        :facts facts)))
    document))

(defn- user-lookup?
  [{:keys [user-id phone email] :as lookup}]
  (and
   (map? lookup)

   (=
    1
    (count
     (filter
      some?
      [user-id phone email])))

   (or
    (and
     (some? user-id)
     (uuid? user-id))

    (and
     (some? phone)
     (user.common/phone? phone))

    (and
     (some? email)
     (user.common/email? email)))))

(defn require-user
  "Returns one valid User document or throws.

   lookup must contain exactly one canonical lookup:
   :user-id, :phone, or :email."
  [ctx lookup]
  (when-not
   (user-lookup? lookup)
    (fail!
     :user/invalid-lookup
     "A User lookup must contain exactly one valid user ID, phone, or email."
     {:lookup lookup}))

  (require-document!
   (user-facts ctx lookup)
   :user/found?
   :user/doc
   identity/document-consistent?
   :user/not-found
   "The requested User does not exist."
   {:lookup lookup}))

(defn require-membership
  "Returns one valid Membership document or throws."
  [ctx membership-id]
  (require-uuid!
   membership-id
   :membership/invalid-id
   "Membership ID must be a UUID."
   {:membership/id membership-id})

  (require-document!
   (membership-facts ctx membership-id)
   :membership/found?
   :membership/doc
   membership/document-consistent?
   :membership/not-found
   "The requested Membership does not exist."
   {:membership/id membership-id}))

(defn require-role-assignment
  "Returns one valid role-assignment document or throws."
  [ctx role-assignment-id]
  (require-uuid!
   role-assignment-id
   :role-assignment/invalid-id
   "Role-assignment ID must be a UUID."
   {:role-assignment/id role-assignment-id})

  (require-document!
   (role-assignment-facts ctx role-assignment-id)
   :role-assignment/found?
   :role-assignment/doc
   role/document-consistent?
   :role-assignment/not-found
   "The requested role assignment does not exist."
   {:role-assignment/id role-assignment-id}))

(defn require-invitation
  "Returns one valid Invitation document or throws.

   Raw bearer-token lookup remains internal to accept-invitation."
  [ctx invitation-id]
  (require-uuid!
   invitation-id
   :invitation/invalid-id
   "Invitation ID must be a UUID."
   {:invitation/id invitation-id})

  (require-document!
   (invitation-facts ctx invitation-id)
   :invitation/found?
   :invitation/doc
   invitation/document-consistent?
   :invitation/not-found
   "The requested Invitation does not exist."
   {:invitation/id invitation-id}))

(defn customer-context
  "Returns normalized customer and Organization-affiliation facts for one User.

   Unlike customer-facts, this result contains documents directly rather than
   Graph envelope nodes."
  [ctx user-id]
  (require-uuid!
   user-id
   :user/invalid-user-id
   "Customer status requires a UUID User ID."
   {:user/id user-id})

  (let [facts
        (customer-facts ctx user-id)

        user
        (require-document!
         facts
         :user/found?
         :user/doc
         identity/document-consistent?
         :user/not-found
         "The requested User does not exist."
         {:user/id user-id})

        membership-nodes
        (or
         (:user/memberships facts)
         [])

        memberships
        (mapv
         :membership/doc
         membership-nodes)]
    (when-not
     (every?
      #(and
        (membership/document-consistent? %)
        (membership/for-user? % user-id))
      memberships)
      (fail!
       :user.core/invalid-read-result
       "User Graph returned an invalid Membership collection."
       {:user/id user-id
        :facts facts}))

    {:user user
     :memberships memberships
     :organization-affiliated?
     (access/organization-affiliated?
      user
      memberships)
     :customer?
     (access/customer?
      user
      memberships)}))

(defn active-role-assignment-documents-at-scope
  "Returns valid active role-assignment documents at one exact scope.

   This normalized read is useful to model code that needs the documents for
   later authorization-version guards."
  [ctx organization-id scope]
  (require-uuid!
   organization-id
   :user/invalid-organization-id
   "Organization ID must be a UUID."
   {:organization/id organization-id})

  (when-not
   (authorization-scope/scope-reference? scope)
    (fail!
     :user/invalid-scope
     "A valid authorization scope is required."
     {:organization/id organization-id
      :scope scope}))

  (let [facts
        (active-role-assignments-at-scope
         ctx
         organization-id
         scope)

        nodes
        (or
         (:user/active-role-assignments-at-scope facts)
         [])

        documents
        (mapv
         :role-assignment/doc
         nodes)]
    (when-not
     (every?
      #(and
        (role/document-consistent? %)
        (role/active? %)
        (role/for-organization? % organization-id)
        (role/at-scope? % scope))
      documents)
      (fail!
       :user.core/invalid-read-result
       "User Graph returned an invalid exact-scope role-assignment collection."
       {:organization/id organization-id
        :scope scope
        :facts facts}))

    documents))

;; =============================================================================
;; Organization-owned scope context
;; =============================================================================

(def scope-context?
  "Returns true for the stable Organization-owned authorization-scope context."
  authorization-scope/scope-context?)

(defn- require-scope-context!
  [scope-context]
  (when-not
   (scope-context? scope-context)
    (fail!
     :user/invalid-scope-context
     "A valid Organization scope context is required."
     {:scope-context scope-context}))

  {:organization-id
   (:organization/id scope-context)

   :target
   (:scope/target scope-context)

   :applicable-scopes
   (:scope/applicable scope-context)

   :operational?
   (:scope/operational? scope-context)})

(defn access-context
  "Returns a compact User access context at an Organization-owned scope.

   input is:

     {:user-id       uuid
      :scope-context organization-scope-context}

   Organization owns scope hierarchy and operational state. User owns identity,
   Membership, roles, skills, and User capabilities.

   The result contains no raw User-model documents. Membership skills are
   organization-local strings and do not themselves grant authority."
  [ctx {:keys [user-id scope-context]}]
  (require-uuid!
   user-id
   :user/invalid-user-id
   "Scoped access requires a UUID User ID."
   {:user/id user-id})

  (let [{:keys
         [organization-id
          target
          applicable-scopes
          operational?]}
        (require-scope-context!
         scope-context)

        facts
        (graph/query
         ctx
         (user.graph/access-query-input
          {:user-id user-id
           :organization-id organization-id
           :applicable-scopes applicable-scopes})
         access-proof-query)

        user
        (require-document!
         facts
         :user/found?
         :user/doc
         identity/document-consistent?
         :user/not-found
         "The requested User does not exist."
         {:user/id user-id
          :organization/id organization-id})

        membership-node
        (when
         (true?
          (:user/current-membership-found? facts))
          (:user/current-membership facts))

        membership-document
        (:membership/doc membership-node)

        role-assignments
        (mapv
         :role-assignment/doc
         (or
          (:membership/role-assignments membership-node)
          []))]

    (when
     membership-node
      (when-not
       (and
        (membership/document-consistent?
         membership-document)

        (membership/for-user?
         membership-document
         user-id)

        (membership/for-organization?
         membership-document
         organization-id)

        (access/current-membership?
         membership-document)

        (every?
         #(access/assignment-for-membership?
           membership-document
           %)
         role-assignments))
        (fail!
         :user.core/invalid-read-result
         "User Graph returned an invalid scoped Membership access result."
         {:user/id user-id
          :organization/id organization-id
          :facts facts})))

    (let [public-context
          (access/access-context
           user
           membership-document
           role-assignments
           applicable-scopes
           organization-id)]
      (when-not
       (access/access-context?
        public-context)
        (fail!
         :user.core/invalid-access-context
         "User access facts could not be normalized into a valid public context."
         {:user/id user-id
          :organization/id organization-id
          :facts facts}))

      (assoc
       public-context
       :scope/target target
       :scope/operational? operational?
       :user/display-name (:user/display-name user)))))

;; =============================================================================
;; Effectful operations
;; =============================================================================

(defn invite-helper-to-location
  "Invites a helper to one active Location.

   User FX reloads and authorizes all current facts before committing."
  [ctx input]
  (user.fx/invite-helper-to-location
   ctx
   input))

(defn accept-invitation
  "Accepts a pending location-scoped helper invitation for the signed-in User."
  [ctx input]
  (user.fx/accept-invitation
   ctx
   input))

(defn add-member-skill
  "Adds one organization-local skill to a Membership.

   input is:

     {:organization-id uuid
      :location-id     uuid
      :membership-id   uuid
      :skill           string}

   User FX reloads the Location, actor access, and target Membership before
   committing."
  [ctx input]
  (user.fx/add-member-skill
   ctx
   input))

(defn remove-member-skill
  "Removes one organization-local skill from a Membership.

   Authorization and input shape are the same as add-member-skill."
  [ctx input]
  (user.fx/remove-member-skill
   ctx
   input))

(def operations
  "Effectful operations currently exposed by the User model."
  {:user/invite-helper-to-location #'invite-helper-to-location
   :user/accept-invitation #'accept-invitation
   :user/add-member-skill #'add-member-skill
   :user/remove-member-skill #'remove-member-skill})

;; =============================================================================
;; Public entity and vocabulary values
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
  authorization-scope/scope-types)

(def capabilities
  access/capabilities)

(def invite-helper-to-location-capability
  access/invite-helper-to-location-capability)

(def manage-member-skills-capability
  access/manage-member-skills-capability)

;; =============================================================================
;; Shared scalar values
;; =============================================================================

(defn role?
  [value]
  (user.common/role? value))

(defn scope-type?
  [value]
  (authorization-scope/scope-type? value))

(defn scope-reference?
  [value]
  (authorization-scope/scope-reference? value))

(defn same-scope?
  [a b]
  (authorization-scope/same-scope? a b))

(defn capability?
  [value]
  (access/capability? value))

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

(defn normalize-skill
  "Returns the canonical organization-local representation of skill."
  [value]
  (user.common/normalize-skill value))

(defn skill?
  [value]
  (user.common/skill? value))

(defn normalize-skills
  [values]
  (user.common/normalize-skills values))

(defn skills?
  [value]
  (user.common/skills? value))

;; =============================================================================
;; Authorization-scope values
;; =============================================================================

(defn organization-scope
  [organization-id]
  (authorization-scope/organization-scope organization-id))

(defn organization-group-scope
  [organization-group-id]
  (authorization-scope/organization-group-scope organization-group-id))

(defn location-scope
  [location-id]
  (authorization-scope/location-scope location-id))

(defn organization-scope?
  [value]
  (authorization-scope/organization-scope? value))

(defn organization-group-scope?
  [value]
  (authorization-scope/organization-group-scope? value))

(defn location-scope?
  [value]
  (authorization-scope/location-scope? value))

;; =============================================================================
;; User identity facts
;; =============================================================================

(defn user-status?
  [value]
  (identity/status? value))

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

(defn membership-status?
  [value]
  (membership/status? value))

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

(defn membership-skills
  "Returns the organization-local skill set stored on Membership."
  [membership-document]
  (membership/skills membership-document))

(defn membership-has-skill?
  "Returns true when Membership carries the canonicalized organization-local
   skill string."
  [membership-document skill]
  (membership/has-skill?
   membership-document
   skill))

(defn current-membership?
  [membership-document]
  (access/current-membership? membership-document))

(defn current-membership-for-user?
  [user membership-document]
  (access/current-membership-for-user?
   user
   membership-document))

(defn access-enabled-membership?
  [user membership-document]
  (access/access-enabled-membership?
   user
   membership-document))

;; =============================================================================
;; Role-assignment facts
;; =============================================================================

(defn role-assignment-status?
  [value]
  (role/status? value))

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

(defn role-assignment-for-membership?
  [role-assignment membership-id]
  (role/for-membership?
   role-assignment
   membership-id))

(defn role-assignment-for-organization?
  [role-assignment organization-id]
  (role/for-organization?
   role-assignment
   organization-id))

(defn role-assignment-grants-role?
  [role-assignment expected-role]
  (role/grants-role?
   role-assignment
   expected-role))

(defn role-assignment-at-scope?
  [role-assignment expected-scope]
  (role/at-scope?
   role-assignment
   expected-scope))

;; =============================================================================
;; Invitation facts
;; =============================================================================

(defn invitation-status?
  [value]
  (invitation/status? value))

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

(defn invitation-recipient-type
  [invitation-document]
  (invitation/recipient-type invitation-document))

(defn invitation-recipient-value
  [invitation-document]
  (invitation/recipient-value invitation-document))

(defn invitation-scope
  [invitation-document]
  (invitation/scope invitation-document))

;; =============================================================================
;; Composed access facts and capabilities
;; =============================================================================

(defn organization-affiliated?
  [user memberships]
  (access/organization-affiliated?
   user
   memberships))

(defn customer?
  [user memberships]
  (access/customer?
   user
   memberships))

(defn member-skills
  "Returns the effective organization-local skill set for an access-enabled
   Membership. Suspended or otherwise access-disabled Memberships yield #{}."
  [user membership-document]
  (access/member-skills
   user
   membership-document))

(defn member-has-skill?
  "Returns true when an access-enabled Membership has skill."
  [user membership-document skill]
  (access/has-skill?
   user
   membership-document
   skill))

(defn applicable-scopes?
  [value]
  (access/applicable-scopes? value))

(defn access-context?
  [value]
  (and
   (access/access-context? value)

   (authorization-scope/scope-reference?
    (:scope/target value))

   (boolean?
    (:scope/operational? value))

   (identity/display-name?
    (:user/display-name value))))

(defn has-capability?
  [access-context expected-capability]
  (access/has-capability?
   access-context
   expected-capability))

(defn can-invite-helper?
  "Returns true when an access context may display the helper-invitation UI.

   invite-helper-to-location always reloads and reauthorizes current facts."
  [access-context]
  (access/can-invite-helper?
   access-context))

(defn can-manage-member-skills?
  "Returns true when an access context may display Membership skill-management
   UI.

   add-member-skill and remove-member-skill always reload and reauthorize
   current facts."
  [access-context]
  (access/can-manage-member-skills?
   access-context))

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
