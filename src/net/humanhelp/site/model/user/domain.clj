(ns net.humanhelp.site.model.user.domain
  "Public pure-domain facade for the HumanHelp user model.

   This namespace composes the user model's document types:

     identity
     membership
     role assignment
     invitation
     request capability

   It also owns facts that require more than one of those document types, such
   as whether a user has staff authority within an organization or location.

   This namespace performs no database reads, persistence, authorization, token
   verification, or other effects."
  (:require
   [net.humanhelp.site.model.user.capability :as capability]
   [net.humanhelp.site.model.user.identity :as identity]
   [net.humanhelp.site.model.user.invitation :as invitation]
   [net.humanhelp.site.model.user.membership :as membership]
   [net.humanhelp.site.model.user.role :as role]))

;; =============================================================================
;; Model components
;; =============================================================================

(def models
  {:identity
   identity/model

   :membership
   membership/model

   :role-assignment
   role/model

   :invitation
   invitation/model

   :request-capability
   capability/model})

(def entity-types
  (into {}
        (map
         (fn [[model-key model]]
           [model-key
            (:entity-type model)]))
        models))

;; =============================================================================
;; Error messages
;; =============================================================================

(defn error-message
  "Returns the user-facing message associated with a user-model error keyword."
  [error]
  (case
   (namespace error)

    "user"
    (identity/error-message error)

    "membership"
    (membership/error-message error)

    "role-assignment"
    (role/error-message error)

    "invitation"
    (invitation/error-message error)

    "request-capability"
    (capability/error-message error)

    "The user operation could not be completed."))

;; =============================================================================
;; Membership collections
;; =============================================================================

(defn active-memberships
  [memberships]
  (into []
        (filter membership/active?)
        (or memberships [])))

(defn memberships-for-user
  [memberships user-id]
  (into []
        (filter
         #(membership/belongs-to-user?
           %
           user-id))
        (or memberships [])))

(defn memberships-for-organization
  [memberships organization-id]
  (into []
        (filter
         #(membership/belongs-to-organization?
           %
           organization-id))
        (or memberships [])))

(defn active-memberships-for-user
  [memberships user-id]
  (into []
        (filter
         #(and
           (membership/active? %)
           (membership/belongs-to-user?
            %
            user-id)))
        (or memberships [])))

(defn active-memberships-for-organization
  [memberships organization-id]
  (into []
        (filter
         #(and
           (membership/active? %)
           (membership/belongs-to-organization?
            %
            organization-id)))
        (or memberships [])))

(defn active-membership-for
  "Returns the active membership connecting user-id to organization-id.

   Returns nil when no such membership exists.

   Throws when duplicate active memberships exist because that violates the
   model's natural uniqueness rule and must not be hidden by selecting one."
  [memberships user-id organization-id]
  (let [matches
        (into []
              (filter
               #(and
                 (membership/active? %)
                 (membership/belongs-to?
                  %
                  user-id
                  organization-id)))
              (or memberships []))]
    (case
     (count matches)

      0
      nil

      1
      (first matches)

      (throw
       (ex-info
        "Multiple active memberships exist for one user and organization."
        {:error/type
         :membership/duplicate-active

         :user/id
         user-id

         :organization/id
         organization-id

         :membership/ids
         (mapv :xt/id matches)})))))

;; =============================================================================
;; Role-assignment collections
;; =============================================================================

(defn active-role-assignments
  [assignments]
  (into []
        (filter role/active?)
        (or assignments [])))

(defn assignments-for-membership
  [assignments membership-id]
  (into []
        (filter
         #(role/belongs-to-membership?
           %
           membership-id))
        (or assignments [])))

(defn active-assignments-for-membership
  [assignments membership-id]
  (into []
        (filter
         #(and
           (role/active? %)
           (role/belongs-to-membership?
            %
            membership-id)))
        (or assignments [])))

(defn roles-for-membership
  "Returns the active roles granted through membership-id."
  [assignments membership-id]
  (into #{}
        (map :role-assignment/role)
        (active-assignments-for-membership
         assignments
         membership-id)))

(defn organization-wide-roles-for-membership
  "Returns the active organization-wide roles granted through membership-id."
  [assignments membership-id]
  (into #{}
        (comp
         (filter
          #(and
            (role/active? %)
            (role/belongs-to-membership?
             %
             membership-id)
            (role/organization-wide? %)))

         (map
          :role-assignment/role))
        (or assignments [])))

(defn location-ids-for-membership
  "Returns the locations named by active location-scoped role assignments."
  [assignments membership-id]
  (into #{}
        (comp
         (filter
          #(and
            (role/active? %)
            (role/belongs-to-membership?
             %
             membership-id)
            (role/location-scoped? %)))

         (map
          :role-assignment/location))
        (or assignments [])))

;; =============================================================================
;; Cross-document authority
;; =============================================================================

(defn- membership-id->membership
  [memberships]
  (into {}
        (map
         (juxt
          :xt/id
          identity))
        (or memberships [])))

(defn authorized-role-assignments
  "Returns active role assignments backed by active memberships.

   Options may restrict the result by:

     :user-id
     :organization-id
     :location-id
     :roles

   When :location-id is supplied, organization-wide assignments and assignments
   scoped to that location apply.

   When :location-id is absent, both organization-wide and location-scoped
   assignments are returned."
  [memberships assignments
   {:keys
    [user-id
     organization-id
     location-id
     roles]}]
  (let [membership-id->membership
        (membership-id->membership
         memberships)

        roles
        (when (some? roles)
          (set roles))]
    (into []
          (filter
           (fn [assignment]
             (let [membership
                   (get
                    membership-id->membership
                    (:role-assignment/membership assignment))]
               (and
                (role/active? assignment)

                (some? membership)

                (membership/active? membership)

                (or
                 (nil? user-id)

                 (membership/belongs-to-user?
                  membership
                  user-id))

                (or
                 (nil? organization-id)

                 (membership/belongs-to-organization?
                  membership
                  organization-id))

                (or
                 (nil? location-id)

                 (role/applies-to-location?
                  assignment
                  location-id))

                (or
                 (nil? roles)

                 (contains?
                  roles
                  (:role-assignment/role assignment)))))))
          (or assignments []))))

(defn has-role?
  "Returns true when the user has an active role through an active membership.

   organization-id and location-id may be nil when the caller does not need to
   restrict the role to that scope."
  [memberships assignments
   {:keys
    [user-id
     organization-id
     location-id
     role]}]
  (boolean
   (seq
    (authorized-role-assignments
     memberships
     assignments
     {:user-id user-id
      :organization-id organization-id
      :location-id location-id
      :roles #{role}}))))

(defn helper?
  [memberships assignments scope]
  (has-role?
   memberships
   assignments
   (assoc scope
          :role
          :helper)))

(defn supervisor?
  [memberships assignments scope]
  (has-role?
   memberships
   assignments
   (assoc scope
          :role
          :supervisor)))

(defn admin?
  [memberships assignments scope]
  (has-role?
   memberships
   assignments
   (assoc scope
          :role
          :admin)))

(defn staff?
  "Returns true when the user has at least one active role assignment supported
   by an active membership."
  [memberships assignments user-id]
  (boolean
   (seq
    (authorized-role-assignments
     memberships
     assignments
     {:user-id user-id}))))

(defn customer?
  "Returns true when the user has no membership or role-assignment documents.

   Customer is not a persisted role. It is the absence of organizational
   affiliation in the user model.

   This intentionally examines all supplied documents rather than only active
   documents. Whether revoked historical relationships should be omitted from
   the supplied collections is a Graph/query policy."
  [memberships assignments]
  (and
   (empty?
    (or memberships []))

   (empty?
    (or assignments []))))

;; =============================================================================
;; Invitation relationships
;; =============================================================================

(defn invitations-for-organization
  [invitations organization-id]
  (into []
        (filter
         #(=
           organization-id
           (:invitation/organization %)))
        (or invitations [])))

(defn pending-invitations
  [invitations]
  (into []
        (filter invitation/pending?)
        (or invitations [])))

(defn usable-invitations-at
  [invitations now]
  (into []
        (filter
         #(invitation/usable-at?
           %
           now))
        (or invitations [])))

;; =============================================================================
;; Capability relationships
;; =============================================================================

(defn capabilities-for-request
  [capabilities request-id]
  (into []
        (filter
         #(capability/belongs-to-request?
           %
           request-id))
        (or capabilities [])))

(defn usable-capabilities-at
  [capabilities now]
  (into []
        (filter
         #(capability/usable-at?
           %
           now))
        (or capabilities [])))

;; =============================================================================
;; Public model description
;; =============================================================================

(def model
  {:entity-types entity-types
   :models models
   :roles role/role-order})
