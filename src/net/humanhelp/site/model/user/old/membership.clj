(ns net.humanhelp.site.model.user.membership
  "Pure domain rules for organization memberships.

   A membership connects one user identity to one organization. It does not
   itself grant staff authority; helper, supervisor, and admin authority are
   attached separately through role assignments.

   A user should have at most one active or suspended membership for a given
   organization. Enforcing that uniqueness requires an atomic persistence
   check and therefore belongs to the commit implementation rather than this
   pure namespace.

   Revocation is terminal. A revoked relationship remains available as
   historical data, while restoring access requires creation of a new
   membership."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [tick.core :as tick]))

;; =============================================================================
;; Identity
;; =============================================================================

(def entity-type
  :membership)

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def status-order
  [:active
   :suspended
   :revoked])

(def statuses
  (set status-order))

(def active-statuses
  #{:active})

(def terminal-statuses
  #{:revoked})

(def allowed-transitions
  {[:active :suspend]
   :suspended

   [:suspended :reactivate]
   :active

   [:active :revoke]
   :revoked

   [:suspended :revoke]
   :revoked})

(def action-error-messages
  {:membership/invalid-input
   "Some membership information needs to be corrected."

   :membership/invalid-time
   "The membership could not be changed because its timestamp was invalid."

   :membership/not-active
   "The membership is not active."

   :membership/not-suspended
   "The membership is not suspended."

   :membership/revoked
   "The membership has been revoked."

   :membership/not-suspendable
   "The membership cannot be suspended from its current state."

   :membership/not-reactivatable
   "The membership cannot be reactivated from its current state."

   :membership/not-revocable
   "The membership cannot be revoked from its current state."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn error-message
  [error]
  (get action-error-messages
       error
       "The membership could not be updated."))

(defn valid-change-time?
  [membership now]
  (model.common/valid-change-time?
   membership
   :membership/created-at
   :membership/updated-at
   now))

;; =============================================================================
;; Domain predicates
;; =============================================================================

(defn status?
  [value]
  (contains?
   statuses
   value))

(defn active?
  [membership]
  (= :active
     (:membership/status membership)))

(defn suspended?
  [membership]
  (= :suspended
     (:membership/status membership)))

(defn revoked?
  [membership]
  (= :revoked
     (:membership/status membership)))

(defn terminal?
  [membership]
  (contains?
   terminal-statuses
   (:membership/status membership)))

(defn belongs-to-user?
  [membership user-id]
  (and
   (uuid? user-id)

   (= user-id
      (:membership/user membership))))

(defn belongs-to-organization?
  [membership organization-id]
  (and
   (uuid? organization-id)

   (= organization-id
      (:membership/organization membership))))

(defn belongs-to?
  [membership user-id organization-id]
  (and
   (belongs-to-user?
    membership
    user-id)

   (belongs-to-organization?
    membership
    organization-id)))

(defn membership-key
  "Returns the natural identity of an organization membership.

   Persistence should reject creation of another current membership with the
   same key."
  [membership]
  [(:membership/user membership)
   (:membership/organization membership)])

(defn next-status
  [membership action]
  (get
   allowed-transitions
   [(:membership/status membership)
    action]))

(defn can-transition?
  [membership action]
  (some?
   (next-status membership action)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn lifecycle-times-consistent?
  [{:membership/keys
    [created-at
     updated-at
     ended-at]}]
  (and
   (model.common/timestamp<=
    created-at
    updated-at)

   (model.common/optional-between?
    created-at
    ended-at
    updated-at)))

(defn lifecycle-consistent?
  [{:membership/keys
    [status
     ended-at]
    :as membership}]
  (and
   (status? status)
   (lifecycle-times-consistent? membership)

   (case status
     :active
     (nil? ended-at)

     :suspended
     (nil? ended-at)

     :revoked
     (some? ended-at)

     false)))

(defn document-consistent?
  [membership]
  (and
   (uuid?
    (:xt/id membership))

   (uuid?
    (:membership/user membership))

   (uuid?
    (:membership/organization membership))

   (nat-int?
    (:membership/revision membership))

   (tick/zoned-date-time?
    (:membership/created-at membership))

   (tick/zoned-date-time?
    (:membership/updated-at membership))

   (or
    (nil?
     (:membership/ended-at membership))

    (tick/zoned-date-time?
     (:membership/ended-at membership)))

   (lifecycle-consistent? membership)))

;; =============================================================================
;; Creation validation
;; =============================================================================

(defn create-input-errors
  [{:keys
    [id
     user-id
     organization-id
     now]}]
  (cond-> {}
    (not
     (uuid? id))
    (assoc
     :id
     "A membership UUID is required.")

    (not
     (uuid? user-id))
    (assoc
     :user-id
     "A valid user UUID is required.")

    (not
     (uuid? organization-id))
    (assoc
     :organization-id
     "A valid organization UUID is required.")

    (not
     (tick/zoned-date-time? now))
    (assoc
     :now
     "A valid membership creation time is required.")))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

;; =============================================================================
;; Membership construction
;; =============================================================================

(defn new-membership
  [{:keys
    [id
     user-id
     organization-id
     now]
    :as input}]
  (let [errors
        (create-input-errors input)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :membership/invalid-input
       "Cannot create membership."
       errors
       input))

    {:xt/id
     id

     :membership/user
     user-id

     :membership/organization
     organization-id

     :membership/status
     :active

     :membership/revision
     0

     :membership/created-at
     now

     :membership/updated-at
     now}))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn- transition-error
  [membership action]
  (cond
    (revoked? membership)
    :membership/revoked

    (= action :suspend)
    :membership/not-suspendable

    (= action :reactivate)
    :membership/not-reactivatable

    (= action :revoke)
    :membership/not-revocable

    :else
    :membership/invalid-input))

(defn transition-membership
  [membership action now]
  (cond
    (not
     (valid-change-time? membership now))
    {:ok? false
     :error
     :membership/invalid-time}

    (not
     (can-transition? membership action))
    {:ok? false
     :error
     (transition-error
      membership
      action)}

    :else
    {:ok? true

     :membership
     (-> (case action
           :suspend
           (-> membership
               (assoc
                :membership/status
                :suspended)

               (dissoc
                :membership/ended-at))

           :reactivate
           (-> membership
               (assoc
                :membership/status
                :active)

               (dissoc
                :membership/ended-at))

           :revoke
           (assoc
            membership

            :membership/status
            :revoked

            :membership/ended-at
            now)

           membership)

         (model.common/bump-revision
          :membership/revision
          :membership/updated-at
          now))}))

;; =============================================================================
;; Version descriptions
;; =============================================================================

(defn expected-version
  [membership]
  {:membership/id
   (:xt/id membership)

   :membership/revision
   (:membership/revision membership)

   :membership/status
   (:membership/status membership)

   :membership/updated-at
   (:membership/updated-at membership)})

;; =============================================================================
;; Public model description
;; =============================================================================

(def model
  {:entity-type
   entity-type

   :statuses
   status-order

   :active-statuses
   active-statuses

   :terminal-statuses
   terminal-statuses

   :allowed-transitions
   allowed-transitions})
