(ns net.humanhelp.site.model.user.membership
  "Pure domain rules for organization memberships.

   A membership connects one user to one organization. It does not itself grant
   helper, supervisor, or admin authority; those permissions are represented by
   role-assignment documents attached to the membership.

   This namespace validates and transitions membership documents. Verifying that
   the referenced user and organization exist, preventing duplicate memberships,
   and authorizing membership changes belong to Graph and FX."
  (:require
   [tick.core :as tick])
  (:import
   [java.time ZonedDateTime]
   [java.util UUID]))

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

   :membership/not-suspendable
   "The membership cannot be suspended from its current state."

   :membership/not-reactivatable
   "The membership cannot be reactivated from its current state."

   :membership/not-revocable
   "The membership cannot be revoked from its current state."

   :membership/revoked
   "The membership has been revoked."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn uuid-value?
  [value]
  (instance? UUID value))

(defn zdt-value?
  [value]
  (tick/zoned-date-time? value))

(defn error-message
  [error]
  (get action-error-messages
       error
       "The membership could not be updated."))

(defn- zdt<=
  [a b]
  (and
   (zdt-value? a)
   (zdt-value? b)
   (not
    (.isAfter ^ZonedDateTime a
              ^ZonedDateTime b))))

(defn- optional-between?
  [start value end]
  (or
   (nil? value)
   (and
    (zdt<= start value)
    (zdt<= value end))))

(defn valid-change-time?
  [membership now]
  (and
   (zdt-value? now)
   (zdt<=
    (:membership/created-at membership)
    now)
   (zdt<=
    (:membership/updated-at membership)
    now)))

;; =============================================================================
;; Domain predicates
;; =============================================================================

(defn status?
  [value]
  (contains? statuses value))

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
   (uuid-value? user-id)
   (= user-id
      (:membership/user membership))))

(defn belongs-to-organization?
  [membership organization-id]
  (and
   (uuid-value? organization-id)
   (= organization-id
      (:membership/organization membership))))

(defn belongs-to?
  [membership user-id organization-id]
  (and
   (belongs-to-user? membership user-id)
   (belongs-to-organization? membership organization-id)))

(defn membership-key
  "Returns the natural uniqueness key for a membership.

   Graph or persistence code should use this to prevent more than one current
   membership relationship for the same user and organization."
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
   (zdt<= created-at updated-at)
   (optional-between?
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
   (uuid-value?
    (:xt/id membership))

   (uuid-value?
    (:membership/user membership))

   (uuid-value?
    (:membership/organization membership))

   (lifecycle-consistent? membership)))

;; =============================================================================
;; Input validation
;; =============================================================================

(defn create-input-errors
  [{:keys
    [id
     user-id
     organization-id
     now]}]
  (cond-> {}
    (not (uuid-value? id))
    (assoc
     :id
     "A membership UUID is required.")

    (not (uuid-value? user-id))
    (assoc
     :user-id
     "A valid user UUID is required.")

    (not (uuid-value? organization-id))
    (assoc
     :organization-id
     "A valid organization UUID is required.")

    (not (zdt-value? now))
    (assoc
     :now
     "A valid membership creation time is required.")))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

(defn- throw-invalid!
  [message errors input]
  (throw
   (ex-info
    message
    {:error/type :membership/invalid-input
     :errors errors
     :input input})))

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
    (when (seq errors)
      (throw-invalid!
       "Cannot create membership."
       errors
       input))

    {:xt/id id
     :membership/user user-id
     :membership/organization organization-id
     :membership/status :active
     :membership/revision 0
     :membership/created-at now
     :membership/updated-at now}))

;; =============================================================================
;; Membership lifecycle transitions
;; =============================================================================

(defn- bump-revision
  [membership now]
  (-> membership
      (update
       :membership/revision
       (fnil inc 0))
      (assoc
       :membership/updated-at now)))

(defn- transition-error
  [action]
  (case action
    :suspend
    :membership/not-suspendable

    :reactivate
    :membership/not-reactivatable

    :revoke
    :membership/not-revocable

    :membership/invalid-input))

(defn transition-membership
  [membership action now]
  (let [status'
        (next-status membership action)]
    (cond
      (nil? status')
      {:ok? false
       :error
       (transition-error action)}

      (not
       (valid-change-time? membership now))
      {:ok? false
       :error :membership/invalid-time}

      :else
      {:ok? true
       :membership
       (case action
         :suspend
         (-> membership
             (assoc
              :membership/status status')
             (dissoc
              :membership/ended-at)
             (bump-revision now))

         :reactivate
         (-> membership
             (assoc
              :membership/status status')
             (dissoc
              :membership/ended-at)
             (bump-revision now))

         :revoke
         (-> membership
             (assoc
              :membership/status status'
              :membership/ended-at now)
             (bump-revision now)))})))

(defn suspend-membership-doc
  [membership now]
  (transition-membership
   membership
   :suspend
   now))

(defn reactivate-membership-doc
  [membership now]
  (transition-membership
   membership
   :reactivate
   now))

(defn revoke-membership-doc
  [membership now]
  (transition-membership
   membership
   :revoke
   now))

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
;; Public membership description
;; =============================================================================

(def model
  {:entity-type entity-type
   :statuses status-order
   :active-statuses active-statuses
   :terminal-statuses terminal-statuses
   :allowed-transitions allowed-transitions})
