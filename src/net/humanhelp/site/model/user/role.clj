(ns net.humanhelp.site.model.user.role
  "Pure domain rules for staff role assignments.

   A role assignment grants authority through an organization membership.

   An assignment with :role-assignment/location applies only to that location.
   An assignment without a location applies throughout the membership's
   organization.

   Revocation is terminal. Restoring authority creates a new assignment rather
   than reactivating the old one, preserving an unambiguous audit history.

   This namespace does not verify that the membership or location exists, that
   the location belongs to the membership's organization, or that the actor may
   grant the requested role. Those checks belong to Graph and FX."
  (:require
   [tick.core :as tick])
  (:import
   [java.time ZonedDateTime]
   [java.util UUID]))

;; =============================================================================
;; Identity
;; =============================================================================

(def entity-type
  :role-assignment)

;; =============================================================================
;; Roles
;; =============================================================================

(def role-order
  [:helper
   :supervisor
   :admin])

(def roles
  (set role-order))

;; Roles are intentionally treated as named authorities rather than a numeric
;; hierarchy. Whether one role implies another should be expressed as derived
;; permission facts instead of being assumed here.

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def status-order
  [:active
   :revoked])

(def statuses
  (set status-order))

(def active-statuses
  #{:active})

(def terminal-statuses
  #{:revoked})

(def allowed-transitions
  {[:active :revoke]
   :revoked})

(def action-error-messages
  {:role-assignment/invalid-input
   "Some role information needs to be corrected."

   :role-assignment/invalid-time
   "The role assignment could not be changed because its timestamp was invalid."

   :role-assignment/not-revocable
   "The role assignment cannot be revoked from its current state."

   :role-assignment/revoked
   "The role assignment has already been revoked."})

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
       "The role assignment could not be updated."))

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
  [assignment now]
  (and
   (zdt-value? now)
   (zdt<=
    (:role-assignment/created-at assignment)
    now)
   (zdt<=
    (:role-assignment/updated-at assignment)
    now)))

;; =============================================================================
;; Domain predicates
;; =============================================================================

(defn role?
  [value]
  (contains? roles value))

(defn status?
  [value]
  (contains? statuses value))

(defn active?
  [assignment]
  (= :active
     (:role-assignment/status assignment)))

(defn revoked?
  [assignment]
  (= :revoked
     (:role-assignment/status assignment)))

(defn terminal?
  [assignment]
  (contains?
   terminal-statuses
   (:role-assignment/status assignment)))

(defn organization-wide?
  [assignment]
  (nil?
   (:role-assignment/location assignment)))

(defn location-scoped?
  [assignment]
  (some?
   (:role-assignment/location assignment)))

(defn belongs-to-membership?
  [assignment membership-id]
  (and
   (uuid-value? membership-id)
   (= membership-id
      (:role-assignment/membership assignment))))

(defn grants-role?
  [assignment role]
  (and
   (active? assignment)
   (= role
      (:role-assignment/role assignment))))

(defn applies-to-location?
  "Returns true when an active assignment applies to location-id.

   Organization-wide assignments apply to every location in the organization.
   The caller remains responsible for proving that location-id belongs to the
   membership's organization."
  [assignment location-id]
  (and
   (active? assignment)
   (uuid-value? location-id)
   (or
    (organization-wide? assignment)
    (= location-id
       (:role-assignment/location assignment)))))

(defn grants-role-at-location?
  [assignment role location-id]
  (and
   (grants-role? assignment role)
   (applies-to-location? assignment location-id)))

(defn assignment-key
  "Returns the natural uniqueness key for a role assignment.

   Graph or persistence code should prevent more than one active assignment
   with the same membership, role, and optional location scope."
  [assignment]
  [(:role-assignment/membership assignment)
   (:role-assignment/role assignment)
   (:role-assignment/location assignment)])

(defn next-status
  [assignment action]
  (get
   allowed-transitions
   [(:role-assignment/status assignment)
    action]))

(defn can-transition?
  [assignment action]
  (some?
   (next-status assignment action)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn lifecycle-times-consistent?
  [{:role-assignment/keys
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
  [{:role-assignment/keys
    [status
     ended-at]
    :as assignment}]
  (and
   (status? status)
   (lifecycle-times-consistent? assignment)

   (case status
     :active
     (nil? ended-at)

     :revoked
     (some? ended-at)

     false)))

(defn document-consistent?
  [assignment]
  (and
   (uuid-value?
    (:xt/id assignment))

   (uuid-value?
    (:role-assignment/membership assignment))

   (role?
    (:role-assignment/role assignment))

   (or
    (nil?
     (:role-assignment/location assignment))

    (uuid-value?
     (:role-assignment/location assignment)))

   (lifecycle-consistent? assignment)))

;; =============================================================================
;; Input validation
;; =============================================================================

(defn create-input-errors
  [{:keys
    [id
     membership-id
     role
     location-id
     now]}]
  (cond-> {}
    (not (uuid-value? id))
    (assoc
     :id
     "A role-assignment UUID is required.")

    (not (uuid-value? membership-id))
    (assoc
     :membership-id
     "A valid membership UUID is required.")

    (not (role? role))
    (assoc
     :role
     "Choose a valid staff role.")

    (and
     (some? location-id)
     (not (uuid-value? location-id)))
    (assoc
     :location-id
     "Choose a valid location.")

    (not (zdt-value? now))
    (assoc
     :now
     "A valid role-assignment creation time is required.")))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

(defn- throw-invalid!
  [message errors input]
  (throw
   (ex-info
    message
    {:error/type :role-assignment/invalid-input
     :errors errors
     :input input})))

;; =============================================================================
;; Role-assignment construction
;; =============================================================================

(defn new-role-assignment
  [{:keys
    [id
     membership-id
     role
     location-id
     now]
    :as input}]
  (let [errors
        (create-input-errors input)]
    (when (seq errors)
      (throw-invalid!
       "Cannot create role assignment."
       errors
       input))

    (cond->
     {:xt/id id
      :role-assignment/membership membership-id
      :role-assignment/role role
      :role-assignment/status :active
      :role-assignment/revision 0
      :role-assignment/created-at now
      :role-assignment/updated-at now}

      location-id
      (assoc
       :role-assignment/location
       location-id))))

;; =============================================================================
;; Role-assignment lifecycle transitions
;; =============================================================================

(defn- bump-revision
  [assignment now]
  (-> assignment
      (update
       :role-assignment/revision
       (fnil inc 0))
      (assoc
       :role-assignment/updated-at
       now)))

(defn transition-role-assignment
  [assignment action now]
  (let [status'
        (next-status assignment action)]
    (cond
      (nil? status')
      {:ok? false
       :error
       (if (revoked? assignment)
         :role-assignment/revoked
         :role-assignment/not-revocable)}

      (not
       (valid-change-time? assignment now))
      {:ok? false
       :error :role-assignment/invalid-time}

      :else
      {:ok? true
       :role-assignment
       (case action
         :revoke
         (-> assignment
             (assoc
              :role-assignment/status status'
              :role-assignment/ended-at now)
             (bump-revision now)))})))

(defn revoke-role-assignment-doc
  [assignment now]
  (transition-role-assignment
   assignment
   :revoke
   now))

;; =============================================================================
;; Version descriptions
;; =============================================================================

(defn expected-version
  [assignment]
  {:role-assignment/id
   (:xt/id assignment)

   :role-assignment/revision
   (:role-assignment/revision assignment)

   :role-assignment/status
   (:role-assignment/status assignment)

   :role-assignment/updated-at
   (:role-assignment/updated-at assignment)})

;; =============================================================================
;; Public role description
;; =============================================================================

(def model
  {:entity-type entity-type
   :roles role-order
   :statuses status-order
   :active-statuses active-statuses
   :terminal-statuses terminal-statuses
   :allowed-transitions allowed-transitions})
