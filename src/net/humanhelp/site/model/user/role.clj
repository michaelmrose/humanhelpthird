(ns net.humanhelp.site.model.model.user.role
  "Pure domain rules for staff role assignments.

   A role assignment grants one role through one organization membership. It
   may apply throughout the organization or be scoped to one location.

   Roles are intentionally not treated as a numeric hierarchy. Admin,
   supervisor, and helper express different authority sets; callers should test
   for the exact capability they require.

   Revocation is terminal. Restoring the same authority requires creation of a
   new role assignment."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [tick.core :as tick]))

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

(defn role?
  [value]
  (contains?
   roles
   value))

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
   "Some role-assignment information needs to be corrected."

   :role-assignment/invalid-time
   "The role assignment could not be changed because its timestamp was invalid."

   :role-assignment/not-active
   "The role assignment is not active."

   :role-assignment/revoked
   "The role assignment has been revoked."

   :role-assignment/not-revocable
   "The role assignment cannot be revoked from its current state."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn error-message
  [error]
  (get action-error-messages
       error
       "The role assignment could not be updated."))

(defn valid-change-time?
  [assignment now]
  (model.common/valid-change-time?
   assignment
   :role-assignment/created-at
   :role-assignment/updated-at
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

(defn belongs-to-membership?
  [assignment membership-id]
  (and
   (uuid? membership-id)

   (= membership-id
      (:role-assignment/membership assignment))))

(defn organization-wide?
  "Returns true when the assignment applies throughout its organization."
  [assignment]
  (nil?
   (:role-assignment/location assignment)))

(defn location-scoped?
  [assignment]
  (some?
   (:role-assignment/location assignment)))

(defn grants-role?
  [assignment expected-role]
  (and
   (role? expected-role)

   (= expected-role
      (:role-assignment/role assignment))))

(defn applies-to-location?
  "Returns true when assignment applies at location-id.

   Organization-wide assignments apply to every location. A scoped assignment
   applies only to its matching location."
  [assignment location-id]
  (and
   (uuid? location-id)

   (or
    (organization-wide? assignment)

    (= location-id
       (:role-assignment/location assignment)))))

(defn grants-role-at-location?
  [assignment expected-role location-id]
  (and
   (active? assignment)

   (grants-role?
    assignment
    expected-role)

   (applies-to-location?
    assignment
    location-id)))

(defn assignment-key
  "Returns the natural identity of a role assignment.

   Persistence should reject another current assignment with the same
   membership, role, and location."
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
   (model.common/timestamp<=
    created-at
    updated-at)

   (model.common/optional-between?
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
   (uuid?
    (:xt/id assignment))

   (uuid?
    (:role-assignment/membership assignment))

   (role?
    (:role-assignment/role assignment))

   (or
    (nil?
     (:role-assignment/location assignment))

    (uuid?
     (:role-assignment/location assignment)))

   (nat-int?
    (:role-assignment/revision assignment))

   (tick/zoned-date-time?
    (:role-assignment/created-at assignment))

   (tick/zoned-date-time?
    (:role-assignment/updated-at assignment))

   (or
    (nil?
     (:role-assignment/ended-at assignment))

    (tick/zoned-date-time?
     (:role-assignment/ended-at assignment)))

   (lifecycle-consistent? assignment)))

;; =============================================================================
;; Creation validation
;; =============================================================================

(defn create-input-errors
  [{:keys
    [id
     membership-id
     role
     location-id
     now]}]
  (cond-> {}
    (not
     (uuid? id))
    (assoc
     :id
     "A role-assignment UUID is required.")

    (not
     (uuid? membership-id))
    (assoc
     :membership-id
     "A valid membership UUID is required.")

    (not
     (role? role))
    (assoc
     :role
     "Choose a valid staff role.")

    (and
     (some? location-id)
     (not
      (uuid? location-id)))
    (assoc
     :location-id
     "Choose a valid location.")

    (not
     (tick/zoned-date-time? now))
    (assoc
     :now
     "A valid role-assignment creation time is required.")))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

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
    (when
     (seq errors)
      (model.common/throw-invalid!
       :role-assignment/invalid-input
       "Cannot create role assignment."
       errors
       input))

    (cond->
     {:xt/id
      id

      :role-assignment/membership
      membership-id

      :role-assignment/role
      role

      :role-assignment/status
      :active

      :role-assignment/revision
      0

      :role-assignment/created-at
      now

      :role-assignment/updated-at
      now}

      location-id
      (assoc
       :role-assignment/location
       location-id))))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn revoke-role-assignment-doc
  [assignment now]
  (cond
    (revoked? assignment)
    {:ok? false
     :error
     :role-assignment/revoked}

    (not
     (valid-change-time? assignment now))
    {:ok? false
     :error
     :role-assignment/invalid-time}

    (not
     (can-transition?
      assignment
      :revoke))
    {:ok? false
     :error
     :role-assignment/not-revocable}

    :else
    {:ok? true

     :role-assignment
     (-> assignment
         (assoc
          :role-assignment/status
          :revoked

          :role-assignment/ended-at
          now)

         (model.common/bump-revision
          :role-assignment/revision
          :role-assignment/updated-at
          now))}))

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
;; Public model description
;; =============================================================================

(def model
  {:entity-type
   entity-type

   :roles
   role-order

   :statuses
   status-order

   :active-statuses
   active-statuses

   :terminal-statuses
   terminal-statuses

   :allowed-transitions
   allowed-transitions})
