(ns net.humanhelp.site.model.user.invitation
  "Pure domain rules for staff invitations.

   An invitation proposes one staff role within an organization, optionally
   scoped to a location. It identifies the intended recipient by phone, email,
   or both.

   Invitations store only a hash of the bearer token. The raw token must be
   returned to the caller at creation time and must never be persisted.

   Accepting an invitation ends the invitation lifecycle. Creating the user,
   membership, and role assignment produced by acceptance belongs to FX so
   those documents can be committed atomically."
  (:require
   [clojure.string :as str]
   [net.humanhelp.schema.common :as common]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.identity :as identity]
   [net.humanhelp.site.model.user.role :as role]
   [tick.core :as tick]))

;; =============================================================================
;; Identity and limits
;; =============================================================================

(def entity-type
  :invitation)

(def email-max
  320)

(def token-hash-min
  32)

(def token-hash-max
  256)

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def status-order
  [:pending
   :accepted
   :revoked
   :expired])

(def statuses
  (set status-order))

(def active-statuses
  #{:pending})

(def terminal-statuses
  #{:accepted
    :revoked
    :expired})

(def allowed-transitions
  {[:pending :accept]
   :accepted

   [:pending :revoke]
   :revoked

   [:pending :expire]
   :expired})

(def action-error-messages
  {:invitation/invalid-input
   "Some invitation information needs to be corrected."

   :invitation/invalid-time
   "The invitation could not be changed because its timestamp was invalid."

   :invitation/not-pending
   "The invitation is no longer pending."

   :invitation/not-acceptable
   "The invitation can no longer be accepted."

   :invitation/not-revocable
   "The invitation can no longer be revoked."

   :invitation/not-expired
   "The invitation has not expired."

   :invitation/expired
   "The invitation has expired."

   :invitation/already-accepted
   "The invitation has already been accepted."

   :invitation/revoked
   "The invitation has been revoked."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn error-message
  [error]
  (get action-error-messages
       error
       "The invitation could not be updated."))

(defn- trim-to-nil
  [value]
  (when (some? value)
    (let [value
          (str/trim
           (str value))]
      (when-not
       (str/blank? value)
        value))))

(defn valid-change-time?
  [invitation now]
  (model.common/valid-change-time?
   invitation
   :invitation/created-at
   :invitation/updated-at
   now))

(defn valid-token-hash?
  [value]
  (and
   (string? value)

   (common/non-blank-string?
    value)

   (<= token-hash-min
       (count value)
       token-hash-max)))

;; =============================================================================
;; Input normalization
;; =============================================================================

(defn normalize-email
  [value]
  (identity/normalize-email value))

(defn normalize-create-input
  [input]
  (let [input
        (or input {})]
    (cond-> input
      (contains? input :phone)
      (update
       :phone
       identity/normalize-phone)

      (contains? input :email)
      (update
       :email
       normalize-email)

      (contains? input :token-hash)
      (update
       :token-hash
       trim-to-nil))))

;; =============================================================================
;; Domain predicates
;; =============================================================================

(defn status?
  [value]
  (contains?
   statuses
   value))

(defn pending?
  [invitation]
  (= :pending
     (:invitation/status invitation)))

(defn accepted?
  [invitation]
  (= :accepted
     (:invitation/status invitation)))

(defn revoked?
  [invitation]
  (= :revoked
     (:invitation/status invitation)))

(defn expired-status?
  [invitation]
  (= :expired
     (:invitation/status invitation)))

(defn terminal?
  [invitation]
  (contains?
   terminal-statuses
   (:invitation/status invitation)))

(defn expired-at?
  "Returns true when now is at or after the invitation expiration time."
  [invitation now]
  (and
   (tick/zoned-date-time? now)

   (tick/zoned-date-time?
    (:invitation/expires-at invitation))

   (not
    (model.common/timestamp<
     now
     (:invitation/expires-at invitation)))))

(defn usable-at?
  [invitation now]
  (and
   (pending? invitation)

   (tick/zoned-date-time? now)

   (model.common/timestamp<=
    (:invitation/created-at invitation)
    now)

   (not
    (expired-at? invitation now))))

(defn acceptable-at?
  [invitation now]
  (usable-at?
   invitation
   now))

(defn revocable-at?
  [invitation now]
  (usable-at?
   invitation
   now))

(defn recipient-matches?
  "Returns true when at least one supplied identity value matches the
   invitation recipient."
  [invitation {:keys [phone email]}]
  (let [phone
        (identity/normalize-phone phone)

        email
        (normalize-email email)]
    (or
     (and
      (some? phone)

      (= phone
         (:invitation/phone invitation)))

     (and
      (some? email)

      (= email
         (:invitation/email invitation))))))

(defn organization-wide?
  [invitation]
  (nil?
   (:invitation/location invitation)))

(defn location-scoped?
  [invitation]
  (some?
   (:invitation/location invitation)))

(defn invitation-key
  "Returns the natural key used when looking for duplicate pending
   invitations.

   Phone and email are both included because an invitation may identify the
   recipient with either or both."
  [invitation]
  [(:invitation/organization invitation)
   (:invitation/location invitation)
   (:invitation/phone invitation)
   (:invitation/email invitation)
   (:invitation/role invitation)])

(defn next-status
  [invitation action]
  (get
   allowed-transitions
   [(:invitation/status invitation)
    action]))

(defn can-transition?
  [invitation action]
  (some?
   (next-status invitation action)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn recipient-consistent?
  [{:invitation/keys
    [phone
     email]}]
  (and
   (or
    (some? phone)
    (some? email))

   (or
    (nil? phone)
    (and
     (identity/valid-phone? phone)

     (= phone
        (identity/normalize-phone phone))))

   (or
    (nil? email)
    (and
     (identity/valid-email? email)

     (= email
        (normalize-email email))))))

(defn lifecycle-times-consistent?
  [{:invitation/keys
    [created-at
     updated-at
     expires-at
     accepted-at
     revoked-at]}]
  (and
   (model.common/timestamp<=
    created-at
    updated-at)

   ;; An invitation must have a non-empty usable lifetime.
   (model.common/timestamp<
    created-at
    expires-at)

   (model.common/optional-between?
    created-at
    accepted-at
    updated-at)

   (model.common/optional-between?
    created-at
    revoked-at
    updated-at)

   ;; Acceptance and revocation must occur before expiration.
   (or
    (nil? accepted-at)

    (model.common/timestamp<
     accepted-at
     expires-at))

   (or
    (nil? revoked-at)

    (model.common/timestamp<
     revoked-at
     expires-at))))

(defn lifecycle-consistent?
  [{:invitation/keys
    [status
     accepted-by
     accepted-at
     revoked-at
     expires-at
     updated-at]
    :as invitation}]
  (and
   (status? status)
   (lifecycle-times-consistent? invitation)

   (= (some? accepted-by)
      (some? accepted-at))

   (case status
     :pending
     (and
      (nil? accepted-by)
      (nil? accepted-at)
      (nil? revoked-at)

      ;; A persisted pending invitation has not yet been materialized as
      ;; expired, so its most recent mutation must precede expiration.
      (model.common/timestamp<
       updated-at
       expires-at))

     :accepted
     (and
      (some? accepted-by)
      (some? accepted-at)
      (nil? revoked-at))

     :revoked
     (and
      (nil? accepted-by)
      (nil? accepted-at)
      (some? revoked-at))

     :expired
     (and
      (nil? accepted-by)
      (nil? accepted-at)
      (nil? revoked-at)

      (model.common/timestamp<=
       expires-at
       updated-at))

     false)))

(defn document-consistent?
  [invitation]
  (and
   (uuid?
    (:xt/id invitation))

   (uuid?
    (:invitation/organization invitation))

   (or
    (nil?
     (:invitation/location invitation))

    (uuid?
     (:invitation/location invitation)))

   (role/role?
    (:invitation/role invitation))

   (valid-token-hash?
    (:invitation/token-hash invitation))

   (uuid?
    (:invitation/created-by invitation))

   (or
    (nil?
     (:invitation/accepted-by invitation))

    (uuid?
     (:invitation/accepted-by invitation)))

   (nat-int?
    (:invitation/revision invitation))

   (tick/zoned-date-time?
    (:invitation/created-at invitation))

   (tick/zoned-date-time?
    (:invitation/updated-at invitation))

   (tick/zoned-date-time?
    (:invitation/expires-at invitation))

   (or
    (nil?
     (:invitation/accepted-at invitation))

    (tick/zoned-date-time?
     (:invitation/accepted-at invitation)))

   (or
    (nil?
     (:invitation/revoked-at invitation))

    (tick/zoned-date-time?
     (:invitation/revoked-at invitation)))

   (recipient-consistent? invitation)
   (lifecycle-consistent? invitation)))

;; =============================================================================
;; Creation validation
;; =============================================================================

(defn create-input-errors
  [input]
  (let [raw-input
        (or input {})

        raw-phone
        (:phone raw-input)

        raw-email
        (:email raw-input)

        {:keys
         [id
          organization-id
          location-id
          phone
          email
          role
          token-hash
          created-by
          now
          expires-at]}
        (normalize-create-input raw-input)]
    (cond-> {}
      (not
       (uuid? id))
      (assoc
       :id
       "An invitation UUID is required.")

      (not
       (uuid? organization-id))
      (assoc
       :organization-id
       "A valid organization UUID is required.")

      (and
       (some? location-id)
       (not
        (uuid? location-id)))
      (assoc
       :location-id
       "Choose a valid location.")

      (and
       (nil? phone)
       (nil? email))
      (assoc
       :recipient
       "A phone number or email address is required.")

      (and
       (some? raw-phone)
       (nil? phone))
      (assoc
       :phone
       "Enter a valid ten-digit US phone number.")

      (and
       (some? raw-email)
       (not
        (identity/valid-email? raw-email)))
      (assoc
       :email
       "Enter a valid email address.")

      (and
       (string? email)
       (> (count email)
          email-max))
      (assoc
       :email
       (str
        "Use "
        email-max
        " characters or fewer."))

      (not
       (role/role? role))
      (assoc
       :role
       "Choose a valid staff role.")

      (not
       (valid-token-hash? token-hash))
      (assoc
       :token-hash
       "A valid invitation token hash is required.")

      (not
       (uuid? created-by))
      (assoc
       :created-by
       "A valid inviting user UUID is required.")

      (not
       (tick/zoned-date-time? now))
      (assoc
       :now
       "A valid invitation creation time is required.")

      (not
       (tick/zoned-date-time? expires-at))
      (assoc
       :expires-at
       "A valid invitation expiration time is required.")

      (and
       (tick/zoned-date-time? now)
       (tick/zoned-date-time? expires-at)
       (not
        (model.common/timestamp<
         now
         expires-at)))
      (assoc
       :expires-at
       "The invitation expiration time must be after its creation time."))))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

;; =============================================================================
;; Invitation construction
;; =============================================================================

(defn new-invitation
  [{:keys
    [id
     organization-id
     location-id
     role
     created-by
     now
     expires-at]
    :as input}]
  (let [{:keys
         [phone
          email
          token-hash]
        :as normalized}
        (normalize-create-input input)

        errors
        (create-input-errors normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :invitation/invalid-input
       "Cannot create invitation."
       errors
       input))

    (cond->
     {:xt/id
      id

      :invitation/organization
      organization-id

      :invitation/role
      role

      :invitation/token-hash
      token-hash

      :invitation/status
      :pending

      :invitation/created-by
      created-by

      :invitation/revision
      0

      :invitation/created-at
      now

      :invitation/updated-at
      now

      :invitation/expires-at
      expires-at}

      location-id
      (assoc
       :invitation/location
       location-id)

      phone
      (assoc
       :invitation/phone
       phone)

      email
      (assoc
       :invitation/email
       email))))

;; =============================================================================
;; Invitation lifecycle transitions
;; =============================================================================

(defn- terminal-error
  [invitation]
  (case
   (:invitation/status invitation)

    :accepted
    :invitation/already-accepted

    :revoked
    :invitation/revoked

    :expired
    :invitation/expired

    :invitation/not-pending))

(defn accept-invitation-doc
  [invitation accepted-by now]
  (cond
    (not
     (pending? invitation))
    {:ok? false
     :error
     (terminal-error invitation)}

    (not
     (valid-change-time?
      invitation
      now))
    {:ok? false
     :error
     :invitation/invalid-time}

    (expired-at?
     invitation
     now)
    {:ok? false
     :error
     :invitation/expired}

    (not
     (uuid? accepted-by))
    {:ok? false
     :error
     :invitation/invalid-input

     :errors
     {:accepted-by
      "A valid accepting user UUID is required."}}

    :else
    {:ok? true

     :invitation
     (-> invitation
         (assoc
          :invitation/status
          :accepted

          :invitation/accepted-by
          accepted-by

          :invitation/accepted-at
          now)

         (dissoc
          :invitation/revoked-at)

         (model.common/bump-revision
          :invitation/revision
          :invitation/updated-at
          now))}))

(defn revoke-invitation-doc
  [invitation now]
  (cond
    (not
     (pending? invitation))
    {:ok? false
     :error
     (terminal-error invitation)}

    (not
     (valid-change-time?
      invitation
      now))
    {:ok? false
     :error
     :invitation/invalid-time}

    (expired-at?
     invitation
     now)
    {:ok? false
     :error
     :invitation/expired}

    :else
    {:ok? true

     :invitation
     (-> invitation
         (assoc
          :invitation/status
          :revoked

          :invitation/revoked-at
          now)

         (dissoc
          :invitation/accepted-by
          :invitation/accepted-at)

         (model.common/bump-revision
          :invitation/revision
          :invitation/updated-at
          now))}))

(defn expire-invitation-doc
  [invitation now]
  (cond
    (not
     (pending? invitation))
    {:ok? false
     :error
     (terminal-error invitation)}

    (not
     (valid-change-time?
      invitation
      now))
    {:ok? false
     :error
     :invitation/invalid-time}

    (not
     (expired-at?
      invitation
      now))
    {:ok? false
     :error
     :invitation/not-expired}

    :else
    {:ok? true

     :invitation
     (-> invitation
         (assoc
          :invitation/status
          :expired)

         (dissoc
          :invitation/accepted-by
          :invitation/accepted-at
          :invitation/revoked-at)

         (model.common/bump-revision
          :invitation/revision
          :invitation/updated-at
          now))}))

;; =============================================================================
;; Version descriptions
;; =============================================================================

(defn expected-version
  [invitation]
  {:invitation/id
   (:xt/id invitation)

   :invitation/revision
   (:invitation/revision invitation)

   :invitation/status
   (:invitation/status invitation)

   :invitation/updated-at
   (:invitation/updated-at invitation)})

;; =============================================================================
;; Public invitation description
;; =============================================================================

(def model
  {:entity-type
   entity-type

   :limits
   {:email
    email-max

    :token-hash-min
    token-hash-min

    :token-hash-max
    token-hash-max}

   :statuses
   status-order

   :active-statuses
   active-statuses

   :terminal-statuses
   terminal-statuses

   :allowed-transitions
   allowed-transitions})
