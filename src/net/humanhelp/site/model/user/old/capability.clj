(ns net.humanhelp.site.model.user.capability
  "Pure domain rules for request-access capabilities.

   A request capability grants temporary bearer access to exactly one request.
   It is primarily used when the request owner does not have a signed-in user
   identity.

   A capability may optionally be associated with a user, but access is
   granted by possession of the bearer token rather than by that association.

   Only a cryptographic hash of the bearer token is persisted. Raw tokens,
   token generation, constant-time verification, authorization, and persistence
   belong outside this namespace.

   Revocation and expiration are terminal. Restoring access requires creation
   of a replacement capability."
  (:require
   [net.humanhelp.schema.common :as common]
   [net.humanhelp.site.model.common :as model.common]
   [tick.core :as tick]))

;; =============================================================================
;; Identity and limits
;; =============================================================================

(def entity-type
  :request-capability)

(def token-hash-min
  32)

(def token-hash-max
  256)

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def status-order
  [:active
   :revoked
   :expired])

(def statuses
  (set status-order))

(def active-statuses
  #{:active})

(def terminal-statuses
  #{:revoked
    :expired})

(def allowed-transitions
  {[:active :revoke]
   :revoked

   [:active :expire]
   :expired})

(def action-error-messages
  {:request-capability/invalid-input
   "Some request-access information needs to be corrected."

   :request-capability/invalid-time
   "Request access could not be changed because its timestamp was invalid."

   :request-capability/not-active
   "Request access is no longer active."

   :request-capability/not-revocable
   "Request access cannot be revoked from its current state."

   :request-capability/not-expired
   "Request access has not expired."

   :request-capability/revoked
   "Request access has been revoked."

   :request-capability/expired
   "Request access has expired."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn error-message
  [error]
  (get action-error-messages
       error
       "Request access could not be updated."))

(defn valid-change-time?
  [capability now]
  (model.common/valid-change-time?
   capability
   :request-capability/created-at
   :request-capability/updated-at
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
;; Domain predicates
;; =============================================================================

(defn status?
  [value]
  (contains?
   statuses
   value))

(defn active?
  [capability]
  (= :active
     (:request-capability/status capability)))

(defn revoked?
  [capability]
  (= :revoked
     (:request-capability/status capability)))

(defn expired-status?
  [capability]
  (= :expired
     (:request-capability/status capability)))

(defn terminal?
  [capability]
  (contains?
   terminal-statuses
   (:request-capability/status capability)))

(defn guest?
  "Returns true when the capability is not associated with a user identity."
  [capability]
  (nil?
   (:request-capability/user capability)))

(defn associated-with-user?
  [capability]
  (some?
   (:request-capability/user capability)))

(defn belongs-to-request?
  [capability request-id]
  (and
   (uuid? request-id)

   (= request-id
      (:request-capability/request capability))))

(defn belongs-to-user?
  [capability user-id]
  (and
   (uuid? user-id)

   (= user-id
      (:request-capability/user capability))))

(defn expired-at?
  "Returns true when now is at or after the capability expiration time."
  [capability now]
  (and
   (tick/zoned-date-time?
    now)

   (tick/zoned-date-time?
    (:request-capability/expires-at capability))

   (not
    (model.common/timestamp<
     now
     (:request-capability/expires-at capability)))))

(defn usable-at?
  "Returns true when the capability is active and now falls within its usable
   lifetime."
  [capability now]
  (and
   (active? capability)

   (tick/zoned-date-time?
    now)

   (model.common/timestamp<=
    (:request-capability/created-at capability)
    now)

   (not
    (expired-at?
     capability
     now))))

(defn next-status
  [capability action]
  (get
   allowed-transitions
   [(:request-capability/status capability)
    action]))

(defn can-transition?
  [capability action]
  (some?
   (next-status capability action)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn lifecycle-times-consistent?
  [{:request-capability/keys
    [created-at
     updated-at
     expires-at
     last-used-at
     revoked-at]}]
  (and
   (model.common/timestamp<=
    created-at
    updated-at)

   ;; A capability must have a non-empty usable lifetime.
   (model.common/timestamp<
    created-at
    expires-at)

   (model.common/optional-between?
    created-at
    last-used-at
    updated-at)

   (model.common/optional-between?
    created-at
    revoked-at
    updated-at)

   ;; Successful use and revocation must occur before expiration.
   (or
    (nil? last-used-at)

    (model.common/timestamp<
     last-used-at
     expires-at))

   (or
    (nil? revoked-at)

    (model.common/timestamp<
     revoked-at
     expires-at))))

(defn lifecycle-consistent?
  [{:request-capability/keys
    [status
     updated-at
     expires-at
     revoked-at]
    :as capability}]
  (and
   (status? status)
   (lifecycle-times-consistent? capability)

   (case status
     :active
     (and
      (nil? revoked-at)

      ;; An active document records only mutations made before expiration.
      ;; Passing wall-clock time does not mutate the persisted status.
      (model.common/timestamp<
       updated-at
       expires-at))

     :revoked
     (some? revoked-at)

     :expired
     (and
      (nil? revoked-at)

      ;; Explicitly materializing expiration updates the document at or after
      ;; its expiration time.
      (model.common/timestamp<=
       expires-at
       updated-at))

     false)))

(defn document-consistent?
  [capability]
  (and
   (uuid?
    (:xt/id capability))

   (uuid?
    (:request-capability/request capability))

   (or
    (nil?
     (:request-capability/user capability))

    (uuid?
     (:request-capability/user capability)))

   (valid-token-hash?
    (:request-capability/token-hash capability))

   (nat-int?
    (:request-capability/revision capability))

   (tick/zoned-date-time?
    (:request-capability/created-at capability))

   (tick/zoned-date-time?
    (:request-capability/updated-at capability))

   (tick/zoned-date-time?
    (:request-capability/expires-at capability))

   (or
    (nil?
     (:request-capability/last-used-at capability))

    (tick/zoned-date-time?
     (:request-capability/last-used-at capability)))

   (or
    (nil?
     (:request-capability/revoked-at capability))

    (tick/zoned-date-time?
     (:request-capability/revoked-at capability)))

   (lifecycle-consistent?
    capability)))

;; =============================================================================
;; Creation validation
;; =============================================================================

(defn create-input-errors
  [{:keys
    [id
     request-id
     user-id
     token-hash
     now
     expires-at]}]
  (cond-> {}
    (not
     (uuid? id))
    (assoc
     :id
     "A request-capability UUID is required.")

    (not
     (uuid? request-id))
    (assoc
     :request-id
     "A valid request UUID is required.")

    (and
     (some? user-id)
     (not
      (uuid? user-id)))
    (assoc
     :user-id
     "A valid user UUID is required.")

    (not
     (valid-token-hash?
      token-hash))
    (assoc
     :token-hash
     "A valid request-capability token hash is required.")

    (not
     (tick/zoned-date-time?
      now))
    (assoc
     :now
     "A valid capability creation time is required.")

    (not
     (tick/zoned-date-time?
      expires-at))
    (assoc
     :expires-at
     "A valid capability expiration time is required.")

    (and
     (tick/zoned-date-time?
      now)

     (tick/zoned-date-time?
      expires-at)

     (not
      (model.common/timestamp<
       now
       expires-at)))
    (assoc
     :expires-at
     "The capability expiration time must be after its creation time.")))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

;; =============================================================================
;; Capability construction
;; =============================================================================

(defn new-capability
  [{:keys
    [id
     request-id
     user-id
     token-hash
     now
     expires-at]
    :as input}]
  (let [errors
        (create-input-errors
         input)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :request-capability/invalid-input
       "Cannot create request capability."
       errors
       input))

    (cond->
     {:xt/id
      id

      :request-capability/request
      request-id

      :request-capability/token-hash
      token-hash

      :request-capability/status
      :active

      :request-capability/revision
      0

      :request-capability/created-at
      now

      :request-capability/updated-at
      now

      :request-capability/expires-at
      expires-at}

      user-id
      (assoc
       :request-capability/user
       user-id))))

;; =============================================================================
;; Successful-use recording
;; =============================================================================

(defn- terminal-error
  [capability]
  (case
   (:request-capability/status capability)

    :revoked
    :request-capability/revoked

    :expired
    :request-capability/expired

    :request-capability/not-active))

(defn record-use-doc
  "Records a successful use of the capability.

   Calling code must invoke this only after securely verifying the presented
   bearer token. Whether every use should be persisted or sampled is an
   application policy."
  [capability now]
  (cond
    (not
     (active? capability))
    {:ok? false
     :error
     (terminal-error capability)}

    (not
     (valid-change-time?
      capability
      now))
    {:ok? false
     :error
     :request-capability/invalid-time}

    (expired-at?
     capability
     now)
    {:ok? false
     :error
     :request-capability/expired}

    :else
    {:ok? true

     :request-capability
     (-> capability
         (assoc
          :request-capability/last-used-at
          now)

         (model.common/bump-revision
          :request-capability/revision
          :request-capability/updated-at
          now))}))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn revoke-capability-doc
  [capability now]
  (cond
    (not
     (active? capability))
    {:ok? false
     :error
     (terminal-error capability)}

    (not
     (valid-change-time?
      capability
      now))
    {:ok? false
     :error
     :request-capability/invalid-time}

    (expired-at?
     capability
     now)
    {:ok? false
     :error
     :request-capability/expired}

    (not
     (can-transition?
      capability
      :revoke))
    {:ok? false
     :error
     :request-capability/not-revocable}

    :else
    {:ok? true

     :request-capability
     (-> capability
         (assoc
          :request-capability/status
          :revoked

          :request-capability/revoked-at
          now)

         (model.common/bump-revision
          :request-capability/revision
          :request-capability/updated-at
          now))}))

(defn expire-capability-doc
  [capability now]
  (cond
    (not
     (active? capability))
    {:ok? false
     :error
     (terminal-error capability)}

    (not
     (valid-change-time?
      capability
      now))
    {:ok? false
     :error
     :request-capability/invalid-time}

    (not
     (expired-at?
      capability
      now))
    {:ok? false
     :error
     :request-capability/not-expired}

    (not
     (can-transition?
      capability
      :expire))
    {:ok? false
     :error
     :request-capability/not-active}

    :else
    {:ok? true

     :request-capability
     (-> capability
         (assoc
          :request-capability/status
          :expired)

         (dissoc
          :request-capability/revoked-at)

         (model.common/bump-revision
          :request-capability/revision
          :request-capability/updated-at
          now))}))

;; =============================================================================
;; Version descriptions
;; =============================================================================

(defn expected-version
  [capability]
  {:request-capability/id
   (:xt/id capability)

   :request-capability/revision
   (:request-capability/revision capability)

   :request-capability/status
   (:request-capability/status capability)

   :request-capability/updated-at
   (:request-capability/updated-at capability)})

;; =============================================================================
;; Public model description
;; =============================================================================

(def model
  {:entity-type
   entity-type

   :limits
   {:token-hash-min
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
