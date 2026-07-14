(ns net.humanhelp.site.model.user.identity
  "Pure domain rules for HumanHelp user identities.

   A user is the canonical identity used by customers and staff. Staff
   authority is attached separately through organization memberships and role
   assignments.

   A user may be identified by phone, email, or both. Phone-authenticated users
   record when their phone number was verified.

   This namespace performs no database reads, persistence, authorization, or
   external verification."
  (:require
   [clojure.string :as str]
   [net.humanhelp.site.model.common :as model.common]
   [tick.core :as tick]))

;; =============================================================================
;; Identity and limits
;; =============================================================================

(def entity-type
  :user)

(def email-max
  320)

(def display-name-max
  80)

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def status-order
  [:active
   :suspended
   :deleted])

(def statuses
  (set status-order))

(def active-statuses
  #{:active})

(def terminal-statuses
  #{:deleted})

(def allowed-transitions
  {[:active :suspend]
   :suspended

   [:suspended :reactivate]
   :active

   [:active :delete]
   :deleted

   [:suspended :delete]
   :deleted})

(def action-error-messages
  {:user/invalid-input
   "Some user information needs to be corrected."

   :user/invalid-time
   "The user could not be changed because its timestamp was invalid."

   :user/not-active
   "The user is not active."

   :user/not-suspended
   "The user is not suspended."

   :user/deleted
   "The user has been deleted."

   :user/not-suspendable
   "The user cannot be suspended from its current state."

   :user/not-reactivatable
   "The user cannot be reactivated from its current state."

   :user/not-deletable
   "The user cannot be deleted from its current state."

   :user/profile-not-editable
   "The user's profile cannot be edited from its current state."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn error-message
  [error]
  (get action-error-messages
       error
       "The user could not be updated."))

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
  [user now]
  (model.common/valid-change-time?
   user
   :user/joined-at
   :user/updated-at
   now))

;; =============================================================================
;; Contact normalization
;; =============================================================================

(defn normalize-phone
  "Normalizes a US phone number to ten digits.

   An eleven-digit number beginning with country code 1 is accepted. Other
   lengths return nil."
  [value]
  (when-some [value
              (trim-to-nil value)]
    (let [digits
          (str/replace value
                       #"\D"
                       "")

          digits
          (if
           (and
            (= 11
               (count digits))

            (str/starts-with?
             digits
             "1"))

            (subs digits 1)

            digits)]
      (when
       (= 10
          (count digits))
        digits))))

(defn valid-phone?
  [value]
  (some?
   (normalize-phone value)))

(defn phone-display
  "Formats a normalized ten-digit phone number as 555-555-5555."
  [value]
  (when-some [phone
              (normalize-phone value)]
    (str
     (subs phone 0 3)
     "-"
     (subs phone 3 6)
     "-"
     (subs phone 6 10))))

(defn normalize-email
  [value]
  (some-> value
          trim-to-nil
          str/lower-case))

(defn valid-email?
  [value]
  (let [email
        (normalize-email value)]
    (and
     (string? email)

     (<=
      (count email)
      email-max)

     (boolean
      (re-matches
       #"^[^@\s]+@[^@\s]+\.[^@\s]+$"
       email)))))

(defn normalize-display-name
  [value]
  (trim-to-nil value))

;; =============================================================================
;; Input normalization
;; =============================================================================

(defn normalize-create-input
  [input]
  (let [input
        (or input {})]
    (cond-> input
      (contains? input :phone)
      (update
       :phone
       normalize-phone)

      (contains? input :email)
      (update
       :email
       normalize-email)

      (contains? input :display-name)
      (update
       :display-name
       normalize-display-name))))

(defn normalize-profile-input
  [input]
  (let [input
        (or input {})]
    (cond-> input
      (contains? input :display-name)
      (update
       :display-name
       normalize-display-name))))

;; =============================================================================
;; Domain predicates
;; =============================================================================

(defn status?
  [value]
  (contains?
   statuses
   value))

(defn active?
  [user]
  (= :active
     (:user/status user)))

(defn suspended?
  [user]
  (= :suspended
     (:user/status user)))

(defn deleted?
  [user]
  (= :deleted
     (:user/status user)))

(defn terminal?
  [user]
  (contains?
   terminal-statuses
   (:user/status user)))

(defn phone-verified?
  [user]
  (some?
   (:user/phone-verified-at user)))

(defn has-phone?
  [user]
  (some?
   (:user/phone user)))

(defn has-email?
  [user]
  (some?
   (:user/email user)))

(defn has-contact-method?
  [user]
  (or
   (has-phone? user)
   (has-email? user)))

(defn next-status
  [user action]
  (get
   allowed-transitions
   [(:user/status user)
    action]))

(defn can-transition?
  [user action]
  (some?
   (next-status user action)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn contact-methods-consistent?
  [{:user/keys
    [phone
     phone-display
     phone-verified-at
     email]}]
  (and
   ;; A user must have at least one durable identity value.
   (or
    (some? phone)
    (some? email))

   ;; Phone display and verification cannot exist without a phone.
   (if
    (some? phone)

     (and
      (valid-phone? phone)

      (= phone
         (normalize-phone phone))

      (= phone-display
         (phone-display phone)))

     (and
      (nil? phone-display)
      (nil? phone-verified-at)))

   (or
    (nil? email)
    (and
     (valid-email? email)

     (= email
        (normalize-email email))))))

(defn profile-consistent?
  [{:user/keys
    [display-name]}]
  (or
   (nil? display-name)

   (and
    (string? display-name)

    (= display-name
       (normalize-display-name display-name))

    (<=
     (count display-name)
     display-name-max))))

(defn lifecycle-times-consistent?
  [{:user/keys
    [joined-at
     updated-at
     phone-verified-at
     suspended-at
     deleted-at]}]
  (and
   (model.common/timestamp<=
    joined-at
    updated-at)

   (every?
    #(model.common/optional-between?
      joined-at
      %
      updated-at)
    [phone-verified-at
     suspended-at
     deleted-at])

   ;; A user deleted while suspended retains suspended-at.
   (or
    (nil? suspended-at)
    (nil? deleted-at)
    (model.common/timestamp<=
     suspended-at
     deleted-at))))

(defn lifecycle-consistent?
  [{:user/keys
    [status
     suspended-at
     deleted-at]
    :as user}]
  (and
   (status? status)
   (lifecycle-times-consistent? user)

   (case status
     :active
     (and
      (nil? suspended-at)
      (nil? deleted-at))

     :suspended
     (and
      (some? suspended-at)
      (nil? deleted-at))

     :deleted
     (some? deleted-at)

     false)))

(defn document-consistent?
  [user]
  (and
   (uuid?
    (:xt/id user))

   (nat-int?
    (:user/revision user))

   (tick/zoned-date-time?
    (:user/joined-at user))

   (tick/zoned-date-time?
    (:user/updated-at user))

   (or
    (nil?
     (:user/phone-verified-at user))

    (tick/zoned-date-time?
     (:user/phone-verified-at user)))

   (or
    (nil?
     (:user/suspended-at user))

    (tick/zoned-date-time?
     (:user/suspended-at user)))

   (or
    (nil?
     (:user/deleted-at user))

    (tick/zoned-date-time?
     (:user/deleted-at user)))

   (contact-methods-consistent? user)
   (profile-consistent? user)
   (lifecycle-consistent? user)))

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
          phone
          email
          display-name
          phone-verified-at
          now]}
        (normalize-create-input raw-input)]
    (cond-> {}
      (not
       (uuid? id))
      (assoc
       :id
       "A user UUID is required.")

      (and
       (nil? phone)
       (nil? email))
      (assoc
       :contact
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
        (valid-email? raw-email)))
      (assoc
       :email
       "Enter a valid email address.")

      (and
       (some? display-name)
       (> (count display-name)
          display-name-max))
      (assoc
       :display-name
       (str
        "Use "
        display-name-max
        " characters or fewer."))

      (not
       (tick/zoned-date-time? now))
      (assoc
       :now
       "A valid user creation time is required.")

      (and
       (some? phone-verified-at)
       (not
        (tick/zoned-date-time?
         phone-verified-at)))
      (assoc
       :phone-verified-at
       "A valid phone verification time is required.")

      (and
       (some? phone-verified-at)
       (nil? phone))
      (assoc
       :phone-verified-at
       "A phone cannot be verified when no phone number is present.")

      (and
       (tick/zoned-date-time? phone-verified-at)
       (tick/zoned-date-time? now)
       (not
        (model.common/timestamp<=
         phone-verified-at
         now)))
      (assoc
       :phone-verified-at
       "The phone verification time cannot be after user creation."))))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

;; =============================================================================
;; Profile validation
;; =============================================================================

(defn profile-input-errors
  [input]
  (let [raw-input
        (or input {})

        {:keys
         [display-name]}
        (normalize-profile-input raw-input)]
    (cond-> {}
      (not
       (contains?
        raw-input
        :display-name))
      (assoc
       :display-name
       "Provide a display name to update.")

      (and
       (some? display-name)
       (> (count display-name)
          display-name-max))
      (assoc
       :display-name
       (str
        "Use "
        display-name-max
        " characters or fewer.")))))

(defn valid-profile-input?
  [input]
  (empty?
   (profile-input-errors input)))

;; =============================================================================
;; User construction
;; =============================================================================

(defn new-user
  [{:keys
    [id
     phone
     email
     display-name
     phone-verified-at
     now]
    :as input}]
  (let [{:keys
         [phone
          email
          display-name]
        :as normalized}
        (normalize-create-input input)

        errors
        (create-input-errors normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :user/invalid-input
       "Cannot create user."
       errors
       input))

    (cond->
     {:xt/id id
      :user/status :active
      :user/revision 0
      :user/joined-at now
      :user/updated-at now}

      phone
      (assoc
       :user/phone
       phone

       :user/phone-display
       (phone-display phone))

      email
      (assoc
       :user/email
       email)

      display-name
      (assoc
       :user/display-name
       display-name)

      phone-verified-at
      (assoc
       :user/phone-verified-at
       phone-verified-at))))

(defn new-verified-phone-user
  "Creates a user whose phone was verified at creation time."
  [{:keys [now]
    :as input}]
  (new-user
   (assoc input
          :phone-verified-at
          now)))

;; =============================================================================
;; Profile updates
;; =============================================================================

(defn edit-profile-doc
  [user input now]
  (let [input
        (normalize-profile-input input)

        errors
        (profile-input-errors input)]
    (cond
      (deleted? user)
      {:ok? false
       :error :user/deleted}

      (not
       (active? user))
      {:ok? false
       :error :user/profile-not-editable}

      (not
       (valid-change-time? user now))
      {:ok? false
       :error :user/invalid-time}

      (seq errors)
      {:ok? false
       :error :user/invalid-input
       :errors errors}

      :else
      {:ok? true
       :user
       (-> user
           ((fn [user]
              (if-some [display-name
                        (:display-name input)]

                (assoc
                 user
                 :user/display-name
                 display-name)

                (dissoc
                 user
                 :user/display-name))))
           (model.common/bump-revision
            :user/revision
            :user/updated-at
            now))})))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn- transition-error
  [user action]
  (cond
    (deleted? user)
    :user/deleted

    (= action :suspend)
    :user/not-suspendable

    (= action :reactivate)
    :user/not-reactivatable

    (= action :delete)
    :user/not-deletable

    :else
    :user/invalid-input))

(defn transition-user
  [user action now]
  (cond
    (not
     (valid-change-time? user now))
    {:ok? false
     :error :user/invalid-time}

    (not
     (can-transition? user action))
    {:ok? false
     :error
     (transition-error
      user
      action)}

    :else
    {:ok? true
     :user
     (-> (case action
           :suspend
           (-> user
               (assoc
                :user/status
                :suspended

                :user/suspended-at
                now)
               (dissoc
                :user/deleted-at))

           :reactivate
           (-> user
               (assoc
                :user/status
                :active)
               (dissoc
                :user/suspended-at
                :user/deleted-at))

           :delete
           (-> user
               (assoc
                :user/status
                :deleted

                :user/deleted-at
                now))

           user)

         (model.common/bump-revision
          :user/revision
          :user/updated-at
          now))}))

;; =============================================================================
;; Version descriptions
;; =============================================================================

(defn expected-version
  [user]
  {:user/id
   (:xt/id user)

   :user/revision
   (:user/revision user)

   :user/status
   (:user/status user)

   :user/updated-at
   (:user/updated-at user)})

;; =============================================================================
;; Public model description
;; =============================================================================

(def model
  {:entity-type
   entity-type

   :limits
   {:email
    email-max

    :display-name
    display-name-max}

   :statuses
   status-order

   :active-statuses
   active-statuses

   :terminal-statuses
   terminal-statuses

   :allowed-transitions
   allowed-transitions})
