(ns net.humanhelp.site.model.user.identity
  "Pure domain rules for the canonical HumanHelp user identity.

   A user is a durable identity. Being signed in is session state, not a field
   on the user document.

   A customer is not a user status or role. Customer behavior is derived by
   Graph from the absence of applicable organization memberships and role
   assignments.

   This namespace owns only the user document itself. Organization membership,
   staff roles, invitations, and request capabilities live in their respective
   user-model namespaces."
  (:require
   [clojure.string :as str]
   [net.humanhelp.schema.common :as common]
   [tick.core :as tick])
  (:import
   [java.time ZonedDateTime]
   [java.util UUID]))

;; =============================================================================
;; Identity and limits
;; =============================================================================

(def entity-type
  :user)

(def email-max
  320)

(def display-name-max
  120)

(def phone-digits-count
  10)

(def phone-display-max
  20)

;; =============================================================================
;; Account lifecycle
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
   "The user account is not active."

   :user/not-suspendable
   "The user account cannot be suspended from its current state."

   :user/not-reactivatable
   "The user account cannot be reactivated from its current state."

   :user/not-deletable
   "The user account cannot be deleted from its current state."

   :user/deleted
   "The user account has been deleted."})

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
       "The user account could not be updated."))

(defn without-nils
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- trim-to-nil
  [value]
  (if (string? value)
    (let [value (str/trim value)]
      (when-not (str/blank? value)
        value))
    value))

(defn- zdt<=
  [a b]
  (and (zdt-value? a)
       (zdt-value? b)
       (not (.isAfter ^ZonedDateTime a
                      ^ZonedDateTime b))))

(defn- optional-between?
  [start value end]
  (or (nil? value)
      (and (zdt<= start value)
           (zdt<= value end))))

(defn valid-change-time?
  [user now]
  (and (zdt-value? now)
       (zdt<= (:user/joined-at user)
              now)
       (zdt<= (:user/updated-at user)
              now)))

;; =============================================================================
;; Phone and profile normalization
;; =============================================================================

(defn normalize-phone
  "Normalizes a US phone number to ten decimal digits.

   Formatting characters are discarded. An eleven-digit number beginning with
   1 is normalized by removing the country-code digit. Other lengths remain
   unchanged so input validation can report an error."
  [value]
  (when-some [value (trim-to-nil value)]
    (if-not (string? value)
      value
      (let [digits
            (str/replace value
                         #"[^0-9]"
                         "")]
        (if (and (= 11 (count digits))
                 (str/starts-with? digits "1"))
          (subs digits 1)
          digits)))))

(defn valid-phone?
  [value]
  (and (string? value)
       (= phone-digits-count
          (count value))
       (boolean
        (re-matches #"[0-9]{10}"
                    value))))

(defn phone-display
  "Formats a canonical ten-digit phone number as 555-555-5555."
  [phone]
  (when (valid-phone? phone)
    (str (subs phone 0 3)
         "-"
         (subs phone 3 6)
         "-"
         (subs phone 6 10))))

(defn normalize-email
  [value]
  (trim-to-nil value))

(defn normalize-display-name
  [value]
  (trim-to-nil value))

(defn normalize-create-input
  [input]
  (let [input
        (or input {})

        phone
        (normalize-phone
         (:phone input))

        supplied-phone-display
        (trim-to-nil
         (:phone-display input))]
    (-> input
        (assoc :phone phone
               :email
               (normalize-email
                (:email input))
               :display-name
               (normalize-display-name
                (:display-name input))
               :phone-display
               (or supplied-phone-display
                   (phone-display phone))))))

(defn normalize-profile-input
  [input]
  (-> (or input {})
      (update :display-name
              normalize-display-name)))

;; =============================================================================
;; Domain predicates
;; =============================================================================

(defn status?
  [value]
  (contains? statuses value))

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
  (contains? terminal-statuses
             (:user/status user)))

(defn profile-editable?
  [user]
  (active? user))

(defn next-status
  [user action]
  (get allowed-transitions
       [(:user/status user)
        action]))

(defn can-transition?
  [user action]
  (some?
   (next-status user action)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn contact-consistent?
  [{:user/keys
    [email
     phone
     phone-display
     phone-verified-at]}]
  (and
   ;; Every user must have at least one durable means of identification.
   (or (some? email)
       (some? phone))

   ;; Phone-derived fields cannot exist without a phone.
   (or (nil? phone-display)
       (some? phone))

   (or (nil? phone-verified-at)
       (some? phone))))

(defn lifecycle-times-consistent?
  [{:user/keys
    [joined-at
     updated-at
     phone-verified-at
     suspended-at
     deleted-at]}]
  (and
   (zdt<= joined-at
          updated-at)

   (every?
    #(optional-between?
      joined-at
      %
      updated-at)
    [phone-verified-at
     suspended-at
     deleted-at])

   (or (nil? suspended-at)
       (nil? deleted-at)
       (zdt<= suspended-at
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
     (and (nil? suspended-at)
          (nil? deleted-at))

     :suspended
     (and (some? suspended-at)
          (nil? deleted-at))

     :deleted
     (some? deleted-at)

     false)))

(defn document-consistent?
  [user]
  (and
   (uuid-value?
    (:xt/id user))

   (contact-consistent? user)
   (lifecycle-consistent? user)))

;; =============================================================================
;; Input validation
;; =============================================================================

(defn create-input-errors
  [input]
  (let [{:keys
         [id
          email
          phone
          phone-display
          display-name
          phone-verified-at
          now]}
        (normalize-create-input input)]
    (cond-> {}
      (not (uuid-value? id))
      (assoc
       :id
       "A user UUID is required.")

      (and (nil? email)
           (nil? phone))
      (assoc
       :contact
       "A phone number or email address is required.")

      (and (some? email)
           (not (string? email)))
      (assoc
       :email
       "Enter a valid email address.")

      (and (string? email)
           (> (count email)
              email-max))
      (assoc
       :email
       (str "Use "
            email-max
            " characters or fewer."))

      (and (some? phone)
           (not (valid-phone? phone)))
      (assoc
       :phone
       "Enter a valid ten-digit US phone number.")

      (and (some? phone-display)
           (not (string? phone-display)))
      (assoc
       :phone-display
       "Enter a valid display phone number.")

      (and (string? phone-display)
           (> (count phone-display)
              phone-display-max))
      (assoc
       :phone-display
       (str "Use "
            phone-display-max
            " characters or fewer."))

      (and (some? display-name)
           (not (string? display-name)))
      (assoc
       :display-name
       "Enter a valid display name.")

      (and (string? display-name)
           (> (count display-name)
              display-name-max))
      (assoc
       :display-name
       (str "Use "
            display-name-max
            " characters or fewer."))

      (and (some? phone-verified-at)
           (nil? phone))
      (assoc
       :phone-verified-at
       "A phone verification time requires a phone number.")

      (and (some? phone-verified-at)
           (not (zdt-value? phone-verified-at)))
      (assoc
       :phone-verified-at
       "A valid phone verification time is required.")

      (not (zdt-value? now))
      (assoc
       :now
       "A valid account creation time is required.")

      (and (zdt-value? phone-verified-at)
           (zdt-value? now)
           (not (zdt<= phone-verified-at
                       now)))
      (assoc
       :phone-verified-at
       "The phone verification time cannot be in the future."))))

(defn profile-input-errors
  [input]
  (let [{:keys [display-name]}
        (normalize-profile-input input)]
    (cond-> {}
      (and (some? display-name)
           (not (string? display-name)))
      (assoc
       :display-name
       "Enter a valid display name.")

      (and (string? display-name)
           (> (count display-name)
              display-name-max))
      (assoc
       :display-name
       (str "Use "
            display-name-max
            " characters or fewer.")))))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

(defn valid-profile-input?
  [input]
  (empty?
   (profile-input-errors input)))

(defn- throw-invalid!
  [message errors input]
  (throw
   (ex-info
    message
    {:error/type :user/invalid-input
     :errors errors
     :input input})))

;; =============================================================================
;; User construction
;; =============================================================================

(defn new-user
  [{:keys
    [id
     email
     phone
     phone-display
     display-name
     phone-verified-at
     now]
    :as input}]
  (let [{:keys
         [email
          phone
          phone-display
          display-name]
        :as normalized}
        (normalize-create-input input)

        errors
        (create-input-errors normalized)]
    (when (seq errors)
      (throw-invalid!
       "Cannot create user."
       errors
       input))

    (cond->
     {:xt/id id
      :user/status :active
      :user/revision 0
      :user/joined-at now
      :user/updated-at now}

      email
      (assoc
       :user/email email)

      phone
      (assoc
       :user/phone phone)

      phone-display
      (assoc
       :user/phone-display phone-display)

      display-name
      (assoc
       :user/display-name display-name)

      phone-verified-at
      (assoc
       :user/phone-verified-at phone-verified-at))))

(defn new-verified-phone-user
  "Creates a new active user whose phone number was verified at now."
  [{:keys [now]
    :as input}]
  (new-user
   (assoc input
          :phone-verified-at now)))

;; =============================================================================
;; User updates
;; =============================================================================

(defn- bump-revision
  [user now]
  (-> user
      (update
       :user/revision
       (fnil inc 0))
      (assoc
       :user/updated-at now)))

(defn edit-profile-doc
  "Updates the user's optional display name.

   Phone and email changes are deliberately excluded. Changing authentication
   identifiers requires a separate verified workflow."
  [user input now]
  (let [{:keys [display-name]
         :as input}
        (normalize-profile-input input)

        errors
        (profile-input-errors input)]
    (cond
      (not (profile-editable? user))
      {:ok? false
       :error
       (if (deleted? user)
         :user/deleted
         :user/not-active)}

      (seq errors)
      {:ok? false
       :error :user/invalid-input
       :errors errors}

      (not (valid-change-time? user now))
      {:ok? false
       :error :user/invalid-time}

      :else
      {:ok? true
       :user
       (cond->
        (-> user
            (dissoc
             :user/display-name)
            (bump-revision now))

         display-name
         (assoc
          :user/display-name
          display-name))})))

;; =============================================================================
;; Account lifecycle transitions
;; =============================================================================

(defn- transition-error
  [action]
  (case action
    :suspend
    :user/not-suspendable

    :reactivate
    :user/not-reactivatable

    :delete
    :user/not-deletable

    :user/not-active))

(defn transition-user
  [user action now]
  (let [status'
        (next-status user action)]
    (cond
      (nil? status')
      {:ok? false
       :error
       (transition-error action)}

      (not (valid-change-time? user now))
      {:ok? false
       :error :user/invalid-time}

      :else
      {:ok? true
       :user
       (case action
         :suspend
         (-> user
             (assoc
              :user/status status'
              :user/suspended-at now)
             (dissoc
              :user/deleted-at)
             (bump-revision now))

         :reactivate
         (-> user
             (assoc
              :user/status status')
             (dissoc
              :user/suspended-at
              :user/deleted-at)
             (bump-revision now))

         :delete
         (-> user
             (assoc
              :user/status status'
              :user/deleted-at now)
             (bump-revision now)))})))

(defn suspend-user-doc
  [user now]
  (transition-user
   user
   :suspend
   now))

(defn reactivate-user-doc
  [user now]
  (transition-user
   user
   :reactivate
   now))

(defn delete-user-doc
  [user now]
  (transition-user
   user
   :delete
   now))

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
;; Public identity description
;; =============================================================================

(def model
  {:entity-type entity-type

   :limits
   {:email email-max
    :display-name display-name-max
    :phone-digits phone-digits-count
    :phone-display phone-display-max}

   :statuses status-order

   :active-statuses active-statuses

   :terminal-statuses terminal-statuses

   :allowed-transitions allowed-transitions})
