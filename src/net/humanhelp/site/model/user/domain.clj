(ns net.humanhelp.site.model.user.domain
  "Pure rules for global HumanHelp User identity.

   User owns global identity, optional contact/profile facts, contact
   verification state, account lifecycle, and canonical gesso.model commands.

   A User does not require a phone number or email address to exist.
   Authentication and signup flows may impose stronger requirements without
   making a particular authentication mechanism intrinsic to User.

   This namespace contains no persistence reads, uniqueness queries,
   transaction execution, Organization or Membership knowledge, invitations,
   roles, scopes, or authorization."
  (:require
   [clojure.string :as str]
   [gesso.model.command :as command])
  (:import
   [java.time Instant]
   [java.util Locale]))

;; =============================================================================
;; Identity and versioning
;; =============================================================================

(def entity-type :user)

(def version
  {:revision-key :user/revision
   :created-at-key :user/created-at
   :updated-at-key :user/updated-at})

;; =============================================================================
;; Contact and profile values
;; =============================================================================

(def ^:private phone-pattern
  #"^\+[1-9][0-9]{7,14}$")

(def ^:private email-max 254)

(def ^:private email-pattern
  #"^[^\s@]+@[^\s@]+\.[^\s@]+$")

(def display-name-max 120)

(defn normalize-phone
  "Canonicalizes an already-parsed phone string.

   Friendly/local formatting belongs outside User. Blank strings normalize to
   nil; non-string values are left unchanged so validation can reject them."
  [value]
  (cond
    (nil? value)
    nil

    (string? value)
    (let [value (str/trim value)]
      (when-not (str/blank? value)
        value))

    :else
    value))

(defn phone?
  [value]
  (and (string? value)
       (= value (normalize-phone value))
       (boolean (re-matches phone-pattern value))))

(defn normalize-email
  "Canonicalizes a HumanHelp email string case-insensitively.

   Blank strings normalize to nil; non-string values are left unchanged so
   validation can reject them."
  [value]
  (cond
    (nil? value)
    nil

    (string? value)
    (let [value (.toLowerCase ^String (str/trim value) Locale/ROOT)]
      (when-not (str/blank? value)
        value))

    :else
    value))

(defn email?
  [value]
  (and (string? value)
       (= value (normalize-email value))
       (<= (count value) email-max)
       (boolean (re-matches email-pattern value))))

(defn normalize-display-name
  "Trims a display-name string.

   nil means no display name. Unlike optional phone/email input, a blank string
   remains blank so callers cannot accidentally turn invalid input into an
   explicit request to remove the display name."
  [value]
  (cond
    (nil? value) nil
    (string? value) (str/trim value)
    :else value))

(defn display-name?
  [value]
  (and (string? value)
       (= value (normalize-display-name value))
       (not (str/blank? value))
       (<= (count value) display-name-max)))

;; =============================================================================
;; Lifecycle and document facts
;; =============================================================================

(def ^:private statuses
  #{:active
    :suspended
    :deleted})

(def ^:private allowed-transitions
  {[:active :suspend] :suspended
   [:suspended :reactivate] :active
   [:active :delete] :deleted
   [:suspended :delete] :deleted})

(defn status?
  [value]
  (contains? statuses value))

(defn user-id [user] (:xt/id user))
(defn user-phone [user] (:user/phone user))
(defn user-email [user] (:user/email user))
(defn user-display-name [user] (:user/display-name user))
(defn user-status [user] (:user/status user))

(defn active?
  [user]
  (= :active (user-status user)))

(defn suspended?
  [user]
  (= :suspended (user-status user)))

(defn deleted?
  [user]
  (= :deleted (user-status user)))

(defn has-phone?
  [user]
  (some? (user-phone user)))

(defn has-email?
  [user]
  (some? (user-email user)))

(defn has-contact?
  [user]
  (or (has-phone? user)
      (has-email? user)))

(defn phone-verified?
  [user]
  (and (has-phone? user)
       (some? (:user/phone-verified-at user))))

(defn email-verified?
  [user]
  (and (has-email? user)
       (some? (:user/email-verified-at user))))

(defn has-verified-contact?
  [user]
  (or (phone-verified? user)
      (email-verified? user)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn- instant?
  [value]
  (instance? Instant value))

(defn- at-or-before?
  [^Instant left ^Instant right]
  (not (.isAfter left right)))

(defn- optional-uuid?
  [value]
  (or (nil? value)
      (uuid? value)))

(defn- optional-reason?
  [value]
  (or (nil? value)
      (qualified-keyword? value)))

(defn- none-present?
  [document keys]
  (every? #(nil? (get document %)) keys))

(defn- optional-time-within?
  [^Instant created-at value ^Instant updated-at]
  (or
   (nil? value)

   (and
    (instant? value)
    (at-or-before? created-at value)
    (at-or-before? value updated-at))))

(defn- version-consistent?
  [user]
  (let [created-at (:user/created-at user)
        updated-at (:user/updated-at user)]
    (and
     (command/versioned-document? user version)
     (instant? created-at)
     (instant? updated-at)
     (at-or-before? created-at updated-at))))

(defn- audit-consistent?
  [user audit]
  (let [[at-key by-key reason-key]
        (case audit
          :suspended
          [:user/suspended-at
           :user/suspended-by
           :user/suspension-reason]

          :deleted
          [:user/deleted-at
           :user/deleted-by
           :user/deletion-reason])

        at (get user at-key)
        by (get user by-key)
        reason (get user reason-key)]
    (and
     (or (nil? at)
         (instant? at))
     (optional-uuid? by)
     (optional-reason? reason)
     (or
      (some? at)
      (and (nil? by)
           (nil? reason))))))

(defn- lifecycle-consistent?
  [user]
  (case
   (user-status user)

    :active
    (none-present?
     user
     [:user/suspended-at
      :user/suspended-by
      :user/suspension-reason
      :user/deleted-at
      :user/deleted-by
      :user/deletion-reason])

    :suspended
    (and
     (some? (:user/suspended-at user))
     (none-present?
      user
      [:user/deleted-at
       :user/deleted-by
       :user/deletion-reason]))

    :deleted
    (and
     (some? (:user/deleted-at user))
     (none-present?
      user
      [:user/suspended-at
       :user/suspended-by
       :user/suspension-reason]))

    false))

(defn document-consistent?
  "Returns true when user satisfies every local persisted User invariant.

   Contact uniqueness, authentication policy, and relationships to other models
   are intentionally outside this predicate."
  [user]
  (and
   (map? user)
   (uuid? (user-id user))
   (version-consistent? user)
   (status? (user-status user))

   (or
    (nil? (user-phone user))
    (phone? (user-phone user)))

   (or
    (nil? (user-email user))
    (email? (user-email user)))

   (or
    (nil? (user-display-name user))
    (display-name? (user-display-name user)))

   (or
    (nil? (:user/phone-verified-at user))
    (has-phone? user))

   (or
    (nil? (:user/email-verified-at user))
    (has-email? user))

   (let [created-at (:user/created-at user)
         updated-at (:user/updated-at user)]
     (every?
      #(optional-time-within? created-at % updated-at)
      [(:user/phone-verified-at user)
       (:user/email-verified-at user)
       (:user/suspended-at user)
       (:user/deleted-at user)]))

   (audit-consistent? user :suspended)
   (audit-consistent? user :deleted)
   (lifecycle-consistent? user)))

;; =============================================================================
;; Errors and update mechanics
;; =============================================================================

(defn- context
  [user]
  {:user/id (user-id user)
   :user/status (user-status user)})

(defn- fail!
  [error-type message errors context]
  (throw
   (ex-info
    message
    {:error/type error-type
     :error/details
     {:errors errors
      :context context}})))

(defn- ensure!
  [test error-type message errors context]
  (when-not test
    (fail! error-type message errors context)))

(defn- ensure-document!
  [user]
  (ensure!
   (document-consistent? user)
   :user/invalid-document
   "The user operation is invalid."
   {:user "The user document is internally inconsistent."}
   (context user))
  user)

(defn- valid-change-time?
  [user now]
  (and
   (instant? now)
   (instant? (:user/updated-at user))
   (at-or-before? (:user/updated-at user) now)))

(defn- ensure-audit-input!
  [user {:keys [actor-id reason]}]
  (let [ctx (context user)]
    (ensure!
     (optional-uuid? actor-id)
     :user/invalid-input
     "The user operation is invalid."
     {:actor-id "The actor must be a UUID when supplied."}
     ctx)

    (ensure!
     (optional-reason? reason)
     :user/invalid-input
     "The user operation is invalid."
     {:reason "The reason must be a qualified keyword when supplied."}
     ctx)))

(defn- update-user
  [user now f]
  (ensure-document! user)

  (ensure!
   (valid-change-time? user now)
   :user/invalid-time
   "The user operation is invalid."
   {:now "The change time must not precede the last update."}
   (context user))

  (let [changed (f user)]
    (ensure!
     (not= user changed)
     :user/unchanged
     "The user operation is invalid."
     {:user "The operation would not change the user."}
     (context user))

    (ensure-document!
     (command/bump-version changed version now))))

(defn- ensure-changeable!
  [user]
  (ensure-document! user)

  (ensure!
   (not (deleted? user))
   :user/deleted
   "The user operation is invalid."
   {:status "A deleted user cannot be changed."}
   (context user))

  user)

(defn- can-transition?
  [user operation]
  (contains?
   allowed-transitions
   [(user-status user) operation]))

(defn- audit-assoc
  [user audit now actor-id reason]
  (let [[at-key by-key reason-key]
        (case audit
          :suspended
          [:user/suspended-at
           :user/suspended-by
           :user/suspension-reason]

          :deleted
          [:user/deleted-at
           :user/deleted-by
           :user/deletion-reason])]
    (cond->
     (assoc user at-key now)

      actor-id
      (assoc by-key actor-id)

      reason
      (assoc reason-key reason))))

;; =============================================================================
;; Construction
;; =============================================================================

(defn- normalize-create-input
  [input]
  (let [input (or input {})]
    {:id (:id input)
     :phone (normalize-phone (:phone input))
     :email (normalize-email (:email input))
     :display-name (normalize-display-name (:display-name input))
     :phone-verified? (get input :phone-verified? false)
     :email-verified? (get input :email-verified? false)
     :now (:now input)}))

(defn- create-input-errors
  [{:keys
    [id
     phone
     email
     display-name
     phone-verified?
     email-verified?
     now]}]
  (cond-> {}
    (not (uuid? id))
    (assoc :id "A user UUID is required.")

    (and (some? phone)
         (not (phone? phone)))
    (assoc :phone "The phone number must be canonical E.164.")

    (and (some? email)
         (not (email? email)))
    (assoc :email "The email address is invalid.")

    (and (some? display-name)
         (not (display-name? display-name)))
    (assoc
     :display-name
     (str
      "The display name must be non-blank and at most "
      display-name-max
      " characters."))

    (not (boolean? phone-verified?))
    (assoc :phone-verified? "Phone verification must be true or false.")

    (not (boolean? email-verified?))
    (assoc :email-verified? "Email verification must be true or false.")

    (and (true? phone-verified?)
         (nil? phone))
    (assoc
     :phone-verified?
     "A phone is required before phone verification.")

    (and (true? email-verified?)
         (nil? email))
    (assoc
     :email-verified?
     "An email is required before email verification.")

    (not (instant? now))
    (assoc :now "A valid user creation time is required.")))

(defn- new-user
  [input]
  (let [{:keys
         [id
          phone
          email
          display-name
          phone-verified?
          email-verified?
          now]
         :as normalized}
        (normalize-create-input input)

        errors
        (create-input-errors normalized)]
    (when (seq errors)
      (fail!
       :user/invalid-create-input
       "A valid user could not be created."
       errors
       {:user/id id}))

    (ensure-document!
     (cond->
      {:xt/id id
       :user/status :active
       :user/revision 0
       :user/created-at now
       :user/updated-at now}

       phone
       (assoc :user/phone phone)

       email
       (assoc :user/email email)

       display-name
       (assoc :user/display-name display-name)

       phone-verified?
       (assoc :user/phone-verified-at now)

       email-verified?
       (assoc :user/email-verified-at now)))))

;; =============================================================================
;; Profile and contact transitions
;; =============================================================================

(defn- edit-profile
  [user {:keys [display-name now] :as input}]
  (ensure-changeable! user)

  (ensure!
   (contains? input :display-name)
   :user/invalid-input
   "The user operation is invalid."
   {:display-name "Use nil explicitly to remove the display name."}
   (context user))

  (let [display-name (normalize-display-name display-name)]
    (ensure!
     (or
      (nil? display-name)
      (display-name? display-name))
     :user/invalid-input
     "The user operation is invalid."
     {:display-name
      (str
       "The display name must be non-blank and at most "
       display-name-max
       " characters.")}
     (context user))

    (update-user
     user
     now
     #(if
       (nil? display-name)
        (dissoc % :user/display-name)
        (assoc % :user/display-name display-name)))))

(defn- replace-phone
  [user {:keys [phone now]}]
  (ensure-changeable! user)

  (let [phone (normalize-phone phone)]
    (ensure!
     (phone? phone)
     :user/invalid-input
     "The user operation is invalid."
     {:phone "The phone must be canonical E.164."}
     (context user))

    (ensure!
     (not= phone (user-phone user))
     :user/contact-unchanged
     "The user operation is invalid."
     {:phone "The replacement matches the current phone."}
     (context user))

    (update-user
     user
     now
     #(-> %
          (assoc :user/phone phone)
          (dissoc :user/phone-verified-at)))))

(defn- replace-email
  [user {:keys [email now]}]
  (ensure-changeable! user)

  (let [email (normalize-email email)]
    (ensure!
     (email? email)
     :user/invalid-input
     "The user operation is invalid."
     {:email "The email address is invalid."}
     (context user))

    (ensure!
     (not= email (user-email user))
     :user/contact-unchanged
     "The user operation is invalid."
     {:email "The replacement matches the current email."}
     (context user))

    (update-user
     user
     now
     #(-> %
          (assoc :user/email email)
          (dissoc :user/email-verified-at)))))

(defn- remove-phone
  [user {:keys [now]}]
  (ensure-changeable! user)

  (ensure!
   (has-phone? user)
   :user/phone-missing
   "The user operation is invalid."
   {:phone "No phone is attached to the user."}
   (context user))

  (update-user
   user
   now
   #(dissoc %
            :user/phone
            :user/phone-verified-at)))

(defn- remove-email
  [user {:keys [now]}]
  (ensure-changeable! user)

  (ensure!
   (has-email? user)
   :user/email-missing
   "The user operation is invalid."
   {:email "No email is attached to the user."}
   (context user))

  (update-user
   user
   now
   #(dissoc %
            :user/email
            :user/email-verified-at)))

(defn- verify-phone
  "Marks the current phone verified.

   Supplying the expected phone prevents a stale challenge from verifying a
   replacement number."
  [user {:keys [phone now]}]
  (ensure-changeable! user)

  (let [phone (normalize-phone phone)]
    (ensure!
     (phone? phone)
     :user/invalid-input
     "The user operation is invalid."
     {:phone "The verified phone must be canonical E.164."}
     (context user))

    (ensure!
     (= phone (user-phone user))
     :user/verification-target-mismatch
     "The user operation is invalid."
     {:phone "The verification target is no longer current."}
     (context user))

    (ensure!
     (not (phone-verified? user))
     :user/phone-already-verified
     "The user operation is invalid."
     {:phone "The current phone is already verified."}
     (context user))

    (update-user
     user
     now
     #(assoc % :user/phone-verified-at now))))

(defn- verify-email
  "Marks the current email verified.

   Supplying the expected email prevents a stale challenge from verifying a
   replacement address."
  [user {:keys [email now]}]
  (ensure-changeable! user)

  (let [email (normalize-email email)]
    (ensure!
     (email? email)
     :user/invalid-input
     "The user operation is invalid."
     {:email "The verified email address is invalid."}
     (context user))

    (ensure!
     (= email (user-email user))
     :user/verification-target-mismatch
     "The user operation is invalid."
     {:email "The verification target is no longer current."}
     (context user))

    (ensure!
     (not (email-verified? user))
     :user/email-already-verified
     "The user operation is invalid."
     {:email "The current email is already verified."}
     (context user))

    (update-user
     user
     now
     #(assoc % :user/email-verified-at now))))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn- suspend-user
  [user {:keys [now actor-id reason] :as input}]
  (ensure-document! user)
  (ensure-audit-input! user input)

  (ensure!
   (active? user)
   (if (deleted? user)
     :user/deleted
     :user/not-active)
   "The user operation is invalid."
   {:status "Only an active user can be suspended."}
   (context user))

  (update-user
   user
   now
   #(audit-assoc
     (assoc % :user/status :suspended)
     :suspended
     now
     actor-id
     reason)))

(defn- reactivate-user
  [user {:keys [now]}]
  (ensure-document! user)

  (ensure!
   (suspended? user)
   (cond
     (deleted? user) :user/deleted
     (active? user) :user/already-active
     :else :user/not-suspended)
   "The user operation is invalid."
   {:status "Only a suspended user can be reactivated."}
   (context user))

  (update-user
   user
   now
   #(-> %
        (assoc :user/status :active)
        (dissoc :user/suspended-at
                :user/suspended-by
                :user/suspension-reason))))

(defn- delete-user
  [user {:keys [now actor-id reason] :as input}]
  (ensure-document! user)
  (ensure-audit-input! user input)

  (ensure!
   (can-transition? user :delete)
   :user/deleted
   "The user operation is invalid."
   {:status "The user is already deleted."}
   (context user))

  (update-user
   user
   now
   #(audit-assoc
     (-> %
         (assoc :user/status :deleted)
         (dissoc :user/suspended-at
                 :user/suspended-by
                 :user/suspension-reason))
     :deleted
     now
     actor-id
     reason)))

;; =============================================================================
;; Canonical model commands
;; =============================================================================

(defn create-user-command
  [input]
  (command/create
   entity-type
   (new-user input)
   version))

(defn- update-command
  [operation user transition input]
  (command/update-command
   entity-type
   operation
   user
   (transition user input)
   version))

(defn edit-profile-command
  [user input]
  (update-command :edit-profile user edit-profile input))

(defn replace-phone-command
  [user input]
  (update-command :replace-phone user replace-phone input))

(defn replace-email-command
  [user input]
  (update-command :replace-email user replace-email input))

(defn remove-phone-command
  [user input]
  (update-command :remove-phone user remove-phone input))

(defn remove-email-command
  [user input]
  (update-command :remove-email user remove-email input))

(defn verify-phone-command
  [user input]
  (update-command :verify-phone user verify-phone input))

(defn verify-email-command
  [user input]
  (update-command :verify-email user verify-email input))

(defn suspend-user-command
  [user input]
  (update-command :suspend user suspend-user input))

(defn reactivate-user-command
  [user input]
  (update-command :reactivate user reactivate-user input))

(defn delete-user-command
  [user input]
  (update-command :delete user delete-user input))
