(ns net.humanhelp.site.model.user.domain.identity
  "Pure rules for persisted HumanHelp user identity documents.

   This namespace owns identity values, identity lifecycle transitions, and
   command construction. It does not query XTDB, enforce contact uniqueness,
   authorize actors, send verification messages, or manage sessions."
  (:require
   [clojure.string :as str]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.domain.common :as user.common]))

(def entity-type :user)

(def version
  {:revision-key :user/revision
   :created-at-key :user/created-at
   :updated-at-key :user/updated-at})

(def display-name-max 120)
(def statuses
  #{:active :suspended :deleted})

(def allowed-transitions
  {[:active :suspend] :suspended
   [:suspended :reactivate] :active
   [:active :delete] :deleted
   [:suspended :delete] :deleted})

;; Values ----------------------------------------------------------------------

(defn normalize-display-name
  [value]
  (when (string? value)
    (str/trim value)))

(defn display-name?
  [value]
  (or
   (nil? value)
   (and
    (string? value)
    (= value (normalize-display-name value))
    (not (str/blank? value))
    (<= (count value) display-name-max))))

;; Facts -----------------------------------------------------------------------

(defn status?
  [value]
  (contains? statuses value))

(defn active?
  [user]
  (= :active (:user/status user)))

(defn suspended?
  [user]
  (= :suspended (:user/status user)))

(defn deleted?
  [user]
  (= :deleted (:user/status user)))

(defn next-status
  [user operation]
  (get allowed-transitions [(:user/status user) operation]))

(defn can-transition?
  [user operation]
  (some? (next-status user operation)))

(defn has-phone?
  [user]
  (some? (:user/phone user)))

(defn has-email?
  [user]
  (some? (:user/email user)))

(defn has-contact?
  [user]
  (or (has-phone? user)
      (has-email? user)))

(defn phone-verified?
  [user]
  (and
   (has-phone? user)
   (some? (:user/phone-verified-at user))))

(defn email-verified?
  [user]
  (and
   (has-email? user)
   (some? (:user/email-verified-at user))))

(defn has-verified-contact?
  [user]
  (or (phone-verified? user)
      (email-verified? user)))

;; Validation ------------------------------------------------------------------

(defn- optional-uuid?
  [value]
  (or (nil? value)
      (uuid? value)))

(defn- optional-reason?
  [value]
  (or (nil? value)
      (qualified-keyword? value)))

(defn- none-present?
  [m ks]
  (every? nil? (map m ks)))

(defn- timestamp-within-document?
  [user value]
  (model.common/optional-between?
   (:user/created-at user)
   value
   (:user/updated-at user)))

(defn document-consistent?
  [user]
  (and
   (map? user)
   (model.common/versioned-document-consistent? user version)
   (status? (:user/status user))
   (has-contact? user)
   (or (nil? (:user/phone user))
       (user.common/phone? (:user/phone user)))
   (or (nil? (:user/email user))
       (user.common/email? (:user/email user)))
   (display-name? (:user/display-name user))
   (or (nil? (:user/phone-verified-at user))
       (has-phone? user))
   (or (nil? (:user/email-verified-at user))
       (has-email? user))
   (every?
    #(timestamp-within-document? user %)
    [(:user/phone-verified-at user)
     (:user/email-verified-at user)
     (:user/suspended-at user)
     (:user/deleted-at user)])
   (optional-uuid? (:user/suspended-by user))
   (optional-uuid? (:user/deleted-by user))
   (optional-reason? (:user/suspension-reason user))
   (optional-reason? (:user/deletion-reason user))
   (case (:user/status user)
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

     false)))

(defn normalize-create-input
  [input]
  (let [input (or input {})]
    {:id (:id input)
     :phone (user.common/normalize-phone (:phone input))
     :email (user.common/normalize-email (:email input))
     :phone-verified? (get input :phone-verified? false)
     :email-verified? (get input :email-verified? false)
     :display-name (normalize-display-name (:display-name input))
     :now (:now input)}))

(defn create-input-errors
  "Validates normalized create input. Contact uniqueness belongs in Graph/FX."
  [{:keys
    [id phone email phone-verified? email-verified? display-name now]}]
  (cond-> {}
    (not (uuid? id))
    (assoc :id "A user UUID is required.")

    (and (nil? phone) (nil? email))
    (assoc :contact "A phone number or email address is required.")

    (and phone (not (user.common/phone? phone)))
    (assoc :phone "The phone number must be canonical E.164.")

    (and email (not (user.common/email? email)))
    (assoc :email "The email address is invalid.")

    (not (boolean? phone-verified?))
    (assoc :phone-verified? "Phone verification must be true or false.")

    (not (boolean? email-verified?))
    (assoc :email-verified? "Email verification must be true or false.")

    (and (true? phone-verified?) (nil? phone))
    (assoc :phone-verified? "A phone is required before verification.")

    (and (true? email-verified?) (nil? email))
    (assoc :email-verified? "An email is required before verification.")

    (not (display-name? display-name))
    (assoc :display-name
           (str "The display name must be non-blank and at most "
                display-name-max
                " characters."))

    (not (model.common/timestamp-value? now))
    (assoc :now "A valid creation time is required.")))

(defn- context
  [user]
  {:user/id (:xt/id user)
   :user/status (:user/status user)})

(defn- fail!
  [user error-type errors]
  (model.common/throw-invalid!
   error-type
   "The user identity operation is invalid."
   errors
   (context user)))

(defn- ensure!
  [test user error-type errors]
  (when-not test
    (fail! user error-type errors)))

(defn- ensure-document!
  [user]
  (ensure!
   (document-consistent? user)
   user
   :user/invalid-document
   {:user "The user document is internally inconsistent."})
  user)

(defn- ensure-change-time!
  [user now]
  (ensure!
   (model.common/valid-change-time? user version now)
   user
   :user/invalid-time
   {:now "The change time must not precede the last update."}))

(defn- ensure-audit-input!
  [user {:keys [actor-id reason]}]
  (ensure!
   (optional-uuid? actor-id)
   user
   :user/invalid-input
   {:actor-id "The actor must be a UUID when supplied."})
  (ensure!
   (optional-reason? reason)
   user
   :user/invalid-input
   {:reason "The reason must be a qualified keyword when supplied."}))

(defn- update-user
  [user now f]
  (ensure-document! user)
  (ensure-change-time! user now)
  (let [changed (f user)]
    (ensure!
     (not= user changed)
     user
     :user/unchanged
     {:user "The operation would not change the user."})
    (let [updated (model.common/bump-revision changed version now)]
      (ensure-document! updated))))

;; Construction ----------------------------------------------------------------

(defn new-user
  [input]
  (let [{:keys
         [id phone email phone-verified? email-verified? display-name now]
         :as input}
        (normalize-create-input input)
        errors (create-input-errors input)]
    (when (seq errors)
      (model.common/throw-invalid!
       :user/invalid-create-input
       "A valid user identity could not be created."
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

       phone-verified?
       (assoc :user/phone-verified-at now)

       email-verified?
       (assoc :user/email-verified-at now)

       display-name
       (assoc :user/display-name display-name)))))

;; Profile and contact transitions ---------------------------------------------

(defn edit-profile
  [user {:keys [display-name now] :as input}]
  (ensure-document! user)
  (ensure! (not (deleted? user)) user :user/deleted
           {:status "A deleted user cannot be changed."})
  (ensure! (contains? input :display-name) user :user/invalid-input
           {:display-name "Use nil explicitly to remove the display name."})
  (let [display-name' (normalize-display-name display-name)]
    (ensure!
     (or (nil? display-name)
         (display-name? display-name'))
     user
     :user/invalid-input
     {:display-name "The display name is invalid."})
    (update-user
     user
     now
     #(if (nil? display-name)
        (dissoc % :user/display-name)
        (assoc % :user/display-name display-name')))))

(defn replace-phone
  [user {:keys [phone now]}]
  (ensure-document! user)
  (ensure! (not (deleted? user)) user :user/deleted
           {:status "A deleted user cannot be changed."})
  (let [phone' (user.common/normalize-phone phone)]
    (ensure! (user.common/phone? phone') user :user/invalid-input
             {:phone "The phone must be canonical E.164."})
    (ensure! (not= phone' (:user/phone user)) user :user/contact-unchanged
             {:phone "The replacement matches the current phone."})
    (update-user
     user
     now
     #(-> %
          (assoc :user/phone phone')
          (dissoc :user/phone-verified-at)))))

(defn replace-email
  [user {:keys [email now]}]
  (ensure-document! user)
  (ensure! (not (deleted? user)) user :user/deleted
           {:status "A deleted user cannot be changed."})
  (let [email' (user.common/normalize-email email)]
    (ensure! (user.common/email? email') user :user/invalid-input
             {:email "The email address is invalid."})
    (ensure! (not= email' (:user/email user)) user :user/contact-unchanged
             {:email "The replacement matches the current email."})
    (update-user
     user
     now
     #(-> %
          (assoc :user/email email')
          (dissoc :user/email-verified-at)))))

(defn remove-phone
  [user {:keys [now]}]
  (ensure-document! user)
  (ensure! (not (deleted? user)) user :user/deleted
           {:status "A deleted user cannot be changed."})
  (ensure! (has-phone? user) user :user/phone-missing
           {:phone "No phone is attached to the user."})
  (ensure! (has-email? user) user :user/contact-required
           {:contact "Add an email before removing the phone."})
  (update-user
   user
   now
   #(dissoc % :user/phone :user/phone-verified-at)))

(defn remove-email
  [user {:keys [now]}]
  (ensure-document! user)
  (ensure! (not (deleted? user)) user :user/deleted
           {:status "A deleted user cannot be changed."})
  (ensure! (has-email? user) user :user/email-missing
           {:email "No email is attached to the user."})
  (ensure! (has-phone? user) user :user/contact-required
           {:contact "Add a phone before removing the email."})
  (update-user
   user
   now
   #(dissoc % :user/email :user/email-verified-at)))

(defn verify-phone
  "The expected phone prevents an old challenge from verifying a replacement."
  [user {:keys [phone now]}]
  (ensure-document! user)
  (ensure! (not (deleted? user)) user :user/deleted
           {:status "A deleted user cannot be changed."})
  (let [phone' (user.common/normalize-phone phone)]
    (ensure! (user.common/phone? phone') user :user/invalid-input
             {:phone "The verified phone must be canonical E.164."})
    (ensure! (= phone' (:user/phone user)) user
             :user/verification-target-mismatch
             {:phone "The verification target is no longer current."})
    (ensure! (not (phone-verified? user)) user
             :user/phone-already-verified
             {:phone "The current phone is already verified."})
    (update-user user now #(assoc % :user/phone-verified-at now))))

(defn verify-email
  "The expected email prevents an old challenge from verifying a replacement."
  [user {:keys [email now]}]
  (ensure-document! user)
  (ensure! (not (deleted? user)) user :user/deleted
           {:status "A deleted user cannot be changed."})
  (let [email' (user.common/normalize-email email)]
    (ensure! (user.common/email? email') user :user/invalid-input
             {:email "The verified email is invalid."})
    (ensure! (= email' (:user/email user)) user
             :user/verification-target-mismatch
             {:email "The verification target is no longer current."})
    (ensure! (not (email-verified? user)) user
             :user/email-already-verified
             {:email "The current email is already verified."})
    (update-user user now #(assoc % :user/email-verified-at now))))

;; Lifecycle transitions --------------------------------------------------------

(defn suspend
  [user {:keys [now actor-id reason] :as input}]
  (ensure-document! user)
  (ensure-audit-input! user input)
  (ensure! (active? user) user
           (if (deleted? user) :user/deleted :user/not-active)
           {:status "Only an active user can be suspended."})
  (update-user
   user
   now
   #(cond->
     (assoc %
            :user/status :suspended
            :user/suspended-at now)
     actor-id (assoc :user/suspended-by actor-id)
     reason (assoc :user/suspension-reason reason))))

(defn reactivate
  [user {:keys [now]}]
  (ensure-document! user)
  (ensure! (suspended? user) user
           (if (deleted? user) :user/deleted :user/not-suspended)
           {:status "Only a suspended user can be reactivated."})
  (update-user
   user
   now
   #(-> %
        (assoc :user/status :active)
        (dissoc
         :user/suspended-at
         :user/suspended-by
         :user/suspension-reason))))

(defn delete-user
  [user {:keys [now actor-id reason] :as input}]
  (ensure-document! user)
  (ensure-audit-input! user input)
  (ensure! (can-transition? user :delete) user :user/deleted
           {:status "The user is already deleted."})
  (update-user
   user
   now
   #(cond->
     (-> %
         (assoc
          :user/status :deleted
          :user/deleted-at now)
         (dissoc
          :user/suspended-at
          :user/suspended-by
          :user/suspension-reason))
     actor-id (assoc :user/deleted-by actor-id)
     reason (assoc :user/deletion-reason reason))))

;; Commands --------------------------------------------------------------------

(defn create-command
  [input]
  (model.common/create-command entity-type (new-user input) version))

(defn- change-command
  [operation before after]
  (model.common/update-command entity-type operation before after version))

(defn edit-profile-command [user input]
  (change-command :edit-profile user (edit-profile user input)))

(defn replace-phone-command [user input]
  (change-command :replace-phone user (replace-phone user input)))

(defn replace-email-command [user input]
  (change-command :replace-email user (replace-email user input)))

(defn remove-phone-command [user input]
  (change-command :remove-phone user (remove-phone user input)))

(defn remove-email-command [user input]
  (change-command :remove-email user (remove-email user input)))

(defn verify-phone-command [user input]
  (change-command :verify-phone user (verify-phone user input)))

(defn verify-email-command [user input]
  (change-command :verify-email user (verify-email user input)))

(defn suspend-command [user input]
  (change-command :suspend user (suspend user input)))

(defn reactivate-command [user input]
  (change-command :reactivate user (reactivate user input)))

(defn delete-command [user input]
  (change-command :delete user (delete-user user input)))
