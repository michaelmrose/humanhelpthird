(ns net.humanhelp.auth.phone
  (:require
   [net.humanhelp.components.phone-auth.sms :as phone-auth.sms]
   [net.humanhelp.site.model.user.core :as user]))

(defn normalize-phone
  "Normalize phone input for the phone-auth UI/provider boundary.

   The phone-auth component intentionally uses exactly 10 US digits while the
   User model stores canonical E.164."
  [phone]
  (phone-auth.sms/normalize-phone phone))

(defn phone-display
  [phone]
  (phone-auth.sms/phone-display phone))

(defn- user-phone
  "Convert the verified 10-digit US phone-auth value to the canonical E.164
   representation owned by the User model."
  [phone]
  (when-let [digits
             (normalize-phone phone)]
    (str "+1" digits)))

(defn- existing-user
  [ctx canonical-phone]
  (let [facts
        (user/user-facts
         ctx
         {:phone canonical-phone})]
    (when
     (:user/found? facts)
      (:user/doc facts))))

(defn complete-phone-signin!
  "Find or create a User for a phone number that has already been verified by
   the phone-auth provider.

   Phone-auth keeps its reusable UI/provider representation as 10 US digits.
   This boundary converts that value to canonical E.164 before entering the
   User model.

   This function does not create a Ring response or mutate the session. The
   caller should place the returned :user-id into the session as :uid.

   Returns:

     {:ok? true
      :user-id ...
      :phone \"1234567890\"
      :phone-display \"123-456-7890\"
      :new-user? true|false}

   or:

     {:ok? false
      :error ...}"
  [ctx {:keys [phone]}]
  (if-let [phone'
           (normalize-phone phone)]
    (let [canonical-phone
          (user-phone phone')

          phone-display'
          (phone-display phone')

          existing
          (existing-user
           ctx
           canonical-phone)]
      (if
       existing
        {:ok? true
         :user-id (:xt/id existing)
         :phone phone'
         :phone-display phone-display'
         :new-user? false}

        (let [result
              (user/create-user
               ctx
               {:phone canonical-phone
                :phone-verified? true})

              created-user
              (:user result)]
          {:ok? true
           :user-id (:xt/id created-user)
           :phone phone'
           :phone-display phone-display'
           :new-user? true})))

    {:ok? false
     :error "Missing or invalid phone number."}))
