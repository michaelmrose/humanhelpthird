(ns net.humanhelp.auth.phone
  "Phone-authentication boundary for HumanHelp.

   The reusable phone-auth component owns its provider/UI representation:
   exactly 10 US digits.

   User owns canonical persisted identity representation, User lookup, and User
   creation semantics. This namespace converts the verified provider value to
   canonical E.164, finds an existing User through user.core, and commits a
   User creation plan when no User exists.

   Session mutation and Ring responses remain the caller's responsibility."

  (:require
   [com.biffweb.fx :as fx]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.components.phone-auth.sms :as phone-auth.sms]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Phone-auth boundary values
;; =============================================================================

(defn normalize-phone
  "Normalize phone input for the phone-auth UI/provider boundary.

   The phone-auth component intentionally uses exactly 10 US digits while the
   User model stores canonical E.164."
  [phone]
  (phone-auth.sms/normalize-phone
   phone))

(defn phone-display
  [phone]
  (phone-auth.sms/phone-display
   phone))

(defn- user-phone
  "Convert the verified 10-digit US phone-auth value to the canonical E.164
   representation accepted by User."
  [phone]
  (when-let [digits
             (normalize-phone
              phone)]
    (user/normalize-phone
     (str
      "+1"
      digits))))

;; =============================================================================
;; User creation execution
;; =============================================================================

(defn- planned-transaction
  [{:keys
    [transaction-fragment
     transaction-options]}]
  (merge
   transaction-fragment
   transaction-options))

(fx/defmachine create-user-machine
  :start
  (fn [{::keys
        [create-user-input]
        :as    ctx}]
    (let [{:keys
           [result]
           :as   planned}
          (user/plan-create-user
           ctx
           create-user-input)]
      {::create-user-result
       result

       ::create-user-transaction
       [model.tx/transact-effect
        (planned-transaction
         planned)]

       :biff.fx/next
       :finish}))

  :finish
  (fn [{::keys
        [create-user-result
         create-user-transaction]}]
    {:biff.fx/return
     (assoc
      create-user-result
      :transaction
      create-user-transaction)}))

(defn- create-user!
  [ctx input]
  (create-user-machine
   (assoc
    ctx
    ::create-user-input
    input)))

;; =============================================================================
;; Sign-in completion
;; =============================================================================

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

  [ctx {:keys
        [phone]}]
  (if-let [phone'
           (normalize-phone
            phone)]
    (let [canonical-phone
          (user-phone
           phone')

          phone-display'
          (phone-display
           phone')

          existing
          (user/user-by-phone
           ctx
           canonical-phone)]

      (if
       existing
        {:ok?
         true

         :user-id
         (user/user-id
          existing)

         :phone
         phone'

         :phone-display
         phone-display'

         :new-user?
         false}

        (let [result
              (create-user!
               ctx
               {:phone
                canonical-phone

                :phone-verified?
                true})

              created-user
              (:user
               result)]

          {:ok?
           true

           :user-id
           (user/user-id
            created-user)

           :phone
           phone'

           :phone-display
           phone-display'

           :new-user?
           true})))

    {:ok?
     false

     :error
     "Missing or invalid phone number."}))
