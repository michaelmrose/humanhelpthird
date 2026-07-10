(ns net.humanhelp.auth.phone
  (:require
   [com.biffweb.experimental :as biffx]
   [net.humanhelp.components.phone-auth.sms :as phone-auth.sms]
   [tick.core :as tick]))

(defn- ctx-keys
  [ctx]
  (when (map? ctx)
    (->> (keys ctx)
         (map str)
         sort
         vec)))

(defn- node
  [ctx]
  (or (:biff/node ctx)
      (throw
       (ex-info "Phone auth requires :biff/node."
                {:ctx-keys (ctx-keys ctx)}))))

(defn normalize-phone
  [phone]
  (phone-auth.sms/normalize-phone phone))

(defn phone-display
  [phone]
  (phone-auth.sms/phone-display phone))

(defn get-user-id
  [node phone]
  (-> (biffx/q node
               {:select :xt/id
                :from :user
                :where [:= :user/phone phone]})
      first
      :xt/id))

(defn new-user-tx
  [{:keys [user-id phone phone-display now]}]
  [[:put-docs :user
    {:xt/id user-id
     :user/phone phone
     :user/phone-display phone-display
     :user/phone-verified-at now
     :user/joined-at now}]
   (biffx/assert-unique :user {:user/phone phone})])

(defn complete-phone-signin!
  "Find or create a user for a verified phone number.

  This function completes the identity part of phone auth, but deliberately does
  not create a Ring response and does not mutate the session. The caller should
  put the returned :user-id into the session as :uid.

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
  (if-let [phone' (normalize-phone phone)]
    (let [node'            (node ctx)
          phone-display'   (phone-display phone')
          existing-user-id (get-user-id node' phone')]
      (if existing-user-id
        {:ok? true
         :user-id existing-user-id
         :phone phone'
         :phone-display phone-display'
         :new-user? false}

        (let [user-id (random-uuid)
              now     (tick/zoned-date-time)]
          (biffx/submit-tx
           ctx
           (new-user-tx
            {:user-id user-id
             :phone phone'
             :phone-display phone-display'
             :now now}))

          {:ok? true
           :user-id user-id
           :phone phone'
           :phone-display phone-display'
           :new-user? true})))

    {:ok? false
     :error "Missing or invalid phone number."}))
