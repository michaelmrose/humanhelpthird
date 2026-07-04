(ns net.humanhelp.auth.phone
  (:require
   [com.biffweb :as biff]
   [net.humanhelp.components.phone-auth.sms :as phone-auth.sms]
   [xtdb.api :as xt]))

(defn- ctx-keys
  [ctx]
  (when (map? ctx)
    (->> (keys ctx)
         (map str)
         sort
         vec)))

(defn- request-db
  [ctx]
  (or (:biff/db ctx)
      (some-> (:biff.xtdb/node ctx) xt/db)
      (throw
       (ex-info "Phone auth requires :biff/db or :biff.xtdb/node."
                {:ctx-keys (ctx-keys ctx)}))))

(defn- fresh-db
  [ctx]
  (or (some-> (:biff.xtdb/node ctx) xt/db)
      (:biff/db ctx)
      (throw
       (ex-info "Phone auth requires :biff.xtdb/node or :biff/db."
                {:ctx-keys (ctx-keys ctx)}))))

(defn normalize-phone
  [phone]
  (phone-auth.sms/normalize-phone phone))

(defn phone-display
  [phone]
  (phone-auth.sms/phone-display phone))

(defn get-user-id
  [db phone]
  (biff/lookup-id db :user/phone phone))

(defn new-user-tx
  [{:keys [phone phone-display]}]
  [{:db/doc-type :user
    :db.op/upsert {:user/phone phone}
    :user/phone phone
    :user/phone-display phone-display
    :user/phone-verified-at :db/now
    :user/joined-at :db/now}])

(defn verified-existing-user-tx
  [{:keys [user-id phone phone-display]}]
  [{:db/doc-type :user
    :db/op :update
    :xt/id user-id
    :user/phone phone
    :user/phone-display phone-display
    :user/phone-verified-at :db/now}])

(defn complete-phone-signin!
  "Find or create a user for a verified phone number.

  Input:
    {:phone ...}

  Returns:
    {:ok? true
     :user-id ...
     :phone \"1234567890\"
     :phone-display \"123-456-7890\"
     :new-user? true|false}

  This function deliberately does not create a Ring response and does not mutate
  the session. home.clj should use :user-id from this result to assoc :uid into
  the session and redirect to /app."
  [ctx {:keys [phone]}]
  (if-let [phone' (normalize-phone phone)]
    (let [phone-display'  (phone-display phone')
          db              (request-db ctx)
          existing-user-id (get-user-id db phone')
          new-user?       (nil? existing-user-id)
          tx              (if existing-user-id
                            (verified-existing-user-tx
                             {:user-id existing-user-id
                              :phone phone'
                              :phone-display phone-display'})
                            (new-user-tx
                             {:phone phone'
                              :phone-display phone-display'}))]
      (biff/submit-tx ctx tx)
      (let [user-id (or existing-user-id
                        (get-user-id (fresh-db ctx) phone'))]
        (if user-id
          {:ok? true
           :user-id user-id
           :phone phone'
           :phone-display phone-display'
           :new-user? new-user?}

          {:ok? false
           :phone phone'
           :phone-display phone-display'
           :error "Could not finish signing in."})))

    {:ok? false
     :error "Missing or invalid phone number."}))
