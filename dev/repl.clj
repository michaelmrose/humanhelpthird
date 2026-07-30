(ns repl
  (:require
   [com.biffweb.background :as biff.background]
   [com.biffweb.config :as biff.config]
   [com.biffweb.xtdb :as biff.xtdb]
   [net.humanhelp :as main])
  (:import
   [java.time Instant]))

;; REPL-driven development
;; ----------------------------------------------------------------------------------------
;; The Biff 2 dev task starts the application, watches source files, and starts an nREPL
;; server. Connect your editor to that running nREPL server rather than starting a second
;; application process from the editor.
;; ----------------------------------------------------------------------------------------

;; This function should only be used from the REPL. Regular application code should receive
;; the system map from its parent component or from the Ring request context.
(defn get-context
  []
  @main/system)

<<<<<<< HEAD
(defn add-fixtures []
  (let [user-id (random-uuid)]
    (biffx/submit-tx (get-context)
                     [[:put-docs :user {:xt/id user-id
                                        :email "a@example.com"
                                        :foo "Some Value"
                                        :joined-at (Instant/now)}]
                      [:put-docs :msg {:xt/id (random-uuid)
                                       :user user-id
                                       :text "hello there"
                                       :sent-at (Instant/now)}]])))
=======
(defn add-fixtures
  []
  (let [user-id
        (random-uuid)]
    (biff.xtdb/submit-tx
     (get-context)
     [[:put-docs
       :user
       {:xt/id
        user-id
>>>>>>> biff2-migration

        :email
        "a@example.com"

        :foo
        "Some Value"

        :joined-at
        (Instant/now)}]

      [:put-docs
       :msg
       {:xt/id
        (random-uuid)

        :user
        user-id

        :text
        "hello there"

        :sent-at
        (Instant/now)}]])))

(defn secret-value
  [config k]
  (some->
   (get config k)
   force))

(defn check-config
  []
  (let [prod-config
        (biff.config/use-aero-config
         {:biff.config/profile
          :prod})

        dev-config
        (biff.config/use-aero-config
         {:biff.config/profile
          :dev})

        ;; Add keys for any other secrets you've added to resources/config.edn.
        secret-keys
        [:biff.ring/cookie-secret
         :mailersend/api-key
         :recaptcha/secret-key]

        get-secrets
        (fn [config]
          (into
           {}
           (map
            (fn [k]
              [k
               (secret-value
                config
                k)]))
           secret-keys))]
    {:prod-config
     prod-config

     :dev-config
     dev-config

     :prod-secrets
     (get-secrets
      prod-config)

     :dev-secrets
     (get-secrets
      dev-config)}))

(defn submit-job
  [queue-id job]
  (first
   (biff.background/submit-jobs
    (get-context)
    queue-id
    [job])))

(defn submit-job-for-result
  [queue-id job]
  (let [result
        (promise)]
    (biff.background/submit-jobs
     (get-context)
     queue-id
     [(assoc
       job
       :biff/callback
       #(deliver
         result
         %))])
    result))

(comment
  ;; Call this function after changing runtime components, module assembly,
  ;; config.env, config.edn, or deps.edn.
  (main/refresh)

  ;; Add starter fixture data. To reset a local XTDB database, stop the app,
  ;; remove the configured local XTDB storage directory, and restart it.
  (add-fixtures)

  ;; Query the database.
  (biff.xtdb/q
   (get-context)
   "select * from user")

<<<<<<< HEAD
  ;; Update an existing user's email address
  (let [{:keys [biff/node] :as ctx} (get-context)
        [{user-id :xt/id}] (xt/q node ["select _id from user where email = ?"
                                       "hello@example.com"])]
    (biffx/submit-tx ctx
                     [[:patch-docs :user {:xt/id user-id
                                          :email "new.address@example.com"}]]))
=======
  ;; Update an existing user's email address.
  (let [ctx
        (get-context)
>>>>>>> biff2-migration

        [{user-id
          :xt/id}]
        (biff.xtdb/q
         ctx
         ["select _id from user where email = ?"
          "hello@example.com"])]
    (biff.xtdb/submit-tx
     ctx
     [[:patch-docs
       :user
       {:xt/id
        user-id

        :email
        "new.address@example.com"}]]))

  (sort
   (keys
    (get-context)))

  ;; Check the terminal for output.
  (submit-job
   :echo
   {:foo
    "bar"})

  (deref
   (submit-job-for-result
    :echo
    {:foo
     "bar"})))
