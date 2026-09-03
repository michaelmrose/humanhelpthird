(ns net.humanhelp
  (:require
   [aleph.http :as aleph]
   [clojure.tools.logging :as log]
   [clojure.tools.namespace.repl :as tn-repl]
   [com.biffweb.config :as biff.config]
   [com.biffweb.core :as biff.core]
   [com.biffweb.fx :as biff.fx]
   [com.biffweb.graph :as biff.graph]
   [com.biffweb.xtdb :as biff.xtdb]
   [gesso.live.core :as live]
   [malli.core :as malc]
   [malli.registry :as malr]
   [net.humanhelp.app :as app]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.home :as home]
   [net.humanhelp.middleware :as mid]
   [net.humanhelp.schema :as schema]
   [net.humanhelp.ui :as ui]
   [nrepl.cmdline :as nrepl-cmd]
   [reitit.ring :as reitit-ring])
  (:gen-class))

;; -----------------------------------------------------------------------------
;; Modules
;; -----------------------------------------------------------------------------

(def modules
  "Application modules plus the Biff 2 infrastructure modules required by
   HumanHelp and Gesso.

   HumanHelp keeps its existing :routes module contributions for now. Routing
   is assembled below with Reitit exactly as before; converting every
   application module to :biff.ring/routes is intentionally outside this
   mechanical Biff 2 migration."
  [(biff.core/module)
   (biff.fx/module)
   (biff.graph/module)
   (biff.xtdb/module)
   app/module
   client-plumbing/module
   home/module
   schema/module])

;; -----------------------------------------------------------------------------
;; Routing
;; -----------------------------------------------------------------------------

(def routes
  [[""
    {:middleware
     [mid/wrap-site-defaults]}

    (keep
     :routes
     modules)]])

(defn- error-handler
  [status]
  (fn [ctx]
    (ui/on-error
     (assoc
      ctx
      :status
      status))))

(def default-handler
  (reitit-ring/create-default-handler
   {:not-found
    (error-handler
     404)

    :method-not-allowed
    (error-handler
     405)

    :not-acceptable
    (error-handler
     406)}))

(def handler
  (-> (reitit-ring/ring-handler
       (reitit-ring/router
        routes)
       default-handler)
      mid/wrap-base-defaults))

;; -----------------------------------------------------------------------------
;; Malli compatibility boundary used by Gesso model transactions
;; -----------------------------------------------------------------------------

(def malli-opts
  "Gesso's current transaction boundary still accepts :biff/malli-opts.

   Biff 2 itself uses its registered schema registry, but retaining this value
   preserves the existing Gesso/HumanHelp model transaction contract during the
   migration."
  {:registry
   (malr/composite-registry
    malc/default-registry
    schema/schema)})

;; -----------------------------------------------------------------------------
;; Initial system
;; -----------------------------------------------------------------------------

(def initial-system
  {:biff.ring/handler
   #'handler

   :biff.ring/on-error
   #'ui/on-error

   ;; Temporary Gesso model transaction compatibility. This is not a Biff 2
   ;; runtime key.
   :biff/malli-opts
   #'malli-opts})

(defonce system
  (atom
   {}))

;; -----------------------------------------------------------------------------
;; Gesso Live system component
;; -----------------------------------------------------------------------------

(defn gesso-live-rules
  "Collect Gesso Live invalidation rules from registered application modules."
  []
  (vec
   (mapcat
    :live-rules
    modules)))

(defn use-gesso-live
  "Create the app-wide Gesso Live system and attach it to the Biff 2 ctx."
  [ctx]
  (let [live-system
        (live/create
         {:rules
          (gesso-live-rules)

          ;; The first invalidation wakes immediately; repeated invalidations
          ;; within the window collapse to one trailing wakeup.
          :source-options
          {:coalesce-window-ms
           1000}

          ;; submit-expanded! currently submits plain jobs, so do not enable
          ;; :on-overflow :coalesce without explicit coalesce keys.
          :dispatch-options
          {:threads
           4

           :queue-size
           50000}

          :fragment-options
          {:ttl-ms
           1000}})]

    (log/info
     "Gesso Live system started.")

    (-> ctx
        (assoc
         :gesso.live/system
         live-system)

        (update
         :biff.core/stop
         (fnil conj [])
         (fn stop-gesso-live
           []
           (log/info
            "Stopping Gesso Live system.")
           (live/close!
            live-system))))))

;; -----------------------------------------------------------------------------
;; Aleph HTTP server component
;; -----------------------------------------------------------------------------

(defn- parse-port
  [port]
  (cond
    (integer?
     port)
    port

    (string?
     port)
    (Long/parseLong
     port)

    :else
    (long
     port)))

(defn use-aleph
  "Start the HumanHelp Aleph/Netty server from the Biff 2 Ring ctx.

   Biff 2's standard server component is Jetty. HumanHelp intentionally keeps
   Aleph, but consumes the same :biff.ring/host, :biff.ring/port, and
   :biff.ring/handler keys."
  [{:biff.ring/keys
    [host
     port
     handler]

    :or
    {host
     "0.0.0.0"

     port
     8080}

    :as
    ctx}]

  (when-not
   handler
    (throw
     (ex-info
      "Cannot start Aleph server without :biff.ring/handler."
      {:ctx-keys
       (set
        (keys
         ctx))})))

  (let [port'
        (parse-port
         port)

        handler'
        (fn [request]
          (handler
           (merge
            ctx
            request)))

        thread-factory
        (io.netty.util.concurrent.DefaultThreadFactory.
         "aleph-worker"
         true)

        worker-pool
        (java.util.concurrent.ThreadPoolExecutor.
         800
         800
         60
         java.util.concurrent.TimeUnit/SECONDS
         (java.util.concurrent.ArrayBlockingQueue.
          50000)
         thread-factory)

        server
        (aleph/start-server
         handler'
         {:host
          host

          :port
          port'

          :executor
          worker-pool})]

    (log/info
     "ALEPH SERVER STARTED"
     {:host
      host

      :port
      port'

      :server
      server})

    (update
     ctx
     :biff.core/stop
     (fnil conj [])
     (fn stop-aleph
       []
       (log/info
        "STOPPING ALEPH SERVER"
        {:host
         host

         :port
         port'})
       (.close
        server)
       (.shutdown
        worker-pool)))))

;; -----------------------------------------------------------------------------
;; Biff 2 components
;; -----------------------------------------------------------------------------

(def components
  [biff.config/use-aero-config
   biff.xtdb/use-xtdb
   use-gesso-live
   use-aleph])

;; -----------------------------------------------------------------------------
;; Lifecycle
;; -----------------------------------------------------------------------------

(defn start
  []
  (let [new-system
        (biff.core/start
         initial-system
         #'modules
         components)]

    (reset!
     system
     new-system)

    (log/info
     "Go to"
     (:biff.ring/base-url
      new-system))

    new-system))

(defn stop
  []
  (biff.core/stop
   @system)

  (reset!
   system
   {})

  :stopped)

(defn refresh
  []
  (stop)
  (tn-repl/refresh
   :after
   `start)
  :done)

(defn -main
  [& _args]
  (let [{:biff.tasks/keys
         [nrepl-port]}
        (start)]

    (if
     nrepl-port
      (nrepl-cmd/-main
       "--port"
       (str
        nrepl-port)

       "--middleware"
       (pr-str
        '[cider.nrepl/cider-middleware
          refactor-nrepl.middleware/wrap-refactor]))

      ;; Production or other profiles may intentionally omit an nREPL port.
      @(promise)))

  ;; nREPL blocks while it is running. Keep the process alive as a fallback if
  ;; its behavior changes or it exits without terminating the application.
  @(promise))
