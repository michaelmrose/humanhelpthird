(ns net.humanhelp-test
  "Top-level runtime-preflight regression tests for HumanHelp.

   These tests freeze the two joins that were previously missing while the lower
   assembly/unit suite remained green:

     HumanHelp start
       -> canonical Gesso start-biff-application! boundary

   and, after the Claim formulation repair:

     actual no-progression Request-card Claim render
       -> semantic identity remains :request/claim
       -> Gesso UI construction rejects missing optimistic realization
       -> no malformed anonymous HTMX action can exist.

   The authoritative request-frontier regressions additionally freeze:

     Aleph request boundary
       -> browser minimum progression decoded
       -> one trusted XTDB frontier bound
       -> matching Biff snapshot + Gesso progression
       -> real Request-card Claim obtains a canonical optimistic realization.

   A separate synthetic downgrade regression preserves the outer runtime guard:
   if downstream code nevertheless constructs an anonymous POST targeting the
   assembled Claim route, the current HumanHelp ApplicationAssembly must reject
   it before response serialization.  Together the tests freeze both the early
   construction boundary and the application-wide pre-browser backstop."
  (:require
   [aleph.http :as aleph]
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.config :as biff.config]
   [com.biffweb.xtdb :as biff.xtdb]
   [com.biffweb.core :as biff.core]
   [gesso.live.application-preflight :as application-preflight]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.consistency.xtdb :as live.xtdb]
   [gesso.live.core :as live]
   [gesso.live.ui :as live.ui]
   [gesso.model.tx :as model.tx]
   [gesso.model.command :as command]
   [net.humanhelp :as humanhelp]
   [net.humanhelp.app :as app]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.app :as example]
   [net.humanhelp.example.application-preflight :as example.application-preflight]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.components.request-card.core :as request-card]
   [net.humanhelp.example.routes :as example.routes]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
   [net.humanhelp.site.model.request.domain :as request.domain]
   [net.humanhelp.home :as home]
   [net.humanhelp.schema :as schema]
   [net.humanhelp.ui :as ui])
  (:import
   [java.nio.file Files]
   [java.time Instant]
   [java.util UUID]))

(def request-id
  (UUID/fromString "71000000-0000-0000-0000-000000000001"))

(def organization-id
  (UUID/fromString "72000000-0000-0000-0000-000000000001"))

(def location-id
  (UUID/fromString "73000000-0000-0000-0000-000000000001"))

(def requestor-id
  (UUID/fromString "74000000-0000-0000-0000-000000000001"))

(def created-at
  (Instant/parse "2026-09-09T00:00:00Z"))

(def open-request
  (command/after
   (request.domain/create-request-command
    {:id request-id
     :organization-id organization-id
     :location-id location-id
     :requestor (request.domain/user-requestor requestor-id)
     :content {:title "Need groceries"
               :details "Please pick up milk."
               :location-detail "Front desk"}
     :now created-at})))

(def open-row
  {:request open-request
   :primary-assignment nil
   :requestor-user nil
   :primary-helper-user nil})

(def claim-affordance
  {:operation request.choreo/claim-operation
   :capability request.choreo/claim-capability
   :arguments {:request-id request-id}})

(defn- exception-info-data
  [error]
  (loop [current error]
    (cond
      (nil? current)
      nil

      (instance? clojure.lang.ExceptionInfo current)
      (ex-data current)

      :else
      (recur (.getCause ^Throwable current)))))

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      (exception-info-data error))))

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))]
    (Files/deleteIfExists (.toPath file))))

(defn- with-temp-dir
  [f]
  (let [path
        (Files/createTempDirectory
         "humanhelp-runtime-preflight-test-"
         (make-array java.nio.file.attribute.FileAttribute 0))
        dir (.toFile path)]
    (try
      (f dir)
      (finally
        (delete-tree! dir)))))

(defn- child-path
  [dir name]
  (str (.resolve (.toPath dir) name)))

(defn- with-stamped-humanhelp-application
  [f]
  (with-temp-dir
   (fn [dir]
     (let [artifact-path (child-path dir "gesso-live.js")
           browser-assembly
           (example.application-preflight/require-browser-assembly!)]
       ;; This test exercises artifact correspondence/currentness, not Closure
       ;; optimization.  Record harmless bytes under the exact HumanHelp browser
       ;; manifest using the same public receipt machinery as the supported build.
       (spit artifact-path
             "console.log('humanhelp runtime-preflight fixture');\n"
             :encoding "UTF-8")
       (browser-build/record-generated-artifact!
        browser-assembly
        artifact-path)
       (with-redefs [humanhelp/browser-artifact-path artifact-path]
         (f {:artifact-path artifact-path
             :application-assembly
             (humanhelp/require-application-assembly!)}))))))

(defn- contributes-module?
  [module]
  (boolean
   (some
    #(identical? module %)
    humanhelp/modules)))

(deftest application-entrypoint-assembles-test
  (testing "the top-level Ring application is assembled without starting runtime components"
    (is (vector? humanhelp/modules))
    (is (seq humanhelp/modules))
    (is (vector? humanhelp/routes))
    (is (seq humanhelp/routes))
    (is (ifn? humanhelp/handler)))

  (testing "the global /app module is the production-model-backed example proving application"
    (is (identical? example/module app/module)))

  (testing "the example proving module is structurally ready to become the global /app surface"
    (is (map? example/module))
    (is (vector? (:routes example/module)))
    (is (seq (:routes example/module)))
    (is (vector? (:live-rules example/module)))
    (is (seq (:live-rules example/module))))

  (testing "the application modules required by the top-level entrypoint are registered"
    (is (contributes-module? app/module))
    (is (contributes-module? client-plumbing/module))
    (is (contributes-module? home/module))
    (is (contributes-module? schema/module)))

  (testing "the initial Biff system points at the assembled application boundaries"
    (is (identical?
         #'humanhelp/handler
         (:biff.ring/handler
          humanhelp/initial-system)))

    (is (identical?
         #'humanhelp/malli-opts
         (:biff/malli-opts
          humanhelp/initial-system)))

    (is (identical?
         #'ui/on-error
         (:biff.ring/on-error
          humanhelp/initial-system)))))

(deftest application-model-transaction-handler-is-installed-test
  (testing "the active application installs the one Gesso model transaction effect"
    (let [contributing-modules
          (filterv
           #(contains?
             (:biff.fx/handlers %)
             model.tx/transact-effect)
           humanhelp/modules)

          handlers
          (apply
           merge
           {}
           (keep
            :biff.fx/handlers
            humanhelp/modules))]

      (is (= 1
             (count contributing-modules))
          "Authentication and every active UI surface must share one application-wide model transaction boundary.")

      (is (identical?
           model.tx/transact!
           (get handlers model.tx/transact-effect))
          "An FX machine must execute Gesso model transactions rather than returning an unhandled effect vector as ordinary data."))))

(deftest application-live-rules-assemble-test
  (testing "Live rules are collected from the registered application modules"
    (let [expected-rules
          (vec
           (mapcat
            :live-rules
            humanhelp/modules))

          actual-rules
          (humanhelp/gesso-live-rules)]
      (is (vector? actual-rules))
      (is (= expected-rules
             actual-rules))))

  (testing "registered modules either omit Live rules or contribute a sequential rule collection"
    (is (every?
         #(or
           (nil? (:live-rules %))
           (sequential? (:live-rules %)))
         humanhelp/modules))))

(deftest runtime-component-order-is-explicit-test
  (testing "HumanHelp starts only config, XTDB, Gesso Live, and Aleph"
    (is (vector? humanhelp/components))
    (is (= [biff.config/use-aero-config
            biff.xtdb/use-xtdb
            humanhelp/use-gesso-live
            humanhelp/use-aleph]
           humanhelp/components))))

(deftest start-delegates-to-the-canonical-gesso-application-boundary-test
  (let [assembly ::application-assembly
        started-system
        {:biff.ring/base-url "http://preflight.test"
         ::started true}
        call (atom nil)
        raw-biff-start-called? (atom false)
        prior-system @humanhelp/system]
    (try
      (with-redefs [humanhelp/require-application-assembly!
                    (fn [] assembly)
                    application-preflight/start-biff-application!
                    (fn [actual-assembly initial-system modules-var components]
                      (reset! call
                              {:application-assembly actual-assembly
                               :initial-system initial-system
                               :modules-var modules-var
                               :components components})
                      started-system)
                    biff.core/start
                    (fn [& _]
                      (reset! raw-biff-start-called? true)
                      (throw
                       (ex-info
                        "HumanHelp must not bypass Gesso with raw biff.core/start."
                        {})))]
        (is (= started-system (humanhelp/start)))
        (is (= assembly (:application-assembly @call)))
        (is (= humanhelp/initial-system (:initial-system @call)))
        (is (identical? #'humanhelp/modules (:modules-var @call)))
        (is (= humanhelp/components (:components @call)))
        (is (= started-system @humanhelp/system))
        (is (false? @raw-biff-start-called?)))
      (finally
        (reset! humanhelp/system prior-system)))))

(deftest top-level-application-assembly-is-the-current-production-humanhelp-assembly-test
  (with-stamped-humanhelp-application
   (fn [{:keys [artifact-path application-assembly]}]
     (is (application-preflight/application-assembly? application-assembly))
     (is (= artifact-path
            (:browser-artifact-path application-assembly)))
     (is (= example.application-preflight/request-operations
            (set
             (keys
              (get-in application-assembly
                      [:operation-acquisition-assembly
                       :execution-assembly
                       :route-assembly
                       :operation-assembly
                       :operations]))))))))

(deftest current-no-progression-claim-render-fails-closed-before-malformed-hiccup-exists-test
  (let [ctx
        (live/with-optimistic-browser-plans
         {:anti-forgery-token "test-token"}
         request.choreo/browser-plans)

        failure
        (thrown-data
         #(request-card/action-button
           ctx
           open-row
           claim-affordance
           "#humanhelp-board-state"))]
    (testing "Claim keeps its semantic identity when no optimistic basis can be justified"
      (is (= :gesso.live.ui/optimistic-error
             (:error/type failure)))
      (is (= :missing-optimistic-binding
             (:error/kind failure)))
      (is (= request.choreo/claim-operation
             (:operation failure))))

    (testing "the malformed anonymous Claim POST can no longer be constructed"
      (is (map? failure)))))

(deftest aleph-request-boundary-orders-browser-minimum-before-authoritative-frontier-test
  (let [captured-handler (atom nil)
        events (atom [])
        handler
        (fn [ctx]
          (swap! events conj
                 [:handler
                  (::browser-progression-bound? ctx)
                  (::authoritative-frontier-bound? ctx)])
          {:status 204})]
    (with-redefs [aleph/start-server
                  (fn [actual-handler _opts]
                    (reset! captured-handler actual-handler)
                    ::test-server)

                  live/bind-request-progression
                  (fn [ctx]
                    (swap! events conj :browser-progression)
                    (assoc ctx ::browser-progression-bound? true))

                  live.xtdb/bind-request-frontier
                  (fn [ctx]
                    (swap! events conj
                           [:authoritative-frontier
                            (::browser-progression-bound? ctx)])
                    (assoc ctx ::authoritative-frontier-bound? true))]
      (humanhelp/use-aleph
       {:biff.ring/handler handler
        :biff.ring/port 8080
        ::system-value :system})

      (is (ifn? @captured-handler))
      (is (= {:status 204}
             (@captured-handler
              {:request-method :get
               :uri "/app"
               ::request-value :request})))
      (is (= [:browser-progression
              [:authoritative-frontier true]
              [:handler true true]]
             @events)
          "The untrusted browser minimum must be decoded before the trusted XTDB frontier, and both must precede rendering."))))

(deftest normal-authoritative-get-binds-one-frontier-that-realizes-claim-test
  (let [frontier
        (live.xtdb/basis
         :xtdb
         676
         (Instant/parse "2026-09-10T22:30:00Z"))

        snapshot-token
        (live.xtdb/basis-snapshot-token frontier)

        captured-handler
        (atom nil)

        frontier-calls
        (atom [])

        handled-ctx
        (atom nil)

        rendered
        (atom nil)

        handler
        (fn [ctx]
          (reset! handled-ctx ctx)
          (let [render-ctx
                (-> ctx
                    (assoc :anti-forgery-token "test-token")
                    (live/with-optimistic-browser-plans
                     request.choreo/browser-plans))

                action
                (request-card/action-button
                 render-ctx
                 open-row
                 claim-affordance
                 "#humanhelp-board-state")]
            (reset! rendered action)
            {:status 200
             :body action}))]

    (with-redefs [aleph/start-server
                  (fn [actual-handler _opts]
                    (reset! captured-handler actual-handler)
                    ::test-server)

                  live.xtdb/latest-completed-basis
                  (fn [ctx database]
                    (swap! frontier-calls conj
                           {:ctx ctx
                            :database database})
                    frontier)]
      (humanhelp/use-aleph
       {:biff.ring/handler handler
        :biff.ring/port 8080
        :biff.xtdb/node ::trusted-node
        ::system-value :system})

      (let [response
            (@captured-handler
             {:request-method :get
              :uri "/app"
              :headers {}
              ::request-value :request})

            bound
            @handled-ctx

            claim-node
            @rendered

            button
            (nth claim-node 3)

            button-attrs
            (second button)

            action
            (edn/read-string
             (:data-gesso-live-optimistic button-attrs))

            affordances
            (live.ui/rendered-choreo-affordances claim-node)

            wrapped-body
            ((:biff.core/wrap-db-snapshot bound)
             (fn [ctx]
               (:biff.xtdb/snapshot-token ctx)))]

        (testing "the real v676 boundary joins system and request context before observing one authoritative frontier"
          (is (= 200 (:status response)))
          (is (= 1 (count @frontier-calls)))
          (is (= :system
                 (get-in @frontier-calls [0 :ctx ::system-value])))
          (is (= :request
                 (get-in @frontier-calls [0 :ctx ::request-value])))
          (is (nil? (get-in @frontier-calls [0 :database]))))

        (testing "Biff snapshot and Gesso progression name the same authoritative observation"
          (is (= snapshot-token
                 (:biff.xtdb/snapshot-token bound)))
          (is (= frontier
                 (board/observed-basis bound)))
          (is (= snapshot-token
                 (wrapped-body {}))))

        (testing "the authoritative GET context is sufficient for the real Claim constructor"
          (is (= [{:gesso.live.ui/type :gesso.live.ui/rendered-choreo-affordance
                   :gesso.live.ui/version 1
                   :kind :post-button
                   :operation request.choreo/claim-operation
                   :plan-key request.choreo/claim-operation
                   :method :post
                   :path (example.routes/claim-request-url request-id)
                   :render-path [3]}]
                 affordances))
          (is (= request.choreo/claim-operation
                 (:operation action)))
          (is (= request.choreo/claim-operation
                 (:plan-key action)))
          (is (= frontier
                 (:observed-basis action)))
          (is (= {:request-id request-id}
                 (:arguments action)))
          (is (= (example.routes/claim-request-url request-id)
                 (:hx-post button-attrs)))
          (is (not (contains? action :command-id))
              "The server render must not fabricate the browser-owned command identity.")
          (is (not (contains? action :execution-id))
              "The server render must not fabricate the browser-owned execution identity."))))))

(deftest bypassing-authoritative-frontier-at-http-boundary-restores-the-expected-claim-failure-test
  (let [captured-handler (atom nil)
        handler
        (fn [ctx]
          (request-card/action-button
           (-> ctx
               (assoc :anti-forgery-token "test-token")
               (live/with-optimistic-browser-plans
                request.choreo/browser-plans))
           open-row
           claim-affordance
           "#humanhelp-board-state"))]
    (with-redefs [aleph/start-server
                  (fn [actual-handler _opts]
                    (reset! captured-handler actual-handler)
                    ::test-server)

                  live.xtdb/bind-request-frontier
                  identity]
      (humanhelp/use-aleph
       {:biff.ring/handler handler
        :biff.ring/port 8080
        :biff.xtdb/node ::trusted-node})

      (let [failure
            (thrown-data
             #(@captured-handler
               {:request-method :get
                :uri "/app"
                :headers {}}))]
        (is (= :gesso.live.ui/optimistic-error
               (:error/type failure)))
        (is (= :missing-optimistic-binding
               (:error/kind failure)))
        (is (= request.choreo/claim-operation
               (:operation failure)))))))

(deftest application-preflight-remains-a-backstop-against-synthetic-claim-downgrade-test
  (with-stamped-humanhelp-application
   (fn [{:keys [application-assembly]}]
     (let [claim-path
           (example.routes/claim-request-url request-id)

           ;; This Hiccup deliberately simulates downstream code bypassing the
           ;; corrected Request-card constructor and erasing Claim's semantic
           ;; identity while retaining its physical semantic-operation route.
           rendered
           [:button {:type "button"
                     :hx-post claim-path
                     :hx-swap "none"}
            "Claim"]

           report
           (application-preflight/check-rendered-surface
            application-assembly
            :net.humanhelp-test/synthetic-claim-downgrade
            rendered)

           semantic-downgrade
           (some
            #(when (= :rendered-semantic-route-without-choreo-operation
                      (:kind %))
               %)
            (:errors report))

           response-rendered? (atom false)
           failure
           (thrown-data
            #(application-preflight/checked-rendered-response!
              application-assembly
              :net.humanhelp-test/synthetic-claim-downgrade
              (fn [_]
                (reset! response-rendered? true)
                {:status 200})
              rendered))]
       (testing "the canonical HumanHelp assembly recognizes the anonymous POST as degraded Claim"
         (is (false? (:valid? report)))
         (is (= #{request.choreo/claim-operation}
                (:candidate-operations semantic-downgrade)))
         (is (= :post (:method semantic-downgrade)))
         (is (= claim-path (:path semantic-downgrade))))

       (testing "the application-wide guard still rejects bypassed downgrade before serialization"
         (is (= :gesso.live.application-preflight/error
                (:error/type failure)))
         (is (= :rendered-surface-preflight-failed
                (:error/kind failure)))
         (is (false? @response-rendered?)))))))
