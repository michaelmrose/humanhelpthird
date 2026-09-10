(ns net.humanhelp-test
  "Top-level runtime-preflight regression tests for HumanHelp.

   These tests freeze the two joins that were previously missing while the lower
   assembly/unit suite remained green:

     HumanHelp start
       -> canonical Gesso start-biff-application! boundary

   and, before the Claim formulation is repaired:

     actual no-progression Request-card Claim render
       -> current HumanHelp ApplicationAssembly
       -> Gesso dynamic rendered-surface preflight
       -> rejection before response serialization.

   The second test is intentionally a red-before-repair checkpoint for the
   current malformed Claim rendering.  It must be revised when HumanHelp stops
   emitting the degraded ordinary HTMX action; until then it proves the running
   application boundary can see the exact defect that formerly reached the
   browser and failed as a 500."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.config :as biff.config]
   [com.biffweb.xtdb :as biff.xtdb]
   [com.biffweb.core :as biff.core]
   [gesso.live.application-preflight :as application-preflight]
   [gesso.live.browser.build :as browser-build]
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

(deftest current-no-progression-claim-render-is-rejected-before-browser-serialization-test
  (with-stamped-humanhelp-application
   (fn [{:keys [application-assembly]}]
     (let [ctx
           (live/with-optimistic-browser-plans
            {:anti-forgery-token "test-token"}
            request.choreo/browser-plans)

           ;; This is the actual current HumanHelp Request-card rendering path.
           ;; There is deliberately no Gesso progression in ctx, so
           ;; board/optimistic-binding returns nil and the current component drops
           ;; :choreo/op while leaving its semantic Claim hx-post behind.
           rendered
           (request-card/action-button
            ctx
            open-row
            claim-affordance
            "#humanhelp-board-state")

           report
           (application-preflight/check-rendered-surface
            application-assembly
            :net.humanhelp-test/current-no-progression-claim
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
              :net.humanhelp-test/current-no-progression-claim
              (fn [_]
                (reset! response-rendered? true)
                {:status 200})
              rendered))]
       (testing "the current component really did produce the degraded form that caused the browser 500"
         (let [button (get rendered 3)
               attrs (second button)]
           (is (= :button (first button)))
           (is (= (str "/app/requests/" request-id "/claim")
                  (:hx-post attrs)))
           (is (nil? (get attrs live.ui/optimistic-action-attr)))))

       (testing "the real HumanHelp ApplicationAssembly rejects that degraded semantic route"
         (is (false? (:valid? report)))
         (is (= #{request.choreo/claim-operation}
                (:candidate-operations semantic-downgrade)))
         (is (= :post (:method semantic-downgrade)))
         (is (= (str "/app/requests/" request-id "/claim")
                (:path semantic-downgrade))))

       (testing "failure occurs before a response renderer can serialize the malformed button"
         (is (= :gesso.live.application-preflight/error
                (:error/type failure)))
         (is (= :rendered-surface-preflight-failed
                (:error/kind failure)))
         (is (false? @response-rendered?)))))))
