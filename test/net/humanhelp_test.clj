(ns net.humanhelp-test
  "Application-assembly smoke tests for HumanHelp.

   This namespace is intentionally small. Its first job is to require the real
   application entrypoint so namespace/API drift cannot hide behind narrower
   model tests. If net.humanhelp or one of its transitive application modules no
   longer compiles against the current Gesso/Biff stack, loading this test
   namespace must fail before any assertion runs.

   During the temporary HumanHelp proving-app phase, the global /app module is
   intentionally the production-model-backed example application. This is a
   deliberate integration boundary: the example UI is the active proving
   surface while production site.model.* namespaces remain the semantic source
   of truth.

   These tests do not start XTDB, Aleph, or Gesso Live. The heavier
   runtime/integration suites own those boundaries."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.config :as biff.config]
   [com.biffweb.xtdb :as biff.xtdb]
   [gesso.model.tx :as model.tx]
   [net.humanhelp :as humanhelp]
   [net.humanhelp.app :as app]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.app :as example]
   [net.humanhelp.home :as home]
   [net.humanhelp.schema :as schema]
   [net.humanhelp.ui :as ui]))

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
