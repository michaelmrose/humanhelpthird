(ns net.humanhelp-test
  "Application-assembly smoke tests for HumanHelp.

   This namespace is intentionally small. Its first job is to require the real
   application entrypoint so namespace/API drift cannot hide behind narrower
   model tests. If net.humanhelp or one of its transitive application modules no
   longer compiles against the current Gesso/Biff stack, loading this test
   namespace must fail before any assertion runs.

   These tests do not start XTDB, Aleph, background workers, or Gesso Live. The
   heavier runtime/integration suites own those boundaries."
  (:require
   [clojure.test :refer [deftest is testing]]
   [net.humanhelp :as humanhelp]
   [net.humanhelp.app :as app]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.home :as home]
   [net.humanhelp.schema :as schema]
   [net.humanhelp.site.app :as site]
   [net.humanhelp.ui :as ui]
   [net.humanhelp.worker :as worker]))

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

  (testing "the production /app module is the real HumanHelp site module"
    (is (identical?
         site/module
         app/module)))

  (testing "the application modules required by the top-level entrypoint are registered"
    (is (contributes-module? app/module))
    (is (contributes-module? client-plumbing/module))
    (is (contributes-module? home/module))
    (is (contributes-module? schema/module))
    (is (contributes-module? worker/module)))

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

(deftest application-live-rules-assemble-test
  (testing "Live rules are collected from the registered production modules"
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

  (testing "an application with no production Live rules is a valid assembly state"
    ;; The removable example application is no longer the production /app
    ;; module. Until the real site explicitly contributes Live rules, an empty
    ;; collection is correct and must not be mistaken for an assembly failure.
    (is (every?
         #(or
           (nil? (:live-rules %))
           (sequential? (:live-rules %)))
         humanhelp/modules))))

(deftest runtime-component-order-is-explicit-test
  (testing "HumanHelp keeps Live between database/background setup and the Aleph server"
    (is (vector? humanhelp/components))
    (is (= humanhelp/use-gesso-live
           (nth humanhelp/components 4)))
    (is (= humanhelp/use-aleph
           (nth humanhelp/components 5)))
    (is (= 6
           (count humanhelp/components)))))
