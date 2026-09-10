(ns net.humanhelp.example.application-preflight-test
  "Cross-layer regression tests for HumanHelp's *production* application-preflight
   constructor.

   Earlier versions of this test namespace rebuilt a second browser/route/execution/
   acquisition assembly inside the test itself.  That was exactly the wrong thing to
   certify: the test could stay green while net.humanhelp.example.application-preflight
   drifted or remained unused.

   This namespace now treats net.humanhelp.example.application-preflight as the sole
   HumanHelp assembly owner.  Tests inspect products returned by that source namespace
   and adversarially remove source declarations to prove the production constructor
   itself fails closed.

   The application boundary frozen here is:

     production Request Choreo
       -> canonical HumanHelp browser declaration
       -> concrete HumanHelp lifecycle routes
       -> exact prepared optimistic server
       -> Request publication
       -> compiled HumanHelp Live graph
       -> authoritative toolbar/list acquisition.

   Physical generated-artifact currentness and runtime handler installation are later
   boundaries and are intentionally not reconstructed in this test namespace."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.acquisition-preflight :as acquisition-preflight]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition-preflight]
   [gesso.live.optimistic.execution-preflight :as execution-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [net.humanhelp.example.application-preflight :as application-preflight]
   [net.humanhelp.example.live :as app-live]
   [net.humanhelp.site.model.request.choreo :as request.choreo]))

(def expected-operations
  #{request.choreo/claim-operation
    request.choreo/unclaim-operation
    request.choreo/mark-on-the-way-operation
    request.choreo/complete-operation
    request.choreo/cancel-operation
    request.choreo/reassign-operation})

(def expected-board-fragments
  #{:request-toolbar
    :request-list})

(def expected-board-scopes
  #{:request-toolbar
    :request-list})

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

(deftest production-preflight-namespace-owns-the-browser-and-route-assembly-test
  (let [plan-registry
        (application-preflight/require-plan-registry!)

        browser-assembly
        (application-preflight/require-browser-assembly!)

        operation-assembly
        (application-preflight/require-operation-assembly!)

        route-assembly
        (application-preflight/require-route-assembly!)

        route-capabilities
        (application-preflight/request-route-capabilities)]
    (is (= expected-operations
           application-preflight/request-operations))
    (is (choreo-preflight/plan-registry? plan-registry))
    (is (browser-preflight/assembly-manifest? browser-assembly))
    (is (operation-preflight/operation-assembly? operation-assembly))
    (is (route-preflight/route-assembly? route-assembly))

    ;; The PlanRegistry exposed by HumanHelp is literally the registry embedded in
    ;; its one canonical compiled BrowserAssemblyManifest, not a separately rebuilt
    ;; test registry.
    (is (= plan-registry
           (:plan-registry browser-assembly)))
    (is (= request.choreo/browser-plans
           (get-in browser-assembly [:plan-registry :plans])))
    (is (= expected-operations
           (:required-plan-keys browser-assembly)))
    (is (= expected-operations
           (set (keys route-capabilities))))

    (doseq [[operation capability] route-capabilities]
      (testing (str operation " is realized as one semantic HumanHelp POST route")
        (is (= operation (:operation capability)))
        (is (= :post (:method capability)))
        (is (= #{:htmx} (:transports capability)))
        (is (re-matches #"/app/requests/:request-id/(claim|unclaim|mark-on-the-way|complete|cancel|reassign)"
                        (:path capability)))))))

(deftest production-preflight-constructor-closes-exact-server-publication-and-live-acquisition-test
  (let [execution-assembly
        (application-preflight/require-execution-assembly!)

        acquisition-assembly
        (application-preflight/require-acquisition-assembly!)

        operation-acquisition-assembly
        (application-preflight/require-operation-acquisition-assembly!)

        publication
        (execution-preflight/published-change-topics execution-assembly)

        obligations
        (acquisition-preflight/acquisition-obligations app-live/compiled-live)

        affected-fragments
        (operation-acquisition-preflight/affected-fragments
         operation-acquisition-assembly)

        affected-scopes
        (operation-acquisition-preflight/affected-scopes
         operation-acquisition-assembly)

        explanation
        (operation-acquisition-preflight/explain
         operation-acquisition-assembly)]
    (is (execution-preflight/execution-assembly? execution-assembly))
    (is (acquisition-preflight/acquisition-assembly? acquisition-assembly))
    (is (operation-acquisition-preflight/operation-acquisition-assembly?
         operation-acquisition-assembly))

    (is (= expected-operations
           (set (keys publication))))
    (is (= expected-board-fragments
           (set (keys obligations))))
    (is (= expected-board-fragments
           (set (keys (:realizations acquisition-assembly)))))
    (is (= expected-operations
           (:operations explanation)))
    (is (= {}
           (:unhandled-published-change-topics explanation)))

    (doseq [operation expected-operations]
      (testing (str operation " closes through Request publication and authoritative board reacquisition")
        (is (= #{:request}
               (get publication operation)))
        (is (= #{:request}
               (get (:published-change-topics explanation) operation)))
        (is (= expected-board-scopes
               (get affected-scopes operation)))
        (is (= expected-board-fragments
               (get affected-fragments operation)))
        (is (= expected-board-scopes
               (get (:affected-scopes explanation) operation)))
        (is (= expected-board-fragments
               (get (:affected-fragments explanation) operation)))))

    (doseq [[fragment obligation] obligations]
      (testing (str fragment " is derived from semantic Request publication")
        (is (= #{:request}
               (:change-topics obligation)))))))

(deftest deleting-a-production-request-operation-breaks-the-production-constructor-test
  (let [operation request.choreo/claim-operation
        error-data
        (with-redefs [request.choreo/operation-entries
                      (dissoc request.choreo/operation-entries operation)]
          (thrown-data
           application-preflight/require-operation-assembly!))]
    (is (map? error-data))
    (is (= :gesso.live.optimistic.preflight/error
           (:error/type error-data)))
    (is (= :operation-assembly-preflight-failed
           (:error/kind error-data)))
    (is (some #(and (= :missing-trusted-server-operation (:kind %))
                    (= operation (:operation %)))
              (get-in error-data [:preflight :errors])))))

(deftest deleting-a-humanhelp-route-breaks-the-production-constructor-test
  (let [operation request.choreo/claim-operation
        error-data
        (with-redefs [application-preflight/request-operation-route-ids
                      (dissoc application-preflight/request-operation-route-ids
                              operation)]
          (thrown-data
           application-preflight/require-route-assembly!))]
    (is (map? error-data))
    (is (= :gesso.live.optimistic.route-preflight/error
           (:error/type error-data)))
    (is (= :route-assembly-preflight-failed
           (:error/kind error-data)))
    (is (some #(and (= :missing-trusted-route (:kind %))
                    (= operation (:operation %)))
              (get-in error-data [:preflight :errors])))))

(deftest deleting-a-managed-fragment-realization-breaks-the-production-constructor-test
  (let [fragment :request-list
        error-data
        (with-redefs [application-preflight/request-fragment-route-ids
                      (dissoc application-preflight/request-fragment-route-ids
                              fragment)]
          (thrown-data
           application-preflight/require-acquisition-assembly!))]
    (is (map? error-data))
    (is (= :gesso.live.acquisition-preflight/error
           (:error/type error-data)))
    (is (= :authoritative-acquisition-assembly-failed
           (:error/kind error-data)))
    (is (some #(and (= :missing-acquisition-realization (:kind %))
                    (= fragment (:fragment %)))
              (get-in error-data [:preflight :errors])))))
