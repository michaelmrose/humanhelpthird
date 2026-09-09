(ns net.humanhelp.example.application-preflight-test
  "Cross-layer preflight regression tests for the HumanHelp example Request board.

   The production Request choreography owns semantic operation identity and trusted
   publication declarations.  The example application owns the concrete HTTP
   routes and Gesso Live projection.  These tests intentionally join those layers
   through Gesso's closed preflight products rather than maintaining a parallel
   HumanHelp operation -> fragment registry.

   The key invariant frozen here is:

     each exposed Request lifecycle operation
       -> declares semantic :request publication
       -> reaches the compiled example Live graph
       -> affects both Request board scopes/fragments
       -> has progression-safe authoritative fragment + stream reacquisition.

   This is verified application assembly relative to HumanHelp's trusted route and
   publication declarations; it is not a claim that arbitrary model code is
   machine-proved to emit those declarations at runtime."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.acquisition-preflight :as acquisition-preflight]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition-preflight]
   [gesso.live.optimistic.execution-preflight :as execution-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [net.humanhelp.example.live :as app-live]
   [net.humanhelp.example.optimistic :as optimistic]
   [net.humanhelp.example.routes :as routes]
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

(def request-operation-routes
  {request.choreo/claim-operation
   routes/claim-request-route

   request.choreo/unclaim-operation
   routes/unclaim-request-route

   request.choreo/mark-on-the-way-operation
   routes/mark-on-the-way-request-route

   request.choreo/complete-operation
   routes/complete-request-route

   request.choreo/cancel-operation
   routes/cancel-request-route

   request.choreo/reassign-operation
   routes/reassign-request-route})

(defn- request-route-capabilities
  []
  (into
   (sorted-map)
   (map
    (fn [[operation relative-path]]
      [operation
       (route-preflight/route-capability
        {:operation operation
         :method :post
         :path (routes/path relative-path)
         :transports #{:htmx}})]))
   request-operation-routes))

(defn- request-plan-registry
  []
  (choreo-preflight/require-plan-registry!
   {:name :net.humanhelp.example/request-plans
    :plans request.choreo/browser-plans
    :required-keys expected-operations
    :single-role? true
    :expected-role request.choreo/request-client-role}))

(defn- request-browser-assembly
  []
  (browser-preflight/require-browser-assembly!
   {:name :net.humanhelp.example/request-browser
    :plan-registry (request-plan-registry)
    :browser-role request.choreo/request-client-role
    :required-plan-keys expected-operations
    :optimistic? true
    :optimistic-htmx? true}))

(defn- request-operation-assembly
  []
  (operation-preflight/require-operation-assembly!
   {:name :net.humanhelp.example/request-operations
    :browser-assembly (request-browser-assembly)
    :operation-capabilities request.choreo/capabilities
    :server-operations request.choreo/operation-entries}))

(defn- request-route-assembly
  []
  (route-preflight/require-route-assembly!
   {:name :net.humanhelp.example/request-routes
    :operation-assembly (request-operation-assembly)
    :route-capabilities (request-route-capabilities)}))

(defn- request-execution-assembly
  []
  ;; This deliberately closes through net.humanhelp.example.optimistic's exact
  ;; private prepared server.  v655 introduced this seam specifically so
  ;; application preflight cannot construct a second server facade that merely
  ;; happens to resemble the one run-command uses.
  (optimistic/require-execution-assembly!
   (request-route-assembly)))

(defn- acquisition-realization
  [fragment fragment-url stream-url]
  (acquisition-preflight/require-acquisition-realization!
   {:name (keyword "net.humanhelp.example"
                   (str (name fragment) "-acquisition"))
    :live-app app-live/compiled-live
    :fragment fragment
    :fragment-route
    (acquisition-preflight/fragment-route
     {:fragment fragment
      :path fragment-url})
    :stream-route
    (acquisition-preflight/stream-route
     {:fragment fragment
      :path stream-url})}))

(defn- request-acquisition-assembly
  []
  (acquisition-preflight/require-acquisition-assembly!
   {:name :net.humanhelp.example/request-acquisition
    :live-app app-live/compiled-live
    :realizations
    {:request-toolbar
     (acquisition-realization
      :request-toolbar
      (routes/request-toolbar-fragment-url)
      (routes/request-toolbar-stream-url))

     :request-list
     (acquisition-realization
      :request-list
      (routes/request-list-fragment-url)
      (routes/request-list-stream-url))}}))

(defn- request-operation-acquisition-assembly
  []
  (operation-acquisition-preflight/require-operation-acquisition-assembly!
   {:name :net.humanhelp.example/request-operation-acquisition
    :execution-assembly (request-execution-assembly)
    :acquisition-assembly (request-acquisition-assembly)}))

(deftest every-production-request-operation-declares-the-semantic-request-publication-test
  (let [execution-assembly (request-execution-assembly)
        publication (execution-preflight/published-change-topics execution-assembly)]
    (is (= expected-operations
           (set (keys publication))))
    (doseq [operation expected-operations]
      (testing (str operation " publishes the one semantic Request topic")
        (is (= #{:request}
               (get publication operation)))))))

(deftest request-live-graph-derives-both-board-acquisition-obligations-test
  (let [acquisition-assembly (request-acquisition-assembly)
        obligations (acquisition-preflight/acquisition-obligations
                     app-live/compiled-live)]
    (is (= expected-board-fragments
           (set (keys obligations))))
    (is (= expected-board-fragments
           (set (keys (:realizations acquisition-assembly)))))
    (doseq [[fragment obligation] obligations]
      (testing (str fragment " is invalidated by semantic Request publication")
        (is (= #{:request}
               (:change-topics obligation)))))))

(deftest all-six-request-operations-close-through-live-to-authoritative-board-reacquisition-test
  (let [assembly (request-operation-acquisition-assembly)
        affected-fragments
        (operation-acquisition-preflight/affected-fragments assembly)
        affected-scopes
        (operation-acquisition-preflight/affected-scopes assembly)
        explanation
        (operation-acquisition-preflight/explain assembly)]
    (is (= expected-operations
           (:operations explanation)))
    (is (= {}
           (:unhandled-published-change-topics explanation)))
    (doseq [operation expected-operations]
      (testing (str operation " closes from trusted publication to both board acquisitions")
        (is (= #{:request}
               (get (:published-change-topics explanation) operation)))
        (is (= expected-board-scopes
               (get affected-scopes operation)))
        (is (= expected-board-fragments
               (get affected-fragments operation)))
        (is (= expected-board-scopes
               (get (:affected-scopes explanation) operation)))
        (is (= expected-board-fragments
               (get (:affected-fragments explanation) operation)))))))
