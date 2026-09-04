(ns net.humanhelp.example.app-test
  "HTTP-boundary tests for the example app's production Request lifecycle.

   The example application is now a proving surface for production HumanHelp
   Request Choreo. These tests keep the HTTP layer narrow: it may decode browser
   transport, bind a concrete route operation and Request UUID, delegate to
   example.optimistic, and render the trusted settlement. It must not revive the
   old example-model lifecycle, reinterpret authority, or permit route
   retargeting."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.core :as g]
   [gesso.choreo.identity :as identity]
   [gesso.live.core :as live]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.ui :as live.ui]
   [net.humanhelp.example.app :as app]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.optimistic :as optimistic]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.example.views :as views]
   [net.humanhelp.site.mock-data :as mock-data])
  (:import
   [java.util UUID]))

(def request-id
  (UUID/fromString "10000000-0000-0000-0000-000000000508"))

(def other-request-id
  (UUID/fromString "10000000-0000-0000-0000-000000000509"))

(def helper-id
  (UUID/fromString "40000000-0000-0000-0000-000000000508"))

(def command-id
  (identity/command-id "humanhelp-example-app-command-508"))

(def execution-id
  (identity/execution-id "humanhelp-example-app-execution-508"))

(def canonical-view-state
  {:search ""
   :created-order :newest
   :mine-first? false
   :unclaimed-first? false
   :show-terminal? false})

(def lifecycle-handlers
  [{:operation :request/claim
    :route-id routes/claim-request-id
    :handler app/claim-request!}
   {:operation :request/unclaim
    :route-id routes/unclaim-request-id
    :handler app/unclaim-request!}
   {:operation :request/mark-on-the-way
    :route-id routes/mark-on-the-way-request-id
    :handler app/mark-on-the-way-request!}
   {:operation :request/complete
    :route-id routes/complete-request-id
    :handler app/complete-request!}
   {:operation :request/cancel
    :route-id routes/cancel-request-id
    :handler app/cancel-request!}
   {:operation :request/reassign
    :route-id routes/reassign-request-id
    :handler app/reassign-request!}])

(defn- command
  ([operation]
   (command operation request-id))
  ([operation target-request-id]
   (protocol/command
    {:command-id command-id
     :execution-id execution-id
     :operation operation
     :arguments
     (cond->
      {:request-id target-request-id}
       (= operation :request/reassign)
       (assoc :helper-id helper-id))})))

(defn- encoded-command
  ([operation]
   (encoded-command operation request-id))
  ([operation target-request-id]
   (pr-str
    (protocol/command->wire
     (command operation target-request-id)))))

(defn- ctx
  [route-request-id encoded]
  {:path-params {:request-id (str route-request-id)}
   :form-params {app/optimistic-command-param encoded}})

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      error)))

(defn- error-type
  [f]
  (some-> (thrown f) ex-data :error/type))

(defn- with-lifecycle-shell
  "Run f with presentation/transport effects reduced to deterministic values.

   Route binding and optimistic command decoding remain real. run-command is
   replaced by f's caller when a test wants to observe delegation rather than
   execute production XTDB authority."
  [f]
  (with-redefs
   [board/normalize-view-state (fn [_] canonical-view-state)
    views/request-lifecycle-extras
    (fn [_ props]
      [:lifecycle-extra props])
    views/request-action-error
    (fn [props]
      [:lifecycle-error props])
    live.ui/optimistic-settlement-marker
    (fn [settlement]
      [:settlement-marker settlement])
    views/oob-response
    (fn [& nodes]
      (into [:oob] nodes))
    g/html-response identity
    live/complete-optimistic-send (fn [_] nil)]
   (f)))

(deftest handlers-register-the-production-request-lifecycle-test
  (is (= 6 (count lifecycle-handlers)))
  (doseq [{:keys [route-id handler]} lifecycle-handlers]
    (is (identical? handler (get app/handlers route-id))
        (str route-id " must route to its production lifecycle handler.")))
  (is (= #{routes/claim-request-id
           routes/unclaim-request-id
           routes/mark-on-the-way-request-id
           routes/complete-request-id
           routes/cancel-request-id
           routes/reassign-request-id}
         (set (map :route-id lifecycle-handlers)))
      "The lifecycle contract names only canonical production route identities."))

(deftest every-lifecycle-route-binds-exact-operation-and-request-id-test
  (doseq [{:keys [operation handler]} lifecycle-handlers]
    (testing (str operation)
      (let [seen (atom nil)
            settlement
            {:resolution :confirmed
             :outcome operation
             :authoritative
             {:projection {:request/id request-id}}}
            prepared {:settlement settlement
                      :prepared/sentinel operation}
            response
            (with-lifecycle-shell
              #(with-redefs
                [optimistic/run-command
                 (fn [request-ctx decoded-command]
                   (reset! seen [request-ctx decoded-command])
                   prepared)]
                (handler
                 (ctx request-id
                      (encoded-command operation)))))]
        (is (= operation
               (get-in @seen [1 :operation])))
        (is (= request-id
               (get-in @seen [1 :arguments :request-id])))
        (when (= operation :request/reassign)
          (is (= helper-id
                 (get-in @seen [1 :arguments :helper-id]))))
        (is (= [:oob
                [:settlement-marker settlement]
                [:lifecycle-extra
                 {:action operation
                  :request {:request/id request-id}
                  :view-state canonical-view-state}]]
               response))))))

(deftest lifecycle-route-rejects-operation-substitution-before-authority-test
  (let [run-count (atom 0)
        error
        (with-lifecycle-shell
          #(with-redefs
            [optimistic/run-command
             (fn [& _]
               (swap! run-count inc)
               (throw (ex-info "must not run" {})))]
            (thrown
             (fn []
               (app/claim-request!
                (ctx request-id
                     (encoded-command :request/cancel)))))))]
    (is (= 0 @run-count))
    (is (= :net.humanhelp.example.optimistic/route-operation-mismatch
           (:error/type (ex-data error))))
    (is (= :request/claim
           (:expected-operation (ex-data error))))
    (is (= :request/cancel
           (:actual-operation (ex-data error))))))

(deftest lifecycle-route-rejects-request-retargeting-before-authority-test
  (let [run-count (atom 0)
        error
        (with-lifecycle-shell
          #(with-redefs
            [optimistic/run-command
             (fn [& _]
               (swap! run-count inc)
               (throw (ex-info "must not run" {})))]
            (thrown
             (fn []
               (app/claim-request!
                (ctx request-id
                     (encoded-command :request/claim
                                      other-request-id)))))))]
    (is (= 0 @run-count))
    (is (= :net.humanhelp.example.optimistic/route-request-mismatch
           (:error/type (ex-data error))))
    (is (= request-id
           (:expected-request-id (ex-data error))))
    (is (= other-request-id
           (:actual-request-id (ex-data error))))))

(deftest malformed-route-request-id-fails-before-command-execution-test
  (let [decode-count (atom 0)
        run-count (atom 0)
        error
        (with-lifecycle-shell
          #(with-redefs
            [optimistic/decode-command
             (fn [wire]
               (swap! decode-count inc)
               wire)
             optimistic/run-command
             (fn [& _]
               (swap! run-count inc)
               (throw (ex-info "must not run" {})))]
            (thrown
             (fn []
               (app/claim-request!
                {:path-params {:request-id "not-a-uuid"}
                 :form-params
                 {app/optimistic-command-param
                  (encoded-command :request/claim)}})))))]
    (is (= 0 @decode-count))
    (is (= 0 @run-count))
    (is (= :humanhelp.example/invalid-request-id
           (:error/type (ex-data error))))))

(deftest missing-and-unreadable-command-transport-fail-before-authority-test
  (testing "missing command"
    (let [run-count (atom 0)
          error
          (with-lifecycle-shell
            #(with-redefs
              [optimistic/run-command
               (fn [& _]
                 (swap! run-count inc))]
              (thrown
               (fn []
                 (app/claim-request!
                  {:path-params
                   {:request-id (str request-id)}})))))]
      (is (= 0 @run-count))
      (is (instance? clojure.lang.ExceptionInfo error))))

  (testing "unreadable EDN"
    (let [run-count (atom 0)
          error
          (with-lifecycle-shell
            #(with-redefs
              [optimistic/run-command
               (fn [& _]
                 (swap! run-count inc))]
              (thrown
               (fn []
                 (app/claim-request!
                  (ctx request-id "[not valid edn"))))))]
      (is (= 0 @run-count))
      (is (instance? clojure.lang.ExceptionInfo error)))))

(deftest example-app-has-no-legacy-model-dependency-test
  (let [dependencies
        (->> (ns-aliases 'net.humanhelp.example.app)
             vals
             (map ns-name)
             set)]
    (is (not (contains? dependencies
                        'net.humanhelp.example.model))
        "The active example app must not load the retired parallel demo model.")))

(deftest settlement-resolution-is-presentation-only-at-the-http-boundary-test
  (testing "rejection renders feedback without constructing replacement authority"
    (let [settlement {:resolution :rejected
                      :reason :request/not-claimable}
          prepared {:settlement settlement}
          response
          (with-lifecycle-shell
            #(with-redefs
              [optimistic/run-command (fn [_ _] prepared)]
              (app/claim-request!
               (ctx request-id
                    (encoded-command :request/claim)))))]
      (is (= [:oob
              [:settlement-marker settlement]
              [:lifecycle-error
               {:result {:reason :request/not-claimable}}]]
             response))))

  (testing "unsupported settlement resolutions fail closed"
    (let [prepared {:settlement {:resolution :mystery}}
          error
          (with-lifecycle-shell
            #(with-redefs
              [optimistic/run-command (fn [_ _] prepared)]
              (thrown
               (fn []
                 (app/claim-request!
                  (ctx request-id
                       (encoded-command :request/claim)))))))]
      (is (instance? clojure.lang.ExceptionInfo error))
      (is (= :request/claim
             (:operation (ex-data error)))))))

;; =============================================================================
;; Production fixture middleware
;; =============================================================================

(deftest production-fixture-middleware-uses-post-ensure-context-test
  (let [incoming-ctx
        {:request/sentinel :incoming}

        committed-ctx
        {:request/sentinel :post-fixture-commit
         :gesso.live/progression {:fixture/progression :committed}}

        events
        (atom [])

        response
        (with-redefs
         [mock-data/ensure!
          (fn [actual-ctx]
            (swap! events conj [:ensure actual-ctx])
            {:organization-id mock-data/organization-id
             :locations mock-data/locations
             :created-count 4
             :transaction {:status :committed}
             :ctx committed-ctx})]
          ((app/wrap-production-fixtures
            (fn [actual-ctx]
              (swap! events conj [:handler actual-ctx])
              {:status 204
               :ctx actual-ctx}))
           incoming-ctx))]

    (is (= [[:ensure incoming-ctx]
            [:handler committed-ctx]]
           @events)
        "Fixture establishment must happen before the example handler, and the handler must receive the authoritative post-ensure ctx.")

    (is (= 204 (:status response)))
    (is (identical? committed-ctx (:ctx response))
        "The middleware must not discard or reconstruct mock-data/ensure!'s progression-aware ctx.")))

(deftest production-fixture-middleware-preserves-idempotent-context-test
  (let [incoming-ctx
        {:request/sentinel :already-initialized}

        seen
        (atom nil)

        response
        (with-redefs
         [mock-data/ensure!
          (fn [actual-ctx]
            {:organization-id mock-data/organization-id
             :locations mock-data/locations
             :created-count 0
             :transaction nil
             :ctx actual-ctx})]
          ((app/wrap-production-fixtures
            (fn [actual-ctx]
              (reset! seen actual-ctx)
              :handled))
           incoming-ctx))]

    (is (= :handled response))
    (is (identical? incoming-ctx @seen)
        "An already-initialized production fixture set must not manufacture a replacement request ctx.")))

(deftest production-fixture-middleware-fails-before-handler-when-fixtures-fail-test
  (let [handler-count
        (atom 0)

        fixture-error
        (ex-info
         "fixture conflict"
         {:error/type :mock-data/location-conflict})

        error
        (with-redefs
         [mock-data/ensure!
          (fn [_]
            (throw fixture-error))]
          (thrown
           (fn []
             ((app/wrap-production-fixtures
               (fn [_]
                 (swap! handler-count inc)
                 :must-not-run))
              {:request/sentinel :incoming}))))]

    (is (identical? fixture-error error)
        "Fixture authority failures must propagate unchanged rather than being converted into an example-app response.")
    (is (= 0 @handler-count)
        "The example handler must not run against missing or conflicting production fixtures.")))

