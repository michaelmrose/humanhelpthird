(ns net.humanhelp.example.optimistic-test
  "Contract tests for the example app's production-backed optimistic boundary.

   The example app is a proving surface for the production HumanHelp models and
   Gesso Choreo. These tests therefore reject any reintroduction of the obsolete
   example model, prove that authenticated application identity is canonicalized
   to the production User/model authority context, verify that HTTP route binding
   fails closed against operation/Request substitution, and execute a real
   production Request Choreo registry entry through the example boundary."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.core :as live]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.progression :as progression]
   [gesso.model.command :as command]
   [net.humanhelp.example.optimistic :as optimistic]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain :as request.domain]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Stable fixtures
;; =============================================================================

(def request-id
  (UUID/fromString "10000000-0000-0000-0000-000000000101"))

(def organization-id
  (UUID/fromString "20000000-0000-0000-0000-000000000101"))

(def location-id
  (UUID/fromString "30000000-0000-0000-0000-000000000101"))

(def requestor-id
  (UUID/fromString "40000000-0000-0000-0000-000000000101"))

(def helper-id
  (UUID/fromString "40000000-0000-0000-0000-000000000102"))

(def other-helper-id
  (UUID/fromString "40000000-0000-0000-0000-000000000103"))

(def assignment-id
  (UUID/fromString "50000000-0000-0000-0000-000000000101"))

(def t0
  (Instant/parse "2026-09-03T20:00:00Z"))

(def t1
  (Instant/parse "2026-09-03T20:01:00Z"))

(def t2
  (Instant/parse "2026-09-03T20:02:00Z"))

(def observed-basis
  (xtdb-live/basis 90 t0))

(def committed-basis
  (xtdb-live/basis 91 t2))

(def committed-progression
  (progression/requirement committed-basis))

(def command-id
  (identity/command-id "humanhelp-example-production-command"))

(def execution-id
  (identity/execution-id "humanhelp-example-production-execution"))

(defn- claimed-request
  []
  (let [open-request
        (command/after
         (request.domain/create-request-command
          {:id request-id
           :organization-id organization-id
           :location-id location-id
           :requestor (request.domain/user-requestor requestor-id)
           :content
           {:title "Need help"
            :details "Production Request exercised through example app."
            :location-detail "Front desk"}
           :now t0}))]
    (command/after
     (request.domain/claim-request-command
      open-request
      {:now t1}))))

(defn- primary-assignment
  []
  (command/after
   (request.domain/create-assignment-command
    {:id assignment-id
     :request-id request-id
     :helper-id helper-id
     :role :primary
     :source :request/claim
     :actor-id helper-id
     :now t1})))

(defn- committed-claim-result
  []
  {:request (claimed-request)
   :primary-assignment (primary-assignment)
   :commit/status :committed
   :progression committed-progression})

(defn- claim-command
  ([]
   (claim-command {}))
  ([overrides]
   (protocol/command
    (merge
     {:command-id command-id
      :execution-id execution-id
      :operation request.choreo/claim-operation
      :arguments {:request-id request-id}
      :observed-basis observed-basis
      :scope [:request request-id]
      :fact-versions {:request/revision 0}}
     overrides))))

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

;; =============================================================================
;; Architecture / exact production registry
;; =============================================================================

(deftest example-optimistic-boundary-does-not-depend-on-the-example-model-test
  (let [dependencies
        (->> (ns-aliases 'net.humanhelp.example.optimistic)
             vals
             (map ns-name)
             set)]
    (is (not (contains? dependencies
                        'net.humanhelp.example.model)))
    (is (not-any?
         #(str/starts-with? (str %) "net.humanhelp.example.model")
         dependencies))))

(deftest example-server-reuses-the-exact-production-request-registry-test
  (let [expected
        #{request.choreo/claim-operation
          request.choreo/unclaim-operation
          request.choreo/mark-on-the-way-operation
          request.choreo/complete-operation
          request.choreo/cancel-operation
          request.choreo/reassign-operation}]
    (is (identical? request.choreo/operation-entries
                    optimistic/operation-entries)
        "The example must not wrap or rebuild the production trusted registry.")
    (is (= request.choreo/operation-entries
           optimistic/operation-entries))
    (is (= expected
           optimistic/supported-operations))
    (is (not (contains? optimistic/supported-operations
                        :request/take-over)))
    (is (not (contains? optimistic/supported-operations
                        :request/done)))))

;; =============================================================================
;; Authenticated production identity
;; =============================================================================

(deftest authenticated-user-id-canonicalizes-uuid-identity-test
  (testing "a UUID application identity remains the same production User id"
    (is (= helper-id
           (optimistic/authenticated-user-id
            {:current-user/id helper-id}))))

  (testing "the canonical string emitted by client-plumbing is parsed back to UUID"
    (is (= helper-id
           (optimistic/authenticated-user-id
            {:session {:uid (str helper-id)}}))))

  (testing "a malformed higher-precedence identity fails closed instead of falling through"
    (let [error
          (thrown
           #(optimistic/authenticated-user-id
             {:current-user/id "not-a-uuid"
              :session {:uid (str helper-id)}}))]
      (is (instance? clojure.lang.ExceptionInfo error))
      (is (= :net.humanhelp.example.optimistic/invalid-user-id
             (:error/type (ex-data error))))
      (is (= "not-a-uuid"
             (:user/id (ex-data error))))))

  (testing "absence of authenticated identity remains the app authentication error"
    (is (= :net.humanhelp.client-plumbing/missing-user-id
           (error-type
            #(optimistic/authenticated-user-id {}))))))

(deftest production-context-binds-choreo-principal-and-request-actor-to-one-user-test
  (let [reads (atom [])
        ctx {:session {:uid (str helper-id)}
             :request/sentinel :preserved}
        result
        (with-redefs
         [user/require-user
          (fn [read-ctx user-id]
            (swap! reads conj [read-ctx user-id])
            {:xt/id user-id
             :user/status :active})]
          (optimistic/production-context ctx))]
    (is (= [[ctx helper-id]] @reads))
    (is (= helper-id (:current-user/id result)))
    (is (= :preserved (:request/sentinel result)))
    (is (= (:session ctx) (:session result))))

  (testing "a production User lookup failure is not replaced by a weaker identity path"
    (let [missing
          (ex-info "missing user" {:error/type :user/not-found})]
      (with-redefs
       [user/require-user
        (fn [_ctx _user-id]
          (throw missing))]
        (is (identical?
             missing
             (thrown
              #(optimistic/production-context
                {:current-user/id helper-id})))))))

  (testing "the production document must agree on a UUID model identity"
    (with-redefs
     [user/require-user
      (fn [_ctx _user-id]
        {:xt/id "not-a-uuid"})]
      (is (= :net.humanhelp.example.optimistic/invalid-production-user
             (error-type
              #(optimistic/production-context
                {:current-user/id helper-id})))))))

;; =============================================================================
;; Route binding / forgery rejection
;; =============================================================================

(deftest request-route-binding-accepts-only-the-selected-production-command-test
  (let [command (claim-command)
        expected {:operation request.choreo/claim-operation
                  :request-id request-id}]
    (is (identical?
         command
         (optimistic/require-request-command! command expected))))

  (testing "the route itself may expose only a registered production operation"
    (is (= :net.humanhelp.example.optimistic/unsupported-route-operation
           (error-type
            #(optimistic/require-request-command!
              (claim-command)
              {:operation :request/take-over
               :request-id request-id})))))

  (testing "a browser cannot substitute another registered operation"
    (is (= :net.humanhelp.example.optimistic/route-operation-mismatch
           (error-type
            #(optimistic/require-request-command!
              (claim-command)
              {:operation request.choreo/cancel-operation
               :request-id request-id})))))

  (testing "a browser cannot retarget a route to another Request"
    (is (= :net.humanhelp.example.optimistic/route-request-mismatch
           (error-type
            #(optimistic/require-request-command!
              (claim-command)
              {:operation request.choreo/claim-operation
               :request-id
               (UUID/fromString
                "10000000-0000-0000-0000-000000000199")})))))

  (testing "the HTTP boundary expects an already decoded command map"
    (is (= :net.humanhelp.example.optimistic/invalid-command
           (error-type
            #(optimistic/require-request-command!
              "wire-not-decoded"
              {:operation request.choreo/claim-operation
               :request-id request-id}))))))

;; =============================================================================
;; Production Choreo execution through the example boundary
;; =============================================================================

(deftest run-command-executes-the-production-request-choreography-with-authenticated-model-context-test
  (let [user-reads (atom [])
        request-calls (atom [])
        original-ctx
        {:session {:uid (str helper-id)}
         :request/sentinel :example-http-context}
        prepared
        (with-redefs
         [user/require-user
          (fn [ctx user-id]
            (swap! user-reads conj [ctx user-id])
            {:xt/id user-id
             :user/status :active})

          request/claim
          (fn [ctx arguments]
            (swap! request-calls conj [ctx arguments])
            (committed-claim-result))]
          (optimistic/run-command
           original-ctx
           (claim-command)))
        settlement (:settlement prepared)
        authoritative (:authoritative settlement)
        [model-ctx model-arguments] (first @request-calls)]

    (testing "the production User is resolved inside one Biff FX runtime context"
      (is (= 1 (count @user-reads)))
      (let [[user-ctx user-id] (first @user-reads)]
        (is (= helper-id user-id))
        (is (= (:session original-ctx) (:session user-ctx)))
        (is (= :example-http-context (:request/sentinel user-ctx)))
        (is (instance? Instant (:biff.fx/now user-ctx)))
        (is (integer? (:biff.fx/seed user-ctx)))
        (is (= (:biff.fx/now user-ctx) (:biff.fx/now model-ctx)))
        (is (= (:biff.fx/seed user-ctx) (:biff.fx/seed model-ctx)))))

    (testing "the public Request operation sees the canonical authenticated actor"
      (is (= 1 (count @request-calls)))
      (is (= helper-id (:current-user/id model-ctx)))
      (is (= :example-http-context
             (:request/sentinel model-ctx)))
      (is (= {:request-id request-id}
             model-arguments))
      (is (not (contains? model-arguments :principal)))
      (is (not (contains? model-arguments :observed-basis))))

    (testing "execution is the production Gesso prepared-send contract"
      (is (live/optimistic-prepared-send? prepared))
      (is (= request.choreo/claim-operation
             (:operation prepared))))

    (testing "semantic and execution identity remain browser correlation only"
      (is (= command-id (:command-id settlement)))
      (is (= execution-id (:execution-id settlement))))

    (testing "only committed production authority confirms the operation"
      (is (= :confirmed (:resolution settlement)))
      (is (= :request/claimed (:outcome settlement)))
      (is (= :authoritative (:authority authoritative)))
      (is (= :present (:presence authoritative)))
      (is (= committed-basis (:basis authoritative)))
      (is (not= observed-basis (:basis authoritative))))

    (testing "the authoritative projection comes from the production Request aggregate"
      (is (= request-id
             (get-in authoritative [:projection :request/id])))
      (is (= :claimed
             (get-in authoritative [:projection :request/status])))
      (is (= helper-id
             (get-in authoritative
                     [:projection
                      :request/primary-assignment
                      :request-assignment/helper]))))))

(deftest run-command-does-not-let-browser-arguments-become-the-authenticated-principal-test
  (let [request-calls (atom [])
        command
        (claim-command
         {:arguments
          {:request-id request-id
           :helper-id other-helper-id}})]
    (with-redefs
     [user/require-user
      (fn [_ctx user-id]
        {:xt/id user-id
         :user/status :active})

      request/claim
      (fn [ctx arguments]
        (swap! request-calls conj [ctx arguments])
        (committed-claim-result))]
      (optimistic/run-command
       {:current-user/id helper-id}
       command))

    (let [[model-ctx arguments] (first @request-calls)]
      (is (= helper-id (:current-user/id model-ctx))
          "Authenticated principal/model actor comes only from trusted ctx.")
      (is (= other-helper-id (:helper-id arguments))
          "Operation-specific helper-id remains an untrusted semantic argument for request.core policy to authorize.")
      (is (not= (:current-user/id model-ctx)
                (:helper-id arguments))
          "The example boundary must never conflate an operation target with the authenticated principal."))))
