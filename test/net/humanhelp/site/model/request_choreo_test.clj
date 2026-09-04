(ns net.humanhelp.site.model.request-choreo-test
  "Focused contract tests for HumanHelp's production Request claim choreography.

   These tests sit above request.core/claim. They verify that the model-owned
   choreography is compiled from the intended Gesso optimistic program, that it
   remains dependent only on Request's public model boundary, and that the
   trusted authority adapter turns one committed Request claim into the exact
   protocol-v3 authoritative settlement expected by the browser runtime.

   Request policy itself is intentionally not duplicated here. Domain, FX, and
   request.core tests own authorization, aggregate planning, guards, and atomic
   persistence. This namespace owns the distributed seam that the example app
   will exercise directly."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.machine :as machine]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.server :as optimistic-server]
   [gesso.live.progression :as progression]
   [gesso.model.command :as command]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain :as request.domain])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed identities / times
;; =============================================================================

(def request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def organization-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000001"))

(def location-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000001"))

(def requestor-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000001"))

(def helper-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000002"))

(def assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000001"))

(def t0
  (Instant/parse
   "2026-09-03T00:00:00Z"))

(def t1
  (Instant/parse
   "2026-09-03T00:01:00Z"))

(def t2
  (Instant/parse
   "2026-09-03T00:02:00Z"))

(def command-id
  (identity/command-id
   "humanhelp-request-claim-command"))

(def execution-id
  (identity/execution-id
   "humanhelp-request-claim-execution"))

(def trusted-principal
  (identity/principal
   helper-id))

(def observed-basis
  (xtdb-live/basis
   40
   t0))

(def committed-basis
  (xtdb-live/basis
   41
   t2))

(def committed-progression
  (progression/requirement
   committed-basis))

(def trusted-ctx
  {:authenticated-principal trusted-principal
   :current-user/id helper-id
   :test/context :request-choreo})

;; =============================================================================
;; Canonical authoritative fixtures
;; =============================================================================

(defn- claimed-request
  []
  (let [open-request
        (command/after
         (request.domain/create-request-command
          {:id request-id
           :organization-id organization-id
           :location-id location-id
           :requestor
           (request.domain/user-requestor
            requestor-id)
           :content
           {:title "Need help"
            :details "Please help me find the right item."
            :location-detail "Near the front desk"}
           :now t0}))]
    (command/after
     (request.domain/claim-request-command
      open-request
      {:now t1}))))

(defn- primary-assignment
  ([]
   (primary-assignment
    request-id))
  ([request-id']
   (command/after
    (request.domain/create-assignment-command
     {:id assignment-id
      :request-id request-id'
      :helper-id helper-id
      :role :primary
      :source :request/claim
      :actor-id helper-id
      :now t1}))))

(defn- committed-claim-result
  ([]
   (committed-claim-result
    {}))
  ([overrides]
   (merge
    {:request
     (claimed-request)

     :primary-assignment
     (primary-assignment)

     :commit/status
     :committed

     :progression
     committed-progression}
    overrides)))

(defn- command-envelope
  ([]
   (command-envelope
    {}))
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

(defn- prepared-server
  []
  (optimistic-server/server
   {:principal-fn
    (fn [ctx]
      (:authenticated-principal
       ctx))

    :operations
    {request.choreo/claim-operation
     request.choreo/claim-operation-entry}}))

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      error)))

;; =============================================================================
;; Architecture / static artifacts
;; =============================================================================

(deftest request-choreo-depends-only-on-the-public-request-model-boundary-test
  (let [request-model-dependencies
        (->> (ns-aliases
              'net.humanhelp.site.model.request.choreo)
             vals
             (map ns-name)
             (filter
              #(str/starts-with?
                (str %)
                "net.humanhelp.site.model.request."))
             set)]
    (is (= '#{net.humanhelp.site.model.request.core}
           request-model-dependencies)
        "Request choreography must remain above request.core and never reach into domain/schema/graph/FX internals.")))

(deftest claim-static-artifacts-are-canonical-gesso-values-test
  (testing "the public application vocabulary remains stable"
    (is (= :request/claim
           request.choreo/claim-operation))
    (is (= :request/claim-optimistic
           request.choreo/claim-choreography-name))
    (is (= :helper
           request.choreo/helper-role))
    (is (= :request-authority
           request.choreo/request-authority-role)))

  (testing "both projected sides are executable plans produced before request execution"
    (is (machine/executable-plan?
         request.choreo/claim-browser-plan))
    (is (machine/executable-plan?
         request.choreo/claim-authority-plan))
    (is (= request.choreo/claim-authority-plan
           (:authority-plan
            request.choreo/claim-operation-entry))))

  (testing "the rendered affordance is inert while the server registry entry is trusted configuration"
    (is (capability/operation-capability?
         request.choreo/claim-capability))
    (is (= request.choreo/claim-operation
           (:operation
            request.choreo/claim-capability)))
    (is (= request.choreo/claim-plan-key
           (:plan-key
            request.choreo/claim-capability)))
    (is (optimistic-server/operation?
         request.choreo/claim-operation-entry))
    (is (= request.choreo/helper-role
           (:browser-role
            request.choreo/claim-operation-entry)))
    (is (= request.choreo/request-authority-role
           (:authority-role
            request.choreo/claim-operation-entry)))))

;; =============================================================================
;; Trusted Request authority execution
;; =============================================================================

(deftest committed-claim-becomes-confirmed-authoritative-protocol-v3-settlement-test
  (let [request-calls
        (atom [])

        authoritative-request
        (claimed-request)

        authoritative-assignment
        (primary-assignment)

        prepared
        (with-redefs
         [request/claim
          (fn [ctx arguments]
            (swap! request-calls
                   conj
                   [ctx arguments])
            (committed-claim-result
             {:request authoritative-request
              :primary-assignment authoritative-assignment}))]

          (optimistic-server/run-command
           (prepared-server)
           trusted-ctx
           (command-envelope)))

        settlement
        (:settlement
         prepared)

        authoritative
        (:authoritative
         settlement)]

    (testing "the choreography invokes exactly the public Request operation"
      (is (= [[trusted-ctx
               {:request-id request-id}]]
             @request-calls)
          "Observed basis, role, principal, protocol identity, and settlement data must not be smuggled into Request's semantic input."))

    (testing "the trusted operation completes as a prepared Gesso settlement send"
      (is (optimistic-server/prepared-send?
           prepared))
      (is (= :request/claim
             (:operation prepared))))

    (testing "semantic command and concrete execution identities survive unchanged"
      (is (= command-id
             (:command-id settlement)))
      (is (= execution-id
             (:execution-id settlement))))

    (testing "the model-owned continuation is confirmed only from committed authority"
      (is (= :confirmed
             (:resolution settlement)))
      (is (= :request/claimed
             (:outcome settlement)))
      (is (= :authoritative
             (:authority authoritative)))
      (is (= :present
             (:presence authoritative))))

    (testing "the authoritative basis comes from the commit, never the browser's older observation"
      (is (= committed-basis
             (:basis authoritative)))
      (is (not= observed-basis
                (:basis authoritative))))

    (testing "the authority projection contains only public Request facts required for reconciliation"
      (is (= {:request/id request-id
              :request/status :claimed
              :request/revision
              (request/revision
               authoritative-request)
              :request/primary-assignment
              {:request-assignment/id assignment-id
               :request-assignment/request request-id
               :request-assignment/helper helper-id
               :request-assignment/role :primary
               :request-assignment/status :active
               :request-assignment/source :request/claim
               :request-assignment/revision
               (request/assignment-revision
                authoritative-assignment)}}
             (:projection authoritative)))
      (is (= {:request/revision
              (request/revision
               authoritative-request)
              :request-assignment/revision
              (request/assignment-revision
               authoritative-assignment)}
             (:fact-versions authoritative))))))

(deftest browser-observed-basis-is-context-not-a-generic-stale-command-rule-test
  (let [calls
        (atom 0)

        much-older-basis
        (xtdb-live/basis
         1
         (Instant/parse
          "2026-01-01T00:00:00Z"))

        prepared
        (with-redefs
         [request/claim
          (fn [_ctx _arguments]
            (swap! calls inc)
            (committed-claim-result))]

          (optimistic-server/run-command
           (prepared-server)
           trusted-ctx
           (command-envelope
            {:observed-basis much-older-basis})))]

    (is (= 1
           @calls)
        "Gesso/HumanHelp choreography must reach current Request semantics instead of rejecting merely because authority advanced after rendering.")
    (is (= committed-basis
           (get-in prepared
                   [:settlement
                    :authoritative
                    :basis])))))

(deftest confirmed-settlement-fails-closed-without-transaction-established-progression-test
  (let [error
        (with-redefs
         [request/claim
          (fn [_ctx _arguments]
            (dissoc
             (committed-claim-result)
             :progression))]

          (thrown
           #(optimistic-server/run-command
             (prepared-server)
             trusted-ctx
             (command-envelope))))]

    (is (instance?
         clojure.lang.ExceptionInfo
         error))
    (is (= :net.humanhelp.site.model.request.choreo/error
           (:error/type
            (ex-data error))))
    (is (= :missing-commit-progression
           (:error/kind
            (ex-data error)))
        "A successful model mutation may not be presented as confirmed browser authority using an invented basis.")))

(deftest confirmed-settlement-rejects-an-incoherent-request-assignment-aggregate-test
  (let [other-request-id
        (UUID/fromString
         "10000000-0000-0000-0000-000000000099")

        error
        (with-redefs
         [request/claim
          (fn [_ctx _arguments]
            (committed-claim-result
             {:primary-assignment
              (primary-assignment
               other-request-id)}))]

          (thrown
           #(optimistic-server/run-command
             (prepared-server)
             trusted-ctx
             (command-envelope))))]

    (is (= :claim-result-aggregate-mismatch
           (:error/kind
            (ex-data error)))
        "The distributed boundary must fail closed rather than publish a Request and primary assignment belonging to different aggregates.")))

(deftest request-operation-errors-escape-the-choreography-unchanged-test
  (doseq [[label error]
          [[:pre-commit
            (ex-info
             "Request claim rejected before commit."
             {:error/type :request/not-authorized})]

           [:post-commit
            (ex-info
             "Request committed but Live delivery failed."
             {:error/type :gesso.live/post-commit-delivery-failure
              :commit/status :committed
              :progression committed-progression})]]]
    (testing
     (name label)
      (let [actual
            (with-redefs
             [request/claim
              (fn [_ctx _arguments]
                (throw error))]

              (thrown
               #(optimistic-server/run-command
                 (prepared-server)
                 trusted-ctx
                 (command-envelope))))]
        (is (identical?
             error
             actual)
            "Request Choreo must never manufacture a generic settlement that hides whether authoritative mutation committed.")))))
