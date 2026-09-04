(ns net.humanhelp.site.model.request-choreo-test
  "Focused contract tests for HumanHelp's production Request lifecycle choreography.

   These tests sit above the public Request lifecycle API. They verify that the
   model-owned choreographies are compiled from the intended Gesso optimistic
   programs, that they remain dependent only on Request's public model boundary,
   and that the
   trusted authority adapters turn committed Request lifecycle transitions into
   the exact protocol-v3 authoritative settlements expected by the browser
   runtime.

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

;; =============================================================================
;; Complete production Request lifecycle choreography
;; =============================================================================

(def helper-b-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000003"))

(def manager-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000004"))

(def replacement-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000002"))

(def collaborator-assignment-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000003"))

(defn- open-request
  []
  (command/after
   (request.domain/create-request-command
    {:id request-id
     :organization-id organization-id
     :location-id location-id
     :requestor
     (request.domain/user-requestor requestor-id)
     :content
     {:title "Need help"
      :details "Please help me find the right item."
      :location-detail "Near the front desk"}
     :now t0})))

(defn- on-the-way-request
  []
  (command/after
   (request.domain/mark-on-the-way-command
    (claimed-request)
    {:now t2})))

(defn- done-request
  []
  (command/after
   (request.domain/complete-request-command
    (on-the-way-request)
    {:now (.plusSeconds t2 60)})))

(defn- cancelled-request
  []
  (command/after
   (request.domain/cancel-request-command
    (claimed-request)
    {:now t2
     :reason :request/test-cancelled})))

(defn- active-assignment
  [{:keys [id role source] :as options}]
  (let [assignment-id' (or id assignment-id)
        assignment-helper-id (or (:helper-id options) helper-id)
        assignment-role (or role :primary)
        assignment-source (or source :request/claim)]
    (command/after
     (request.domain/create-assignment-command
      {:id assignment-id'
       :request-id request-id
       :helper-id assignment-helper-id
       :role assignment-role
       :source assignment-source
       :actor-id assignment-helper-id
       :now t1}))))

(defn- ended-assignment
  ([assignment]
   (ended-assignment assignment :request/test-ended))
  ([assignment reason]
   (command/after
    (request.domain/end-assignment-command
     assignment
     {:actor-id helper-id
      :reason reason
      :now t2}))))

(defn- committed-result
  [result]
  (merge
   {:commit/status :committed
    :progression committed-progression}
   result))

(defn- lifecycle-server
  []
  (optimistic-server/server
   {:principal-fn
    (fn [ctx]
      (:authenticated-principal ctx))
    :operations request.choreo/operation-entries}))

(defn- lifecycle-envelope
  [operation arguments suffix]
  (protocol/command
   {:command-id
    (identity/command-id
     (str "humanhelp-request-" suffix "-command"))
    :execution-id
    (identity/execution-id
     (str "humanhelp-request-" suffix "-execution"))
    :operation operation
    :arguments arguments
    :observed-basis observed-basis
    :scope [:request request-id]
    :fact-versions {:request/revision 1}}))

(defn- lifecycle-cases
  []
  (let [old-primary
        (active-assignment {})
        ended-primary
        (ended-assignment
         old-primary
         :request/test-ended-primary)
        old-collaborator
        (active-assignment
         {:id collaborator-assignment-id
          :helper-id helper-b-id
          :role :collaborator
          :source :request/collaborator-added})
        ended-collaborator
        (ended-assignment
         old-collaborator
         :request/test-ended-collaborator)
        replacement-primary
        (active-assignment
         {:id replacement-assignment-id
          :helper-id helper-b-id
          :role :primary
          :source :request/reassignment})]
    [{:label :unclaim
      :operation request.choreo/unclaim-operation
      :model-var #'request/unclaim
      :arguments {:request-id request-id}
      :result
      (committed-result
       {:request (open-request)
        :assignments [ended-primary]})
      :outcome :request/unclaimed
      :status :open
      :browser-role request.choreo/helper-role}

     {:label :mark-on-the-way
      :operation request.choreo/mark-on-the-way-operation
      :model-var #'request/mark-on-the-way
      :arguments {:request-id request-id}
      :result
      (committed-result
       {:request (on-the-way-request)})
      :outcome :request/on-the-way
      :status :on-the-way
      :browser-role request.choreo/helper-role}

     {:label :complete
      :operation request.choreo/complete-operation
      :model-var #'request/complete
      :arguments {:request-id request-id}
      :result
      (committed-result
       {:request (done-request)
        :assignments [ended-primary ended-collaborator]})
      :outcome :request/completed
      :status :done
      :browser-role request.choreo/helper-role}

     {:label :cancel
      :operation request.choreo/cancel-operation
      :model-var #'request/cancel
      :arguments {:request-id request-id
                  :reason :request/test-cancelled}
      :result
      (committed-result
       {:request (cancelled-request)
        :assignments [ended-primary]})
      :outcome :request/cancelled
      :status :cancelled
      :browser-role request.choreo/requestor-role}

     {:label :reassign
      :operation request.choreo/reassign-operation
      :model-var #'request/reassign
      :arguments {:request-id request-id
                  :helper-id helper-b-id}
      :result
      (committed-result
       {:request (claimed-request)
        :primary-assignment replacement-primary
        :previous-primary-assignment ended-primary
        :previous-collaborator-assignment ended-collaborator})
      :outcome :request/reassigned
      :status :claimed
      :browser-role request.choreo/manager-role
      :primary-helper helper-b-id}]))

(deftest lifecycle-static-artifacts-cover-the-production-request-board-test
  (let [expected-operations
        #{request.choreo/claim-operation
          request.choreo/unclaim-operation
          request.choreo/mark-on-the-way-operation
          request.choreo/complete-operation
          request.choreo/cancel-operation
          request.choreo/reassign-operation}
        expected-roles
        {request.choreo/claim-operation request.choreo/helper-role
         request.choreo/unclaim-operation request.choreo/helper-role
         request.choreo/mark-on-the-way-operation request.choreo/helper-role
         request.choreo/complete-operation request.choreo/helper-role
         request.choreo/cancel-operation request.choreo/requestor-role
         request.choreo/reassign-operation request.choreo/manager-role}]
    (testing "the public registries describe exactly the production lifecycle operations"
      (is (= expected-operations
             (set (keys request.choreo/capabilities))))
      (is (= expected-operations
             (set (keys request.choreo/browser-plans))))
      (is (= expected-operations
             (set (keys request.choreo/operation-entries))))
      (is (= expected-operations
             (set (keys request.choreo/authority-plans)))))

    (doseq [operation expected-operations]
      (testing (str operation " is represented only by canonical Gesso artifacts")
        (let [capability (get request.choreo/capabilities operation)
              browser-plan (get request.choreo/browser-plans operation)
              operation-entry (get request.choreo/operation-entries operation)
              authority-plan (get request.choreo/authority-plans operation)]
          (is (capability/operation-capability? capability))
          (is (= operation (:operation capability)))
          (is (= operation (:plan-key capability)))
          (is (machine/executable-plan? browser-plan))
          (is (optimistic-server/operation? operation-entry))
          (is (= (get expected-roles operation)
                 (:browser-role operation-entry)))
          (is (= request.choreo/request-authority-role
                 (:authority-role operation-entry)))
          (is (machine/executable-plan? authority-plan))
          (is (= authority-plan
                 (:authority-plan operation-entry))))))))

(deftest each-production-lifecycle-entry-invokes-only-its-public-request-operation-test
  (doseq [{:keys
           [label operation model-var arguments result outcome status
            browser-role primary-helper]}
          (lifecycle-cases)]
    (testing (name label)
      (let [calls (atom [])
            envelope (lifecycle-envelope operation arguments (name label))
            prepared
            (with-redefs-fn
              {model-var
               (fn [ctx actual-arguments]
                 (swap! calls conj [ctx actual-arguments])
                 result)}
              #(optimistic-server/run-command
                (lifecycle-server)
                trusted-ctx
                envelope))
            settlement (:settlement prepared)
            authoritative (:authoritative settlement)
            projection (:projection authoritative)]
        (is (= [[trusted-ctx arguments]]
               @calls)
            "Choreography protocol context must not leak into the semantic Request command.")
        (is (optimistic-server/prepared-send? prepared))
        (is (= operation (:operation prepared)))
        (is (= (:command-id envelope)
               (:command-id settlement)))
        (is (= (:execution-id envelope)
               (:execution-id settlement)))
        (is (= :confirmed (:resolution settlement)))
        (is (= outcome (:outcome settlement)))
        (is (= :authoritative (:authority authoritative)))
        (is (= :present (:presence authoritative)))
        (is (= committed-basis (:basis authoritative)))
        (is (not= observed-basis (:basis authoritative)))
        (is (= request-id (:request/id projection)))
        (is (= status (:request/status projection)))
        (is (= (request/revision (:request result))
               (:request/revision projection)))
        (is (= {:request/revision
                (request/revision (:request result))}
               (select-keys (:fact-versions authoritative)
                            [:request/revision])))
        (is (= browser-role
               (:browser-role
                (get request.choreo/operation-entries operation))))
        (if primary-helper
          (do
            (is (= primary-helper
                   (get-in projection
                           [:request/primary-assignment
                            :request-assignment/helper])))
            (is (= :active
                   (get-in projection
                           [:request/primary-assignment
                            :request-assignment/status])))
            (is (= :request/reassignment
                   (get-in projection
                           [:request/primary-assignment
                            :request-assignment/source])))
            (is (contains? (:fact-versions authoritative)
                           :request-assignment/revision)))
          (is (not (contains? projection
                              :request/primary-assignment))
              "Ended or absent assignments are not projected as current primary authority."))))))

(deftest lifecycle-operations-fail-closed-without-transaction-established-progression-test
  (doseq [{:keys [label operation model-var arguments result]}
          (lifecycle-cases)]
    (testing (name label)
      (let [error
            (with-redefs-fn
              {model-var
               (fn [_ctx _arguments]
                 (dissoc result :progression))}
              #(thrown
                (fn []
                  (optimistic-server/run-command
                   (lifecycle-server)
                   trusted-ctx
                   (lifecycle-envelope operation arguments (str (name label) "-no-progression"))))))]
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (= :net.humanhelp.site.model.request.choreo/error
               (:error/type (ex-data error))))
        (is (= :missing-commit-progression
               (:error/kind (ex-data error))))))))

(deftest lifecycle-operations-reject-authoritative-request-state-that-does-not-match-the-operation-test
  (doseq [{:keys [label operation model-var arguments result]}
          (lifecycle-cases)]
    (testing (name label)
      (let [wrong-request
            (if (= :open (request/status (:request result)))
              (claimed-request)
              (open-request))
            error
            (with-redefs-fn
              {model-var
               (fn [_ctx _arguments]
                 (assoc result :request wrong-request))}
              #(thrown
                (fn []
                  (optimistic-server/run-command
                   (lifecycle-server)
                   trusted-ctx
                   (lifecycle-envelope operation arguments (str (name label) "-wrong-state"))))))]
        (is (= :unexpected-authoritative-request-state
               (:error/kind (ex-data error))))))))

(deftest assignment-ending-lifecycle-results-reject-still-active-assignments-test
  (doseq [{:keys [label operation model-var arguments result]}
          (filter
           #(contains? #{:unclaim :complete :cancel} (:label %))
           (lifecycle-cases))]
    (testing (name label)
      (let [error
            (with-redefs-fn
              {model-var
               (fn [_ctx _arguments]
                 (assoc result
                        :assignments
                        [(active-assignment {})]))}
              #(thrown
                (fn []
                  (optimistic-server/run-command
                   (lifecycle-server)
                   trusted-ctx
                   (lifecycle-envelope operation arguments (str (name label) "-active-assignment"))))))]
        (is (= :unexpected-authoritative-assignment-state
               (:error/kind (ex-data error))))))))

(deftest reassign-requires-ended-old-primary-before-publishing-the-replacement-test
  (let [{:keys [operation model-var arguments result]}
        (first
         (filter #(= :reassign (:label %))
                 (lifecycle-cases)))
        error
        (with-redefs-fn
          {model-var
           (fn [_ctx _arguments]
             (assoc result
                    :previous-primary-assignment
                    (active-assignment {})))}
          #(thrown
            (fn []
              (optimistic-server/run-command
               (lifecycle-server)
               trusted-ctx
               (lifecycle-envelope operation arguments "reassign-active-old-primary")))))]
    (is (= :unexpected-authoritative-assignment-state
           (:error/kind (ex-data error))))))

(deftest every-lifecycle-operation-preserves-model-errors-exactly-test
  (doseq [{:keys [label operation model-var arguments]}
          (lifecycle-cases)
          [phase error]
          [[:pre-commit
            (ex-info
             "Request operation rejected before commit."
             {:error/type :request/not-authorized
              :operation operation})]
           [:post-commit
            (ex-info
             "Request committed but delivery failed."
             {:error/type :gesso.live/post-commit-delivery-failure
              :operation operation
              :commit/status :committed
              :progression committed-progression})]]]
    (testing (str (name label) " / " (name phase))
      (let [actual
            (with-redefs-fn
              {model-var
               (fn [_ctx _arguments]
                 (throw error))}
              #(thrown
                (fn []
                  (optimistic-server/run-command
                   (lifecycle-server)
                   trusted-ctx
                   (lifecycle-envelope operation arguments (str (name label) "-" (name phase)))))))]
        (is (identical? error actual)
            "Choreo must not reinterpret whether the authoritative Request transition committed.")))))

