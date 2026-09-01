(ns net.humanhelp.site.model.request-claim-test
  "Focused public-boundary tests for the authoritative Request claim operation.

   Request FX owns domain planning and all model policy. request.core/claim owns
   only the independently callable authoritative operation boundary needed by
   higher distributed layers:

     request.choreo
       -> request.core/claim
       -> request.fx/plan-claim-request
       -> gesso.model.tx/transact!

   These tests intentionally stub the planner and generic transaction boundary.
   The existing Request test corpus owns domain/authorization/guard correctness;
   this namespace pins the composition contract that Choreo will depend on."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.request.core :as request]))

(def sample-ctx
  {:current-user/id :helper-1
   :test/context :authoritative})

(def sample-input
  {:request-id :request-1
   :helper-id :helper-1})

(def sample-result
  {:request
   {:xt/id :request-1
    :request/status :claimed}

   :primary-assignment
   {:xt/id :assignment-1
    :request-assignment/request :request-1
    :request-assignment/helper :helper-1}})

(def sample-fragment
  {:assertions
   [{:assert :request-open}]

   :writes
   [{:write :claimed-request}
    {:write :primary-assignment}]

   :changes
   [{:topic :request
     :id :request-1
     :request/operation :claim}]})

(def sample-options
  {:emit :sync
   :entry-fn
   (fn [change]
     {:coalesce-key
      [:request (:id change)]})})

(def sample-progression
  {:gesso.live.progression/kind :test/progression
   :basis 42})

(defn planned-claim
  []
  {:result sample-result
   :transaction-fragment sample-fragment
   :transaction-options sample-options})

(deftest claim-plans-and-commits-one-complete-transaction-test
  (let [planner-calls (atom [])
        transaction-calls (atom [])
        transaction-result
        {:commit/status :committed
         :progression sample-progression
         :ctx {:opaque :consistency-aware}
         :tx-result {:opaque :xtdb}
         :changes [{:opaque :live-change}]}

        result
        (with-redefs
          [request/plan-claim-request
           (fn [ctx input]
             (swap! planner-calls conj [ctx input])
             (planned-claim))

           model.tx/transact!
           (fn [ctx plan]
             (swap! transaction-calls conj [ctx plan])
             transaction-result)]

          (request/claim
           sample-ctx
           sample-input))]

    (testing "the public operation invokes the existing Request planner exactly once"
      (is (= [[sample-ctx sample-input]]
             @planner-calls)))

    (testing "the complete planner fragment and options cross one generic atomic transaction boundary"
      (is (= 1
             (count @transaction-calls)))
      (is (= sample-ctx
             (ffirst @transaction-calls)))
      (is (= (merge sample-fragment sample-options)
             (second
              (first @transaction-calls)))))

    (testing "the authoritative semantic result is planner-owned"
      (is (= sample-result
             (dissoc result
                     :commit/status
                     :progression))))

    (testing "only operation-relevant commit metadata is exposed"
      (is (= :committed
             (:commit/status result)))
      (is (= sample-progression
             (:progression result)))
      (is (not (contains? result :ctx)))
      (is (not (contains? result :tx-result)))
      (is (not (contains? result :changes))))))

(deftest claim-does-not-invent-progression-when-transaction-does-not-produce-one-test
  (let [result
        (with-redefs
          [request/plan-claim-request
           (fn [_ctx _input]
             (planned-claim))

           model.tx/transact!
           (fn [_ctx _plan]
             {:commit/status :committed})]

          (request/claim
           sample-ctx
           sample-input))]

    (is (= :committed
           (:commit/status result)))

    (is (not (contains? result :progression))
        "Request may expose transaction-established progression, but must never manufacture one.")))

(deftest planner-failure-escapes-before-any-transaction-attempt-test
  (let [transaction-calls (atom 0)
        planner-error
        (ex-info
         "Request policy rejected claim."
         {:error/type :request/not-authorized})

        actual
        (try
          (with-redefs
            [request/plan-claim-request
             (fn [_ctx _input]
               (throw planner-error))

             model.tx/transact!
             (fn [_ctx _plan]
               (swap! transaction-calls inc)
               :impossible)]

            (request/claim
             sample-ctx
             sample-input))
          nil
          (catch Throwable error
            error))]

    (is (identical? planner-error actual)
        "The public operation must preserve the model's original policy failure.")

    (is (zero? @transaction-calls)
        "A rejected Request plan must never cross the commit boundary.")))

(deftest transaction-precommit-failure-escapes-unchanged-test
  (let [transaction-calls (atom 0)
        transaction-error
        (ex-info
         "XTDB assertion failed before commit."
         {:error/type :gesso.model.tx/pre-commit-test})

        actual
        (try
          (with-redefs
            [request/plan-claim-request
             (fn [_ctx _input]
               (planned-claim))

             model.tx/transact!
             (fn [_ctx _plan]
               (swap! transaction-calls inc)
               (throw transaction-error))]

            (request/claim
             sample-ctx
             sample-input))
          nil
          (catch Throwable error
            error))]

    (is (= 1 @transaction-calls))
    (is (identical? transaction-error actual)
        "request.core must not reinterpret a failed atomic commit as a different Request outcome.")))

(deftest committed-postcommit-delivery-failure-escapes-unchanged-test
  (let [transaction-calls (atom 0)
        committed-error
        (ex-info
         "Mutation committed; Live delivery later failed."
         {:error/type :gesso.live/post-commit-delivery-failure
          :commit/status :committed
          :progression sample-progression})

        actual
        (try
          (with-redefs
            [request/plan-claim-request
             (fn [_ctx _input]
               (planned-claim))

             model.tx/transact!
             (fn [_ctx _plan]
               (swap! transaction-calls inc)
               (throw committed-error))]

            (request/claim
             sample-ctx
             sample-input))
          nil
          (catch Throwable error
            error))]

    (is (= 1 @transaction-calls))

    (is (identical? committed-error actual)
        "A committed delivery failure must remain the original classified throwable.")

    (is (= :committed
           (:commit/status
            (ex-data actual)))
        "Higher distributed layers must remain able to distinguish uncertainty after commit from rollback.")

    (is (= sample-progression
           (:progression
            (ex-data actual))))))

(deftest request-core-remains-choreo-independent-public-model-boundary-test
  (let [publics
        (ns-publics
         'net.humanhelp.site.model.request.core)

        alias-namespaces
        (set
         (map
          (comp ns-name val)
          (ns-aliases
           'net.humanhelp.site.model.request.core)))]

    (is (contains? publics 'claim)
        "The authoritative operation must be independently callable through the supported Request facade.")

    (is (contains? publics 'plan-claim-request)
        "The existing planning seam remains available for model-level composition/tests.")

    (is (not-any?
         #(or (= 'gesso.choreo %)
              (= "gesso.choreo"
                 (namespace %))
              (.startsWith
               (str %)
               "gesso.choreo."))
         alias-namespaces)
        "Request semantic truth must not depend on its higher-level distributed choreography.")))
