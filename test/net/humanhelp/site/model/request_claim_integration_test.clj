(ns net.humanhelp.site.model.request-claim-integration-test
  "Real-XTDB integration for the public authoritative Request claim operation.

   This namespace proves the persistence boundary that request.choreo will rely
   on before choreography is introduced.

   Request-owned behavior is real:

   - Request aggregate reads come from XTDB;
   - Request and RequestAssignment documents are validated by gesso.model;
   - claim planning uses the real Request FX implementation;
   - revision/dependency guards and the no-active-assignment assertion enter one
     real XTDB transaction;
   - gesso.model.tx and Gesso Live execute the real commit;
   - the transaction-established progression returned by request.core/claim can
     be carried forward as an authoritative read requirement;
   - persisted Request and RequestAssignment state is read back through the
     public Request core.

   Organization and Membership are deliberately represented only by their
   public dependency seams. Their own persistence correctness belongs to those
   models' integration suites; this test isolates the Request aggregate and its
   authoritative commit boundary."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [com.biffweb.fx :as fx]
   [gesso.live.core :as live]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [malli.registry :as mr]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain :as request.domain]
   [xtdb.node :as xtn])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Stable fixture identities
;; =============================================================================

(def request-id
  (UUID/fromString
   "a1000000-0000-0000-0000-000000000001"))

(def assignment-id
  (UUID/fromString
   "a2000000-0000-0000-0000-000000000001"))

(def organization-id
  (UUID/fromString
   "a3000000-0000-0000-0000-000000000001"))

(def location-id
  (UUID/fromString
   "a4000000-0000-0000-0000-000000000001"))

(def requestor-id
  (UUID/fromString
   "a5000000-0000-0000-0000-000000000001"))

(def helper-id
  (UUID/fromString
   "a6000000-0000-0000-0000-000000000001"))

(def helper-membership-id
  (UUID/fromString
   "a7000000-0000-0000-0000-000000000001"))

(def created-at
  (Instant/parse
   "2026-09-01T16:00:00Z"))

(def claimed-at
  (Instant/parse
   "2026-09-01T16:01:00Z"))

(def retry-at
  (Instant/parse
   "2026-09-01T16:02:00Z"))

(def request-content
  {:title
   "Need help in the integration fixture"

   :details
   "The Request claim path must commit Request and primary assignment atomically."

   :location-detail
   "Integration fixture"})

;; =============================================================================
;; Real XTDB / Live runtime
;; =============================================================================

(defonce ^:private !runtime
  (atom nil))

(defn- runtime
  []
  (or
   @!runtime

   (throw
    (ex-info
     "Request claim integration runtime is not initialized."
     {}))))

(def malli-opts
  {:registry
   (mr/composite-registry
    m/default-registry
    request/schema)})

(defn- live-rules
  []
  [{:when-topic
    :request

    :expand
    (fn [_ctx change]
      [change])}

   {:when-topic
    :request-assignment

    :expand
    (fn [_ctx change]
      [change])}])

(defn- with-runtime
  [f]
  (with-open
   [node
    (xtn/start-node
     {})]

    (let [live-system
          (live/create
           {:rules
            (live-rules)

            :dispatch-options
            {:threads
             1

             :queue-size
             32}})

          ctx
          {:biff.xtdb/node
           node

           :biff/malli-opts
           malli-opts

           :gesso.live/system
           live-system}]

      (reset!
       !runtime
       {:node
        node

        :live-system
        live-system

        :ctx
        ctx})

      (try
        (f)

        (finally
          (reset!
           !runtime
           nil)

          (live/close!
           live-system))))))

(use-fixtures
  :each
  with-runtime)

;; =============================================================================
;; Fixture seed
;; =============================================================================

(defn- seed-request-command
  []
  (request.domain/create-request-command
   {:id
    request-id

    :organization-id
    organization-id

    :location-id
    location-id

    :requestor
    (request/user-requestor
     requestor-id)

    :content
    request-content

    :now
    created-at}))

(defn- seed-open-request!
  []
  (let [{:keys
         [ctx]}
        (runtime)]
    (model.tx/transact!
     ctx
     {:commands
      [(seed-request-command)]

      :emit
      false})))

;; =============================================================================
;; Foreign-model public dependency seam
;; =============================================================================

(defn- scope-context
  [scope]
  {:organization/id
   organization-id

   :scope/target
   scope

   :scope/applicable
   [scope
    (organization/organization-scope
     organization-id)]

   :scope/operational?
   true})

(defn- helper-dependency
  []
  {:membership
   {:xt/id
    helper-membership-id

    :membership/user
    helper-id

    :membership/organization
    organization-id

    :membership/status
    :active

    :membership/skills
    #{}}

   :transaction-fragment
   model.tx/empty-fragment})

(defn- with-request-authority
  [f]
  (with-redefs
   [fx/uuid7
    (fn [_seed _now]
      [assignment-id])

    organization/require-scope-dependency
    (fn [_ctx scope]
      {:scope-context
       (scope-context
        scope)

       :transaction-fragment
       model.tx/empty-fragment})

    membership/require-helper-dependency
    (fn [_ctx requested-helper-id _scope]
      (if
       (= helper-id
          requested-helper-id)
        (helper-dependency)

        (throw
         (ex-info
          "Unexpected helper requested by Request claim integration fixture."
          {:error/type
           :test/unexpected-helper

           :expected
           helper-id

           :actual
           requested-helper-id}))))]

    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- claim-ctx
  [base now]
  (assoc
   base

   :current-user/id
   helper-id

   :biff.fx/seed
   1

   :biff.fx/now
   now))

(defn- error-from
  [thunk]
  (try
    (thunk)
    nil

    (catch Throwable error
      error)))

(defn- progression-read-ctx
  [ctx claim-result]
  (if-let [progression
           (:progression
            claim-result)]
    (live/with-progression
      ctx
      progression)

    ctx))

;; =============================================================================
;; Real authoritative claim contract
;; =============================================================================

(deftest public-claim-commits-request-and-primary-assignment-atomically-test
  (seed-open-request!)

  (let [{:keys
         [ctx]}
        (runtime)

        before
        (request/require-request-snapshot
         ctx
         request-id)]

    (testing "the real persisted fixture begins as one open Request with no active assignments"
      (is (request/open?
           (:request before)))

      (is (= []
             (:assignments before))))

    (let [claim-result
          (with-request-authority
            #(request/claim
              (claim-ctx
               ctx
               claimed-at)

              {:request-id
               request-id}))

          read-ctx
          (progression-read-ctx
           ctx
           claim-result)

          persisted
          (request/require-request-snapshot
           read-ctx
           request-id)

          persisted-request
          (:request
           persisted)

          persisted-primary
          (:primary-assignment
           persisted)

          returned-request
          (:request
           claim-result)

          returned-primary
          (:primary-assignment
           claim-result)]

      (testing "the public authoritative operation reports one committed model transaction"
        (is (= :committed
               (:commit/status
                claim-result)))

        (is (some?
             (:progression
              claim-result))
            "A real XTDB authoritative mutation should expose the transaction-established progression needed by downstream convergence."))

      (testing "the Request lifecycle mutation is real and exactly the planner-owned result"
        (is (request/claimed?
             persisted-request))

        (is (= returned-request
               persisted-request))

        (is (= 1
               (request/revision
                persisted-request)))

        (is (= claimed-at
               (request/updated-at
                persisted-request))))

      (testing "the primary RequestAssignment was committed in the same aggregate transition"
        (is (some?
             persisted-primary))

        (is (request/active-primary-assignment?
             persisted-primary))

        (is (= assignment-id
               (request/assignment-id
                persisted-primary)))

        (is (= helper-id
               (request/assignment-helper-id
                persisted-primary)))

        (is (= request-id
               (request/assignment-request-id
                persisted-primary)))

        (is (= :request/claim
               (request/assignment-source
                persisted-primary)))

        (is (= returned-primary
               persisted-primary)))

      (testing "there is exactly one active assignment after the atomic claim"
        (is (= [assignment-id]
               (mapv
                request/assignment-id
                (request/active-assignments-for-request
                 read-ctx
                 request-id))))

        (is (= assignment-id
               (some->
                (request/active-primary-assignment-for-request
                 read-ctx
                 request-id)
                request/assignment-id))))

      (testing "the returned progression is usable as the canonical authoritative read requirement"
        (is (= (:progression claim-result)
               (live/progression
                read-ctx)))))))

(deftest rejected-second-claim-cannot-partially-change-the-persisted-aggregate-test
  (seed-open-request!)

  (let [{:keys
         [ctx]}
        (runtime)

        first-result
        (with-request-authority
          #(request/claim
            (claim-ctx
             ctx
             claimed-at)

            {:request-id
             request-id}))

        read-ctx
        (progression-read-ctx
         ctx
         first-result)

        before-retry
        (request/require-request-snapshot
         read-ctx
         request-id)

        retry-error
        (with-request-authority
          #(error-from
            (fn []
              (request/claim
               (claim-ctx
                read-ctx
                retry-at)

               {:request-id
                request-id}))))

        after-retry
        (request/require-request-snapshot
         read-ctx
         request-id)]

    (testing "a second claim is rejected by authoritative current Request state"
      (is (some?
           retry-error))

      (is (not=
           :committed
           (:commit/status
            (ex-data
             retry-error)))
          "A model-policy/pre-commit rejection must not be mislabeled as a committed second mutation."))

    (testing "the rejected retry leaves the complete Request aggregate byte-for-value unchanged"
      (is (= (:request before-retry)
             (:request after-retry)))

      (is (= (:assignments before-retry)
             (:assignments after-retry)))

      (is (= (:primary-assignment before-retry)
             (:primary-assignment after-retry))))

    (testing "no duplicate active assignment was created"
      (is (= 1
             (count
              (:assignments
               after-retry))))

      (is (= assignment-id
             (some->
              (:primary-assignment
               after-retry)
              request/assignment-id)))

      (is (= [assignment-id]
             (mapv
              request/assignment-id
              (request/active-assignments-for-request
               read-ctx
               request-id)))))))
