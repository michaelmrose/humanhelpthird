(ns net.humanhelp.example.board-test
  "Focused tests for the production-model-backed HumanHelp example board.

   The example board is deliberately a read/presentation composition layer, not
   a second HumanHelp model. These tests protect that boundary while the example
   application is being rewritten around production models and production
   Choreo.

   In particular they verify that:

   - the board has no dependency on net.humanhelp.example.model;
   - production Request/User documents remain production documents in board rows;
   - search/filter/sort are presentation concerns only;
   - rendered lifecycle affordances are the exact production Request Choreo
     capabilities;
   - XTDB Live progression, never Request revision, supplies :observed-basis;
   - Request revision is carried only as an optional model-specific fact version.

   Authorization and Request transition policy remain tested below this layer in
   the production model and choreography suites."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.core :as live]
   [gesso.model.command :as command]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain :as request.domain]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.model.user.domain :as user.domain])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed identities and times
;; =============================================================================

(def request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def other-request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000002"))

(def terminal-request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000003"))

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

(def other-helper-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000003"))

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

(def t3
  (Instant/parse
   "2026-09-03T00:03:00Z"))

;; =============================================================================
;; Production-document fixtures
;; =============================================================================

(defn- production-user
  [id display-name]
  (command/after
   (user.domain/create-user-command
    {:id id
     :display-name display-name
     :email
     (str
      (str/replace
       (str/lower-case display-name)
       #"\s+"
       ".")
      "@example.test")
     :now t0})))

(def requestor-user
  (production-user
   requestor-id
   "Requestor Person"))

(def helper-user
  (production-user
   helper-id
   "Helper Person"))

(def other-helper-user
  (production-user
   other-helper-id
   "Other Helper"))

(defn- open-request
  ([id now]
   (open-request
    id
    now
    {}))
  ([id now content-overrides]
   (command/after
    (request.domain/create-request-command
     {:id id
      :organization-id organization-id
      :location-id location-id
      :requestor
      (request.domain/user-requestor
       requestor-id)
      :content
      (merge
       {:title "Need help"
        :details "Please help me find the right item."
        :location-detail "Near the front desk"}
       content-overrides)
      :now now}))))

(defn- claimed-request
  ([id]
   (claimed-request id t0 t1))
  ([id created-at claimed-at]
   (command/after
    (request.domain/claim-request-command
     (open-request id created-at)
     {:now claimed-at}))))

(defn- on-the-way-request
  [id]
  (command/after
   (request.domain/mark-on-the-way-command
    (claimed-request id t0 t1)
    {:now t2})))

(defn- done-request
  [id]
  (command/after
   (request.domain/complete-request-command
    (claimed-request id t0 t1)
    {:now t2})))

(defn- primary-assignment
  ([request-id']
   (primary-assignment
    request-id'
    helper-id))
  ([request-id' helper-id']
   (command/after
    (request.domain/create-assignment-command
     {:id
      (if (= helper-id' helper-id)
        assignment-id
        (UUID/fromString
         "50000000-0000-0000-0000-000000000002"))
      :request-id request-id'
      :helper-id helper-id'
      :role :primary
      :source :request/claim
      :actor-id helper-id'
      :now t1}))))

(defn- row
  ([request-document]
   (row request-document nil nil nil))
  ([request-document assignment requestor helper]
   {:request request-document
    :primary-assignment assignment
    :requestor-user requestor
    :primary-helper-user helper}))

;; =============================================================================
;; Architecture
;; =============================================================================

(deftest example-board-is-not-a-parallel-model-test
  (let [dependencies
        (->> (ns-aliases
              'net.humanhelp.example.board)
             vals
             (map ns-name)
             set)]
    (is (not
         (contains?
          dependencies
          'net.humanhelp.example.model))
        "The production-backed board must never route through the old example model.")
    (is (contains?
         dependencies
         'net.humanhelp.site.model.request.core))
    (is (contains?
         dependencies
         'net.humanhelp.site.model.request.choreo))
    (is (contains?
         dependencies
         'net.humanhelp.site.model.user.core))))

(deftest request-row-preserves-production-documents-test
  (let [request-document
        (claimed-request request-id)

        assignment
        (primary-assignment request-id)

        user-reads
        (atom [])]

    (with-redefs
     [request/active-primary-assignment-for-request
      (fn [_ctx actual-request-id]
        (is (= request-id actual-request-id))
        assignment)

      user/require-user
      (fn [_ctx actual-user-id]
        (swap! user-reads conj actual-user-id)
        (cond
          (= requestor-id actual-user-id)
          requestor-user

          (= helper-id actual-user-id)
          helper-user

          :else
          nil))]

      (let [actual
            (board/request-row
             {:test/context :board}
             request-document)]
        (is (= #{:request
                 :primary-assignment
                 :requestor-user
                 :primary-helper-user}
               (set (keys actual))))
        (is (identical?
             request-document
             (:request actual)))
        (is (identical?
             assignment
             (:primary-assignment actual)))
        (is (= requestor-user
               (:requestor-user actual)))
        (is (= helper-user
               (:primary-helper-user actual)))
        (is (= [requestor-id helper-id]
               @user-reads))
        (is (not
             (contains?
              actual
              :claimed-by)))
        (is (not
             (contains?
              actual
              :visible-revision)))))))

;; =============================================================================
;; Presentation state
;; =============================================================================

(deftest normalize-view-state-is-presentation-only-test
  (is (= board/default-view-state
         (board/normalize-view-state
          nil)))
  (is (= {:search "needle"
          :created-order :oldest
          :mine-first? true
          :unclaimed-first? true
          :show-terminal? true}
         (board/normalize-view-state
          {:search "  needle  "
           :created-order "oldest"
           :mine-first? "on"
           :unclaimed-first? "true"
           :show-terminal? 1
           :visible-revision 999
           :observed-basis :fabricated})))
  (is (= :newest
         (:created-order
          (board/normalize-view-state
           {:created-order "not-a-real-order"})))))

(deftest visible-rows-search-filter-and-created-order-test
  (let [older
        (row
         (open-request
          request-id
          t0
          {:title "Need groceries"})
         nil
         requestor-user
         nil)

        newer
        (row
         (open-request
          other-request-id
          t1
          {:title "Need a ride"})
         nil
         other-helper-user
         nil)

        terminal
        (row
         (done-request terminal-request-id)
         (primary-assignment terminal-request-id)
         requestor-user
         helper-user)]

    (testing "terminal rows are hidden by default and newest is first"
      (is (= [other-request-id request-id]
             (mapv
              board/row-request-id
              (board/visible-rows
               [older newer terminal]
               helper-id
               {})))))

    (testing "oldest ordering is deterministic"
      (is (= [request-id other-request-id]
             (mapv
              board/row-request-id
              (board/visible-rows
               [newer older]
               helper-id
               {:created-order :oldest})))))

    (testing "search operates over production Request/User presentation text"
      (is (= [other-request-id]
             (mapv
              board/row-request-id
              (board/visible-rows
               [older newer]
               helper-id
               {:search "ride"}))))
      (is (= [request-id]
             (mapv
              board/row-request-id
              (board/visible-rows
               [older newer]
               helper-id
               {:search "requestor person"})))))

    (testing "terminal rows can be shown explicitly"
      (is (= #{request-id other-request-id terminal-request-id}
             (set
              (map
               board/row-request-id
               (board/visible-rows
                [older newer terminal]
                helper-id
                {:show-terminal? true}))))))))

(deftest mine-first-and-unclaimed-first-are-only-sort-preferences-test
  (let [mine
        (row
         (claimed-request request-id)
         (primary-assignment request-id helper-id)
         requestor-user
         helper-user)

        unclaimed
        (row
         (open-request other-request-id t1)
         nil
         requestor-user
         nil)]

    (is (= [request-id other-request-id]
           (mapv
            board/row-request-id
            (board/visible-rows
             [unclaimed mine]
             helper-id
             {:mine-first? true}))))

    (is (= [other-request-id request-id]
           (mapv
            board/row-request-id
            (board/visible-rows
             [mine unclaimed]
             helper-id
             {:unclaimed-first? true}))))))

;; =============================================================================
;; Production Choreo affordances
;; =============================================================================

(defn- affordance-operations
  [row viewer-id]
  (mapv
   :operation
   (board/operation-affordances
    row
    viewer-id)))

(deftest lifecycle-affordances-use-production-request-vocabulary-test
  (let [open-row
        (row
         (open-request request-id t0)
         nil
         requestor-user
         nil)

        claimed-row
        (row
         (claimed-request request-id)
         (primary-assignment request-id)
         requestor-user
         helper-user)

        on-the-way-row
        (row
         (on-the-way-request request-id)
         (primary-assignment request-id)
         requestor-user
         helper-user)

        done-row
        (row
         (done-request request-id)
         (primary-assignment request-id)
         requestor-user
         helper-user)]

    (testing "an open Request exposes real claim/cancel operations"
      (is (= [:request/claim]
             (affordance-operations
              open-row
              helper-id)))
      (is (= [:request/claim
              :request/cancel]
             (affordance-operations
              open-row
              requestor-id))))

    (testing "an assigned helper sees the real production lifecycle"
      (is (= [:request/mark-on-the-way
              :request/complete
              :request/unclaim]
             (affordance-operations
              claimed-row
              helper-id)))
      (is (= [:request/complete]
             (affordance-operations
              on-the-way-row
              helper-id))))

    (testing "terminal Requests have no simple lifecycle affordance"
      (is (empty?
           (board/operation-affordances
            done-row
            helper-id))))

    (testing "the obsolete demo vocabulary is absent"
      (is (not
           (contains?
            (set board/operation-order)
            :request/take-over)))
      (is (not
           (contains?
            (set board/operation-order)
            :request/done)))
      (is (contains?
           (set
            (keys
             request.choreo/capabilities))
           :request/reassign)
          "Reassign remains a production capability even though it needs a dedicated manager UI."))))

(deftest every-affordance-carries-the-exact-production-capability-test
  (let [request-row
        (row
         (claimed-request request-id)
         (primary-assignment request-id)
         requestor-user
         helper-user)

        affordances
        (board/operation-affordances
         request-row
         helper-id)]

    (doseq [{:keys [operation capability arguments]}
            affordances]
      (is (= (get
              request.choreo/capabilities
              operation)
             capability))
      (is (= {:request-id request-id}
             arguments)))))

;; =============================================================================
;; Authority frontier / optimistic binding
;; =============================================================================

(deftest observed-basis-comes-only-from-live-progression-test
  (let [requirement
        {:test/progression :authoritative}

        basis
        {:test/basis :xtdb}

        strongest-input
        (atom nil)]

    (with-redefs
     [live/progression
      (fn [ctx]
        (is (= {:ctx :value}
               ctx))
        requirement)

      xtdb-live/strongest-required-basis
      (fn [actual-requirement]
        (reset! strongest-input actual-requirement)
        basis)]

      (is (= basis
             (board/observed-basis
              {:ctx :value})))
      (is (= requirement
             @strongest-input))))

  (testing "no Live progression means no observed basis; model revision is never a fallback"
    (with-redefs
     [live/progression
      (constantly nil)

      xtdb-live/strongest-required-basis
      (fn [_]
        (throw
         (ex-info
          "must not be called"
          {})))]

      (is (nil?
           (board/observed-basis
            {:request/revision 999
             :visible-revision 999}))))))

(deftest optimistic-binding-separates-authority-basis-from-request-fact-version-test
  (let [request-document
        (claimed-request request-id)

        request-row
        (row
         request-document
         (primary-assignment request-id)
         requestor-user
         helper-user)

        requirement
        {:test/progression :requirement}

        basis
        {:test/basis :authoritative}

        binding
        (with-redefs
         [live/progression
          (constantly requirement)

          xtdb-live/strongest-required-basis
          (fn [actual]
            (is (= requirement actual))
            basis)]

          (board/optimistic-binding
           {:ctx :value}
           request-row
           request.choreo/complete-operation
           {:request-id request-id}
           "request-card-1"))]

    (is (= basis
           (:observed-basis binding)))
    (is (= {:request/revision
            (request/revision request-document)}
           (:fact-versions binding)))
    (is (= [:request request-id]
           (:scope binding)))
    (is (= "request-card-1"
           (:target-id binding)))
    (is (= {:request-id request-id}
           (:arguments binding)))
    (is (= request.choreo/complete-capability
           (:capability binding)))
    (is (not=
         (:observed-basis binding)
         (request/revision request-document))
        "The model fact revision must never masquerade as the XTDB authority frontier.")))

(deftest optimistic-binding-refuses-to-fabricate-a-basis-test
  (let [request-document
        (claimed-request request-id)

        request-row
        (row
         request-document
         (primary-assignment request-id)
         requestor-user
         helper-user)]

    (with-redefs
     [live/progression
      (constantly nil)]

      (is (nil?
           (board/optimistic-binding
            {:request/revision
             (request/revision request-document)}
            request-row
            request.choreo/complete-operation
            {:request-id request-id}
            "request-card-1"))))))

;; =============================================================================
;; Board query
;; =============================================================================

(deftest board-data-composes-production-rows-without-changing-domain-truth-test
  (let [active-row
        (row
         (open-request request-id t0)
         nil
         requestor-user
         nil)

        terminal-row
        (row
         (done-request terminal-request-id)
         (primary-assignment terminal-request-id)
         requestor-user
         helper-user)

        basis
        {:test/basis :board}

        reads
        (atom [])]

    (with-redefs
     [board/request-rows-for-location
      (fn [ctx actual-location-id]
        (swap! reads conj [ctx actual-location-id])
        [active-row terminal-row])

      board/observed-basis
      (constantly basis)]

      (let [actual
            (board/board-data
             {:ctx :production-read}
             {:location-id location-id
              :viewer helper-user
              :view-state {}})]

        (is (= [[{:ctx :production-read}
                 location-id]]
               @reads))
        (is (= helper-id
               (:viewer-id actual)))
        (is (= basis
               (:observed-basis actual)))
        (is (= 2
               (:total-count actual)))
        (is (= 1
               (:active-count actual)))
        (is (= 1
               (:terminal-count actual)))
        (is (= [request-id]
               (mapv
                board/row-request-id
                (:rows actual))))))))
