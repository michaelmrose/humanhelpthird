(ns net.humanhelp.site.model.request-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [malli.core :as m]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain :as domain]
   [net.humanhelp.site.model.request.fx :as request.fx]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.request.schema :as request.schema])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn uuid
  [value]
  (UUID/fromString value))

(def request-id
  (uuid "80000000-0000-0000-0000-000000000001"))

(def other-request-id
  (uuid "80000000-0000-0000-0000-000000000002"))

(def organization-id
  (uuid "10000000-0000-0000-0000-000000000001"))

(def location-id
  (uuid "30000000-0000-0000-0000-000000000001"))

(def other-location-id
  (uuid "30000000-0000-0000-0000-000000000002"))

(def customer-id
  (uuid "40000000-0000-0000-0000-000000000001"))

(def other-customer-id
  (uuid "40000000-0000-0000-0000-000000000002"))

(def helper-id
  (uuid "50000000-0000-0000-0000-000000000001"))

(def other-helper-id
  (uuid "50000000-0000-0000-0000-000000000002"))

(def capability-id
  (uuid "60000000-0000-0000-0000-000000000001"))

(def location-version-id
  location-id)

(def t-before
  (Instant/parse "2026-06-30T11:59:00Z"))

(def t0
  (Instant/parse "2026-07-01T12:00:00Z"))

(def t1
  (Instant/parse "2026-07-01T12:01:00Z"))

(def t2
  (Instant/parse "2026-07-01T12:02:00Z"))

(def t3
  (Instant/parse "2026-07-01T12:03:00Z"))

(def t4
  (Instant/parse "2026-07-01T12:04:00Z"))

(def canonical-content
  {:title "Need assistance"
   :details "Please help me find the correct item."
   :location-detail "Aisle 8 near the freezer case"})

(def minimal-content
  {:title "Need assistance"
   :details nil
   :location-detail nil})

(def user-requestor
  (domain/user-requestor customer-id))

(def capability-requestor
  (domain/capability-requestor capability-id))

(def location-guard
  {:model/entity-type :location
   :model/expected
   {:model/id location-version-id
    :model/revision-key :location/revision
    :model/revision 3
    :model/updated-at-key :location/updated-at
    :model/updated-at t1}})

(def customer-guard
  {:model/entity-type :user
   :model/expected
   {:model/id customer-id
    :model/revision-key :user/revision
    :model/revision 4
    :model/updated-at-key :user/updated-at
    :model/updated-at t1}})

(def helper-guard
  {:model/entity-type :user
   :model/expected
   {:model/id helper-id
    :model/revision-key :user/revision
    :model/revision 4
    :model/updated-at-key :user/updated-at
    :model/updated-at t1}})

(defn open-request
  ([]
   (open-request {}))
  ([overrides]
   (domain/new-request
    (merge
     {:id request-id
      :organization-id organization-id
      :location-id location-id
      :requestor user-requestor
      :content canonical-content
      :now t0}
     overrides))))

(defn capability-owned-request
  []
  (open-request
   {:requestor capability-requestor}))

(defn claimed-request
  []
  (domain/claim-request
   (open-request)
   {:helper-id helper-id
    :now t1}))

(defn on-the-way-request
  []
  (domain/mark-request-on-the-way
   (claimed-request)
   {:helper-id helper-id
    :now t2}))

(defn done-request
  []
  (domain/complete-request
   (on-the-way-request)
   {:helper-id helper-id
    :now t3}))

(defn cancelled-request
  []
  (domain/cancel-request
   (open-request)
   {:now t1
    :reason :test/customer-cancelled}))

(defn command-document
  [command]
  (model.common/command-document command))

(defn query-contains?
  [query value]
  (boolean
   (some
    #{value}
    (tree-seq coll? seq query))))

(defn resolver-fn
  [resolver]
  (:biff.graph/resolve-fn resolver))

(defn resolve-resolver
  [resolver ctx input]
  ((resolver-fn resolver)
   (assoc
    ctx
    :biff.graph/input
    input)))

(defn private-fn
  [namespace-symbol var-symbol]
  (some->
   (ns-resolve
    namespace-symbol
    var-symbol)
   var-get))

(defn error-type
  [f]
  (try
    (f)
    ::did-not-throw
    (catch Throwable error
      (loop [error error]
        (when error
          (or
           (:error/type
            (ex-data error))
           (recur
            (ex-cause error))))))))

;; =============================================================================
;; Requestor values and content
;; =============================================================================

(deftest requestor-value-test
  (is
   (=
    {:requestor/type :user
     :requestor/id customer-id}
    user-requestor))

  (is
   (=
    {:requestor/type :capability
     :requestor/id capability-id}
    capability-requestor))

  (is
   (domain/requestor-reference?
    user-requestor))

  (is
   (domain/requestor-reference?
    capability-requestor))

  (is
   (domain/user-requestor?
    user-requestor))

  (is
   (domain/capability-requestor?
    capability-requestor))

  (is
   (false?
    (domain/requestor-reference?
     (assoc
      user-requestor
      :unexpected true))))

  (is
   (false?
    (domain/requestor-reference?
     {:requestor/type :employee
      :requestor/id customer-id}))))

(deftest requestor-ownership-test
  (let [request
        (open-request)]
    (is
     (domain/requested-by?
      request
      user-requestor))

    (is
     (domain/requested-by-user?
      request
      customer-id))

    (is
     (false?
      (domain/requested-by-user?
       request
       other-customer-id)))

    (is
     (domain/controlled-by?
      request
      {:user-id customer-id}))

    (is
     (false?
      (domain/controlled-by?
       request
       {:user-id other-customer-id}))))

  (let [request
        (capability-owned-request)]
    (is
     (domain/requested-by-capability?
      request
      capability-id))

    (is
     (domain/controlled-by?
      request
      {:capability-id capability-id}))

    (is
     (false?
      (domain/controlled-by?
       request
       {:user-id customer-id})))))

(deftest content-normalization-test
  (is
   (=
    {:title "Need assistance"
     :details "More detail"
     :location-detail nil}
    (domain/normalize-content
     {:title " Need assistance "
      :details " More detail "
      :location-detail "  "})))

  (is
   (domain/content?
    canonical-content))

  (is
   (domain/content?
    minimal-content))

  (is
   (false?
    (domain/content?
     (dissoc
      minimal-content
      :details))))

  (is
   (false?
    (domain/title?
     " Need assistance ")))

  (is
   (=
    #{:title
      :details
      :location-detail}
    (set
     (keys
      (domain/content-errors
       {:title ""
        :details
        (apply
         str
         (repeat
          (inc domain/details-max)
          "x"))
        :location-detail
        (apply
         str
         (repeat
          (inc domain/location-detail-max)
          "x"))}))))))

;; =============================================================================
;; Construction, invariants, and schema
;; =============================================================================

(deftest request-construction-test
  (let [request
        (open-request)]
    (is
     (=
      request-id
      (domain/request-id request)))

    (is
     (=
      organization-id
      (domain/organization-id request)))

    (is
     (=
      location-id
      (domain/location-id request)))

    (is
     (=
      user-requestor
      (domain/requestor request)))

    (is
     (=
      canonical-content
      (domain/content request)))

    (is
     (domain/open? request))

    (is
     (domain/active? request))

    (is
     (false?
      (domain/terminal? request)))

    (is
     (=
      0
      (domain/revision request)))

    (is
     (=
      t0
      (domain/created-at request)))

    (is
     (=
      t0
      (domain/updated-at request)))

    (is
     (domain/request-document-consistent?
      request))

    (is
     (m/validate
      request.schema/request-document-schema
      request))

    (is
     (m/validate
      (:request request.schema/schema)
      request))))

(deftest request-create-validation-test
  (is
   (=
    :request/invalid-create-input
    (error-type
     #(domain/new-request
       {:id request-id
        :organization-id organization-id
        :location-id location-id
        :requestor user-requestor
        :content {:title ""}
        :now t0}))))

  (is
   (=
    #{:id
      :organization-id
      :location-id
      :requestor
      :content
      :now}
    (set
     (keys
      (domain/create-input-errors
       {:id nil
        :organization-id nil
        :location-id nil
        :requestor nil
        :content {:title ""}
        :now nil}))))))

(deftest request-document-invariant-test
  (let [request
        (open-request)]
    (is
     (false?
      (domain/request-document-consistent?
       (assoc
        request
        :request/status
        :future-status))))

    (is
     (false?
      (domain/request-document-consistent?
       (assoc
        request
        :request/helper
        helper-id))))

    (is
     (false?
      (domain/request-document-consistent?
       (assoc
        request
        :request/cancelled-at
        t0))))

    (is
     (false?
      (domain/request-document-consistent?
       (-> request
           (assoc
            :request/status
            :on-the-way
            :request/helper
            helper-id
            :request/claimed-at
            t2
            :request/on-the-way-at
            t1
            :request/revision
            2
            :request/updated-at
            t2)))))

    (is
     (false?
      (m/validate
       request.schema/request-document-schema
       (assoc
        request
        :unexpected/value
        true))))))

(deftest request-schema-registry-test
  (doseq [request
          [(open-request)
           (claimed-request)
           (on-the-way-request)
           (done-request)
           (cancelled-request)]]
    (is
     (m/validate
      request.schema/request-document-schema
      request))

    (is
     (m/validate
      (:request/doc request.schema/schema)
      request)))

  (is
   (m/validate
    request.schema/requestor-reference-schema
    user-requestor))

  (is
   (m/validate
    request.schema/content-schema
    canonical-content))

  (is
   (m/validate
    request.schema/expected-version-schema
    (model.common/expected-version
     (open-request)
     domain/request-version)))

  (is
   (m/validate
    (:request/include-terminal?
     request.schema/schema)
    true))

  (is
   (m/validate
    (:request/include-terminal?
     request.schema/schema)
    false))

  (is
   (false?
    (m/validate
     (:request/include-terminal?
      request.schema/schema)
     :yes)))

  (is
   (=
    request.schema/request-document-schema
    (:request request.schema/schema)))

  (is
   (=
    request.schema/request-document-schema
    (:request/doc request.schema/schema))))

;; =============================================================================
;; Editing
;; =============================================================================

(deftest request-editing-test
  (let [edited
        (domain/edit-request
         (open-request)
         {:content
          {:title " Updated request "
           :details " New details "
           :location-detail " Front counter "}
          :now t1})]
    (is
     (=
      "Updated request"
      (:request/title edited)))

    (is
     (=
      "New details"
      (:request/details edited)))

    (is
     (=
      "Front counter"
      (:request/location-detail edited)))

    (is
     (=
      1
      (domain/revision edited)))

    (is
     (=
      t1
      (domain/updated-at edited)))

    (is
     (domain/request-document-consistent?
      edited)))

  (let [claimed
        (claimed-request)

        edited
        (domain/edit-request
         claimed
         {:content
          {:title "Corrected after claim"
           :details nil
           :location-detail "New aisle"}
          :now t2})

        traveling
        (domain/mark-request-on-the-way
         edited
         {:helper-id helper-id
          :now t3})

        reedited
        (domain/edit-request
         traveling
         {:content
          {:title "Corrected while traveling"
           :details "Updated note"
           :location-detail "Service desk"}
          :now t4})]
    (is
     (domain/editable?
      claimed))

    (is
     (domain/editable?
      traveling))

    (is
     (domain/on-the-way?
      reedited))

    (is
     (=
      helper-id
      (domain/helper-id reedited)))

    (is
     (=
      4
      (domain/revision reedited))))

  (is
   (=
    :request/not-editable
    (error-type
     #(domain/edit-request
       (done-request)
       {:content minimal-content
        :now t4}))))

  (is
   (=
    :request/not-editable
    (error-type
     #(domain/edit-request
       (cancelled-request)
       {:content minimal-content
        :now t2}))))

  (is
   (=
    :request/unchanged
    (error-type
     #(domain/edit-request
       (open-request)
       {:content canonical-content
        :now t1}))))

  (is
   (=
    :request/invalid-change-time
    (error-type
     #(domain/edit-request
       (open-request)
       {:content minimal-content
        :now t-before})))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(deftest request-lifecycle-happy-path-test
  (let [open
        (open-request)

        claimed
        (domain/claim-request
         open
         {:helper-id helper-id
          :now t1})

        traveling
        (domain/mark-request-on-the-way
         claimed
         {:helper-id helper-id
          :now t2})

        completed
        (domain/complete-request
         traveling
         {:helper-id helper-id
          :now t3})]
    (is
     (=
      :claimed
      (domain/next-status
       open
       :claim)))

    (is
     (=
      :on-the-way
      (domain/next-status
       claimed
       :mark-on-the-way)))

    (is
     (=
      :done
      (domain/next-status
       traveling
       :complete)))

    (is
     (nil?
      (domain/next-status
       open
       :complete)))

    (is
     (domain/claimed?
      claimed))

    (is
     (domain/actively-assigned?
      claimed))

    (is
     (domain/assigned-to?
      claimed
      helper-id))

    (is
     (=
      t1
      (:request/claimed-at claimed)))

    (is
     (domain/on-the-way?
      traveling))

    (is
     (=
      t2
      (:request/on-the-way-at traveling)))

    (is
     (domain/done?
      completed))

    (is
     (domain/has-helper?
      completed))

    (is
     (false?
      (domain/actively-assigned?
       completed)))

    (is
     (false?
      (domain/assigned-to?
       completed
       helper-id)))

    (is
     (=
      3
      (domain/revision completed)))

    (is
     (domain/request-document-consistent?
      completed))))

(deftest request-unclaim-and-reclaim-test
  (let [unclaimed
        (domain/unclaim-request
         (claimed-request)
         {:helper-id helper-id
          :now t2})

        reclaimed
        (domain/claim-request
         unclaimed
         {:helper-id other-helper-id
          :now t3})]
    (is
     (domain/open?
      unclaimed))

    (is
     (not
      (contains?
       unclaimed
       :request/helper)))

    (is
     (not
      (contains?
       unclaimed
       :request/claimed-at)))

    (is
     (=
      other-helper-id
      (domain/helper-id reclaimed)))

    (is
     (domain/request-document-consistent?
      reclaimed))))

(deftest request-cancellation-test
  (doseq [[active now]
          [[(open-request) t1]
           [(claimed-request) t2]
           [(on-the-way-request) t3]]]
    (let [cancelled
          (domain/cancel-request
           active
           {:now now
            :reason :test/customer-cancelled})]
      (is
       (domain/cancelled?
        cancelled))

      (is
       (=
        now
        (:request/cancelled-at cancelled)))

      (is
       (=
        :test/customer-cancelled
        (:request/cancellation-reason cancelled)))

      (is
       (false?
        (domain/actively-assigned?
         cancelled)))

      (is
       (domain/request-document-consistent?
        cancelled)))))

(deftest request-invalid-transition-test
  (is
   (=
    :request/not-assigned-helper
    (error-type
     #(domain/unclaim-request
       (claimed-request)
       {:helper-id other-helper-id
        :now t2}))))

  (is
   (=
    :request/invalid-transition
    (error-type
     #(domain/unclaim-request
       (on-the-way-request)
       {:helper-id helper-id
        :now t3}))))

  (is
   (=
    :request/invalid-transition
    (error-type
     #(domain/complete-request
       (open-request)
       {:helper-id helper-id
        :now t1}))))

  (is
   (=
    :request/invalid-transition
    (error-type
     #(domain/claim-request
       (claimed-request)
       {:helper-id other-helper-id
        :now t2}))))

  (is
   (=
    :request/not-assigned-helper
    (error-type
     #(domain/complete-request
       (claimed-request)
       {:helper-id other-helper-id
        :now t2}))))

  (is
   (=
    :request/invalid-cancellation-reason
    (error-type
     #(domain/cancel-request
       (open-request)
       {:now t1
        :reason :unqualified})))))

;; =============================================================================
;; Commands
;; =============================================================================

(deftest request-command-test
  (let [create-command
        (domain/create-command
         {:id request-id
          :organization-id organization-id
          :location-id location-id
          :requestor user-requestor
          :content canonical-content
          :now t0})

        created
        (command-document
         create-command)

        claim-command
        (domain/claim-command
         created
         {:helper-id helper-id
          :now t1})]
    (is
     (=
      :request
      (:model/entity-type create-command)))

    (is
     (=
      :create
      (:model/operation create-command)))

    (is
     (=
      request-id
      (:model/id create-command)))

    (is
     (nil?
      (:model/expected create-command)))

    (is
     (=
      :claim
      (:model/operation claim-command)))

    (is
     (=
      (model.common/expected-version
       created
       domain/request-version)
      (:model/expected claim-command)))

    (is
     (=
      helper-id
      (domain/helper-id
       (command-document claim-command)))))

  (is
   (=
    [:create
     :edit
     :claim
     :unclaim
     :mark-on-the-way
     :complete
     :cancel]
    domain/operation-order))

  (is
   (every?
    domain/operation?
    domain/operation-order)))

;; =============================================================================
;; Graph
;; =============================================================================

(deftest request-graph-input-builder-test
  (is
   (=
    {:request/id request-id}
    (request.graph/request-query-input
     {:request-id request-id})))

  (is
   (=
    {}
    (request.graph/request-query-input
     {:request-id nil})))

  (is
   (=
    {:request/organization-id organization-id
     :request/location-id location-id
     :request/include-terminal? false}
    (request.graph/location-requests-query-input
     {:organization-id organization-id
      :location-id location-id})))

  (is
   (=
    {:request/organization-id organization-id
     :request/location-id location-id
     :request/include-terminal? true}
    (request.graph/location-requests-query-input
     {:organization-id organization-id
      :location-id location-id
      :include-terminal? true})))

  (is
   (=
    {:request/include-terminal? false}
    (request.graph/location-requests-query-input
     {}))))

(deftest request-graph-query-contract-test
  (doseq [value
          [:request/found?
           :request/doc
           :request/expected-version]]
    (is
     (query-contains?
      request.graph/request-command-query
      value)))

  (doseq [value
          [:request/requestor
           :request/status
           :request/editable?
           :request/claimable?
           :request/actively-assigned?]]
    (is
     (query-contains?
      request.graph/request-facts-query
      value)))

  (doseq [value
          [:request/location-requests
           :request/doc
           :request/expected-version
           :request/active?
           :request/terminal?]]
    (is
     (query-contains?
      request.graph/location-requests-query
      value))))

(deftest request-graph-uses-biff-connection-test
  (let [q-fn
        (private-fn
         'net.humanhelp.site.model.request.graph
         'q)

        calls
        (atom [])]
    (is
     (ifn?
      q-fn))

    (with-redefs
     [biffx/q
      (fn [connectable query]
        (swap!
         calls
         conj
         [connectable query])
        :result)]

      (is
       (=
        :result
        (q-fn
         {:biff/conn :canonical
          :biff/node :must-not-be-used}
         {:select [:xt/id]}))))

    (is
     (=
      [[:canonical
        {:select [:xt/id]}]]
      @calls))))

(deftest request-field-and-lifecycle-resolver-test
  (let [request-document
        (claimed-request)

        fields
        (resolve-resolver
         request.graph/request-fields
         {}
         {:request/doc request-document})

        facts
        (resolve-resolver
         request.graph/request-lifecycle-facts
         {}
         {:request/doc request-document})]
    (is
     (=
      request-id
      (:request/id fields)))

    (is
     (=
      organization-id
      (:request/organization-id fields)))

    (is
     (=
      location-id
      (:request/location-id fields)))

    (is
     (=
      user-requestor
      (:request/requestor fields)))

    (is
     (=
      (model.common/expected-version
       request-document
       domain/request-version)
      (:request/expected-version fields)))

    (is
     (true?
      (:request/claimed? facts)))

    (is
     (true?
      (:request/actively-assigned? facts)))

    (is
     (false?
      (:request/open? facts)))

    (is
     (false?
      (:request/terminal? facts)))))

(deftest request-by-id-resolver-test
  (let [calls
        (atom [])

        request-document
        (open-request)]
    (with-redefs
     [biffx/q
      (fn [connectable query]
        (swap!
         calls
         conj
         [connectable query])
        [request-document])]

      (is
       (=
        {:request/found? true
         :request/doc request-document}
        (resolve-resolver
         request.graph/request-by-id
         {:biff/conn :connection}
         {:request/id request-id}))))

    (is
     (=
      :connection
      (ffirst @calls)))

    (is
     (query-contains?
      (second
       (first @calls))
      request-id)))

  (with-redefs
   [biffx/q
    (fn [& _]
      [])]

    (is
     (=
      {:request/found? false}
      (resolve-resolver
       request.graph/request-by-id
       {:biff/conn :connection}
       {:request/id other-request-id})))))

(deftest location-request-collection-resolver-test
  (let [open
        (open-request)

        claimed
        (assoc
         (claimed-request)
         :xt/id
         other-request-id)

        calls
        (atom [])]
    (with-redefs
     [biffx/q
      (fn [connectable query]
        (swap!
         calls
         conj
         [connectable query])
        [claimed open])]

      (is
       (=
        {:request/location-requests
         [{:request/doc claimed}
          {:request/doc open}]}
        (resolve-resolver
         request.graph/requests-at-location
         {:biff/conn :connection}
         {:request/organization-id organization-id
          :request/location-id location-id
          :request/include-terminal? false}))))

    (let [[connectable query]
          (first @calls)]
      (is
       (=
        :connection
        connectable))

      (is
       (=
        request.graph/request-document-columns
        (:select query)))

      (is
       (=
        domain/request-entity-type
        (:from query)))

      (is
       (query-contains?
        (:where query)
        [:= :request/organization organization-id]))

      (is
       (query-contains?
        (:where query)
        [:= :request/location location-id]))

      (is
       (query-contains?
        (:where query)
        request.graph/active-status-predicate))

      (is
       (=
        [[:request/created-at :desc]
         [:xt/id :desc]]
        (:order-by query)))))

  (let [captured
        (atom nil)

        completed
        (done-request)]
    (with-redefs
     [biffx/q
      (fn [_connectable query]
        (reset!
         captured
         query)
        [completed])]

      (is
       (=
        {:request/location-requests
         [{:request/doc completed}]}
        (resolve-resolver
         request.graph/requests-at-location
         {:biff/conn :connection}
         {:request/organization-id organization-id
          :request/location-id location-id
          :request/include-terminal? true}))))

    (is
     (false?
      (query-contains?
       (:where @captured)
       request.graph/active-status-predicate))))

  (let [called?
        (atom false)]
    (with-redefs
     [biffx/q
      (fn [& _]
        (reset!
         called?
         true)
        [])]

      (is
       (=
        {:request/location-requests []}
        (resolve-resolver
         request.graph/requests-at-location
         {:biff/conn :connection}
         {:request/organization-id nil
          :request/location-id location-id
          :request/include-terminal? false}))))

    (is
     (false?
      @called?))))

;; =============================================================================
;; FX planning and machine start
;; =============================================================================

(deftest request-fx-create-plan-test
  (let [command
        (domain/create-command
         {:id request-id
          :organization-id organization-id
          :location-id location-id
          :requestor user-requestor
          :content canonical-content
          :now t0})

        plan
        (request.fx/plan-create-request
         {:command command
          :location-authorization-versions
          [location-guard]
          :actor-authorization-versions
          [customer-guard]})

        transaction-plan
        (:transaction-plan plan)

        change
        (first
         (:changes transaction-plan))]
    (is
     (=
      [command]
      (:commands transaction-plan)))

    (is
     (=
      [location-guard
       customer-guard]
      (:authorization-versions
       transaction-plan)))

    (is
     (empty?
      (:assertions transaction-plan)))

    (is
     (=
      :created
      (:change/kind change)))

    (is
     (=
      :create
      (:request/operation change)))

    (is
     (=
      request-id
      (:request/id change)))

    (is
     (=
      organization-id
      (:organization/id change)))

    (is
     (=
      location-id
      (:location/id change)))

    (is
     (=
      :open
      (:request/status change)))

    (is
     (=
      {:coalesce-key
       [:request request-id]}
      ((:entry-fn transaction-plan)
       change)))

    (is
     (=
      (command-document command)
      (get-in
       plan
       [:result :request])))))

(deftest request-fx-update-plan-test
  (let [before
        (open-request)

        command
        (domain/claim-command
         before
         {:helper-id helper-id
          :now t1})

        plan
        (request.fx/plan-update-request
         {:before before
          :command command
          :location-authorization-versions
          [location-guard]
          :actor-authorization-versions
          [helper-guard]})

        transaction-plan
        (:transaction-plan plan)

        change
        (first
         (:changes transaction-plan))]
    (is
     (=
      [command]
      (:commands transaction-plan)))

    (is
     (=
      [location-guard
       helper-guard]
      (:authorization-versions
       transaction-plan)))

    (is
     (empty?
      (:assertions transaction-plan)))

    (is
     (=
      :updated
      (:change/kind change)))

    (is
     (=
      :claim
      (:request/operation change)))

    (is
     (=
      :open
      (:request/previous-status change)))

    (is
     (=
      :claimed
      (:request/status change)))

    (is
     (=
      helper-id
      (:request/helper change)))

    (is
     (=
      1
      (:request/revision change)))

    (is
     (=
      helper-id
      (get-in
       plan
       [:result
        :request
        :request/helper])))))

(deftest request-fx-update-machine-start-test
  (let [ctx
        {:request.fx/input
         {:request-id request-id}

         :request.fx/operation
         :claim}

        state
        (request.fx/update-request-machine
         ctx
         :start)

        descriptor
        (:request.fx/request-facts
         state)]
    (is
     (=
      ctx
      (:request.fx/base-ctx state)))

    (is
     (=
      request-id
      (:request.fx/request-id state)))

    (is
     (=
      :biff.graph.fx/query
      (first descriptor)))

    (is
     (=
      {:request/id request-id}
      (second descriptor)))

    (is
     (=
      request.graph/request-command-query
      (nth descriptor 2)))

    (is
     (=
      :plan
      (:biff.fx/next state)))))

(deftest request-fx-operation-registry-test
  (is
   (=
    #{:request/create
      :request/edit
      :request/claim
      :request/unclaim
      :request/mark-on-the-way
      :request/complete
      :request/cancel}
    (set
     (keys
      request.fx/operations)))))

;; =============================================================================
;; Public Core facade
;; =============================================================================

(deftest request-core-contract-test
  (is
   (=
    request.schema/schema
    request/schema))

  (is
   (=
    request.graph/resolvers
    request/resolvers))

  (is
   (=
    {:schema request/schema
     :biff.graph/resolvers request/resolvers}
    request/module))

  (is
   (=
    request.graph/request-document-query
    request/request-document-query))

  (is
   (=
    request.graph/request-facts-query
    request/request-query))

  (is
   (=
    request.graph/location-requests-query
    request/location-requests-query))

  (is
   (=
    :request
    request/request-entity-type))

  (is
   (=
    domain/statuses
    request/statuses))

  (is
   (=
    domain/active-statuses
    request/active-statuses))

  (is
   (=
    domain/terminal-statuses
    request/terminal-statuses))

  (is
   (=
    domain/operations
    request/operations-set))

  (doseq [[key value]
          [[:request/create
            #'request/create-request]

           [:request/edit
            #'request/edit-request]

           [:request/claim
            #'request/claim-request]

           [:request/unclaim
            #'request/unclaim-request]

           [:request/mark-on-the-way
            #'request/mark-request-on-the-way]

           [:request/complete
            #'request/complete-request]

           [:request/cancel
            #'request/cancel-request]]]
    (is
     (identical?
      value
      (get
       request/operations
       key))))

  (doseq [symbol
          '[create-command
            new-request
            plan-create-request
            request-query-input
            location-requests-query-input]]
    (is
     (nil?
      (ns-resolve
       'net.humanhelp.site.model.request.core
       symbol)))))

(deftest request-core-read-delegation-test
  (let [queries
        (atom [])

        ctx
        {:ctx true}

        facts
        {:request/found? true
         :request/doc
         (open-request)}

        collection
        {:request/location-requests
         [{:request/doc
           (open-request)}]}

        location-input
        {:organization-id organization-id
         :location-id location-id
         :include-terminal? true}]
    (with-redefs
     [graph/query
      (fn [query-ctx input query]
        (swap!
         queries
         conj
         [query-ctx input query])
        (if
         (=
          query
          request/request-query)
          facts
          collection))]

      (is
       (=
        facts
        (request/request-facts
         ctx
         request-id)))

      (is
       (=
        (open-request)
        (request/request-document
         ctx
         request-id)))

      (is
       (=
        collection
        (request/location-requests
         ctx
         location-input)))

      (is
       (=
        (:request/location-requests collection)
        (request/location-request-items
         ctx
         location-input))))

    (is
     (=
      [[ctx
        {:request/id request-id}
        request/request-query]

       [ctx
        {:request/id request-id}
        request/request-query]

       [ctx
        {:request/organization-id organization-id
         :request/location-id location-id
         :request/include-terminal? true}
        request/location-requests-query]

       [ctx
        {:request/organization-id organization-id
         :request/location-id location-id
         :request/include-terminal? true}
        request/location-requests-query]]
      @queries))))

(deftest request-core-operation-delegation-test
  (let [calls
        (atom [])

        ctx
        {:ctx true}

        input
        {:request-id request-id}]
    (with-redefs
     [request.fx/create-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:create
          actual-ctx
          actual-input])
        :create)

      request.fx/edit-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:edit
          actual-ctx
          actual-input])
        :edit)

      request.fx/claim-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:claim
          actual-ctx
          actual-input])
        :claim)

      request.fx/unclaim-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:unclaim
          actual-ctx
          actual-input])
        :unclaim)

      request.fx/mark-request-on-the-way
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:mark-on-the-way
          actual-ctx
          actual-input])
        :mark-on-the-way)

      request.fx/complete-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:complete
          actual-ctx
          actual-input])
        :complete)

      request.fx/cancel-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:cancel
          actual-ctx
          actual-input])
        :cancel)]

      (is
       (=
        :create
        (request/create-request
         ctx
         input)))

      (is
       (=
        :edit
        (request/edit-request
         ctx
         input)))

      (is
       (=
        :claim
        (request/claim-request
         ctx
         input)))

      (is
       (=
        :unclaim
        (request/unclaim-request
         ctx
         input)))

      (is
       (=
        :mark-on-the-way
        (request/mark-request-on-the-way
         ctx
         input)))

      (is
       (=
        :complete
        (request/complete-request
         ctx
         input)))

      (is
       (=
        :cancel
        (request/cancel-request
         ctx
         input)))

      (is
       (=
        :claim
        (request/perform-action
         ctx
         :claim
         input))))

    (is
     (=
      8
      (count @calls)))

    (is
     (every?
      #(=
        ctx
        (nth % 1))
      @calls))

    (is
     (every?
      #(=
        input
        (nth % 2))
      @calls))))

(deftest request-core-unsupported-operation-test
  (is
   (=
    :request/unsupported-operation
    (error-type
     #(request/perform-action
       {}
       :take-over
       {:request-id request-id})))))
