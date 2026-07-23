(ns net.humanhelp.site.model.request-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [malli.core :as m]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.assignment :as assignment]
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

(def primary-assignment-id
  (uuid "81000000-0000-0000-0000-000000000001"))

(def collaborator-assignment-id
  (uuid "81000000-0000-0000-0000-000000000002"))

(def other-assignment-id
  (uuid "81000000-0000-0000-0000-000000000003"))

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

(def collaborator-id
  (uuid "50000000-0000-0000-0000-000000000002"))

(def replacement-helper-id
  (uuid "50000000-0000-0000-0000-000000000003"))

(def manager-id
  (uuid "50000000-0000-0000-0000-000000000004"))

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

(def manager-guard
  {:model/entity-type :user
   :model/expected
   {:model/id manager-id
    :model/revision-key :user/revision
    :model/revision 2
    :model/updated-at-key :user/updated-at
    :model/updated-at t1}})

(def replacement-helper-guard
  {:model/entity-type :user
   :model/expected
   {:model/id replacement-helper-id
    :model/revision-key :user/revision
    :model/revision 1
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
   {:now t1}))

(defn on-the-way-request
  []
  (domain/mark-request-on-the-way
   (claimed-request)
   {:now t2}))

(defn done-request
  []
  (domain/complete-request
   (on-the-way-request)
   {:now t3}))

(defn cancelled-request
  []
  (domain/cancel-request
   (open-request)
   {:now t1
    :reason :test/customer-cancelled}))

(defn primary-assignment
  ([]
   (primary-assignment {}))
  ([overrides]
   (assignment/new-assignment
    (merge
     {:id primary-assignment-id
      :request-id request-id
      :helper-id helper-id
      :role :primary
      :source :request/claim
      :actor-id helper-id
      :now t1}
     overrides))))

(defn collaborator-assignment
  ([]
   (collaborator-assignment {}))
  ([overrides]
   (assignment/new-assignment
    (merge
     {:id collaborator-assignment-id
      :request-id request-id
      :helper-id collaborator-id
      :role :collaborator
      :source :request/collaboration
      :actor-id helper-id
      :now t2}
     overrides))))

(defn ended-assignment
  ([]
   (ended-assignment
    (collaborator-assignment)))
  ([assignment-document]
   (assignment/end-assignment
    assignment-document
    {:actor-id helper-id
     :reason :request/collaborator-removed
     :now t3})))

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

(defn private-var
  [namespace-symbol var-symbol]
  (ns-resolve
   namespace-symbol
   var-symbol))

(defn private-fn
  [namespace-symbol var-symbol]
  (some->
   (private-var namespace-symbol var-symbol)
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
;; Request construction, invariants, and schema
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
     (false?
      (domain/lifecycle-expects-primary-assignment?
       request)))

    (is
     (not
      (contains?
       request
       :request/helper)))

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
        :request/cancelled-at
        t0))))

    (is
     (false?
      (domain/request-document-consistent?
       (-> request
           (assoc
            :request/status :on-the-way
            :request/claimed-at t2
            :request/on-the-way-at t1
            :request/revision 2
            :request/updated-at t2)))))

    (testing "the closed persisted schema rejects the removed helper field"
      (is
       (false?
        (m/validate
         request.schema/request-document-schema
         (assoc
          request
          :request/helper
          helper-id)))))

    (is
     (false?
      (m/validate
       request.schema/request-document-schema
       (assoc
        request
        :unexpected/value
        true))))))

(deftest request-schema-registry-test
  (doseq [document
          [(open-request)
           (claimed-request)
           (on-the-way-request)
           (done-request)
           (cancelled-request)]]
    (is
     (m/validate
      request.schema/request-document-schema
      document))

    (is
     (m/validate
      (:request/doc request.schema/schema)
      document)))

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
;; Request editing and lifecycle
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
         {:now t3})

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
     (not
      (contains?
       reedited
       :request/helper)))

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

(deftest request-lifecycle-happy-path-test
  (let [open
        (open-request)

        claimed
        (domain/claim-request
         open
         {:now t1})

        traveling
        (domain/mark-request-on-the-way
         claimed
         {:now t2})

        completed
        (domain/complete-request
         traveling
         {:now t3})]
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
     (domain/lifecycle-expects-primary-assignment?
      claimed))

    (is
     (=
      t1
      (:request/claimed-at claimed)))

    (is
     (domain/on-the-way?
      traveling))

    (is
     (domain/lifecycle-expects-primary-assignment?
      traveling))

    (is
     (=
      t2
      (:request/on-the-way-at traveling)))

    (is
     (domain/done?
      completed))

    (is
     (false?
      (domain/lifecycle-expects-primary-assignment?
       completed)))

    (is
     (not
      (contains?
       completed
       :request/helper)))

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
         {:now t2})

        reclaimed
        (domain/claim-request
         unclaimed
         {:now t3})]
    (is
     (domain/open?
      unclaimed))

    (is
     (not
      (contains?
       unclaimed
       :request/claimed-at)))

    (is
     (false?
      (domain/lifecycle-expects-primary-assignment?
       unclaimed)))

    (is
     (domain/claimed?
      reclaimed))

    (is
     (domain/lifecycle-expects-primary-assignment?
      reclaimed))

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
        (domain/lifecycle-expects-primary-assignment?
         cancelled)))

      (is
       (domain/request-document-consistent?
        cancelled)))))

(deftest request-invalid-transition-test
  (is
   (=
    :request/invalid-transition
    (error-type
     #(domain/unclaim-request
       (on-the-way-request)
       {:now t3}))))

  (is
   (=
    :request/invalid-transition
    (error-type
     #(domain/complete-request
       (open-request)
       {:now t1}))))

  (is
   (=
    :request/invalid-transition
    (error-type
     #(domain/claim-request
       (claimed-request)
       {:now t2}))))

  (is
   (=
    :request/invalid-cancellation-reason
    (error-type
     #(domain/cancel-request
       (open-request)
       {:now t1
        :reason :unqualified})))))

;; =============================================================================
;; Request commands and operation vocabulary
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
         {:now t1})]
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
     (domain/claimed?
      (command-document claim-command)))

    (is
     (not
      (contains?
       (command-document claim-command)
       :request/helper))))

  (is
   (=
    [:create
     :edit
     :claim
     :unclaim
     :mark-on-the-way
     :complete
     :cancel
     :add-collaborator
     :remove-collaborator
     :reassign]
    domain/operation-order))

  (is
   (=
    #{:create
      :edit
      :claim
      :unclaim
      :mark-on-the-way
      :complete
      :cancel}
    domain/document-operations))

  (is
   (=
    #{:add-collaborator
      :remove-collaborator
      :reassign}
    domain/assignment-operations))

  (is
   (every?
    domain/operation?
    domain/operation-order)))

;; =============================================================================
;; Request Assignment domain
;; =============================================================================

(deftest assignment-construction-test
  (let [primary
        (primary-assignment)

        collaborator
        (collaborator-assignment)]
    (is
     (=
      primary-assignment-id
      (assignment/assignment-id primary)))

    (is
     (=
      request-id
      (assignment/request-id primary)))

    (is
     (=
      helper-id
      (assignment/helper-id primary)))

    (is
     (assignment/primary?
      primary))

    (is
     (assignment/active?
      primary))

    (is
     (assignment/active-primary?
      primary))

    (is
     (false?
      (assignment/collaborator?
       primary)))

    (is
     (=
      :request/claim
      (assignment/source primary)))

    (is
     (=
      helper-id
      (assignment/assigned-by primary)))

    (is
     (=
      t1
      (assignment/assigned-at primary)))

    (is
     (assignment/document-consistent?
      primary))

    (is
     (m/validate
      request.schema/request-assignment-document-schema
      primary))

    (is
     (m/validate
      (:request-assignment
       request.schema/schema)
      primary))

    (is
     (assignment/collaborator?
      collaborator))

    (is
     (assignment/active-collaborator?
      collaborator))

    (is
     (assignment/for-helper?
      collaborator
      collaborator-id))

    (is
     (assignment/for-request?
      collaborator
      request-id))))

(deftest assignment-create-validation-test
  (is
   (=
    :request-assignment/invalid-create-input
    (error-type
     #(assignment/new-assignment
       {:id nil
        :request-id nil
        :helper-id nil
        :role :invalid
        :source :unqualified
        :actor-id "bad"
        :now nil}))))

  (is
   (=
    #{:id
      :request-id
      :helper-id
      :role
      :source
      :actor-id
      :now}
    (set
     (keys
      (assignment/create-input-errors
       {:id nil
        :request-id nil
        :helper-id nil
        :role :invalid
        :source :unqualified
        :actor-id "bad"
        :now nil}))))))

(deftest assignment-end-lifecycle-test
  (let [active
        (collaborator-assignment)

        ended
        (assignment/end-assignment
         active
         {:actor-id helper-id
          :reason :request/collaborator-removed
          :now t3})]
    (is
     (assignment/ended?
      ended))

    (is
     (false?
      (assignment/active?
       ended)))

    (is
     (=
      t3
      (assignment/ended-at ended)))

    (is
     (=
      helper-id
      (assignment/ended-by ended)))

    (is
     (=
      :request/collaborator-removed
      (assignment/end-reason ended)))

    (is
     (=
      1
      (assignment/revision ended)))

    (is
     (=
      t3
      (assignment/updated-at ended)))

    (is
     (assignment/document-consistent?
      ended))

    (is
     (m/validate
      request.schema/request-assignment-document-schema
      ended))

    (is
     (=
      :request-assignment/already-ended
      (error-type
       #(assignment/end-assignment
         ended
         {:actor-id helper-id
          :reason :request/collaborator-removed
          :now t4}))))))

(deftest assignment-collection-facts-test
  (let [primary
        (primary-assignment)

        collaborator
        (collaborator-assignment)

        ended
        (ended-assignment
         collaborator)

        assignments
        [primary
         collaborator
         ended]]
    (is
     (=
      [primary
       collaborator]
      (assignment/active-assignments
       assignments)))

    (is
     (=
      [ended]
      (assignment/ended-assignments
       assignments)))

    (is
     (=
      [primary]
      (assignment/active-primary-assignments
       assignments)))

    (is
     (=
      [collaborator]
      (assignment/active-collaborator-assignments
       assignments)))

    (is
     (=
      primary
      (assignment/active-primary-assignment
       assignments)))

    (is
     (=
      collaborator
      (assignment/active-assignment-for-helper
       assignments
       collaborator-id)))

    (is
     (=
      #{helper-id
        collaborator-id}
      (assignment/active-helper-ids
       assignments)))

    (is
     (=
      #{collaborator-id}
      (assignment/active-collaborator-helper-ids
       assignments)))))

(deftest assignment-ambiguity-test
  (let [another-primary
        (primary-assignment
         {:id other-assignment-id
          :helper-id replacement-helper-id})]
    (is
     (=
      :request-assignment/ambiguous-primary
      (error-type
       #(assignment/active-primary-assignment
         [(primary-assignment)
          another-primary])))))

  (let [second-for-helper
        (collaborator-assignment
         {:id other-assignment-id
          :helper-id helper-id})]
    (is
     (=
      :request-assignment/ambiguous-helper
      (error-type
       #(assignment/active-assignment-for-helper
         [(primary-assignment)
          second-for-helper]
         helper-id))))))

(deftest assignment-command-test
  (let [create-command
        (assignment/create-command
         {:id primary-assignment-id
          :request-id request-id
          :helper-id helper-id
          :role :primary
          :source :request/claim
          :actor-id helper-id
          :now t1})

        created
        (command-document
         create-command)

        end-command
        (assignment/end-command
         created
         {:actor-id helper-id
          :reason :request/unclaimed
          :now t2})]
    (is
     (=
      assignment/entity-type
      (:model/entity-type create-command)))

    (is
     (=
      :create
      (:model/operation create-command)))

    (is
     (nil?
      (:model/expected create-command)))

    (is
     (=
      :end
      (:model/operation end-command)))

    (is
     (=
      (model.common/expected-version
       created
       assignment/version)
      (:model/expected end-command)))

    (is
     (assignment/ended?
      (command-document end-command)))))

(deftest assignment-schema-registry-test
  (let [primary
        (primary-assignment)

        ended
        (ended-assignment)]
    (is
     (m/validate
      request.schema/request-assignment-document-schema
      primary))

    (is
     (m/validate
      request.schema/request-assignment-document-schema
      ended))

    (is
     (m/validate
      request.schema/assignment-expected-version-schema
      (model.common/expected-version
       primary
       assignment/version)))

    (is
     (=
      request.schema/request-assignment-document-schema
      (:request-assignment
       request.schema/schema)))

    (is
     (=
      request.schema/request-assignment-document-schema
      (:request-assignment/doc
       request.schema/schema)))))

;; =============================================================================
;; Graph input builders and query contracts
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
    {:request-assignment/id primary-assignment-id}
    (request.graph/assignment-query-input
     {:assignment-id primary-assignment-id})))

  (is
   (=
    {:request/id request-id
     :request-assignment/include-ended? false}
    (request.graph/request-assignments-query-input
     {:request-id request-id})))

  (is
   (=
    {:request/id request-id
     :request-assignment/include-ended? true}
    (request.graph/request-assignments-query-input
     {:request-id request-id
      :include-ended? true}))))

(deftest request-graph-query-contract-test
  (doseq [value
          [:request/found?
           :request/doc
           :request/expected-version
           :request/assignments
           :request/has-primary-assignment?
           :request/active-helper-ids
           :request/active-collaborator-helper-ids]]
    (is
     (query-contains?
      request.graph/request-command-query
      value)))

  (doseq [value
          [:request/requestor
           :request/status
           :request/editable?
           :request/claimable?
           :request/expects-primary-assignment?
           :request/assignments
           :request-assignment/doc
           :request-assignment/active?]]
    (is
     (query-contains?
      request.graph/request-facts-query
      value)))

  (doseq [value
          [:request/location-requests
           :request/doc
           :request/expected-version
           :request/active?
           :request/terminal?
           :request/assignments
           :request/active-helper-ids]]
    (is
     (query-contains?
      request.graph/location-requests-query
      value)))

  (doseq [value
          [:request-assignment/found?
           :request-assignment/doc
           :request-assignment/expected-version
           :request-assignment/active?
           :request-assignment/primary?]]
    (is
     (query-contains?
      request.graph/assignment-facts-query
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

;; =============================================================================
;; Request Graph resolvers
;; =============================================================================

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
      (:request/expects-primary-assignment? facts)))

    (is
     (false?
      (:request/open? facts)))

    (is
     (false?
      (:request/terminal? facts)))))

(deftest assignment-field-and-lifecycle-resolver-test
  (let [document
        (collaborator-assignment)

        fields
        (resolve-resolver
         request.graph/assignment-fields
         {}
         {:request-assignment/doc document})

        facts
        (resolve-resolver
         request.graph/assignment-lifecycle-facts
         {}
         {:request-assignment/doc document})]
    (is
     (=
      collaborator-assignment-id
      (:request-assignment/id fields)))

    (is
     (=
      request-id
      (:request-assignment/request-id fields)))

    (is
     (=
      collaborator-id
      (:request-assignment/helper-id fields)))

    (is
     (=
      :collaborator
      (:request-assignment/role fields)))

    (is
     (=
      (model.common/expected-version
       document
       assignment/version)
      (:request-assignment/expected-version fields)))

    (is
     (true?
      (:request-assignment/active? facts)))

    (is
     (true?
      (:request-assignment/collaborator? facts)))

    (is
     (true?
      (:request-assignment/active-collaborator? facts)))

    (is
     (false?
      (:request-assignment/primary? facts)))))

(deftest request-assignment-summary-resolver-test
  (let [primary
        (primary-assignment)

        collaborator
        (collaborator-assignment)

        result
        (resolve-resolver
         request.graph/request-assignment-summary
         {}
         {:request/doc
          (claimed-request)

          :request/assignments
          [{:request-assignment/doc primary}
           {:request-assignment/doc collaborator}]})]
    (is
     (true?
      (:request/has-primary-assignment? result)))

    (is
     (=
      #{helper-id
        collaborator-id}
      (:request/active-helper-ids result)))

    (is
     (=
      #{collaborator-id}
      (:request/active-collaborator-helper-ids result)))))

(deftest request-assignment-summary-rejects-wrong-request-test
  (let [wrong
        (collaborator-assignment
         {:id other-assignment-id
          :request-id other-request-id})]
    (is
     (=
      :request.graph/assignment-request-mismatch
      (error-type
       #(resolve-resolver
         request.graph/request-assignment-summary
         {}
         {:request/doc
          (claimed-request)

          :request/assignments
          [{:request-assignment/doc wrong}]}))))))

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

(deftest assignment-by-id-resolver-test
  (let [calls
        (atom [])

        document
        (primary-assignment)]
    (with-redefs
     [biffx/q
      (fn [connectable query]
        (swap!
         calls
         conj
         [connectable query])
        [document])]

      (is
       (=
        {:request-assignment/found? true
         :request-assignment/doc document}
        (resolve-resolver
         request.graph/assignment-by-id
         {:biff/conn :connection}
         {:request-assignment/id primary-assignment-id}))))

    (let [[connectable query]
          (first @calls)]
      (is
       (=
        :connection
        connectable))

      (is
       (=
        request.graph/assignment-document-columns
        (:select query)))

      (is
       (=
        assignment/entity-type
        (:from query)))

      (is
       (query-contains?
        (:where query)
        primary-assignment-id)))))

(deftest assignments-for-request-resolver-test
  (let [primary
        (primary-assignment)

        collaborator
        (collaborator-assignment)

        calls
        (atom [])]
    (with-redefs
     [biffx/q
      (fn [connectable query]
        (swap!
         calls
         conj
         [connectable query])
        [primary
         collaborator])]

      (is
       (=
        {:request/assignments
         [{:request-assignment/doc primary}
          {:request-assignment/doc collaborator}]}
        (resolve-resolver
         request.graph/assignments-for-request
         {:biff/conn :connection}
         {:request/id request-id
          :request-assignment/include-ended? false}))))

    (let [[connectable query]
          (first @calls)]
      (is
       (=
        :connection
        connectable))

      (is
       (=
        assignment/entity-type
        (:from query)))

      (is
       (query-contains?
        (:where query)
        [:= :request-assignment/request request-id]))

      (is
       (query-contains?
        (:where query)
        [:= :request-assignment/status :active]))

      (is
       (=
        [[:request-assignment/assigned-at :asc]
         [:xt/id :asc]]
        (:order-by query)))))

  (let [captured
        (atom nil)

        ended
        (ended-assignment)]
    (with-redefs
     [biffx/q
      (fn [_connectable query]
        (reset!
         captured
         query)
        [ended])]

      (is
       (=
        {:request/assignments
         [{:request-assignment/doc ended}]}
        (resolve-resolver
         request.graph/assignments-for-request
         {:biff/conn :connection}
         {:request/id request-id
          :request-assignment/include-ended? true}))))

    (is
     (false?
      (query-contains?
       (:where @captured)
       [:= :request-assignment/status :active])))))

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
       request.graph/active-status-predicate)))))

;; =============================================================================
;; FX planning
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

(deftest request-fx-claim-plan-with-primary-assignment-test
  (let [before
        (open-request)

        request-command
        (domain/claim-command
         before
         {:now t1})

        assignment-command
        (assignment/create-command
         {:id primary-assignment-id
          :request-id request-id
          :helper-id helper-id
          :role :primary
          :source :request/claim
          :actor-id helper-id
          :now t1})

        primary
        (command-document
         assignment-command)

        assertion
        {:assert
         [:= 0
          {:select [[[:count '*']]]
           :from 'request-assignment
           :where
           [:= :request-assignment/request request-id]}]}

        assignment-change
        {:topic :request
         :id request-id
         :change/kind :updated
         :request/operation :claim
         :request/id request-id
         :request-assignment/id primary-assignment-id
         :request-assignment/helper helper-id
         :request-assignment/role :primary
         :request-assignment/status :active
         :request-assignment/previous-status nil}

        plan
        (request.fx/plan-update-request
         {:before before
          :command request-command
          :assignment-commands [assignment-command]
          :assignment-changes [assignment-change]
          :assertions [assertion]
          :location-authorization-versions [location-guard]
          :actor-authorization-versions [helper-guard]})

        transaction-plan
        (:transaction-plan plan)

        changes
        (:changes transaction-plan)]
    (is
     (=
      [request-command
       assignment-command]
      (:commands transaction-plan)))

    (is
     (=
      [location-guard
       helper-guard]
      (:authorization-versions
       transaction-plan)))

    (is
     (=
      [assertion]
      (:assertions transaction-plan)))

    (is
     (=
      2
      (count changes)))

    (is
     (=
      :claim
      (:request/operation
       (first changes))))

    (is
     (=
      :open
      (:request/previous-status
       (first changes))))

    (is
     (=
      :claimed
      (:request/status
       (first changes))))

    (is
     (=
      assignment-change
      (second changes)))

    (is
     (domain/claimed?
      (get-in
       plan
       [:result :request])))

    (is
     (assignment/active-primary?
      primary))))

(deftest request-fx-assignment-only-plan-test
  (let [request-document
        (claimed-request)

        assignment-command
        (assignment/create-command
         {:id collaborator-assignment-id
          :request-id request-id
          :helper-id collaborator-id
          :role :collaborator
          :source :request/collaboration
          :actor-id helper-id
          :now t2})

        collaborator
        (command-document
         assignment-command)

        change
        {:topic :request
         :id request-id
         :change/kind :updated
         :request/operation :add-collaborator
         :request/id request-id
         :request-assignment/id collaborator-assignment-id
         :request-assignment/helper collaborator-id
         :request-assignment/role :collaborator
         :request-assignment/status :active
         :request-assignment/previous-status nil}

        plan
        (request.fx/plan-assignment-operation
         {:request-document request-document
          :operation :add-collaborator
          :assignment-commands [assignment-command]
          :assignment-changes [change]
          :location-authorization-versions [location-guard]
          :actor-authorization-versions [helper-guard]
          :target-authorization-versions [replacement-helper-guard]})

        transaction-plan
        (:transaction-plan plan)]
    (is
     (=
      [assignment-command]
      (:commands transaction-plan)))

    (is
     (=
      [location-guard
       helper-guard
       replacement-helper-guard]
      (:authorization-versions
       transaction-plan)))

    (is
     (=
      [change]
      (:changes transaction-plan)))

    (is
     (=
      request-document
      (get-in
       plan
       [:result :request])))

    (is
     (=
      [collaborator]
      (get-in
       plan
       [:result :assignments])))

    (is
     (=
      {:coalesce-key
       [:request request-id]}
      ((:entry-fn transaction-plan)
       change)))))

;; =============================================================================
;; FX claim authorization semantics
;; =============================================================================

(deftest request-fx-self-claim-authorization-test
  (let [claim-authorization
        (private-fn
         'net.humanhelp.site.model.request.fx
         'claim-authorization)

        eligible-var
        (private-var
         'net.humanhelp.site.model.request.fx
         'eligible-helper-authorization)

        manager-var
        (private-var
         'net.humanhelp.site.model.request.fx
         'manager-authorization)

        eligible-calls
        (atom [])

        manager-calls
        (atom [])]
    (with-redefs-fn
      {eligible-var
       (fn [ctx target-id scope-context skill]
         (swap!
          eligible-calls
          conj
          [ctx target-id scope-context skill])
         {:authorization-versions
          [helper-guard]})

       manager-var
       (fn [& args]
         (swap!
          manager-calls
          conj
          args)
         {:authorization-versions
          [manager-guard]})}
      (fn []
        (is
         (=
          {:actor-id helper-id
           :target-helper-id helper-id
           :required-skill "forklift operator"
           :authorization-versions
           [helper-guard]}
          (claim-authorization
           {:current-user/id helper-id}
           {:skill " Forklift Operator "}
           {:organization/id organization-id})))))

    (is
     (=
      [[{:current-user/id helper-id}
        helper-id
        {:organization/id organization-id}
        "forklift operator"]]
      @eligible-calls))

    (is
     (empty?
      @manager-calls))))

(deftest request-fx-manager-claim-authorization-test
  (let [claim-authorization
        (private-fn
         'net.humanhelp.site.model.request.fx
         'claim-authorization)

        eligible-var
        (private-var
         'net.humanhelp.site.model.request.fx
         'eligible-helper-authorization)

        manager-var
        (private-var
         'net.humanhelp.site.model.request.fx
         'manager-authorization)

        eligible-calls
        (atom [])

        manager-calls
        (atom [])

        scope-context
        {:organization/id organization-id}]
    (with-redefs-fn
      {eligible-var
       (fn [ctx target-id actual-scope skill]
         (swap!
          eligible-calls
          conj
          [ctx target-id actual-scope skill])
         {:authorization-versions
          [replacement-helper-guard]})

       manager-var
       (fn [ctx actor-id actual-scope]
         (swap!
          manager-calls
          conj
          [ctx actor-id actual-scope])
         {:authorization-versions
          [manager-guard]})}
      (fn []
        (is
         (=
          {:actor-id manager-id
           :target-helper-id replacement-helper-id
           :required-skill nil
           :authorization-versions
           [manager-guard
            replacement-helper-guard]}
          (claim-authorization
           {:current-user/id manager-id}
           {:helper-id replacement-helper-id}
           scope-context)))))

    (is
     (=
      [[{:current-user/id manager-id}
        manager-id
        scope-context]]
      @manager-calls))

    (is
     (=
      [[{:current-user/id manager-id}
        replacement-helper-id
        scope-context
        nil]]
      @eligible-calls))))

;; =============================================================================
;; FX machine start and registry
;; =============================================================================

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
      {:request/id request-id
       :request-assignment/include-ended? false}
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
      :request/cancel
      :request/add-collaborator
      :request/remove-collaborator
      :request/reassign}
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
    request.graph/assignment-document-query
    request/assignment-document-query))

  (is
   (=
    request.graph/request-command-query
    request/request-command-query))

  (is
   (=
    request.graph/request-facts-query
    request/request-query))

  (is
   (=
    request.graph/assignment-facts-query
    request/assignment-query))

  (is
   (=
    request.graph/active-assignments-query
    request/active-assignments-query))

  (is
   (=
    request.graph/assignment-history-query
    request/assignment-history-query))

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
    :request-assignment
    request/assignment-entity-type))

  (is
   (=
    domain/operations
    request/operations-set))

  (is
   (=
    assignment/roles
    request/assignment-roles))

  (is
   (=
    assignment/statuses
    request/assignment-statuses))

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
            #'request/cancel-request]

           [:request/add-collaborator
            #'request/add-collaborator]

           [:request/remove-collaborator
            #'request/remove-collaborator]

           [:request/reassign
            #'request/reassign-request]]]
    (is
     (identical?
      value
      (get
       request/operations
       key))))

  (doseq [symbol
          '[create-command
            new-request
            new-assignment
            end-command
            plan-create-request
            plan-assignment-operation
            request-query-input
            assignment-query-input]]
    (is
     (nil?
      (ns-resolve
       'net.humanhelp.site.model.request.core
       symbol)))))

(deftest request-core-assignment-projection-test
  (let [primary
        (primary-assignment)

        collaborator
        (collaborator-assignment)

        facts
        {:request/assignments
         [{:request-assignment/doc primary}
          {:request-assignment/doc collaborator}]}]
    (is
     (=
      [primary
       collaborator]
      (request/assignment-documents
       facts)))

    (is
     (=
      primary
      (request/primary-assignment
       facts)))

    (is
     (=
      [collaborator]
      (request/collaborator-assignment-documents
       facts)))

    (is
     (=
      helper-id
      (request/assignment-helper-id
       primary)))

    (is
     (request/primary-assignment?
      primary))

    (is
     (request/active-collaborator-assignment?
      collaborator))))

(deftest request-core-read-delegation-test
  (let [queries
        (atom [])

        ctx
        {:ctx true}

        request-facts
        {:request/found? true
         :request/doc
         (open-request)
         :request/assignments []}

        assignment-facts
        {:request-assignment/found? true
         :request-assignment/doc
         (primary-assignment)}

        assignment-collection
        {:request/assignments
         [{:request-assignment/doc
           (primary-assignment)}]}

        location-collection
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
        (cond
          (= query request/request-query)
          request-facts

          (= query request/request-command-query)
          request-facts

          (= query request/assignment-query)
          assignment-facts

          (= query request/active-assignments-query)
          assignment-collection

          (= query request/assignment-history-query)
          assignment-collection

          (= query request/location-requests-query)
          location-collection

          :else
          (throw
           (ex-info
            "Unexpected public Request query."
            {:query query}))))]

      (is
       (=
        request-facts
        (request/request-facts
         ctx
         request-id)))

      (is
       (=
        request-facts
        (request/request-command-facts
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
        assignment-facts
        (request/assignment-facts
         ctx
         primary-assignment-id)))

      (is
       (=
        (primary-assignment)
        (request/assignment-document
         ctx
         primary-assignment-id)))

      (is
       (=
        assignment-collection
        (request/request-assignments
         ctx
         request-id)))

      (is
       (=
        assignment-collection
        (request/request-assignment-history
         ctx
         request-id)))

      (is
       (=
        (:request/assignments
         assignment-collection)
        (request/request-assignment-items
         ctx
         request-id)))

      (is
       (=
        (:request/assignments
         assignment-collection)
        (request/request-assignment-history-items
         ctx
         request-id)))

      (is
       (=
        location-collection
        (request/location-requests
         ctx
         location-input)))

      (is
       (=
        (:request/location-requests
         location-collection)
        (request/location-request-items
         ctx
         location-input))))

    (is
     (some
      #{[ctx
         {:request/id request-id
          :request-assignment/include-ended? false}
         request/request-query]}
      @queries))

    (is
     (some
      #{[ctx
         {:request/id request-id
          :request-assignment/include-ended? false}
         request/request-command-query]}
      @queries))

    (is
     (some
      #{[ctx
         {:request-assignment/id primary-assignment-id}
         request/assignment-query]}
      @queries))

    (is
     (some
      #{[ctx
         {:request/id request-id
          :request-assignment/include-ended? false}
         request/active-assignments-query]}
      @queries))

    (is
     (some
      #{[ctx
         {:request/id request-id
          :request-assignment/include-ended? true}
         request/assignment-history-query]}
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
         [:create actual-ctx actual-input])
        :create)

      request.fx/edit-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:edit actual-ctx actual-input])
        :edit)

      request.fx/claim-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:claim actual-ctx actual-input])
        :claim)

      request.fx/unclaim-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:unclaim actual-ctx actual-input])
        :unclaim)

      request.fx/mark-request-on-the-way
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:mark-on-the-way actual-ctx actual-input])
        :mark-on-the-way)

      request.fx/complete-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:complete actual-ctx actual-input])
        :complete)

      request.fx/cancel-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:cancel actual-ctx actual-input])
        :cancel)

      request.fx/add-collaborator
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:add-collaborator actual-ctx actual-input])
        :add-collaborator)

      request.fx/remove-collaborator
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:remove-collaborator actual-ctx actual-input])
        :remove-collaborator)

      request.fx/reassign-request
      (fn [actual-ctx actual-input]
        (swap!
         calls
         conj
         [:reassign actual-ctx actual-input])
        :reassign)]

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
        :add-collaborator
        (request/add-collaborator
         ctx
         input)))

      (is
       (=
        :remove-collaborator
        (request/remove-collaborator
         ctx
         input)))

      (is
       (=
        :reassign
        (request/reassign-request
         ctx
         input)))

      (doseq [operation
              [:claim
               :add-collaborator
               :remove-collaborator
               :reassign]]
        (is
         (=
          operation
          (request/perform-action
           ctx
           operation
           input)))))

    (is
     (=
      14
      (count
       @calls)))

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
