(ns net.humanhelp.site.model.request-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [malli.core :as m]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain.content :as content]
   [net.humanhelp.site.model.request.domain.core :as domain]
   [net.humanhelp.site.model.request.domain.entity :as entity]
   [net.humanhelp.site.model.request.domain.lifecycle :as lifecycle]
   [net.humanhelp.site.model.request.domain.requestor :as requestor]
   [net.humanhelp.site.model.request.fx :as request.fx]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.request.schema :as request.schema]
   [net.humanhelp.site.model.user.core :as user])
  (:import [java.time Instant] [java.util UUID]))

(defn uuid [s] (UUID/fromString s))
(def request-id (uuid "80000000-0000-0000-0000-000000000001"))
(def other-request-id (uuid "80000000-0000-0000-0000-000000000002"))
(def organization-id (uuid "10000000-0000-0000-0000-000000000001"))
(def location-id (uuid "30000000-0000-0000-0000-000000000001"))
(def other-location-id (uuid "30000000-0000-0000-0000-000000000002"))
(def customer-id (uuid "40000000-0000-0000-0000-000000000001"))
(def other-customer-id (uuid "40000000-0000-0000-0000-000000000002"))
(def helper-id (uuid "50000000-0000-0000-0000-000000000001"))
(def other-helper-id (uuid "50000000-0000-0000-0000-000000000002"))
(def capability-id (uuid "60000000-0000-0000-0000-000000000001"))
(def membership-id (uuid "70000000-0000-0000-0000-000000000001"))
(def role-id (uuid "71000000-0000-0000-0000-000000000001"))
(def t-before (Instant/parse "2026-06-30T11:59:00Z"))
(def t0 (Instant/parse "2026-07-01T12:00:00Z"))
(def t1 (Instant/parse "2026-07-01T12:01:00Z"))
(def t2 (Instant/parse "2026-07-01T12:02:00Z"))
(def t3 (Instant/parse "2026-07-01T12:03:00Z"))
(def t4 (Instant/parse "2026-07-01T12:04:00Z"))

(def canonical-content
  {:title "Need assistance"
   :details "Please help me find the correct item."
   :location-detail "Aisle 8 near the freezer case"})
(def minimal-content {:title "Need assistance" :details nil :location-detail nil})
(def user-requestor (requestor/user-requestor customer-id))
(def capability-requestor (requestor/capability-requestor capability-id))
(def location-scope {:scope/type :location :scope/id location-id})
(def organization-scope {:scope/type :organization :scope/id organization-id})
(def scope-context
  {:organization/id organization-id
   :scope/target location-scope
   :scope/applicable [location-scope organization-scope]
   :scope/operational? true})
(def location-document
  {:xt/id location-id :location/organization organization-id
   :location/revision 3 :location/created-at t0 :location/updated-at t1})
(def helper-document
  {:xt/id helper-id :user/status :active :user/revision 4
   :user/created-at t0 :user/updated-at t1})
(def customer-document (assoc helper-document :xt/id customer-id))
(def membership-document
  {:xt/id membership-id :membership/user helper-id
   :membership/organization organization-id :membership/status :active
   :membership/revision 2 :membership/created-at t0 :membership/updated-at t1})
(def role-document
  {:xt/id role-id :role-assignment/membership membership-id
   :role-assignment/organization organization-id :role-assignment/role :helper
   :role-assignment/scope-type :location :role-assignment/scope-id location-id
   :role-assignment/status :active :role-assignment/revision 1
   :role-assignment/created-at t0 :role-assignment/updated-at t1})

(defn expected-version [doc revision-key updated-at-key]
  {:model/id (:xt/id doc)
   :model/revision-key revision-key
   :model/revision (get doc revision-key)
   :model/updated-at-key updated-at-key
   :model/updated-at (get doc updated-at-key)})
(def location-guard
  {:model/entity-type :location
   :model/expected (expected-version location-document :location/revision :location/updated-at)})
(def helper-guard
  {:model/entity-type :user
   :model/expected (expected-version helper-document :user/revision :user/updated-at)})
(def customer-guard
  {:model/entity-type :user
   :model/expected (expected-version customer-document :user/revision :user/updated-at)})
(def membership-guard
  {:model/entity-type :membership
   :model/expected (expected-version membership-document :membership/revision :membership/updated-at)})
(def role-guard
  {:model/entity-type :role-assignment
   :model/expected (expected-version role-document :role-assignment/revision :role-assignment/updated-at)})

(defn open-request
  ([] (open-request {}))
  ([overrides]
   (domain/new-request
    (merge {:id request-id :organization-id organization-id :location-id location-id
            :requestor user-requestor :content canonical-content :now t0}
           overrides))))
(defn capability-owned-request [] (open-request {:requestor capability-requestor}))
(defn claimed-request []
  (domain/claim-request (open-request) {:helper-id helper-id :now t1}))
(defn on-the-way-request []
  (domain/mark-request-on-the-way (claimed-request) {:helper-id helper-id :now t2}))
(defn done-request []
  (domain/complete-request (on-the-way-request) {:helper-id helper-id :now t3}))
(defn cancelled-request []
  (domain/cancel-request (open-request) {:now t1 :reason :test/customer-cancelled}))
(defn command-document [command] (model.common/command-document command))
(defn topic-set [changes] (set (map :topic changes)))
(defn query-contains? [query x] (boolean (some #{x} (tree-seq coll? seq query))))
(defn resolver-fn [resolver]
  (:biff.graph/resolve-fn resolver))

(defn resolve-resolver
  [resolver ctx input]
  ((resolver-fn resolver)
   (assoc ctx :biff.graph/input input)))
(defn private-fn [ns-sym var-sym] (some-> (ns-resolve ns-sym var-sym) var-get))
(defn error-type [f]
  (try (f) ::did-not-throw
       (catch Throwable e
         (loop [e e]
           (when e (or (:error/type (ex-data e)) (recur (ex-cause e))))))))

(deftest requestor-test
  (is (= {:requestor/type :user :requestor/id customer-id} user-requestor))
  (is (= {:requestor/type :capability :requestor/id capability-id} capability-requestor))
  (is (requestor/requestor-reference? user-requestor))
  (is (requestor/requestor-reference? capability-requestor))
  (is (requestor/user-requestor? user-requestor))
  (is (requestor/capability-requestor? capability-requestor))
  (is (false? (requestor/requestor-reference? (assoc user-requestor :extra true))))
  (is (false? (requestor/requestor-reference?
               {:requestor/type :employee :requestor/id customer-id})))
  (let [r (open-request)]
    (is (requestor/requested-by-user? r customer-id))
    (is (false? (requestor/requested-by-user? r other-customer-id)))
    (is (requestor/controlled-by? r {:user-id customer-id}))
    (is (false? (requestor/controlled-by? r {:user-id other-customer-id}))))
  (let [r (capability-owned-request)]
    (is (requestor/requested-by-capability? r capability-id))
    (is (requestor/controlled-by? r {:capability-id capability-id}))
    (is (false? (requestor/controlled-by? r {:user-id customer-id})))))

(deftest content-test
  (is (= {:title "Need assistance" :details "More detail" :location-detail nil}
         (content/normalize-content
          {:title " Need assistance " :details " More detail " :location-detail "  "})))
  (is (content/content? canonical-content))
  (is (content/content? minimal-content))
  (is (false? (content/content? (dissoc minimal-content :details))))
  (is (false? (content/title? " Need assistance ")))
  (is (= #{:title :details :location-detail}
         (set (keys
               (content/content-errors
                {:title ""
                 :details (apply str (repeat (inc content/details-max) "x"))
                 :location-detail (apply str (repeat (inc content/location-detail-max) "x"))})))))
  (let [changed (content/apply-content
                 (open-request)
                 {:title " Updated " :details " " :location-detail nil})]
    (is (= "Updated" (:request/title changed)))
    (is (not (contains? changed :request/details)))
    (is (not (contains? changed :request/location-detail)))
    (is (= {:title "Updated" :details nil :location-detail nil}
           (content/content changed)))))

(deftest construction-and-schema-test
  (let [r (open-request)]
    (is (= request-id (:xt/id r)))
    (is (= organization-id (:request/organization r)))
    (is (= location-id (:request/location r)))
    (is (= :user (:request/requestor-type r)))
    (is (= customer-id (:request/requestor-id r)))
    (is (= :open (:request/status r)))
    (is (= 0 (:request/revision r)))
    (is (= t0 (:request/created-at r)))
    (is (= t0 (:request/updated-at r)))
    (is (= canonical-content (domain/content r)))
    (is (entity/structurally-consistent? r))
    (is (lifecycle/lifecycle-consistent? r))
    (is (domain/request-consistent? r))
    (is (m/validate request.schema/request-document-schema r))
    (is (m/validate (:request request.schema/schema) r))
    (is (false? (m/validate request.schema/request-document-schema
                            (assoc r :unexpected/value true)))))
  (is (domain/request-consistent? (capability-owned-request)))
  (is (= :request/invalid-create-input
         (error-type #(domain/new-request
                       {:id request-id :organization-id organization-id
                        :location-id location-id :requestor user-requestor
                        :content {:title ""} :now t0}))))
  (is (= #{:id :organization-id :location-id :requestor :content :now}
         (set (keys
               (domain/create-input-errors
                {:id nil :organization-id nil :location-id nil
                 :requestor nil :content {:title ""} :now nil}))))))

(deftest invariant-test
  (let [r (open-request)
        unknown (assoc r :request/status :future-status)]
    (is (entity/structurally-consistent? unknown))
    (is (false? (lifecycle/lifecycle-consistent? unknown)))
    (is (false? (domain/request-consistent? unknown)))
    (is (false? (domain/request-consistent? (assoc r :request/helper helper-id))))
    (is (false? (domain/request-consistent? (assoc r :request/cancelled-at t0))))
    (is (false?
         (domain/request-consistent?
          (-> r
              (assoc :request/status :on-the-way
                     :request/helper helper-id
                     :request/claimed-at t2
                     :request/on-the-way-at t1
                     :request/revision 2
                     :request/updated-at t2)))))
    (is (= :request/immutable-identity
           (error-type #(entity/revise r t1
                                       (fn [x] (assoc x :request/location other-location-id))))))
    (is (= :request/invalid-version-mutation
           (error-type #(entity/revise r t1
                                       (fn [x] (assoc x :request/revision 99))))))))

(deftest editing-test
  (let [edited
        (domain/edit-request
         (open-request)
         {:content {:title " Updated request " :details " New details "
                    :location-detail " Front counter "}
          :now t1})]
    (is (= "Updated request" (:request/title edited)))
    (is (= "New details" (:request/details edited)))
    (is (= "Front counter" (:request/location-detail edited)))
    (is (= 1 (:request/revision edited)))
    (is (= t1 (:request/updated-at edited)))
    (is (domain/request-consistent? edited)))
  (let [claimed (claimed-request)
        edited (domain/edit-request
                claimed
                {:content {:title "Corrected after claim" :details nil
                           :location-detail "New aisle"}
                 :now t2})
        traveling (domain/mark-request-on-the-way
                   edited {:helper-id helper-id :now t3})
        reedited (domain/edit-request
                  traveling
                  {:content {:title "Corrected while traveling"
                             :details "Updated note"
                             :location-detail "Service desk"}
                   :now t4})]
    (is (domain/editable? claimed))
    (is (domain/editable? traveling))
    (is (= :on-the-way (:request/status reedited)))
    (is (= helper-id (:request/helper reedited)))
    (is (= 4 (:request/revision reedited))))
  (is (= :request/not-editable
         (error-type #(domain/edit-request
                       (done-request) {:content minimal-content :now t4}))))
  (is (= :request/not-editable
         (error-type #(domain/edit-request
                       (cancelled-request) {:content minimal-content :now t2}))))
  (is (= :request/unchanged
         (error-type #(domain/edit-request
                       (open-request) {:content canonical-content :now t1}))))
  (is (= :request/invalid-change-time
         (error-type #(domain/edit-request
                       (open-request) {:content minimal-content :now t-before})))))

(deftest lifecycle-happy-path-test
  (let [open (open-request)
        claimed (domain/claim-request open {:helper-id helper-id :now t1})
        traveling (domain/mark-request-on-the-way
                   claimed {:helper-id helper-id :now t2})
        completed (domain/complete-request
                   traveling {:helper-id helper-id :now t3})]
    (is (= :claimed (domain/next-status open :claim)))
    (is (= :on-the-way (domain/next-status claimed :mark-on-the-way)))
    (is (= :done (domain/next-status traveling :complete)))
    (is (nil? (domain/next-status open :complete)))
    (is (domain/actively-assigned? claimed))
    (is (domain/assigned-to? claimed helper-id))
    (is (= t1 (:request/claimed-at claimed)))
    (is (domain/actively-assigned? traveling))
    (is (= t2 (:request/on-the-way-at traveling)))
    (is (domain/done? completed))
    (is (= helper-id (:request/helper completed)))
    (is (= t3 (:request/completed-at completed)))
    (is (domain/has-helper? completed))
    (is (false? (domain/actively-assigned? completed)))
    (is (false? (domain/assigned-to? completed helper-id)))
    (is (= 3 (:request/revision completed)))
    (is (domain/request-consistent? completed))))

(deftest unclaim-cancel-and-invalid-transition-test
  (let [unclaimed (domain/unclaim-request
                   (claimed-request) {:helper-id helper-id :now t2})
        reclaimed (domain/claim-request
                   unclaimed {:helper-id other-helper-id :now t3})]
    (is (domain/open? unclaimed))
    (is (not (contains? unclaimed :request/helper)))
    (is (not (contains? unclaimed :request/claimed-at)))
    (is (= other-helper-id (:request/helper reclaimed)))
    (is (domain/request-consistent? reclaimed)))
  (doseq [[active now] [[(open-request) t1]
                        [(claimed-request) t2]
                        [(on-the-way-request) t3]]]
    (let [cancelled (domain/cancel-request
                     active {:now now :reason :test/customer-cancelled})]
      (is (domain/cancelled? cancelled))
      (is (= now (:request/cancelled-at cancelled)))
      (is (false? (domain/actively-assigned? cancelled)))
      (is (domain/request-consistent? cancelled))))
  (is (= :request/not-assigned-helper
         (error-type #(domain/unclaim-request
                       (claimed-request) {:helper-id other-helper-id :now t2}))))
  (is (= :request/invalid-transition
         (error-type #(domain/unclaim-request
                       (on-the-way-request) {:helper-id helper-id :now t3}))))
  (is (= :request/invalid-transition
         (error-type #(domain/complete-request
                       (open-request) {:helper-id helper-id :now t1}))))
  (is (= :request/invalid-transition
         (error-type #(domain/claim-request
                       (claimed-request) {:helper-id other-helper-id :now t2}))))
  (is (= :request/not-assigned-helper
         (error-type #(domain/complete-request
                       (claimed-request) {:helper-id other-helper-id :now t2}))))
  (is (= :request/invalid-cancellation-reason
         (error-type #(domain/cancel-request
                       (open-request) {:now t1 :reason :unqualified})))))

(deftest command-test
  (let [create-command
        (domain/create-command
         {:id request-id :organization-id organization-id :location-id location-id
          :requestor user-requestor :content canonical-content :now t0})
        open (command-document create-command)
        claim-command (domain/claim-command
                       open {:helper-id helper-id :now t1})]
    (is (= :request (:model/entity-type create-command)))
    (is (= :create (:model/operation create-command)))
    (is (= request-id (:model/id create-command)))
    (is (nil? (:model/expected create-command)))
    (is (= :claim (:model/operation claim-command)))
    (is (= (model.common/expected-version open domain/version)
           (:model/expected claim-command)))
    (is (= helper-id (:request/helper (command-document claim-command)))))
  (is (= [:create :edit :claim :unclaim :mark-on-the-way :complete :cancel]
         domain/operation-order))
  (is (every? domain/operation? domain/operation-order)))

(deftest schema-registry-test
  (doseq [r [(open-request) (claimed-request) (done-request) (cancelled-request)]]
    (is (m/validate request.schema/request-document-schema r))
    (is (m/validate (:request/doc request.schema/schema) r)))
  (is (m/validate request.schema/requestor-reference-schema user-requestor))
  (is (m/validate request.schema/content-schema canonical-content))
  (is (m/validate request.schema/expected-version-schema
                  (model.common/expected-version (open-request) domain/version)))
  (is (m/validate (:request/include-terminal? request.schema/schema) true))
  (is (m/validate (:request/include-terminal? request.schema/schema) false))
  (is (false? (m/validate (:request/include-terminal? request.schema/schema) :yes)))
  (is (= request.schema/request-document-schema (:request request.schema/schema)))
  (is (= request.schema/request-document-schema (:request/doc request.schema/schema)))
  (is (= request.schema/requestor-reference-schema
         (:request/requestor request.schema/schema)))
  (is (false? (m/validate request.schema/request-document-schema
                          (assoc (open-request) :request/helper helper-id))))
  (is (false? (m/validate request.schema/status-schema :unknown))))

;; =============================================================================
;; Graph
;; =============================================================================

(deftest graph-contract-test
  (is (= {:request/id request-id}
         (request.graph/request-query-input {:request-id request-id})))
  (is (= {} (request.graph/request-query-input {:request-id nil})))
  (is (= {:request/organization-id organization-id
          :request/location-id location-id
          :request/include-terminal? false}
         (request.graph/location-requests-query-input
          {:organization-id organization-id :location-id location-id})))
  (is (= {:request/organization-id organization-id
          :request/location-id location-id
          :request/include-terminal? true}
         (request.graph/location-requests-query-input
          {:organization-id organization-id :location-id location-id
           :include-terminal? true})))
  (is (= {:request/include-terminal? false}
         (request.graph/location-requests-query-input {})))
  (doseq [x [:request/found? :request/doc :request/expected-version]]
    (is (query-contains? request.graph/request-command-query x)))
  (doseq [x [:request/requestor :request/status :request/editable?
             :request/claimable? :request/actively-assigned?]]
    (is (query-contains? request.graph/request-facts-query x)))
  (doseq [x [:request/location-requests :request/doc :request/expected-version
             :request/active? :request/terminal?]]
    (is (query-contains? request.graph/location-requests-query x)))
  (let [q-fn (private-fn 'net.humanhelp.site.model.request.graph 'q)
        calls (atom [])]
    (is (ifn? q-fn))
    (with-redefs [biffx/q
                  (fn [conn query]
                    (swap! calls conj [conn query])
                    :result)]
      (is (= :result
             (q-fn {:biff/conn :canonical :biff/node :wrong}
                   {:select [:xt/id]}))))
    (is (= [[:canonical {:select [:xt/id]}]] @calls))))

(deftest graph-field-and-lifecycle-resolver-test
  (let [r (claimed-request)
        fields
        (resolve-resolver
         request.graph/request-fields
         {}
         {:request/doc r})

        facts
        (resolve-resolver
         request.graph/request-lifecycle-facts
         {}
         {:request/doc r})]
    (is (= request-id (:request/id fields)))
    (is (= organization-id (:request/organization-id fields)))
    (is (= location-id (:request/location-id fields)))
    (is (= user-requestor (:request/requestor fields)))
    (is (= (model.common/expected-version r domain/version)
           (:request/expected-version fields)))
    (is (true? (:request/claimed? facts)))
    (is (true? (:request/actively-assigned? facts)))
    (is (false? (:request/open? facts)))
    (is (false? (:request/terminal? facts)))))

(deftest graph-request-by-id-resolver-test
  (let [calls (atom [])
        r (open-request)]
    (with-redefs [biffx/q
                  (fn [conn query]
                    (swap! calls conj [conn query])
                    [r])]
      (is (= {:request/found? true :request/doc r}
             (resolve-resolver
              request.graph/request-by-id
              {:biff/conn :connection}
              {:request/id request-id}))))
    (is (= :connection (ffirst @calls)))
    (is (query-contains? (second (first @calls)) request-id)))
  (with-redefs [biffx/q (fn [& _] [])]
    (is (= {:request/found? false}
           (resolve-resolver
            request.graph/request-by-id
            {:biff/conn :connection}
            {:request/id other-request-id})))))

(deftest graph-location-collection-resolver-test
  (let [open (open-request)
        claimed (assoc (claimed-request) :xt/id other-request-id)
        calls (atom [])]
    (with-redefs [biffx/q
                  (fn [conn query]
                    (swap! calls conj [conn query])
                    [claimed open])]
      (is (= {:request/location-requests
              [{:request/doc claimed} {:request/doc open}]}
             (resolve-resolver
              request.graph/requests-at-location
              {:biff/conn :connection}
              {:request/organization-id organization-id
               :request/location-id location-id
               :request/include-terminal? false}))))
    (let [[conn query] (first @calls)]
      (is (= :connection conn))
      (is (= request.graph/request-document-columns (:select query)))
      (is (= domain/entity-type (:from query)))
      (is (query-contains? (:where query)
                           [:= :request/organization organization-id]))
      (is (query-contains? (:where query)
                           [:= :request/location location-id]))
      (is (query-contains? (:where query)
                           request.graph/active-status-predicate))
      (is (= [[:request/created-at :desc] [:xt/id :desc]]
             (:order-by query)))))
  (let [captured (atom nil)]
    (with-redefs [biffx/q
                  (fn [_ query]
                    (reset! captured query)
                    [(done-request)])]
      (is (= {:request/location-requests [{:request/doc (done-request)}]}
             (resolve-resolver
              request.graph/requests-at-location
              {:biff/conn :connection}
              {:request/organization-id organization-id
               :request/location-id location-id
               :request/include-terminal? true}))))
    (is (false? (query-contains? (:where @captured)
                                 request.graph/active-status-predicate))))
  (let [called? (atom false)]
    (with-redefs [biffx/q
                  (fn [& _]
                    (reset! called? true)
                    [])]
      (is (= {:request/location-requests []}
             (resolve-resolver
              request.graph/requests-at-location
              {:biff/conn :connection}
              {:request/organization-id nil
               :request/location-id location-id
               :request/include-terminal? false}))))
    (is (false? @called?))))

;; =============================================================================
;; FX
;; =============================================================================

(deftest fx-plan-test
  (let [create-command
        (domain/create-command
         {:id request-id :organization-id organization-id :location-id location-id
          :requestor user-requestor :content canonical-content :now t0})
        create-guards [location-guard customer-guard]
        create-tx
        (:transaction-plan
         (request.fx/plan-create-request
          {:command create-command :authorization-versions create-guards}))
        create-change (first (:changes create-tx))
        before (open-request)
        claim-command (domain/claim-command before {:helper-id helper-id :now t1})
        update-guards [location-guard helper-guard membership-guard role-guard]
        update-tx
        (:transaction-plan
         (request.fx/plan-update-request
          {:before before :command claim-command
           :authorization-versions update-guards}))
        update-change (first (:changes update-tx))]
    (is (= [create-command] (:commands create-tx)))
    (is (= create-guards (:authorization-versions create-tx)))
    (is (not (contains? create-tx :assertions)))
    (is (= :created (:change/kind create-change)))
    (is (= :create (:request/operation create-change)))
    (is (= {:coalesce-key [:request request-id]}
           ((:entry-fn create-tx) create-change)))
    (is (= [claim-command] (:commands update-tx)))
    (is (= update-guards (:authorization-versions update-tx)))
    (is (not (contains? update-tx :assertions)))
    (is (= :updated (:change/kind update-change)))
    (is (= :open (:request/previous-status update-change)))
    (is (= :claimed (:request/status update-change)))
    (is (= helper-id (:request/helper update-change)))
    (is (= #{:request} (topic-set (:changes update-tx))))))

(deftest fx-plan-authorization-boundary-test
  (let [command
        (domain/create-command
         {:id request-id :organization-id organization-id :location-id location-id
          :requestor user-requestor :content canonical-content :now t0})
        duplicate-guards [location-guard location-guard]
        malformed-guard
        {:model/entity-type :location
         :model/expected {:model/id location-id}}]
    (is (= duplicate-guards
           (get-in
            (request.fx/plan-create-request
             {:command command :authorization-versions duplicate-guards})
            [:transaction-plan :authorization-versions])))
    (is (= [malformed-guard]
           (get-in
            (request.fx/plan-create-request
             {:command command :authorization-versions [malformed-guard]})
            [:transaction-plan :authorization-versions])))
    (is (= :request.fx/invalid-authorization-versions
           (error-type
            #(request.fx/plan-create-request
              {:command command
               :authorization-versions #{location-guard}}))))))

(defn base-stubs
  ([helper?]
   {#'organization/location-context
    (fn [& _]
      {:location/found? true
       :location/doc location-document
       :location/organization-id organization-id
       :location/scope-context scope-context
       :location/authorization-versions [location-guard]})
    #'organization/location-scope
    (fn [id] {:scope/type :location :scope/id id})
    #'organization/scope-context? (constantly true)
    #'organization/same-scope? =
    #'user/user-facts
    (fn [_ {:keys [user-id]}]
      {:user/found? true
       :user/doc (if (= customer-id user-id)
                   customer-document
                   helper-document)})
    #'user/user-active? (constantly true)
    #'user/access-context
    (fn [& _]
      {:user/id helper-id
       :organization/id organization-id
       :scope/target location-scope
       :membership/id membership-id
       :membership/active? true
       :user/effective-roles (if helper? #{:helper} #{})
       :user/capabilities #{}
       :user/helper? helper?
       :user/supervisor? false
       :user/admin? false
       :user/staff? helper?})
    #'user/membership-facts
    (fn [& _]
      {:membership/found? true :membership/doc membership-document})
    #'user/active-role-assignments-at-scope
    (fn [& _]
      {:user/active-role-assignments-at-scope
       [{:role-assignment/doc role-document}]})
    #'user/effective-assignment-for-role
    (fn [& _] (when helper? role-document))})
  ([helper? overrides]
   (merge (base-stubs helper?) overrides)))

(defn with-stubs
  ([helper? f] (with-stubs helper? {} f))
  ([helper? overrides f]
   (with-redefs-fn (base-stubs helper? overrides) f)))

(defn plan-state
  [r operation input current-user-id now]
  (request.fx/update-request-machine
   {:request.fx/base-ctx {:current-user/id current-user-id :biff.fx/now now}
    :request.fx/input (merge {:request-id (domain/request-id r)} input)
    :request.fx/operation operation
    :request.fx/request-id (domain/request-id r)
    :request.fx/request-facts {:request/found? true :request/doc r}}
   :plan))

(deftest fx-authorization-state-test
  (testing "effective helper may claim"
    (with-stubs
     true
     (fn []
       (let [state (plan-state (open-request) :claim {} helper-id t2)
             claimed (get-in state [:request.fx/result :request])
             tx (:request.fx/transaction-plan state)]
         (is (= :claimed (:request/status claimed)))
         (is (= helper-id (:request/helper claimed)))
         (is (= [location-guard helper-guard membership-guard role-guard]
                (:authorization-versions tx)))
         (is (not (contains? tx :assertions)))
         (is (= :commit (:biff.fx/next state)))))))
  (testing "non-helper may not claim"
    (with-stubs
     false
     (fn []
       (is (= :user/not-authorized
              (error-type
               #(plan-state (open-request) :claim {} helper-id t2)))))))
  (testing "owner may edit and stranger may not"
    (with-stubs
     true
     (fn []
       (let [state
             (plan-state
              (open-request) :edit
              {:content {:title "Owner edit" :details nil :location-detail nil}}
              customer-id t2)]
         (is (= "Owner edit"
                (get-in state [:request.fx/result :request :request/title])))
         (is (= [location-guard customer-guard]
                (get-in state
                        [:request.fx/transaction-plan
                         :authorization-versions]))))
       (is (= :request/not-authorized
              (error-type
               #(plan-state
                 (open-request) :edit {:content minimal-content}
                 other-customer-id t2)))))))
  (testing "capability writes fail explicitly until authentication exists"
    (with-stubs
     true
     (fn []
       (is (= :request/capability-authorization-unavailable
              (error-type
               #(plan-state
                 (capability-owned-request) :edit {:content minimal-content}
                 customer-id t2)))))))
  (testing "unclaim does not require a still-current helper role"
    (with-stubs
     true
     {#'user/access-context
      (fn [& _] (throw (AssertionError. "must not load helper access")))
      #'user/active-role-assignments-at-scope
      (fn [& _] (throw (AssertionError. "must not load role assignments")))
      #'user/effective-assignment-for-role
      (fn [& _] (throw (AssertionError. "must not derive helper role")))}
     (fn []
       (let [state (plan-state (claimed-request) :unclaim {} helper-id t2)
             unclaimed (get-in state [:request.fx/result :request])]
         (is (domain/open? unclaimed))
         (is (not (contains? unclaimed :request/helper)))
         (is (= [location-guard helper-guard]
                (get-in state
                        [:request.fx/transaction-plan
                         :authorization-versions])))
         (is (= :commit (:biff.fx/next state))))))))

(deftest fx-location-authorization-contract-test
  (with-stubs
   true
   {#'organization/location-context
    (fn [& _]
      {:location/found? true
       :location/doc location-document
       :location/organization-id organization-id
       :location/scope-context scope-context
       :location/authorization-versions #{location-guard}})}
   (fn []
     (is (= :request.fx/invalid-location-authorization-versions
            (error-type
             #(plan-state (open-request) :claim {} helper-id t2))))))
  (let [malformed
        {:model/entity-type :location
         :model/expected {:model/id location-id}}]
    (with-stubs
     true
     {#'organization/location-context
      (fn [& _]
        {:location/found? true
         :location/doc location-document
         :location/organization-id organization-id
         :location/scope-context scope-context
         :location/authorization-versions [malformed]})}
     (fn []
       (is (= malformed
              (first
               (get-in
                (plan-state (open-request) :claim {} helper-id t2)
                [:request.fx/transaction-plan
                 :authorization-versions]))))))))

(deftest fx-start-state-contract-test
  (let [ctx {:request.fx/input {:request-id request-id}
             :request.fx/operation :claim}
        state (request.fx/update-request-machine ctx :start)
        descriptor (:request.fx/request-facts state)]
    (is (= ctx (:request.fx/base-ctx state)))
    (is (= request-id (:request.fx/request-id state)))
    (is (= :biff.graph.fx/query (first descriptor)))
    (is (= {:request/id request-id} (second descriptor)))
    (is (= request.graph/request-command-query (nth descriptor 2)))
    (is (= :plan (:biff.fx/next state))))
  (with-stubs
   true
   (fn []
     (let [state
           (request.fx/create-request-machine
            {:current-user/id customer-id
             :biff.fx/now t2
             :biff.fx/seed 42
             :request.fx/input
             {:organization-id organization-id
              :location-id location-id
              :content canonical-content}}
            :start)
           created (get-in state [:request.fx/result :request])]
       (is (instance? UUID (:xt/id created)))
       (is (= 7 (.version ^UUID (:xt/id created))))
       (is (= customer-id (:request/requestor-id created)))
       (is (domain/request-consistent? created))
       (is (= [location-guard customer-guard]
              (get-in state
                      [:request.fx/transaction-plan
                       :authorization-versions])))
       (is (= :commit (:biff.fx/next state)))))))

;; =============================================================================
;; Core facade
;; =============================================================================

(deftest core-contract-test
  (is (= request.schema/schema request/schema))
  (is (= request.graph/resolvers request/resolvers))
  (is (= {:schema request/schema :biff.graph/resolvers request/resolvers}
         request/module))
  (is (= request.graph/request-document-query request/request-document-query))
  (is (= request.graph/request-command-query request/request-command-query))
  (is (= request.graph/request-facts-query request/request-query))
  (is (= request.graph/location-requests-query request/location-requests-query))
  (is (= :request request/request-entity-type))
  (is (= domain/statuses request/statuses))
  (is (= domain/operations request/operations-set))
  (doseq [[key v]
          [[:request/create #'request/create-request]
           [:request/edit #'request/edit-request]
           [:request/claim #'request/claim-request]
           [:request/unclaim #'request/unclaim-request]
           [:request/mark-on-the-way #'request/mark-request-on-the-way]
           [:request/complete #'request/complete-request]
           [:request/cancel #'request/cancel-request]]]
    (is (identical? v (get request/operations key))))
  (doseq [sym '[create-command new-request plan-create-request
                location-requests-query-input]]
    (is (nil? (ns-resolve 'net.humanhelp.site.model.request.core sym)))))

(deftest core-read-delegation-test
  (let [queries (atom [])
        ctx {:ctx true}
        location-input
        {:organization-id organization-id
         :location-id location-id
         :include-terminal? true}]
    (with-redefs [graph/query
                  (fn [query-ctx input query]
                    (swap! queries conj [query-ctx input query])
                    :facts)]
      (is (= :facts (request/request-facts ctx request-id)))
      (is (= :facts (request/request-command-facts ctx request-id)))
      (is (= :facts (request/location-requests ctx location-input))))
    (is (= [[ctx {:request/id request-id} request/request-query]
            [ctx {:request/id request-id} request/request-command-query]
            [ctx
             {:request/organization-id organization-id
              :request/location-id location-id
              :request/include-terminal? true}
             request/location-requests-query]]
           @queries))))

(deftest core-operation-delegation-test
  (let [calls (atom [])
        ctx {:ctx true}
        input {:request-id request-id}]
    (with-redefs
     [request.fx/create-request
      (fn [c i] (swap! calls conj [:create c i]) :create)
      request.fx/edit-request
      (fn [c i] (swap! calls conj [:edit c i]) :edit)
      request.fx/claim-request
      (fn [c i] (swap! calls conj [:claim c i]) :claim)
      request.fx/unclaim-request
      (fn [c i] (swap! calls conj [:unclaim c i]) :unclaim)
      request.fx/mark-request-on-the-way
      (fn [c i] (swap! calls conj [:on-the-way c i]) :on-the-way)
      request.fx/complete-request
      (fn [c i] (swap! calls conj [:complete c i]) :complete)
      request.fx/cancel-request
      (fn [c i] (swap! calls conj [:cancel c i]) :cancel)]
      (is (= :create (request/create-request ctx input)))
      (is (= :edit (request/edit-request ctx input)))
      (is (= :claim (request/claim-request ctx input)))
      (is (= :unclaim (request/unclaim-request ctx input)))
      (is (= :on-the-way (request/mark-request-on-the-way ctx input)))
      (is (= :complete (request/complete-request ctx input)))
      (is (= :cancel (request/cancel-request ctx input))))
    (is (= 7 (count @calls)))
    (is (every? #(= ctx (nth % 1)) @calls))
    (is (every? #(= input (nth % 2)) @calls))))
