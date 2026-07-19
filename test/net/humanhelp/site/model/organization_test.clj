(ns net.humanhelp.site.model.organization-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [malli.core :as m]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.organization.domain :as domain]
   [net.humanhelp.site.model.organization.fx :as organization.fx]
   [net.humanhelp.site.model.organization.graph :as organization.graph]
   [net.humanhelp.site.model.organization.schema :as organization.schema])
  (:import
   [java.time Instant]
   [java.util UUID]))

(defn uuid [value] (UUID/fromString value))

(def organization-id (uuid "10000000-0000-0000-0000-000000000001"))

(def other-organization-id (uuid "10000000-0000-0000-0000-000000000002"))

(def root-group-id (uuid "20000000-0000-0000-0000-000000000001"))

(def child-group-id (uuid "20000000-0000-0000-0000-000000000002"))

(def destination-group-id (uuid "20000000-0000-0000-0000-000000000003"))

(def other-organization-group-id (uuid "20000000-0000-0000-0000-000000000004"))

(def location-id (uuid "30000000-0000-0000-0000-000000000001"))

(def other-location-id (uuid "30000000-0000-0000-0000-000000000002"))

(def actor-id (uuid "40000000-0000-0000-0000-000000000001"))

(def user-id (uuid "50000000-0000-0000-0000-000000000001"))

(def membership-id (uuid "60000000-0000-0000-0000-000000000001"))

(def role-assignment-id (uuid "70000000-0000-0000-0000-000000000001"))

(def t-before (Instant/parse "2026-06-30T12:00:00Z"))

(def t0 (Instant/parse "2026-07-01T12:00:00Z"))

(def t1 (Instant/parse "2026-07-01T12:01:00Z"))

(def t2 (Instant/parse "2026-07-01T12:02:00Z"))

(def t3 (Instant/parse "2026-07-01T12:03:00Z"))

(def organization-scope (domain/organization-scope organization-id))

(def root-group-scope (domain/organization-group-scope root-group-id))

(def child-group-scope (domain/organization-group-scope child-group-id))

(def destination-group-scope (domain/organization-group-scope destination-group-id))

(def location-scope (domain/location-scope location-id))

(defn active-organization ([] (active-organization {})) ([overrides] (domain/new-organization (merge
     {:id organization-id :name "Human Help" :now t0} overrides))))

(defn active-group ([] (active-group {})) ([overrides] (domain/new-organization-group (merge {:id root-group-id
      :organization-id organization-id :parent-scope organization-scope :name "Operations" :now t0}
     overrides))))

(defn child-group ([] (child-group {})) ([overrides] (active-group (merge {:id child-group-id
      :parent-scope root-group-scope :name "North Region"} overrides))))

(defn destination-group ([] (destination-group {})) ([overrides] (active-group (merge {:id destination-group-id
      :parent-scope organization-scope :name "South Region"} overrides))))

(defn active-location ([] (active-location {})) ([overrides] (domain/new-location (merge {:id location-id
      :organization-id organization-id :parent-scope child-group-scope :name "Downtown" :now t0} overrides))))

(defn error-type [f] (try (f) ::did-not-throw (catch Throwable error (loop [error error] (when error (or
           (:error/type (ex-data error)) (recur (ex-cause error))))))))

(defn command-document [command] (model.common/command-document command))

(defn query-contains? [query value] (boolean (some #{value} (tree-seq coll? seq query))))

(defn expected-version [document revision-key updated-at-key] {:model/id (:xt/id document) :model/revision-key
   revision-key :model/revision (get document revision-key) :model/updated-at-key updated-at-key
   :model/updated-at (get document updated-at-key)})

(defn authorization-version [entity-type document revision-key updated-at-key] {:model/entity-type entity-type
   :model/expected (expected-version document revision-key updated-at-key)})

(defn organization-guard [document] (authorization-version :organization document :organization/revision
   :organization/updated-at))

(defn group-guard [document] (authorization-version :organization-group document :organization-group/revision
   :organization-group/updated-at))

(defn location-guard [document] (authorization-version :location document :location/revision
   :location/updated-at))

(defn user-authorization ([] (user-authorization 0)) ([revision] {:user/id user-id :organization/id
    organization-id :scope/target organization-scope :user/authorization-versions [{:model/entity-type :user
      :model/expected {:model/id user-id :model/revision-key :user/revision :model/revision revision
       :model/updated-at-key :user/updated-at :model/updated-at t0}} {:model/entity-type :membership
      :model/expected {:model/id membership-id :model/revision-key :membership/revision :model/revision revision
       :model/updated-at-key :membership/updated-at :model/updated-at t0}} {:model/entity-type :role-assignment
      :model/expected {:model/id role-assignment-id :model/revision-key :role-assignment/revision
       :model/revision revision :model/updated-at-key :role-assignment/updated-at :model/updated-at t0}}]}))

(defn scope-facts [document scope-context authorization-versions] {:document document
   :scope-context scope-context :authorization-versions authorization-versions})

(defn topic-set [changes] (set (map :topic changes)))

(deftest name-status-and-scope-values-test (testing "names normalize once and canonical values validate"
    (is (= "Human Help" (domain/normalize-name "  Human Help  "))) (is (domain/name? "Human Help")) (is (false?
         (domain/name? " Human Help "))) (is (false? (domain/name? ""))) (is (false? (domain/name? (apply str
                 (repeat 161 "x")))))) (testing "lifecycle statuses and transitions are explicit"
    (is (= #{:active :suspended :closed} domain/statuses)) (is (domain/status? :active))
    (is (domain/active-status? :active)) (is (domain/suspended-status? :suspended))
    (is (domain/closed-status? :closed)) (is (= :suspended (domain/next-status :active :suspend)))
    (is (= :active (domain/next-status :suspended :reactivate))) (is (= :closed
           (domain/next-status :active :close))) (is (nil? (domain/next-status :closed :reactivate))))
  (testing "scope values are structural and typed" (is (= {:scope/type :organization :scope/id organization-id}
           organization-scope)) (is (= {:scope/type :organization-group :scope/id root-group-id}
           root-group-scope)) (is (= {:scope/type :location :scope/id location-id} location-scope))
    (is (domain/scope-reference? organization-scope)) (is (domain/parent-scope-reference? root-group-scope))
    (is (false? (domain/parent-scope-reference? location-scope)))
    (is (domain/organization-scope? organization-scope))
    (is (domain/organization-group-scope? root-group-scope)) (is (domain/location-scope? location-scope))
    (is (domain/same-scope? root-group-scope (domain/organization-group-scope root-group-id))) (is (false?
         (domain/same-scope? root-group-scope child-group-scope)))))

(deftest create-input-normalization-and-errors-test (is (= {:id organization-id :name "Human Help" :now t0}
         (domain/normalize-organization-create-input {:id organization-id :name "  Human Help  " :now t0})))
  (is (= {:id root-group-id :organization-id organization-id :parent-scope organization-scope :name "Operations"
          :now t0} (domain/normalize-organization-group-create-input {:id root-group-id
           :organization-id organization-id :parent-scope organization-scope :name " Operations " :now t0})))
  (is (= {:id location-id :organization-id organization-id :parent-scope child-group-scope :name "Downtown"
          :now t0} (domain/normalize-location-create-input {:id location-id :organization-id organization-id
           :parent-scope child-group-scope :name " Downtown " :now t0}))) (is (= #{:id :name :now} (set (keys
           (domain/organization-create-input-errors {:id nil :name "" :now nil}))))) (is (= #{:id
           :organization-id :parent-scope :name :now} (set (keys (domain/organization-group-create-input-errors
            {:id nil :organization-id nil :parent-scope location-scope :name "" :now nil}))))) (is (= #{:id
           :organization-id :parent-scope :name :now} (set (keys (domain/location-create-input-errors {:id nil
             :organization-id nil :parent-scope location-scope :name "" :now nil}))))))

(deftest organization-construction-and-schema-test (let [document (active-organization)] (is (= organization-id
           (:xt/id document))) (is (= "Human Help" (:organization/name document))) (is (= :active
           (:organization/status document))) (is (= 0 (:organization/revision document))) (is (= t0
           (:organization/created-at document))) (is (= t0 (:organization/updated-at document)))
    (is (domain/organization-document-consistent? document)) (is (m/validate
         organization.schema/organization-document-schema document)) (is (false? (m/validate
          organization.schema/organization-document-schema (assoc document :unexpected/value true)))))
  (is (= :organization/invalid-create-input (error-type #(domain/new-organization {:id organization-id :name " "
             :now t0})))))

(deftest organization-group-construction-and-schema-test (let [document (active-group)] (is (= root-group-id
           (:xt/id document))) (is (= organization-id (:organization-group/organization document)))
    (is (= :organization (:organization-group/parent-type document))) (is (= organization-id
           (:organization-group/parent-id document))) (is (= organization-scope
           (domain/organization-group-parent-scope document))) (is (= :active
           (:organization-group/status document))) (is (domain/organization-group-document-consistent?
         document)) (is (m/validate organization.schema/organization-group-document-schema document)))
  (is (= :organization-group/invalid-create-input (error-type #(domain/new-organization-group {:id root-group-id
             :organization-id organization-id :parent-scope (domain/organization-group-scope root-group-id)
             :name "Self" :now t0})))))

(deftest location-construction-and-schema-test (let [document (active-location)] (is (= location-id
           (:xt/id document))) (is (= organization-id (:location/organization document)))
    (is (= :organization-group (:location/parent-type document))) (is (= child-group-id
           (:location/parent-id document))) (is (= child-group-scope (domain/location-parent-scope document)))
    (is (= :active (:location/status document))) (is (domain/location-document-consistent? document))
    (is (m/validate organization.schema/location-document-schema document)))
  (is (= :location/invalid-create-input (error-type #(domain/new-location {:id location-id
             :organization-id organization-id :parent-scope location-scope :name "Self-parent" :now t0})))))

(deftest document-invariant-regression-test (let [organization-document (active-organization) group-document
        (active-group) location-document (active-location)] (is (false?
         (domain/organization-document-consistent? (assoc organization-document
                 :organization/status :suspended)))) (is (false? (domain/organization-group-document-consistent?
          (assoc group-document :organization-group/parent-type :location)))) (is (false?
         (domain/location-document-consistent? (assoc location-document :location/status :closed)))) (is (false?
         (domain/location-document-consistent? (assoc location-document :location/updated-at t-before))))))

(deftest rename-transitions-test (let [renamed-organization (domain/rename-organization (active-organization)
         {:name "  Human Help Cooperative  " :now t1}) renamed-group (domain/rename-organization-group
         (active-group) {:name "  Store Operations  " :now t1}) renamed-location (domain/rename-location
         (active-location) {:name "  Downtown Store  " :now t1})] (is (= "Human Help Cooperative"
           (:organization/name renamed-organization))) (is (= "Store Operations"
           (:organization-group/name renamed-group))) (is (= "Downtown Store"
           (:location/name renamed-location))) (is (= 1 (:organization/revision renamed-organization))) (is (= 1
           (:organization-group/revision renamed-group))) (is (= 1 (:location/revision renamed-location)))
    (is (= t1 (:organization/updated-at renamed-organization))) (is (= t1
           (:organization-group/updated-at renamed-group))) (is (= t1 (:location/updated-at renamed-location))))
  (is (= :organization/unchanged (error-type #(domain/rename-organization (active-organization)
            {:name "Human Help" :now t1})))) (is (= :organization-group/invalid-input (error-type
          #(domain/rename-organization-group (active-group) {:name "" :now t1})))) (is (= :location/invalid-time
         (error-type #(domain/rename-location (active-location) {:name "Earlier" :now t-before})))))

(deftest organization-group-move-test (let [moved (domain/move-organization-group (child-group)
         {:parent-scope destination-group-scope :now t1 :actor-id actor-id :reason :test/reorganization})]
    (is (= destination-group-scope (domain/organization-group-parent-scope moved))) (is (= t1
           (:organization-group/moved-at moved))) (is (= actor-id (:organization-group/moved-by moved)))
    (is (= :test/reorganization (:organization-group/move-reason moved))) (is (= 1
           (:organization-group/revision moved))) (is (domain/organization-group-document-consistent? moved)))
  (is (= :organization-group/parent-unchanged (error-type #(domain/move-organization-group (child-group)
            {:parent-scope root-group-scope :now t1})))) (is (= :organization-group/cycle (error-type
          #(domain/move-organization-group (child-group) {:parent-scope child-group-scope :now t1}))))
  (is (= :organization-group/invalid-input (error-type #(domain/move-organization-group (child-group)
            {:parent-scope destination-group-scope :now t1 :actor-id "not-a-uuid"})))))

(deftest location-move-test (let [moved (domain/move-location (active-location)
         {:parent-scope destination-group-scope :now t1 :actor-id actor-id :reason :test/reorganization})]
    (is (= destination-group-scope (domain/location-parent-scope moved))) (is (= t1 (:location/moved-at moved)))
    (is (= actor-id (:location/moved-by moved))) (is (= :test/reorganization (:location/move-reason moved)))
    (is (= 1 (:location/revision moved))) (is (domain/location-document-consistent? moved)))
  (is (= :location/parent-unchanged (error-type #(domain/move-location (active-location)
            {:parent-scope child-group-scope :now t1})))) (is (= :location/invalid-parent (error-type
          #(domain/move-location (active-location) {:parent-scope location-scope :now t1}))))
  (is (= :location/invalid-input (error-type #(domain/move-location (active-location)
            {:parent-scope destination-group-scope :now t1 :reason :unqualified})))))

(deftest organization-lifecycle-test (let [suspended (domain/suspend-organization (active-organization) {:now t1
          :actor-id actor-id :reason :test/maintenance}) reactivated (domain/reactivate-organization suspended
         {:now t2}) closed (domain/close-organization suspended {:now t2 :actor-id actor-id
          :reason :test/closure})] (is (domain/organization-suspended? suspended)) (is (= t1
           (:organization/suspended-at suspended))) (is (= actor-id (:organization/suspended-by suspended)))
    (is (= :test/maintenance (:organization/suspension-reason suspended))) (is (domain/organization-active?
         reactivated)) (is (= 2 (:organization/revision reactivated))) (is (not (contains? reactivated
          :organization/suspended-at))) (is (domain/organization-closed? closed)) (is (= t2
           (:organization/closed-at closed))) (is (= actor-id (:organization/closed-by closed)))
    (is (= :test/closure (:organization/closure-reason closed))) (is (not (contains? closed
          :organization/suspended-at)))) (is (= :organization/already-active (error-type
          #(domain/reactivate-organization (active-organization) {:now t1}))))
  (is (= :organization/already-suspended (error-type #(domain/suspend-organization (domain/suspend-organization
             (active-organization) {:now t1}) {:now t2})))))

(deftest organization-group-lifecycle-test (let [suspended (domain/suspend-organization-group (active-group)
         {:now t1 :actor-id actor-id :reason :test/maintenance}) reactivated
        (domain/reactivate-organization-group suspended {:now t2}) closed (domain/close-organization-group
         suspended {:now t2 :actor-id actor-id :reason :test/closure})]
    (is (domain/organization-group-suspended? suspended)) (is (= t1
           (:organization-group/suspended-at suspended))) (is (domain/organization-group-active? reactivated))
    (is (not (contains? reactivated :organization-group/suspended-at))) (is (domain/organization-group-closed?
         closed)) (is (= t2 (:organization-group/closed-at closed))) (is (not (contains? closed
          :organization-group/suspended-at)))) (is (= :organization-group/closed (error-type
          #(domain/rename-organization-group (domain/close-organization-group (active-group) {:now t1})
            {:name "Cannot rename" :now t2})))))

(deftest location-lifecycle-test (let [suspended (domain/suspend-location (active-location) {:now t1
          :actor-id actor-id :reason :test/maintenance}) reactivated (domain/reactivate-location suspended
         {:now t2}) closed (domain/close-location suspended {:now t2 :actor-id actor-id :reason :test/closure})]
    (is (domain/location-suspended? suspended)) (is (= t1 (:location/suspended-at suspended)))
    (is (domain/location-active? reactivated)) (is (not (contains? reactivated :location/suspended-at)))
    (is (domain/location-closed? closed)) (is (= t2 (:location/closed-at closed))) (is (= actor-id
           (:location/closed-by closed))) (is (= :test/closure (:location/closure-reason closed))) (is (not
         (contains? closed :location/suspended-at)))) (is (= :location/closed (error-type #(domain/move-location
            (domain/close-location (active-location) {:now t1}) {:parent-scope destination-group-scope
             :now t2})))))

(deftest hierarchy-consistency-test (let [organization-document (active-organization) root (active-group) child
        (child-group) location (active-location) direct-location (active-location {:id other-location-id
          :parent-scope organization-scope :name "Direct"})] (is (domain/organization-group-ancestry-consistent?
         organization-document root [])) (is (domain/organization-group-ancestry-consistent?
         organization-document child [root])) (is (false? (domain/organization-group-ancestry-consistent?
          organization-document child []))) (is (false? (domain/organization-group-ancestry-consistent?
          organization-document child [child root]))) (is (false?
         (domain/organization-group-ancestry-consistent? organization-document child [(active-group
            {:id other-organization-group-id :organization-id other-organization-id :parent-scope
             (domain/organization-scope other-organization-id)})]))) (is (domain/location-ancestry-consistent?
         organization-document location [child root])) (is (domain/location-ancestry-consistent?
         organization-document direct-location [])) (is (false? (domain/location-ancestry-consistent?
          organization-document location [root child]))) (is (false? (domain/location-ancestry-consistent?
          organization-document location [child child root])))))

(deftest hierarchy-operational-state-test (let [organization-document (active-organization) root (active-group)
        child (child-group) location (active-location) suspended-root (domain/suspend-organization-group root
         {:now t1}) suspended-organization (domain/suspend-organization organization-document {:now t1})]
    (is (domain/organization-group-operational? organization-document child [root]))
    (is (domain/location-operational? organization-document location [child root])) (is (domain/location-active?
         location)) (is (false? (domain/location-operational? organization-document location
          [child suspended-root]))) (is (false? (domain/location-operational? suspended-organization location
          [child root])))))

(deftest scope-context-construction-test (let [organization-document (active-organization) root (active-group)
        child (child-group) location (active-location) organization-context (domain/organization-scope-context
         organization-document) group-context (domain/organization-group-scope-context organization-document
         child [root]) location-context (domain/location-scope-context organization-document location
         [child root])] (is (= {:organization/id organization-id :scope/target organization-scope
            :scope/applicable [organization-scope] :scope/operational? true} organization-context))
    (is (= child-group-scope (:scope/target group-context))) (is (= [child-group-scope root-group-scope
            organization-scope] (:scope/applicable group-context))) (is (:scope/operational? group-context))
    (is (= location-scope (:scope/target location-context))) (is (= [location-scope child-group-scope
            root-group-scope organization-scope] (:scope/applicable location-context))) (is (:scope/operational?
         location-context)) (is (domain/scope-context? organization-context)) (is (domain/scope-context?
         group-context)) (is (domain/scope-context? location-context)) (is (false? (domain/scope-context?
          (update location-context :scope/applicable conj location-scope)))) (is (false? (domain/scope-context?
          (assoc location-context :scope/applicable [location-scope organization-scope child-group-scope]))))))

(deftest effective-activity-context-regression-test (let [organization-document (active-organization) root
        (active-group) child (domain/suspend-organization-group (child-group) {:now t1}) location
        (active-location) context (domain/location-scope-context organization-document location [child root])]
    (is (domain/location-active? location)) (is (false? (:scope/operational? context))) (is (= [location-scope
            child-group-scope root-group-scope organization-scope] (:scope/applicable context)))))

(deftest authorization-document-order-test (let [organization-document (active-organization) root (active-group)
        child (child-group) location (active-location)] (is (= [child root organization-document]
           (domain/organization-group-authorization-documents organization-document child [root])))
    (is (= [location child root organization-document] (domain/location-authorization-documents
            organization-document location [child root]))) (is (= :location/invalid-ancestry (error-type
            #(domain/location-authorization-documents organization-document location [root child]))))))

(deftest create-command-contract-test (let [organization-command (domain/create-organization-command
         {:id organization-id :name "Human Help" :now t0}) group-command
        (domain/create-organization-group-command {:id root-group-id :organization-id organization-id
          :parent-scope organization-scope :name "Operations" :now t0}) location-command
        (domain/create-location-command {:id location-id :organization-id organization-id
          :parent-scope child-group-scope :name "Downtown" :now t0})] (is (= :create
           (:model/operation organization-command))) (is (= :organization
           (:model/entity-type organization-command))) (is (= organization-id (:xt/id
            (command-document organization-command)))) (is (= :create (:model/operation group-command)))
    (is (= :organization-group (:model/entity-type group-command))) (is (= root-group-id (:xt/id
            (command-document group-command)))) (is (= :create (:model/operation location-command)))
    (is (= :location (:model/entity-type location-command))) (is (= location-id (:xt/id
            (command-document location-command))))))

(deftest update-command-contract-test (let [before (active-location) rename-command
        (domain/rename-location-command before {:name "Downtown Store" :now t1}) move-command
        (domain/move-location-command before {:parent-scope destination-group-scope :now t1 :actor-id actor-id
          :reason :test/move}) suspend-command (domain/suspend-location-command before {:now t1
          :actor-id actor-id :reason :test/suspend})] (is (= [:rename :move :suspend] (mapv :model/operation
            [rename-command move-command suspend-command]))) (doseq [command [rename-command move-command
             suspend-command]] (is (= :location (:model/entity-type command))) (is (= location-id
             (get-in command [:model/expected :model/id]))) (is (= 0 (get-in command [:model/expected
                      :model/revision]))) (is (= t0 (get-in command [:model/expected :model/updated-at]))))
    (is (= "Downtown Store" (:location/name (command-document rename-command)))) (is (= destination-group-scope
           (domain/location-parent-scope (command-document move-command)))) (is (= :suspended (:location/status
            (command-document suspend-command))))))

(deftest schema-registry-test (is (= organization.schema/organization-document-schema (:organization
          organization.schema/schema))) (is (= organization.schema/organization-group-document-schema
         (:organization-group organization.schema/schema))) (is (= organization.schema/location-document-schema
         (:location organization.schema/schema))) (is (m/validate (:organization/doc organization.schema/schema)
       (active-organization))) (is (m/validate (:organization-group/doc organization.schema/schema)
       (active-group))) (is (m/validate (:location/doc organization.schema/schema) (active-location)))
  (let [context (domain/location-scope-context (active-organization) (active-location) [(child-group)
          (active-group)])] (is (m/validate organization.schema/scope-context-schema context)) (is (m/validate
         (:location/scope-context organization.schema/schema) context))))

(deftest graph-input-builder-test (is (= {:organization/id organization-id}
         (organization.graph/organization-query-input {:organization-id organization-id})))
  (is (= {:organization-group/id root-group-id} (organization.graph/organization-group-query-input
          {:organization-group-id root-group-id}))) (is (= {:location/id location-id}
         (organization.graph/location-query-input {:location-id location-id})))
  (is (= {:organization/id organization-id} (organization.graph/organization-scope-context-query-input
          {:organization-id organization-id}))) (is (= {:organization/id organization-id
          :organization-group/id child-group-id}
         (organization.graph/organization-group-scope-context-query-input {:organization-id organization-id
           :organization-group-id child-group-id}))) (is (= {:organization/id organization-id
          :location/id location-id} (organization.graph/location-context-query-input
          {:organization-id organization-id :location-id location-id}))) (is (= {}
         (organization.graph/location-context-query-input {:organization-id nil :location-id nil}))))

(deftest graph-public-query-contract-test (is (query-contains? organization.graph/organization-command-query
       :organization/found?)) (is (query-contains? organization.graph/organization-scope-context-query
       :organization/scope-context)) (is (query-contains? organization.graph/organization-scope-context-query
       :organization/authorization-versions)) (is (query-contains?
       organization.graph/organization-group-scope-context-query :organization-group/ancestor-docs))
  (is (query-contains? organization.graph/organization-group-scope-context-query
       :organization-group/applicable-scopes)) (is (query-contains?
       organization.graph/organization-group-scope-context-query :organization-group/scope-context))
  (is (query-contains? organization.graph/location-context-query :location/ancestor-group-docs))
  (is (query-contains? organization.graph/location-context-query :location/applicable-scopes))
  (is (query-contains? organization.graph/location-context-query :location/scope-context)) (is (query-contains?
       organization.graph/location-context-query :location/authorization-versions)))

(deftest graph-uses-biff-connection-for-queries-test (let [q-var (get (ns-interns
          'net.humanhelp.site.model.organization.graph) 'q) q-fn (var-get q-var) calls (atom [])]
    (is (var? q-var)) (with-redefs [biffx/q (fn [connectable query] (swap! calls conj [connectable query])
        :query-result)] (is (= :query-result (q-fn {:biff/conn :canonical-connection
               :biff/node :must-not-be-used} {:select [:xt/id]}))) (is (= :query-result (q-fn
              {:biff/node :must-not-be-used} {:select [:xt/id]})))) (is (= [[:canonical-connection
             {:select [:xt/id]}] [nil {:select [:xt/id]}]] @calls))))

(deftest core-registration-and-operation-contract-test (is (= organization.schema/schema organization/schema))
  (is (= organization.graph/resolvers organization/resolvers)) (is (= {:schema organization/schema
          :biff.graph/resolvers organization/resolvers} organization/module))
  (is (= domain/organization-entity-type organization/organization-entity-type))
  (is (= domain/organization-group-entity-type organization/organization-group-entity-type))
  (is (= domain/location-entity-type organization/location-entity-type)) (is (identical?
       #'organization/create-organization-group (:organization/create-group organization/operations)))
  (is (identical? #'organization/create-location (:organization/create-location organization/operations)))
  (is (identical? #'organization/move-organization-group (:organization-group/move organization/operations)))
  (is (identical? #'organization/move-location (:location/move organization/operations))) (is (false? (contains?
        organization/operations :organization/create))) (is (false? (contains? organization/operations
        :organization/close))) (is (false? (contains? organization/operations :organization-group/close)))
  (is (false? (contains? organization/operations :location/close))))

(deftest core-pure-facade-test (let [organization-document (active-organization) group-document (active-group)
        location-document (active-location)] (is (= "Human Help" (organization/organization-name
            organization-document))) (is (organization/organization-active? organization-document))
    (is (= organization-scope (organization/organization-scope-of organization-document)))
    (is (= root-group-scope (organization/organization-group-scope-of group-document)))
    (is (= organization-scope (organization/organization-group-parent-scope group-document)))
    (is (organization/organization-group-for-organization? group-document organization-id))
    (is (= location-scope (organization/location-scope-of location-document))) (is (= child-group-scope
           (organization/location-parent-scope location-document))) (is (organization/location-direct-child-of?
         location-document child-group-scope))))

(deftest core-named-read-delegation-test (let [calls (atom []) ctx {:request/id :test} result {:ok true}]
    (with-redefs [graph/query (fn [actual-ctx input query] (swap! calls conj [actual-ctx input query]) result)]
      (is (= result (organization/organization-facts ctx organization-id))) (is (= result
             (organization/organization-group-facts ctx child-group-id))) (is (= result
             (organization/location-facts ctx location-id))) (is (= result (organization/organization-context
              ctx organization-id))) (is (= result (organization/organization-group-context ctx
              {:organization-id organization-id :organization-group-id child-group-id}))) (is (= result
             (organization/location-context ctx {:organization-id organization-id :location-id location-id}))))
    (is (= 6 (count @calls))) (is (= [(organization.graph/organization-query-input
             {:organization-id organization-id}) organization/organization-query] (subvec (vec (nth @calls 0))
            1))) (is (= [(organization.graph/organization-group-query-input
             {:organization-group-id child-group-id}) organization/organization-group-query] (subvec (vec
             (nth @calls 1)) 1))) (is (= [(organization.graph/location-query-input {:location-id location-id})
            organization/location-query] (subvec (vec (nth @calls 2)) 1)))
    (is (= organization/organization-context-query (nth (nth @calls 3) 2)))
    (is (= organization/organization-group-context-query (nth (nth @calls 4) 2)))
    (is (= organization/location-context-query (nth (nth @calls 5) 2)))))

(deftest core-write-delegation-test (let [calls (atom []) ctx {:current-user/id actor-id} input
        {:organization-id organization-id}] (with-redefs [organization.fx/create-organization-group
      (fn [actual-ctx actual-input] (swap! calls conj [:create-group actual-ctx actual-input]) :create-group)
      organization.fx/create-location (fn [actual-ctx actual-input] (swap! calls conj
               [:create-location actual-ctx actual-input]) :create-location) organization.fx/rename-organization
      (fn [actual-ctx actual-input] (swap! calls conj [:rename-organization actual-ctx actual-input])
        :rename-organization) organization.fx/suspend-organization (fn [actual-ctx actual-input]
        (swap! calls conj [:suspend-organization actual-ctx actual-input]) :suspend-organization)
      organization.fx/reactivate-organization (fn [actual-ctx actual-input] (swap! calls conj
               [:reactivate-organization actual-ctx actual-input]) :reactivate-organization)
      organization.fx/rename-organization-group (fn [actual-ctx actual-input] (swap! calls conj
               [:rename-group actual-ctx actual-input]) :rename-group) organization.fx/move-organization-group
      (fn [actual-ctx actual-input] (swap! calls conj [:move-group actual-ctx actual-input]) :move-group)
      organization.fx/suspend-organization-group (fn [actual-ctx actual-input] (swap! calls conj
               [:suspend-group actual-ctx actual-input]) :suspend-group)
      organization.fx/reactivate-organization-group (fn [actual-ctx actual-input] (swap! calls conj
               [:reactivate-group actual-ctx actual-input]) :reactivate-group) organization.fx/rename-location
      (fn [actual-ctx actual-input] (swap! calls conj [:rename-location actual-ctx actual-input])
        :rename-location) organization.fx/move-location (fn [actual-ctx actual-input] (swap! calls conj
               [:move-location actual-ctx actual-input]) :move-location) organization.fx/suspend-location
      (fn [actual-ctx actual-input] (swap! calls conj [:suspend-location actual-ctx actual-input])
        :suspend-location) organization.fx/reactivate-location (fn [actual-ctx actual-input] (swap! calls conj
               [:reactivate-location actual-ctx actual-input]) :reactivate-location)] (is (= :create-group
             (organization/create-organization-group ctx input))) (is (= :create-location
             (organization/create-location ctx input))) (is (= :rename-organization
             (organization/rename-organization ctx input))) (is (= :suspend-organization
             (organization/suspend-organization ctx input))) (is (= :reactivate-organization
             (organization/reactivate-organization ctx input))) (is (= :rename-group
             (organization/rename-organization-group ctx input))) (is (= :move-group
             (organization/move-organization-group ctx input))) (is (= :suspend-group
             (organization/suspend-organization-group ctx input))) (is (= :reactivate-group
             (organization/reactivate-organization-group ctx input))) (is (= :rename-location
             (organization/rename-location ctx input))) (is (= :move-location (organization/move-location
              ctx input))) (is (= :suspend-location (organization/suspend-location ctx input)))
      (is (= :reactivate-location (organization/reactivate-location ctx input)))) (is (= 13 (count @calls)))
    (is (every? #(= ctx (nth % 1)) @calls)) (is (every? #(= input (nth % 2)) @calls))))

(deftest plan-create-child-test
  (let [organization-document
        (active-organization)

        context
        (domain/organization-scope-context
         organization-document)

        command
        (domain/create-location-command
         {:id location-id
          :organization-id organization-id
          :parent-scope organization-scope
          :name "Downtown"
          :now t0})

        plan
        (organization.fx/plan-create-child
         {:entity-kind :location
          :operation :create
          :command command
          :parent-scope-facts
          (scope-facts
           organization-document
           context
           [(organization-guard
             organization-document)])
          :user-authorization
          (user-authorization)})

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
      4
      (count
       (:authorization-versions
        transaction-plan))))

    (is
     (empty?
      (:assertions transaction-plan)))

    (is
     (=
      #{:location}
      (topic-set
       (:changes transaction-plan))))

    (is
     (=
      :created
      (:change/kind change)))

    (is
     (=
      :create
      (:location/operation change)))

    (is
     (=
      location-id
      (:location/id change)))

    (is
     (=
      (command-document command)
      (get-in
       plan
       [:result :location])))

    (is
     (ifn?
      (:entry-fn transaction-plan)))

    (is
     (=
      {:coalesce-key
       [:location location-id]}
      ((:entry-fn transaction-plan)
       change)))))

(deftest plan-update-entity-test
  (let [location-document
        (active-location)

        organization-document
        (active-organization)

        root
        (active-group)

        child
        (child-group)

        context
        (domain/location-scope-context
         organization-document
         location-document
         [child root])

        command
        (domain/rename-location-command
         location-document
         {:name "Downtown Store"
          :now t1})

        plan
        (organization.fx/plan-update-entity
         {:entity-kind :location
          :operation :rename
          :command command
          :target-scope-facts
          (scope-facts
           location-document
           context
           [(location-guard location-document)
            (group-guard child)
            (group-guard root)
            (organization-guard
             organization-document)])
          :user-authorization
          (user-authorization)})

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
      7
      (count
       (:authorization-versions
        transaction-plan))))

    (is
     (empty?
      (:assertions transaction-plan)))

    (is
     (=
      :updated
      (:change/kind change)))

    (is
     (=
      :rename
      (:location/operation change)))

    (is
     (=
      1
      (:location/revision change)))

    (is
     (=
      "Downtown Store"
      (get-in
       plan
       [:result
        :location
        :location/name])))))

(deftest plan-move-entity-defers-shared-guard-deduplication-test
  (let [organization-document
        (active-organization)

        root
        (active-group)

        child
        (child-group)

        destination
        (destination-group)

        location-document
        (active-location)

        current-context
        (domain/location-scope-context
         organization-document
         location-document
         [child root])

        destination-context
        (domain/organization-group-scope-context
         organization-document
         destination
         [])

        command
        (domain/move-location-command
         location-document
         {:parent-scope destination-group-scope
          :now t1
          :actor-id actor-id
          :reason :test/move})

        same-user-authorization
        (user-authorization)

        plan
        (organization.fx/plan-move-entity
         {:entity-kind :location
          :command command
          :current-scope-facts
          (scope-facts
           location-document
           current-context
           [(location-guard location-document)
            (group-guard child)
            (group-guard root)
            (organization-guard
             organization-document)])
          :destination-scope-facts
          (scope-facts
           destination
           destination-context
           [(group-guard destination)
            (organization-guard
             organization-document)])
          :current-user-authorization
          same-user-authorization
          :destination-user-authorization
          same-user-authorization})

        transaction-plan
        (:transaction-plan plan)

        raw-authorization-versions
        (:authorization-versions
         transaction-plan)

        normalized-authorization-versions
        (model.fx/normalize-authorization-versions
         raw-authorization-versions)

        change
        (first
         (:changes transaction-plan))]

    (is
     (=
      [command]
      (:commands transaction-plan)))

    (is
     (=
      12
      (count
       raw-authorization-versions)))

    (is
     (=
      8
      (count
       normalized-authorization-versions)))

    (is
     (empty?
      (:assertions transaction-plan)))

    (is
     (=
      #{:location}
      (topic-set
       (:changes transaction-plan))))

    (is
     (=
      :move
      (:location/operation change)))

    (is
     (=
      destination-group-id
      (:location/parent-id change)))

    (is
     (=
      destination-group-scope
      (domain/location-parent-scope
       (get-in
        plan
        [:result :location]))))))

(deftest plan-defers-conflicting-authorization-version-rejection-test
  (let [organization-document
        (active-organization)

        context
        (domain/organization-scope-context
         organization-document)

        command
        (domain/rename-organization-command
         organization-document
         {:name "New Name"
          :now t1})

        current-guard
        (organization-guard
         organization-document)

        conflicting-guard
        (assoc-in
         current-guard
         [:model/expected
          :model/revision]
         99)

        plan
        (organization.fx/plan-update-entity
         {:entity-kind :organization
          :operation :rename
          :command command
          :target-scope-facts
          (scope-facts
           organization-document
           context
           [current-guard
            conflicting-guard])
          :user-authorization
          (user-authorization)})

        authorization-versions
        (get-in
         plan
         [:transaction-plan
          :authorization-versions])]

    (is
     (=
      :model.fx/conflicting-authorization-versions
      (error-type
       #(model.fx/normalize-authorization-versions
         authorization-versions))))))

(deftest plan-defers-invalid-authorization-version-rejection-test
  (let [organization-document
        (active-organization)

        context
        (domain/organization-scope-context
         organization-document)

        command
        (domain/rename-organization-command
         organization-document
         {:name "New Name"
          :now t1})

        malformed-guard
        {:model/entity-type
         :organization

         :model/expected
         {:model/id
          organization-id}}

        plan
        (organization.fx/plan-update-entity
         {:entity-kind :organization
          :operation :rename
          :command command
          :target-scope-facts
          (scope-facts
           organization-document
           context
           [malformed-guard])
          :user-authorization
          (user-authorization)})

        authorization-versions
        (get-in
         plan
         [:transaction-plan
          :authorization-versions])]

    (is
     (=
      :model.fx/invalid-authorization-version
      (error-type
       #(model.fx/normalize-authorization-versions
         authorization-versions))))))

(deftest fx-and-core-operation-registries-match-test (is (= (set (keys organization.fx/operations)) (set
          (keys organization/operations)))) (is (= #{:organization/create-group :organization/create-location
           :organization/rename :organization/suspend :organization/reactivate :organization-group/rename
           :organization-group/move :organization-group/suspend :organization-group/reactivate :location/rename
           :location/move :location/suspend :location/reactivate} (set (keys organization/operations)))))
