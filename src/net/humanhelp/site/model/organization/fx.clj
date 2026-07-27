(ns net.humanhelp.site.model.organization.fx
  "Authorized Organization hierarchy workflows.

   Domain functions create canonical gesso.model commands. This namespace adds
   hierarchy/User authorization guards and semantic Live changes, then commits
   through gesso.model.tx.

   Organization owns hierarchy authorization requirements; User owns User,
   membership, and role authorization proof. gesso.model owns optimistic-
   concurrency mechanics and transaction execution."
  (:require
   [gesso.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.organization.domain :as organization]
   [net.humanhelp.site.model.organization.graph :as organization.graph]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Common helpers
;; =============================================================================

(defn- fail!
  ([type message] (fail! type message nil))
  ([type message details]
   (throw (ex-info message (cond-> {:error/type type}
                             (some? details) (assoc :error/details details))))))

(defn- user-id! [ctx]
  (or (:current-user/id ctx)
      (fail! :organization/not-authenticated "A signed-in user is required.")))

(defn- change-entry [{:keys [topic id]}] {:coalesce-key [topic id]})
(defn- scope-organization-id [context] (:organization/id context))
(defn- scope-target [context] (:scope/target context))
(defn- operational? [context] (true? (:scope/operational? context)))

(defn- commit-state [{:organization.fx/keys [result transaction-plan]}]
  {:organization.fx/result result
   :organization.fx/transaction [model.tx/transact-effect transaction-plan]
   :biff.fx/next :finish})
(defn- finish-state [{:organization.fx/keys [result transaction]}]
  {:biff.fx/return (assoc result :transaction transaction)})

;; =============================================================================
;; Legacy Organization proof -> canonical Gesso guard boundary
;; =============================================================================

(def legacy-expected-version-keys
  #{:model/id :model/revision-key :model/revision :model/updated-at-key :model/updated-at})

(defn- legacy-proof->guard [proof]
  (if (command/guard? proof)
    proof
    (let [{:model/keys [entity-type expected]} proof
          {:model/keys [id revision-key revision updated-at-key updated-at]} expected]
      (when-not (and (keyword? entity-type) (some? id)
                     (keyword? revision-key) (some? revision)
                     (keyword? updated-at-key) (some? updated-at))
        (fail! :organization.fx/invalid-authorization-version
               "An Organization authorization version is invalid."
               {:authorization-version proof}))
      (command/require-guard
       {:model/entity-type entity-type
        :model/expected {:model/id id
                         :model/checks [[revision-key revision]
                                        [updated-at-key updated-at]]}}))))

(defn- proof-seq! [proofs]
  (when-not (sequential? proofs)
    (fail! :organization.fx/invalid-authorization-versions
           "Organization authorization versions must be sequential."
           {:authorization-versions proofs}))
  proofs)

(defn- guards [& collections]
  (into [] (comp (mapcat proof-seq!) (map legacy-proof->guard)) collections))

;; =============================================================================
;; Scope reads and contracts
;; =============================================================================

(defn- scope-key [scope suffix]
  (keyword (name (:scope/type scope)) suffix))

(defn- scope-context-effect [organization-id scope]
  (when-not (and (uuid? organization-id) (organization/scope-reference? scope))
    (fail! :organization.fx/invalid-scope-query
           "A valid organization and scope are required."
           {:organization/id organization-id :scope scope}))
  (case (:scope/type scope)
    :organization
    [:biff.graph.fx/query
     (organization.graph/organization-scope-context-query-input
      {:organization-id organization-id})
     organization.graph/organization-scope-context-query]
    :organization-group
    [:biff.graph.fx/query
     (organization.graph/organization-group-scope-context-query-input
      {:organization-id organization-id :organization-group-id (:scope/id scope)})
     organization.graph/organization-group-scope-context-query]
    :location
    [:biff.graph.fx/query
     (organization.graph/location-context-query-input
      {:organization-id organization-id :location-id (:scope/id scope)})
     organization.graph/location-context-query]))

(def ^:private not-found
  {:organization [:organization/not-found "The organization no longer exists."]
   :organization-group [:organization-group/not-found "The organization group no longer exists."]
   :location [:location/not-found "The location no longer exists."]})

(defn- require-scope-facts! [facts organization-id scope]
  (let [found-key (scope-key scope "found?")
        doc-key (scope-key scope "doc")
        context-key (scope-key scope "scope-context")
        versions-key (scope-key scope "authorization-versions")
        [not-found-type not-found-message] (get not-found (:scope/type scope))]
    (when-not (true? (get facts found-key))
      (fail! not-found-type not-found-message {:organization/id organization-id :scope scope}))
    (let [document (or (get facts doc-key)
                       (fail! :organization.fx/incomplete-graph-result
                              "Organization Graph reported a found entity without its document."
                              {:document-key doc-key :scope scope}))
          context (or (get facts context-key)
                      (fail! :organization.fx/incomplete-graph-result
                             "Organization Graph did not return the required scope context."
                             {:scope-context-key context-key :scope scope}))
          proofs (or (get facts versions-key)
                     (fail! :organization.fx/incomplete-graph-result
                            "Organization Graph did not return authorization versions."
                            {:authorization-versions-key versions-key :scope scope}))]
      (when-not (organization/scope-context? context)
        (fail! :organization.fx/invalid-scope-context
               "Organization Graph returned an invalid scope context."
               {:scope-context context}))
      (when-not (= organization-id (scope-organization-id context))
        (fail! :organization/ownership-mismatch
               "The requested entity belongs to another organization."
               {:expected-organization-id organization-id
                :actual-organization-id (scope-organization-id context)
                :scope scope}))
      (when-not (organization/same-scope? scope (scope-target context))
        (fail! :organization.fx/scope-mismatch
               "Organization Graph returned a context for the wrong scope."
               {:expected-scope scope :actual-scope (scope-target context)}))
      {:document document :scope-context context :authorization-versions (vec (proof-seq! proofs))})))

(defn- require-operational! [{:keys [scope-context] :as facts}]
  (when-not (operational? scope-context)
    (fail! :organization/scope-not-operational
           "The destination scope is not operational."
           {:organization/id (scope-organization-id scope-context)
            :scope (scope-target scope-context)}))
  facts)

;; =============================================================================
;; User administrator authorization
;; =============================================================================

(defn- require-administrator!
  [ctx scope-context]
  (user/require-role-authorization
   ctx
   {:user-id (user-id! ctx)
    :scope-context scope-context
    :role :admin}))

;; =============================================================================
;; Semantic changes and plans
;; =============================================================================

(defn- command-change [entity operation model-command]
  (let [document (command/after model-command)
        created? (command/create? model-command)
        base {:topic entity :id (:xt/id document) :change/kind (if created? :created :updated)}]
    (case entity
      :organization
      (merge base {:organization/operation operation
                   :organization/id (:xt/id document)
                   :organization/status (:organization/status document)
                   :organization/revision (:organization/revision document)})
      :organization-group
      (merge base {:organization-group/operation operation
                   :organization-group/id (:xt/id document)
                   :organization/id (:organization-group/organization document)
                   :organization-group/parent-type (:organization-group/parent-type document)
                   :organization-group/parent-id (:organization-group/parent-id document)
                   :organization-group/status (:organization-group/status document)
                   :organization-group/revision (:organization-group/revision document)})
      :location
      (merge base {:location/operation operation
                   :location/id (:xt/id document)
                   :organization/id (:location/organization document)
                   :location/parent-type (:location/parent-type document)
                   :location/parent-id (:location/parent-id document)
                   :location/status (:location/status document)
                   :location/revision (:location/revision document)}))))

(defn- plan [entity operation model-command guard-collections]
  (let [fragment
        (model.tx/fragment
         {:commands [model-command]
          :guards (apply guards guard-collections)
          :changes [(command-change entity operation model-command)]})]
    {:transaction-plan (assoc fragment :entry-fn change-entry)
     :result {entity (command/after model-command)}}))

(defn plan-create-child [{:keys [entity-kind operation command parent-scope-facts user-authorization]}]
  (plan entity-kind operation command
        [(:authorization-versions parent-scope-facts)
         (:guards user-authorization)]))

(defn plan-update-entity [{:keys [entity-kind operation command target-scope-facts user-authorization]}]
  (plan entity-kind operation command
        [(:authorization-versions target-scope-facts)
         (:guards user-authorization)]))

(defn plan-move-entity
  [{:keys [entity-kind command current-scope-facts destination-scope-facts
           current-user-authorization destination-user-authorization]}]
  (plan entity-kind :move command
        [(:authorization-versions current-scope-facts)
         (:authorization-versions destination-scope-facts)
         (:guards current-user-authorization)
         (:guards destination-user-authorization)]))

;; =============================================================================
;; Domain operation dispatch
;; =============================================================================

;; Store Vars rather than function values so with-redefs and REPL reloads remain live.
(def ^:private update-commands
  {[:organization :rename] #'organization/rename-organization-command
   [:organization :suspend] #'organization/suspend-organization-command
   [:organization :reactivate] #'organization/reactivate-organization-command
   [:organization-group :rename] #'organization/rename-organization-group-command
   [:organization-group :suspend] #'organization/suspend-organization-group-command
   [:organization-group :reactivate] #'organization/reactivate-organization-group-command
   [:location :rename] #'organization/rename-location-command
   [:location :suspend] #'organization/suspend-location-command
   [:location :reactivate] #'organization/reactivate-location-command})

(def ^:private scope-fns
  {:organization #'organization/organization-scope
   :organization-group #'organization/organization-group-scope
   :location #'organization/location-scope})

(defn- update-command! [entity operation document input]
  (if-let [f (get update-commands [entity operation])]
    (f document input)
    (fail! :organization.fx/unsupported-update "The requested Organization update is not supported."
           {:entity-kind entity :operation operation})))

(defn- entity-scope [entity id] ((get scope-fns entity) id))

;; =============================================================================
;; Create child workflows
;; =============================================================================

(def ^:private create-commands
  {:organization-group #'organization/create-organization-group-command
   :location #'organization/create-location-command})

(fx/defmachine create-child-machine
  :start
  (fn [ctx]
    (let [{:keys [organization-id parent-scope]} (:organization.fx/input ctx)]
      {:organization.fx/parent-scope parent-scope
       :organization.fx/parent-facts (scope-context-effect organization-id parent-scope)
       :biff.fx/next :plan}))

  :plan
  (fn [ctx]
    (let [input (:organization.fx/input ctx)
          entity (:organization.fx/entity-kind ctx)
          organization-id (:organization-id input)
          parent-scope (:organization.fx/parent-scope ctx)
          parent-facts (-> (require-scope-facts! (:organization.fx/parent-facts ctx)
                                                organization-id parent-scope)
                           require-operational!)
          authorization (require-administrator! ctx (:scope-context parent-facts))
          model-command ((get create-commands entity)
                         {:id (first (fx/uuid7 (:biff.fx/seed ctx) (:biff.fx/now ctx)))
                          :organization-id organization-id
                          :parent-scope parent-scope
                          :name (:name input)
                          :now (:biff.fx/now ctx)})
          planned (plan-create-child {:entity-kind entity :operation :create
                                      :command model-command
                                      :parent-scope-facts parent-facts
                                      :user-authorization authorization})]
      {:organization.fx/result (:result planned)
       :organization.fx/transaction-plan (:transaction-plan planned)
       :biff.fx/next :commit}))
  :commit commit-state
  :finish finish-state)

(defn- create-child [ctx entity input]
  (create-child-machine (assoc ctx :organization.fx/entity-kind entity :organization.fx/input input)))
(defn create-organization-group [ctx input] (create-child ctx :organization-group input))
(defn create-location [ctx input] (create-child ctx :location input))

;; =============================================================================
;; Authorized updates
;; =============================================================================

(fx/defmachine update-entity-machine
  :start
  (fn [ctx]
    (let [input (:organization.fx/input ctx)
          entity (:organization.fx/entity-kind ctx)
          scope (entity-scope entity (:entity-id input))]
      {:organization.fx/target-scope scope
       :organization.fx/target-facts (scope-context-effect (:organization-id input) scope)
       :biff.fx/next :plan}))

  :plan
  (fn [ctx]
    (let [input (:organization.fx/input ctx)
          entity (:organization.fx/entity-kind ctx)
          operation (:organization.fx/operation ctx)
          scope (:organization.fx/target-scope ctx)
          facts (require-scope-facts! (:organization.fx/target-facts ctx)
                                      (:organization-id input) scope)
          authorization (require-administrator! ctx (:scope-context facts))
          model-command (update-command!
                         entity operation (:document facts)
                         {:name (:name input) :now (:biff.fx/now ctx)
                          :actor-id (user-id! ctx) :reason (:reason input)})
          planned (plan-update-entity {:entity-kind entity :operation operation
                                       :command model-command :target-scope-facts facts
                                       :user-authorization authorization})]
      {:organization.fx/result (:result planned)
       :organization.fx/transaction-plan (:transaction-plan planned)
       :biff.fx/next :commit}))
  :commit commit-state
  :finish finish-state)

(defn- run-update [ctx entity operation id input]
  (update-entity-machine
   (assoc ctx :organization.fx/entity-kind entity :organization.fx/operation operation
          :organization.fx/input (assoc input :entity-id id))))

(defn rename-organization [ctx {:keys [organization-id] :as input}]
  (run-update ctx :organization :rename organization-id input))
(defn suspend-organization [ctx {:keys [organization-id] :as input}]
  (run-update ctx :organization :suspend organization-id input))
(defn reactivate-organization [ctx {:keys [organization-id] :as input}]
  (run-update ctx :organization :reactivate organization-id input))
(defn rename-organization-group [ctx {:keys [organization-group-id] :as input}]
  (run-update ctx :organization-group :rename organization-group-id input))
(defn suspend-organization-group [ctx {:keys [organization-group-id] :as input}]
  (run-update ctx :organization-group :suspend organization-group-id input))
(defn reactivate-organization-group [ctx {:keys [organization-group-id] :as input}]
  (run-update ctx :organization-group :reactivate organization-group-id input))
(defn rename-location [ctx {:keys [location-id] :as input}]
  (run-update ctx :location :rename location-id input))
(defn suspend-location [ctx {:keys [location-id] :as input}]
  (run-update ctx :location :suspend location-id input))
(defn reactivate-location [ctx {:keys [location-id] :as input}]
  (run-update ctx :location :reactivate location-id input))

;; =============================================================================
;; Move workflows
;; =============================================================================

(defn- require-move-destination! [entity current-scope destination-scope facts]
  (require-operational! facts)
  (when (and (= entity :organization-group)
             (some #(organization/same-scope? current-scope %)
                   (get-in facts [:scope-context :scope/applicable])))
    (fail! :organization-group/cycle
           "The organization group cannot be moved beneath itself or a descendant."
           {:organization-group/scope current-scope :destination-scope destination-scope}))
  facts)

(fx/defmachine move-entity-machine
  :start
  (fn [ctx]
    (let [input (:organization.fx/input ctx)
          entity (:organization.fx/entity-kind ctx)
          current (entity-scope entity (:entity-id input))
          destination (:parent-scope input)
          organization-id (:organization-id input)]
      {:organization.fx/current-scope current
       :organization.fx/destination-scope destination
       :organization.fx/current-facts (scope-context-effect organization-id current)
       :organization.fx/destination-facts (scope-context-effect organization-id destination)
       :biff.fx/next :plan}))

  :plan
  (fn [ctx]
    (let [input (:organization.fx/input ctx)
          entity (:organization.fx/entity-kind ctx)
          organization-id (:organization-id input)
          current (:organization.fx/current-scope ctx)
          destination (:organization.fx/destination-scope ctx)
          current-facts (require-scope-facts! (:organization.fx/current-facts ctx)
                                              organization-id current)
          destination-facts (->> (require-scope-facts! (:organization.fx/destination-facts ctx)
                                                       organization-id destination)
                                 (require-move-destination! entity current destination))
          current-auth (require-administrator! ctx (:scope-context current-facts))
          destination-auth (require-administrator! ctx (:scope-context destination-facts))
          move-fn (case entity
                    :organization-group #'organization/move-organization-group-command
                    :location #'organization/move-location-command)
          model-command (move-fn (:document current-facts)
                                 {:parent-scope destination :now (:biff.fx/now ctx)
                                  :actor-id (user-id! ctx) :reason (:reason input)})
          planned (plan-move-entity
                   {:entity-kind entity :command model-command
                    :current-scope-facts current-facts :destination-scope-facts destination-facts
                    :current-user-authorization current-auth
                    :destination-user-authorization destination-auth})]
      {:organization.fx/result (:result planned)
       :organization.fx/transaction-plan (:transaction-plan planned)
       :biff.fx/next :commit}))
  :commit commit-state
  :finish finish-state)

(defn- run-move [ctx entity id input]
  (move-entity-machine
   (assoc ctx :organization.fx/entity-kind entity
          :organization.fx/input (assoc input :entity-id id))))
(defn move-organization-group [ctx {:keys [organization-group-id] :as input}]
  (run-move ctx :organization-group organization-group-id input))
(defn move-location [ctx {:keys [location-id] :as input}]
  (run-move ctx :location location-id input))

;; =============================================================================
;; Public operation registry
;; =============================================================================

(def operations
  {:organization/create-group #'create-organization-group
   :organization/create-location #'create-location
   :organization/rename #'rename-organization
   :organization/suspend #'suspend-organization
   :organization/reactivate #'reactivate-organization
   :organization-group/rename #'rename-organization-group
   :organization-group/move #'move-organization-group
   :organization-group/suspend #'suspend-organization-group
   :organization-group/reactivate #'reactivate-organization-group
   :location/rename #'rename-location
   :location/move #'move-location
   :location/suspend #'suspend-location
   :location/reactivate #'reactivate-location})
