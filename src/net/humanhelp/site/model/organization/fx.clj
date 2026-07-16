(ns net.humanhelp.site.model.organization.fx
  "Organization hierarchy workflows.
   Writes are authorized through User core, committed through model.fx, and
   published as semantic Organization changes. This slice covers nonterminal
   hierarchy management. Location closure remains intentionally unexposed
   until User core supplies an atomic exact-scope role-revocation planner."
  (:require
   [gesso.fx :as fx]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.organization.domain :as organization]
   [net.humanhelp.site.model.organization.graph :as organization.graph]
   [net.humanhelp.site.model.user.core :as user]))
(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type error-type}
       (some? details)
       (assoc :error/details details))))))
(defn- require-authenticated-user-id!
  [ctx]
  (or
   (:current-user/id ctx)
   (fail!
    :organization/not-authenticated
    "A signed-in user is required.")))
(defn- command-document [command]
  (model.common/command-document command))
(defn- change-entry [{:keys [topic id]}]
  {:coalesce-key [topic id]})
(defn- scope-context-organization-id [scope-context]
  (:organization/id scope-context))
(defn- scope-context-target [scope-context]
  (:scope/target scope-context))
(defn- scope-context-operational? [scope-context]
  (true? (:scope/operational? scope-context)))
(defn- commit-state
  [{:organization.fx/keys [result transaction-plan]}]
  {:organization.fx/result result
   :organization.fx/transaction [model.fx/transact-effect transaction-plan]
   :biff.fx/next :finish})
(defn- finish-state
  [{:organization.fx/keys [result transaction]}]
  {:biff.fx/return (assoc result :transaction transaction)})
(defn- authorization-version-target
  [{:model/keys [entity-type expected]}]
  [entity-type
   (:model/id expected)])
(defn- validate-authorization-version!
  [{:model/keys [entity-type expected] :as guard}]
  (when-not
   (and
    (map? guard)
    (keyword? entity-type)
    (map? expected)
    (uuid? (:model/id expected))
    (keyword? (:model/revision-key expected))
    (nat-int? (:model/revision expected))
    (keyword? (:model/updated-at-key expected))
    (model.common/timestamp-value?
     (:model/updated-at expected)))
    (fail!
     :organization.fx/invalid-authorization-version
     "An authorization-version guard is invalid."
     {:guard guard}))
  guard)
(defn- merge-authorization-versions!
  [& guard-collections]
  (let [guards
        (mapv
         validate-authorization-version!
         (mapcat
          (fn [guards]
            (when-not
             (sequential? guards)
              (fail!
               :organization.fx/invalid-authorization-versions
               "Authorization versions must be sequential."
               {:authorization-versions guards}))
            guards)
          guard-collections))
        grouped
        (group-by
         authorization-version-target
         guards)
        conflicts
        (->> grouped
             (keep
              (fn [[target matching]]
                (when
                 (< 1
                    (count
                     (set
                      (map :model/expected matching))))
                  {:target target
                   :guards matching})))
             vec)]
    (when
     (seq conflicts)
      (fail!
       :organization.fx/conflicting-authorization-versions
       "The same authorization document was loaded at conflicting versions."
       {:conflicts conflicts}))
    (->> guards
         (reduce
          (fn [result guard]
            (assoc
             result
             (authorization-version-target guard)
             guard))
          {})
         vals
         vec)))
(defn- authorization-version-assertions!
  [& guard-collections]
  (mapv
   (fn [{:model/keys [entity-type expected]}]
     (model.fx/assert-document-current
      entity-type
      expected))
   (apply
    merge-authorization-versions!
    guard-collections)))
(defn- scope-context-effect
  [organization-id scope]
  (when-not
   (and
    (uuid? organization-id)
    (organization/scope-reference? scope))
    (fail!
     :organization.fx/invalid-scope-query
     "A valid organization and scope are required."
     {:organization/id organization-id
      :scope scope}))
  (case
   (:scope/type scope)
    :organization
    [:biff.graph.fx/query
     (organization.graph/organization-scope-context-query-input
      {:organization-id organization-id})
     organization.graph/organization-scope-context-query]
    :organization-group
    [:biff.graph.fx/query
     (organization.graph/organization-group-scope-context-query-input
      {:organization-id organization-id
       :organization-group-id (:scope/id scope)})
     organization.graph/organization-group-scope-context-query]
    :location
    [:biff.graph.fx/query
     (organization.graph/location-context-query-input
      {:organization-id organization-id
       :location-id (:scope/id scope)})
     organization.graph/location-context-query]))
(defn- scope-facts-contract
  [scope]
  (case
   (:scope/type scope)
    :organization
    {:found-key
     :organization/found?
     :document-key
     :organization/doc
     :scope-context-key
     :organization/scope-context
     :authorization-versions-key
     :organization/authorization-versions
     :not-found-type
     :organization/not-found
     :not-found-message
     "The organization no longer exists."}
    :organization-group
    {:found-key
     :organization-group/found?
     :document-key
     :organization-group/doc
     :scope-context-key
     :organization-group/scope-context
     :authorization-versions-key
     :organization-group/authorization-versions
     :not-found-type
     :organization-group/not-found
     :not-found-message
     "The organization group no longer exists."}
    :location
    {:found-key
     :location/found?
     :document-key
     :location/doc
     :scope-context-key
     :location/scope-context
     :authorization-versions-key
     :location/authorization-versions
     :not-found-type
     :location/not-found
     :not-found-message
     "The location no longer exists."}))
(defn- require-scope-facts!
  [facts expected-organization-id expected-scope]
  (let [{:keys
         [found-key
          document-key
          scope-context-key
          authorization-versions-key
          not-found-type
          not-found-message]}
        (scope-facts-contract expected-scope)]
    (when-not
     (true?
      (get facts found-key))
      (fail!
       not-found-type
       not-found-message
       {:organization/id expected-organization-id
        :scope expected-scope}))
    (let [document
          (or
           (get facts document-key)
           (fail!
            :organization.fx/incomplete-graph-result
            "Organization Graph reported a found entity without its document."
            {:document-key document-key
             :scope expected-scope}))
          scope-context
          (or
           (get facts scope-context-key)
           (fail!
            :organization.fx/incomplete-graph-result
            "Organization Graph did not return the required scope context."
            {:scope-context-key scope-context-key
             :scope expected-scope}))
          authorization-versions
          (or
           (get facts authorization-versions-key)
           (fail!
            :organization.fx/incomplete-graph-result
            "Organization Graph did not return authorization versions."
            {:authorization-versions-key
             authorization-versions-key
             :scope expected-scope}))]
      (when-not
       (organization/scope-context?
        scope-context)
        (fail!
         :organization.fx/invalid-scope-context
         "Organization Graph returned an invalid scope context."
         {:scope-context scope-context}))
      (when-not
       (= expected-organization-id
          (scope-context-organization-id
           scope-context))
        (fail!
         :organization/ownership-mismatch
         "The requested entity belongs to another organization."
         {:expected-organization-id
          expected-organization-id
          :actual-organization-id
          (scope-context-organization-id
           scope-context)
          :scope expected-scope}))
      (when-not
       (organization/same-scope?
        expected-scope
        (scope-context-target
         scope-context))
        (fail!
         :organization.fx/scope-mismatch
         "Organization Graph returned a context for the wrong scope."
         {:expected-scope expected-scope
          :actual-scope
          (scope-context-target
           scope-context)}))
      {:document document
       :scope-context scope-context
       :authorization-versions
       (merge-authorization-versions!
        authorization-versions)})))
(defn- require-operational-scope!
  [{:keys [scope-context] :as scope-facts}]
  (when-not
   (scope-context-operational?
    scope-context)
    (fail!
     :organization/scope-not-operational
     "The destination scope is not operational."
     {:organization/id
      (scope-context-organization-id
       scope-context)
      :scope
      (scope-context-target
       scope-context)}))
  scope-facts)
(def user-version
  {:revision-key :user/revision :updated-at-key :user/updated-at})
(def membership-version
  {:revision-key :membership/revision
   :updated-at-key :membership/updated-at})
(def role-assignment-version
  {:revision-key :role-assignment/revision
   :updated-at-key :role-assignment/updated-at})
(defn- public-expected-version [document version]
  {:model/id (:xt/id document)
   :model/revision-key (:revision-key version)
   :model/revision (get document (:revision-key version))
   :model/updated-at-key (:updated-at-key version)
   :model/updated-at (get document (:updated-at-key version))})
(defn- require-public-document!
  [facts found-key document-key error-type message details]
  (when-not (true? (get facts found-key))
    (fail! error-type message details))
  (or (get facts document-key)
      (fail! :organization.fx/incomplete-user-result
             "User core reported a found document without returning it."
             {:document-key document-key})))
(defn- role-assignment-documents-at-scopes
  [ctx organization-id scopes]
  (->> scopes
       (mapcat
        (fn [scope]
          (map :role-assignment/doc
               (:user/active-role-assignments-at-scope
                (user/active-role-assignments-at-scope
                 ctx organization-id scope)))))
       (reduce (fn [documents document]
                 (assoc documents (:xt/id document) document))
               {})
       vals
       vec))
(defn- require-administrator-authorization!
  [ctx scope-context]
  (let [user-id (require-authenticated-user-id! ctx)
        organization-id (scope-context-organization-id scope-context)
        scopes (vec (:scope/applicable scope-context))
        access-context
        (user/access-context
         ctx {:user-id user-id :scope-context scope-context})]
    (when-not (true? (:user/admin? access-context))
      (fail! :user/not-authorized
             "Administrator authority at this scope is required."
             {:user/id user-id
              :organization/id organization-id
              :scope/target (scope-context-target scope-context)}))
    (let [membership-id
          (or (:membership/id access-context)
              (fail! :organization.fx/incomplete-user-authorization
                     "Administrator access did not identify a membership."
                     {:user/id user-id
                      :organization/id organization-id}))
          user-document
          (require-public-document!
           (user/user-facts ctx {:user-id user-id})
           :user/found? :user/doc :user/not-found
           "The signed-in user no longer exists."
           {:user/id user-id})
          membership-document
          (require-public-document!
           (user/membership-facts ctx membership-id)
           :membership/found? :membership/doc :membership/not-found
           "The administrator membership no longer exists."
           {:membership/id membership-id})
          assignments
          (role-assignment-documents-at-scopes
           ctx organization-id scopes)
          administrator-assignment
          (user/administrator-assignment
           user-document membership-document assignments scopes)]
      (when-not administrator-assignment
        (fail! :user/not-authorized
               "Administrator authority changed while it was being loaded."
               {:user/id user-id
                :organization/id organization-id
                :scope/target (scope-context-target scope-context)}))
      {:user/id user-id
       :organization/id organization-id
       :scope/target (scope-context-target scope-context)
       :user/authorization-versions
       [{:model/entity-type user/user-entity-type
         :model/expected
         (public-expected-version user-document user-version)}
        {:model/entity-type user/membership-entity-type
         :model/expected
         (public-expected-version membership-document membership-version)}
        {:model/entity-type user/role-assignment-entity-type
         :model/expected
         (public-expected-version
          administrator-assignment role-assignment-version)}]})))
(defn- organization-change
  [document operation change-kind]
  {:topic
   :organization
   :id
   (:xt/id document)
   :change/kind
   change-kind
   :organization/operation
   operation
   :organization/id
   (:xt/id document)
   :organization/status
   (:organization/status document)
   :organization/revision
   (:organization/revision document)})
(defn- organization-group-change
  [document operation change-kind]
  {:topic
   :organization-group
   :id
   (:xt/id document)
   :change/kind
   change-kind
   :organization-group/operation
   operation
   :organization-group/id
   (:xt/id document)
   :organization/id
   (:organization-group/organization document)
   :organization-group/parent-type
   (:organization-group/parent-type document)
   :organization-group/parent-id
   (:organization-group/parent-id document)
   :organization-group/status
   (:organization-group/status document)
   :organization-group/revision
   (:organization-group/revision document)})
(defn- location-change
  [document operation change-kind]
  {:topic
   :location
   :id
   (:xt/id document)
   :change/kind
   change-kind
   :location/operation
   operation
   :location/id
   (:xt/id document)
   :organization/id
   (:location/organization document)
   :location/parent-type
   (:location/parent-type document)
   :location/parent-id
   (:location/parent-id document)
   :location/status
   (:location/status document)
   :location/revision
   (:location/revision document)})
(defn- command-change
  [entity-kind operation command]
  (let [document
        (command-document command)
        change-kind
        (if
         (= :create
            (:model/operation command))
          :created
          :updated)]
    (case
     entity-kind
      :organization
      (organization-change
       document
       operation
       change-kind)
      :organization-group
      (organization-group-change
       document
       operation
       change-kind)
      :location
      (location-change
       document
       operation
       change-kind))))
(defn- empty-fragment []
  {:commands [] :assertions [] :changes []})
(defn- normalize-fragment
  [fragment]
  (merge
   (empty-fragment)
   (select-keys
    (or fragment {})
    [:commands
     :assertions
     :changes])))
(defn- merge-fragments
  [& fragments]
  (reduce
   (fn [result fragment]
     (let [{:keys [commands assertions changes]}
           (normalize-fragment fragment)]
       (-> result
           (update
            :commands
            into
            commands)
           (update
            :assertions
            into
            assertions)
           (update
            :changes
            into
            changes))))
   (empty-fragment)
   fragments))
(defn- transaction-plan
  [{:keys [commands assertions changes]}]
  {:commands
   (vec commands)
   :assertions
   (vec assertions)
   :changes
   (vec changes)
   :entry-fn
   change-entry})
(defn- organization-authorization-fragment
  [scope-facts user-authorization]
  {:assertions
   (authorization-version-assertions!
    (:authorization-versions scope-facts)
    (:user/authorization-versions
     user-authorization))})
(defn plan-create-child
  "Builds an atomic plan for a validated group or location create command."
  [{:keys
    [entity-kind
     operation
     command
     parent-scope-facts
     user-authorization]}]
  (let [document
        (command-document command)
        fragment
        (merge-fragments
         {:commands [command]
          :changes
          [(command-change
            entity-kind
            operation
            command)]}
         (organization-authorization-fragment
          parent-scope-facts
          user-authorization))]
    {:transaction-plan
     (transaction-plan fragment)
     :result
     {entity-kind document}}))
(defn plan-update-entity
  "Builds an atomic plan for one validated Organization-model update command."
  [{:keys
    [entity-kind
     operation
     command
     target-scope-facts
     user-authorization]}]
  (let [document
        (command-document command)
        fragment
        (merge-fragments
         {:commands [command]
          :changes
          [(command-change
            entity-kind
            operation
            command)]}
         (organization-authorization-fragment
          target-scope-facts
          user-authorization))]
    {:transaction-plan
     (transaction-plan fragment)
     :result
     {entity-kind document}}))
(defn plan-move-entity
  "Builds an atomic move plan guarded by both the current hierarchy and the
   destination hierarchy."
  [{:keys
    [entity-kind
     command
     current-scope-facts
     destination-scope-facts
     current-user-authorization
     destination-user-authorization]}]
  (let [document
        (command-document command)
        fragment
        (merge-fragments
         {:commands [command]
          :changes
          [(command-change
            entity-kind
            :move
            command)]}
         {:assertions
          (authorization-version-assertions!
           (:authorization-versions
            current-scope-facts)
           (:authorization-versions
            destination-scope-facts)
           (:user/authorization-versions
            current-user-authorization)
           (:user/authorization-versions
            destination-user-authorization))})]
    {:transaction-plan
     (transaction-plan fragment)
     :result
     {entity-kind document}}))
(defn- update-command
  [entity-kind operation document input]
  (case
   [entity-kind operation]
    [:organization :rename]
    (organization/rename-organization-command
     document
     input)
    [:organization :suspend]
    (organization/suspend-organization-command
     document
     input)
    [:organization :reactivate]
    (organization/reactivate-organization-command
     document
     input)
    [:organization-group :rename]
    (organization/rename-organization-group-command
     document
     input)
    [:organization-group :suspend]
    (organization/suspend-organization-group-command
     document
     input)
    [:organization-group :reactivate]
    (organization/reactivate-organization-group-command
     document
     input)
    [:location :rename]
    (organization/rename-location-command
     document
     input)
    [:location :suspend]
    (organization/suspend-location-command
     document
     input)
    [:location :reactivate]
    (organization/reactivate-location-command
     document
     input)
    (fail!
     :organization.fx/unsupported-update
     "The requested Organization update is not supported."
     {:entity-kind entity-kind
      :operation operation})))
(defn- entity-scope
  [entity-kind entity-id]
  (case
   entity-kind
    :organization
    (organization/organization-scope
     entity-id)
    :organization-group
    (organization/organization-group-scope
     entity-id)
    :location
    (organization/location-scope
     entity-id)))
(fx/defmachine create-child-machine
  :start
  (fn [ctx]
    (let [now
          (:biff.fx/now ctx)
          seed
          (:biff.fx/seed ctx)
          [entity-id _]
          (fx/uuid7 seed now)
          input
          (:organization.fx/input ctx)
          organization-id
          (:organization-id input)
          parent-scope
          (:parent-scope input)]
      {:organization.fx/entity-id
       entity-id
       :organization.fx/parent-facts
       (scope-context-effect
        organization-id
        parent-scope)
       :biff.fx/next
       :plan}))
  :plan
  (fn [ctx]
    (let [input
          (:organization.fx/input ctx)
          entity-kind
          (:organization.fx/entity-kind ctx)
          organization-id
          (:organization-id input)
          parent-scope
          (:parent-scope input)
          parent-scope-facts
          (-> (:organization.fx/parent-facts ctx)
              (require-scope-facts!
               organization-id
               parent-scope)
              require-operational-scope!)
          user-authorization
          (require-administrator-authorization!
           ctx
           (:scope-context
            parent-scope-facts))
          create-input
          {:id
           (:organization.fx/entity-id ctx)
           :organization-id
           organization-id
           :parent-scope
           parent-scope
           :name
           (:name input)
           :now
           (:biff.fx/now ctx)}
          command
          (case
           entity-kind
            :organization-group
            (organization/create-organization-group-command
             create-input)
            :location
            (organization/create-location-command
             create-input))
          plan
          (plan-create-child
           {:entity-kind entity-kind
            :operation :create
            :command command
            :parent-scope-facts parent-scope-facts
            :user-authorization user-authorization})]
      {:organization.fx/result
       (:result plan)
       :organization.fx/transaction-plan
       (:transaction-plan plan)
       :biff.fx/next
       :commit}))
  :commit commit-state
  :finish finish-state)
(defn create-organization-group
  "Creates one active organization group beneath an operational organization
   or group. The authenticated user must be an effective administrator at the
   parent scope."
  [ctx input]
  (create-child-machine
   (assoc
    ctx
    :organization.fx/entity-kind
    :organization-group
    :organization.fx/input
    input)))
(defn create-location
  "Creates one active location beneath an operational organization or group.
   The authenticated user must be an effective administrator at the parent
   scope."
  [ctx input]
  (create-child-machine
   (assoc
    ctx
    :organization.fx/entity-kind
    :location
    :organization.fx/input
    input)))
(fx/defmachine update-entity-machine
  :start
  (fn [ctx]
    (let [input
          (:organization.fx/input ctx)
          entity-kind
          (:organization.fx/entity-kind ctx)
          entity-id
          (:entity-id input)
          organization-id
          (:organization-id input)
          target-scope
          (entity-scope
           entity-kind
           entity-id)]
      {:organization.fx/target-scope
       target-scope
       :organization.fx/target-facts
       (scope-context-effect
        organization-id
        target-scope)
       :biff.fx/next
       :plan}))
  :plan
  (fn [ctx]
    (let [input
          (:organization.fx/input ctx)
          entity-kind
          (:organization.fx/entity-kind ctx)
          operation
          (:organization.fx/operation ctx)
          organization-id
          (:organization-id input)
          target-scope
          (:organization.fx/target-scope ctx)
          target-scope-facts
          (require-scope-facts!
           (:organization.fx/target-facts ctx)
           organization-id
           target-scope)
          user-authorization
          (require-administrator-authorization!
           ctx
           (:scope-context
            target-scope-facts))
          command-input
          {:name
           (:name input)
           :now
           (:biff.fx/now ctx)
           :actor-id
           (require-authenticated-user-id!
            ctx)
           :reason
           (:reason input)}
          command
          (update-command
           entity-kind
           operation
           (:document
            target-scope-facts)
           command-input)
          plan
          (plan-update-entity
           {:entity-kind entity-kind
            :operation operation
            :command command
            :target-scope-facts
            target-scope-facts
            :user-authorization
            user-authorization})]
      {:organization.fx/result
       (:result plan)
       :organization.fx/transaction-plan
       (:transaction-plan plan)
       :biff.fx/next
       :commit}))
  :commit commit-state
  :finish finish-state)
(defn- run-update
  [ctx entity-kind operation input]
  (update-entity-machine
   (assoc
    ctx
    :organization.fx/entity-kind
    entity-kind
    :organization.fx/operation
    operation
    :organization.fx/input
    input)))
(defn rename-organization
  [ctx {:keys [organization-id] :as input}]
  (run-update
   ctx
   :organization
   :rename
   (assoc
    input
    :entity-id
    organization-id)))
(defn suspend-organization
  [ctx {:keys [organization-id] :as input}]
  (run-update
   ctx
   :organization
   :suspend
   (assoc
    input
    :entity-id
    organization-id)))
(defn reactivate-organization
  [ctx {:keys [organization-id] :as input}]
  (run-update
   ctx
   :organization
   :reactivate
   (assoc
    input
    :entity-id
    organization-id)))
(defn rename-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (run-update
   ctx
   :organization-group
   :rename
   (assoc
    input
    :entity-id
    organization-group-id)))
(defn suspend-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (run-update
   ctx
   :organization-group
   :suspend
   (assoc
    input
    :entity-id
    organization-group-id)))
(defn reactivate-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (run-update
   ctx
   :organization-group
   :reactivate
   (assoc
    input
    :entity-id
    organization-group-id)))
(defn rename-location
  [ctx {:keys [location-id] :as input}]
  (run-update
   ctx
   :location
   :rename
   (assoc
    input
    :entity-id
    location-id)))
(defn suspend-location
  [ctx {:keys [location-id] :as input}]
  (run-update
   ctx
   :location
   :suspend
   (assoc
    input
    :entity-id
    location-id)))
(defn reactivate-location
  [ctx {:keys [location-id] :as input}]
  (run-update
   ctx
   :location
   :reactivate
   (assoc
    input
    :entity-id
    location-id)))
(defn- ensure-valid-move-destination!
  [entity-kind current-scope destination-scope destination-scope-facts]
  (require-operational-scope!
   destination-scope-facts)
  (when
   (and
    (= :organization-group
       entity-kind)
    (some
     #(organization/same-scope?
       current-scope
       %)
     (get-in
      destination-scope-facts
      [:scope-context
       :scope/applicable])))
    (fail!
     :organization-group/cycle
     "The organization group cannot be moved beneath itself or a descendant."
     {:organization-group/scope
      current-scope
      :destination-scope
      destination-scope}))
  destination-scope-facts)
(fx/defmachine move-entity-machine
  :start
  (fn [ctx]
    (let [input
          (:organization.fx/input ctx)
          entity-kind
          (:organization.fx/entity-kind ctx)
          entity-id
          (:entity-id input)
          organization-id
          (:organization-id input)
          current-scope
          (entity-scope
           entity-kind
           entity-id)
          destination-scope
          (:parent-scope input)]
      {:organization.fx/current-scope
       current-scope
       :organization.fx/destination-scope
       destination-scope
       :organization.fx/current-facts
       (scope-context-effect
        organization-id
        current-scope)
       :organization.fx/destination-facts
       (scope-context-effect
        organization-id
        destination-scope)
       :biff.fx/next
       :plan}))
  :plan
  (fn [ctx]
    (let [input
          (:organization.fx/input ctx)
          entity-kind
          (:organization.fx/entity-kind ctx)
          organization-id
          (:organization-id input)
          current-scope
          (:organization.fx/current-scope ctx)
          destination-scope
          (:organization.fx/destination-scope ctx)
          current-scope-facts
          (require-scope-facts!
           (:organization.fx/current-facts ctx)
           organization-id
           current-scope)
          destination-scope-facts
          (->>
           (require-scope-facts!
            (:organization.fx/destination-facts ctx)
            organization-id
            destination-scope)
           (ensure-valid-move-destination!
            entity-kind
            current-scope
            destination-scope))
          current-user-authorization
          (require-administrator-authorization!
           ctx
           (:scope-context
            current-scope-facts))
          destination-user-authorization
          (require-administrator-authorization!
           ctx
           (:scope-context
            destination-scope-facts))
          command-input
          {:parent-scope
           destination-scope
           :now
           (:biff.fx/now ctx)
           :actor-id
           (require-authenticated-user-id!
            ctx)
           :reason
           (:reason input)}
          command
          (case
           entity-kind
            :organization-group
            (organization/move-organization-group-command
             (:document
              current-scope-facts)
             command-input)
            :location
            (organization/move-location-command
             (:document
              current-scope-facts)
             command-input))
          plan
          (plan-move-entity
           {:entity-kind entity-kind
            :command command
            :current-scope-facts
            current-scope-facts
            :destination-scope-facts
            destination-scope-facts
            :current-user-authorization
            current-user-authorization
            :destination-user-authorization
            destination-user-authorization})]
      {:organization.fx/result
       (:result plan)
       :organization.fx/transaction-plan
       (:transaction-plan plan)
       :biff.fx/next
       :commit}))
  :commit commit-state
  :finish finish-state)
(defn- run-move [ctx entity-kind input]
  (move-entity-machine
   (assoc ctx :organization.fx/entity-kind entity-kind
          :organization.fx/input input)))
(defn move-organization-group [ctx {:keys [organization-group-id] :as input}]
  (run-move ctx :organization-group
            (assoc input :entity-id organization-group-id)))
(defn move-location [ctx {:keys [location-id] :as input}]
  (run-move ctx :location (assoc input :entity-id location-id)))
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
