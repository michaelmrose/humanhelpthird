(ns net.humanhelp.site.model.organization.fx
  "Organization-specific read dependencies and transaction planning.

   This namespace turns authoritative Organization hierarchy snapshots into
   generic gesso.model guards and combines those dependencies with canonical
   Organization commands and semantic Live changes.

   It does not authorize users and it does not commit transactions. Callers
   compose the returned fragments with other model fragments, then commit the
   complete atomic plan through gesso.model.tx."
  (:require
   [com.biffweb.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.organization.domain :as organization]
   [net.humanhelp.site.model.organization.graph :as organization.graph]))

;; =============================================================================
;; Errors and FX context
;; =============================================================================

(defn- fail!
  ([type message]
   (fail! type message nil))
  ([type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type type}
       (some? details)
       (assoc :error/details details))))))

(defn- now!
  [ctx]
  (or
   (:biff.fx/now ctx)
   (fail!
    :organization.fx/missing-now
    "Organization planning requires :biff.fx/now.")))

(defn- seed!
  [ctx]
  (or
   (:biff.fx/seed ctx)
   (fail!
    :organization.fx/missing-seed
    "Organization creation requires :biff.fx/seed.")))

(defn- generated-id
  [ctx]
  (first
   (fx/uuid7
    (seed! ctx)
    (now! ctx))))

;; =============================================================================
;; Guarded Organization dependencies
;; =============================================================================

(defn- document-guard
  [document]
  (cond
    (contains? document :location/organization)
    (command/guard
     organization/location-entity-type
     document
     organization/location-version)

    (contains? document :organization-group/organization)
    (command/guard
     organization/organization-group-entity-type
     document
     organization/organization-group-version)

    (contains? document :organization/name)
    (command/guard
     organization/organization-entity-type
     document
     organization/organization-version)

    :else
    (fail!
     :organization.fx/unknown-snapshot-document
     "An Organization hierarchy snapshot contains an unknown document type."
     {:model/id (:xt/id document)})))

(defn- snapshot-fragment
  [snapshot]
  (apply
   model.tx/guards-fragment
   (map
    document-guard
    (organization.graph/snapshot-documents
     snapshot))))

(defn scope-dependency
  "Returns the current Organization scope context and the guard-only fragment
   that keeps every persisted Organization document used to derive it current
   until commit.

   Returns nil when the target scope does not exist."
  [ctx scope]
  (when-let [snapshot
             (organization.graph/scope-snapshot
              ctx
              scope)]
    {:scope-context
     (:scope-context snapshot)

     :transaction-fragment
     (snapshot-fragment snapshot)}))

(defn require-scope-dependency
  "Returns scope-dependency or throws when the target scope does not exist."
  [ctx scope]
  (or
   (scope-dependency
    ctx
    scope)

   (fail!
    :organization/scope-not-found
    "The Organization scope does not exist."
    {:scope scope})))

;; =============================================================================
;; Hierarchy requirements
;; =============================================================================

(defn- require-parent-scope!
  [scope]
  (when-not
   (organization/parent-scope? scope)
    (fail!
     :organization.fx/invalid-parent-scope
     "An Organization or Organization Group parent scope is required."
     {:scope scope}))
  scope)

(defn- snapshot-organization-id
  [snapshot]
  (organization/organization-id
   (:organization snapshot)))

(defn- require-operational!
  [snapshot]
  (when-not
   (:operational? snapshot)
    (fail!
     :organization/scope-not-operational
     "The destination scope is not operational."
     {:organization/id
      (snapshot-organization-id snapshot)

      :scope
      (organization/scope-context-target
       (:scope-context snapshot))}))
  snapshot)

(defn- require-same-organization!
  [current destination]
  (let [current-id
        (snapshot-organization-id current)

        destination-id
        (snapshot-organization-id destination)]
    (when-not
     (= current-id destination-id)
      (fail!
       :organization/ownership-mismatch
       "The current and destination scopes belong to different Organizations."
       {:current-organization-id current-id
        :destination-organization-id destination-id}))
    destination))

(defn- require-group-move-destination!
  [current-scope destination]
  (when
   (organization/scope-applies?
    (:scope-context destination)
    current-scope)
    (fail!
     :organization-group/cycle
     "The organization group cannot be moved beneath itself or a descendant."
     {:organization-group/scope current-scope
      :destination-scope
      (organization/scope-context-target
       (:scope-context destination))}))
  destination)

;; =============================================================================
;; Semantic changes and planned Organization mutations
;; =============================================================================

(defn- change-entry
  [{:keys [topic id]}]
  {:coalesce-key
   [topic id]})

(def transaction-options
  "Transaction-wide options to apply after composing an Organization fragment
   with any other model fragments."
  {:entry-fn change-entry})

(defn- command-change
  [entity operation model-command]
  (let [document
        (command/after model-command)

        base
        {:topic entity
         :id (:xt/id document)
         :change/kind
         (if
          (command/create? model-command)
           :created
           :updated)}]
    (case entity
      :organization
      (merge
       base
       {:organization/operation operation
        :organization/id (:xt/id document)
        :organization/status
        (:organization/status document)
        :organization/revision
        (:organization/revision document)})

      :organization-group
      (merge
       base
       {:organization-group/operation operation
        :organization-group/id (:xt/id document)
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

      :location
      (merge
       base
       {:location/operation operation
        :location/id (:xt/id document)
        :organization/id
        (:location/organization document)
        :location/parent-type
        (:location/parent-type document)
        :location/parent-id
        (:location/parent-id document)
        :location/status
        (:location/status document)
        :location/revision
        (:location/revision document)}))))

(defn- mutation-fragment
  [entity operation model-command]
  (model.tx/fragment
   {:commands
    [model-command]

    :changes
    [(command-change
      entity
      operation
      model-command)]}))

(defn- planned
  ([entity operation model-command]
   (planned
    entity
    operation
    model-command
    model.tx/empty-fragment))
  ([entity operation model-command dependency-fragment]
   {:result
    {entity
     (command/after model-command)}

    :transaction-fragment
    (model.tx/compose
     dependency-fragment
     (mutation-fragment
      entity
      operation
      model-command))

    :transaction-options
    transaction-options}))

;; =============================================================================
;; Domain command dispatch
;; =============================================================================

(def ^:private update-command-fns
  {[:organization :rename]
   #'organization/rename-organization-command

   [:organization :suspend]
   #'organization/suspend-organization-command

   [:organization :reactivate]
   #'organization/reactivate-organization-command

   [:organization :close]
   #'organization/close-organization-command

   [:organization-group :rename]
   #'organization/rename-organization-group-command

   [:organization-group :suspend]
   #'organization/suspend-organization-group-command

   [:organization-group :reactivate]
   #'organization/reactivate-organization-group-command

   [:organization-group :close]
   #'organization/close-organization-group-command

   [:location :rename]
   #'organization/rename-location-command

   [:location :suspend]
   #'organization/suspend-location-command

   [:location :reactivate]
   #'organization/reactivate-location-command

   [:location :close]
   #'organization/close-location-command})

(defn- entity-scope
  [entity id]
  (case entity
    :organization
    (organization/organization-scope id)

    :organization-group
    (organization/organization-group-scope id)

    :location
    (organization/location-scope id)))

(defn- update-command!
  [entity operation document input]
  (if-let [command-fn
           (get
            update-command-fns
            [entity operation])]
    (command-fn
     document
     input)

    (fail!
     :organization.fx/unsupported-update
     "The requested Organization update is not supported."
     {:entity-kind entity
      :operation operation})))

;; =============================================================================
;; Create plans
;; =============================================================================

(defn plan-create-organization
  [ctx input]
  (planned
   :organization
   :create
   (organization/create-organization-command
    {:id
     (generated-id ctx)

     :name
     (:name input)

     :now
     (now! ctx)})))

(defn- plan-create-child
  [ctx entity input]
  (let [parent-scope
        (require-parent-scope!
         (:parent-scope input))

        parent
        (-> (organization.graph/require-scope-snapshot
             ctx
             parent-scope)
            require-operational!)

        organization-id
        (snapshot-organization-id parent)

        command-fn
        (case entity
          :organization-group
          organization/create-organization-group-command

          :location
          organization/create-location-command)

        model-command
        (command-fn
         {:id
          (generated-id ctx)

          :organization-id
          organization-id

          :parent-scope
          parent-scope

          :name
          (:name input)

          :now
          (now! ctx)})]
    (planned
     entity
     :create
     model-command
     (snapshot-fragment parent))))

(defn plan-create-organization-group
  [ctx input]
  (plan-create-child
   ctx
   :organization-group
   input))

(defn plan-create-location
  [ctx input]
  (plan-create-child
   ctx
   :location
   input))

;; =============================================================================
;; Ordinary update plans
;; =============================================================================

(defn- plan-update
  [ctx entity operation id input]
  (let [snapshot
        (organization.graph/require-scope-snapshot
         ctx
         (entity-scope entity id))

        model-command
        (update-command!
         entity
         operation
         (:target snapshot)
         {:name
          (:name input)

          :now
          (now! ctx)

          :actor-id
          (:actor-id input)

          :reason
          (:reason input)})]
    (planned
     entity
     operation
     model-command
     (snapshot-fragment snapshot))))

(defn plan-rename-organization
  [ctx {:keys [organization-id] :as input}]
  (plan-update
   ctx
   :organization
   :rename
   organization-id
   input))

(defn plan-suspend-organization
  [ctx {:keys [organization-id] :as input}]
  (plan-update
   ctx
   :organization
   :suspend
   organization-id
   input))

(defn plan-reactivate-organization
  [ctx {:keys [organization-id] :as input}]
  (plan-update
   ctx
   :organization
   :reactivate
   organization-id
   input))

(defn plan-close-organization
  [ctx {:keys [organization-id] :as input}]
  (plan-update
   ctx
   :organization
   :close
   organization-id
   input))

(defn plan-rename-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (plan-update
   ctx
   :organization-group
   :rename
   organization-group-id
   input))

(defn plan-suspend-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (plan-update
   ctx
   :organization-group
   :suspend
   organization-group-id
   input))

(defn plan-reactivate-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (plan-update
   ctx
   :organization-group
   :reactivate
   organization-group-id
   input))

(defn plan-close-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (plan-update
   ctx
   :organization-group
   :close
   organization-group-id
   input))

(defn plan-rename-location
  [ctx {:keys [location-id] :as input}]
  (plan-update
   ctx
   :location
   :rename
   location-id
   input))

(defn plan-suspend-location
  [ctx {:keys [location-id] :as input}]
  (plan-update
   ctx
   :location
   :suspend
   location-id
   input))

(defn plan-reactivate-location
  [ctx {:keys [location-id] :as input}]
  (plan-update
   ctx
   :location
   :reactivate
   location-id
   input))

(defn plan-close-location
  [ctx {:keys [location-id] :as input}]
  (plan-update
   ctx
   :location
   :close
   location-id
   input))

;; =============================================================================
;; Move plans
;; =============================================================================

(defn- plan-move
  [ctx entity id input]
  (let [current-scope
        (entity-scope
         entity
         id)

        destination-scope
        (require-parent-scope!
         (:parent-scope input))

        current
        (organization.graph/require-scope-snapshot
         ctx
         current-scope)

        destination
        (-> (organization.graph/require-scope-snapshot
             ctx
             destination-scope)
            require-operational!)

        _
        (require-same-organization!
         current
         destination)

        _
        (when
         (= entity :organization-group)
          (require-group-move-destination!
           current-scope
           destination))

        command-fn
        (case entity
          :organization-group
          organization/move-organization-group-command

          :location
          organization/move-location-command)

        model-command
        (command-fn
         (:target current)
         {:parent-scope
          destination-scope

          :now
          (now! ctx)

          :actor-id
          (:actor-id input)

          :reason
          (:reason input)})

        dependencies
        (model.tx/compose
         (snapshot-fragment current)
         (snapshot-fragment destination))]
    (planned
     entity
     :move
     model-command
     dependencies)))

(defn plan-move-organization-group
  [ctx {:keys [organization-group-id] :as input}]
  (plan-move
   ctx
   :organization-group
   organization-group-id
   input))

(defn plan-move-location
  [ctx {:keys [location-id] :as input}]
  (plan-move
   ctx
   :location
   location-id
   input))
