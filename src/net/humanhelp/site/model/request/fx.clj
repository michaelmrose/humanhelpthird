(ns net.humanhelp.site.model.request.fx
  (:require
   [gesso.fx :as fx]
   [gesso.live.core :as live]
   [net.humanhelp.site.model.request.domain :as request]
   [net.humanhelp.site.model.request.graph :as request.graph]))

;; =============================================================================
;; Effect contract
;; =============================================================================

(def commit-effect
  ::commit)

(def commit-command-key
  "Optional ctx key containing (fn [ctx command] ...).

  The override is the place to install an atomic compare-and-set request
  transition. If absent, the default handler writes the command's resulting
  request document and publishes its Gesso Live change."
  ::commit-command!)

(def emit-key
  "Optional ctx key controlling Gesso Live emission.

  Supported values are the same as gesso.live/transact-and-notify!:
  :async, :sync, or false. Defaults to :async."
  ::emit)

(defn- live-system
  [ctx]
  (or (:gesso.live/system ctx)
      (:live/system ctx)
      (throw
       (ex-info
        "Request FX requires a Gesso Live system."
        {:error/type :request.fx/missing-live-system
         :expected-one-of [:gesso.live/system
                           :live/system]}))))

(defn- operation->change-kind
  [operation]
  (if (= operation :create)
    :created
    :updated))

(defn- command-document
  [command]
  (or (:request/after command)
      (:request/doc command)
      (throw
       (ex-info
        "Request command does not contain a resulting request document."
        {:error/type :request.fx/missing-command-document
         :command command}))))

(defn- command->tx-ops
  [command]
  [[:put-docs request/entity-type (command-document command)]])

(defn- command->change
  [{:request/keys [operation] :as command}]
  (let [after (command-document command)]
    (merge
     (request/request-change after operation)
     {:topic :request
      :id (:xt/id after)
      :change/kind (operation->change-kind operation)})))

(defn- command->entry
  [command]
  {:coalesce-key
   [:request (:xt/id (command-document command))]})

(defn- emit-mode
  [ctx]
  (if (contains? ctx emit-key)
    (get ctx emit-key)
    :async))

(defn- default-commit-command!
  [ctx command]
  (live/transact-and-notify!
   (live-system ctx)
   ctx
   {:tx-ops (command->tx-ops command)
    :change (command->change command)
    :entry (command->entry command)
    :emit (emit-mode ctx)}))

(defn- handle-commit
  [ctx command]
  (if-some [commit-command! (get ctx commit-command-key)]
    (commit-command! ctx command)
    (default-commit-command! ctx command)))

(def handlers
  {commit-effect handle-commit})

;; =============================================================================
;; Query inputs
;; =============================================================================

(defn- capability-id
  [ctx]
  (or (:current-request-capability/id ctx)
      (:request/capability-id ctx)))

(defn- customer-query-input
  [ctx]
  (request.graph/query-input
   {:request-id (:request/id ctx)
    :user-id (:current-user/id ctx)
    :capability-id (capability-id ctx)}))

(defn- employee-query-input
  [ctx]
  (request/without-nils
   {:request/id (:request/id ctx)
    :current-employee/id (:current-employee/id ctx)}))

;; =============================================================================
;; Shared results and state functions
;; =============================================================================

(defn- not-found-result
  []
  {:biff.fx/return
   {:ok? false
    :error :request/not-found}})

(defn- not-owner-result
  []
  {:biff.fx/return
   {:ok? false
    :error :request/not-owner}})

(defn- prepare-update
  [before operation result]
  (if-not (:ok? result)
    {:biff.fx/return result}
    (let [after (:request result)]
      {:request/before before
       :request/after after
       :request/command
       (request/update-command before after operation)
       :biff.fx/next :commit})))

(defn- commit-state
  [{:request/keys [command]}]
  {:request/commit-result
   [commit-effect command]

   :biff.fx/next
   :finish})

(defn- finish-state
  [{:request/keys [after commit-result]}]
  {:biff.fx/return
   {:ok? true
    :request after
    :commit-result commit-result}})

;; =============================================================================
;; Create request
;; =============================================================================

(defn- prepare-create-input
  [ctx request-id now]
  (let [input
        (assoc (or (:request/create-input ctx) {})
               :id request-id
               :now now)

        requestor-supplied?
        (or (some? (:user-id input))
            (some? (:capability-id input)))

        current-user-id
        (:current-user/id ctx)

        current-capability-id
        (capability-id ctx)]
    (request/normalize-create-input
     (cond
       requestor-supplied?
       input

       (request/uuid-value? current-user-id)
       (assoc input :user-id current-user-id)

       (request/uuid-value? current-capability-id)
       (assoc input :capability-id current-capability-id)

       :else
       input))))

(fx/defmachine create-request
  :start
  (fn [ctx]
    (let [now
          (:biff.fx/now ctx)

          seed
          (:biff.fx/seed ctx)

          [request-id next-seed]
          (fx/uuid7 seed now)]
      {:request/id request-id
       :request/next-seed next-seed
       :request/input
       (prepare-create-input ctx request-id now)

       :biff.fx/next
       :validate}))

  :validate
  (fn [{:request/keys [input]}]
    (let [errors (request/create-input-errors input)]
      (if (seq errors)
        {:biff.fx/return
         {:ok? false
          :error :request/invalid-input
          :errors errors}}

        {:biff.fx/next
         :build})))

  :build
  (fn [{:request/keys [input]}]
    (let [doc (request/new-request input)]
      {:request/after doc
       :request/command
       (request/create-command doc)

       :biff.fx/next
       :commit}))

  :commit
  commit-state

  :finish
  finish-state)

;; =============================================================================
;; Edit request once
;; =============================================================================

(fx/defmachine edit-request
  :start
  (fn [ctx]
    {:request/input
     (request/normalize-edit-input
      (:request/edit-input ctx))

     :request/facts
     [:biff.graph.fx/query
      (customer-query-input ctx)
      request.graph/customer-command-query]

     :biff.fx/next
     :validate})

  :validate
  (fn [{:request/keys [facts input]
        :biff.fx/keys [now]}]
    (cond
      (not (:request/found? facts))
      (not-found-result)

      (not (:request/owned-by-current-actor? facts))
      (not-owner-result)

      (not (:request/can-edit? facts))
      {:biff.fx/return
       {:ok? false
        :error
        (if (:request/edited-at (:request/doc facts))
          :request/edit-used
          :request/not-editable)}}

      :else
      (prepare-update
       (:request/doc facts)
       :edit
       (request/edit-request-doc
        (:request/doc facts)
        input
        now))))

  :commit
  commit-state

  :finish
  finish-state)

;; =============================================================================
;; Customer lifecycle transitions
;; =============================================================================

(defn- customer-transition-machine
  [machine-name action permission-key denied-error]
  (fx/machine
   machine-name

   :start
   (fn [ctx]
     {:request/facts
      [:biff.graph.fx/query
       (customer-query-input ctx)
       request.graph/customer-command-query]

      :biff.fx/next
      :validate})

   :validate
   (fn [{:request/keys [facts]
         :biff.fx/keys [now]}]
     (cond
       (not (:request/found? facts))
       (not-found-result)

       (not (:request/owned-by-current-actor? facts))
       (not-owner-result)

       (not (get facts permission-key))
       {:biff.fx/return
        {:ok? false
         :error denied-error}}

       :else
       (prepare-update
        (:request/doc facts)
        action
        (request/transition-request
         (:request/doc facts)
         action
         {:now now}))))

   :commit
   commit-state

   :finish
   finish-state))

(def cancel-request
  (customer-transition-machine
   ::cancel-request
   :cancel
   :request/can-cancel?
   :request/not-cancellable))

(def mark-request-done
  (customer-transition-machine
   ::mark-request-done
   :done
   :request/can-mark-done?
   :request/not-markable-done))

;; =============================================================================
;; Employee lifecycle transitions
;; =============================================================================

(defn- employee-transition-allowed?
  [facts action]
  (case action
    :claim
    (:request/claimable? facts)

    :unclaim
    (and (:request/unclaimable? facts)
         (:request/assigned-to-current-employee? facts))

    :mark-on-the-way
    (and (:request/markable-on-the-way? facts)
         (:request/assigned-to-current-employee? facts))

    false))

(defn- employee-transition-machine
  [machine-name action denied-error]
  (fx/machine
   machine-name

   :start
   (fn [ctx]
     {:request/facts
      [:biff.graph.fx/query
       (employee-query-input ctx)
       request.graph/employee-command-query]

      :biff.fx/next
      :validate})

   :validate
   (fn [{:request/keys [facts]
         :current-employee/keys [id]
         :biff.fx/keys [now]}]
     (cond
       (not (request/uuid-value? id))
       {:biff.fx/return
        {:ok? false
         :error :request/not-authorized}}

       (not (:request/found? facts))
       (not-found-result)

       (not (employee-transition-allowed? facts action))
       {:biff.fx/return
        {:ok? false
         :error denied-error}}

       :else
       (prepare-update
        (:request/doc facts)
        action
        (request/transition-request
         (:request/doc facts)
         action
         {:actor-id id
          :now now}))))

   :commit
   commit-state

   :finish
   finish-state))

(def claim-request
  (employee-transition-machine
   ::claim-request
   :claim
   :request/not-claimable))

(def unclaim-request
  (employee-transition-machine
   ::unclaim-request
   :unclaim
   :request/not-unclaimable))

(def mark-request-on-the-way
  (employee-transition-machine
   ::mark-request-on-the-way
   :mark-on-the-way
   :request/not-markable-on-the-way))
