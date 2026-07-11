(ns net.humanhelp.model.request
  (:require
   [clojure.string :as str]
   [gesso.fx :as fx]
   [gesso.graph :as graph])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Constants
;; =============================================================================

(def table :request)

(def store-area-text-max 120)
(def title-max 60)
(def details-max 500)

(def status-order
  [:open
   :claimed
   :on-the-way
   :done
   :cancelled])

(def statuses
  (set status-order))

(def active-statuses
  #{:open :claimed :on-the-way})

(def terminal-statuses
  #{:done :cancelled})

(def progress-stages
  [:created :claimed :on-the-way :done])

(def status->progress-stage
  {:open       :created
   :claimed    :claimed
   :on-the-way :on-the-way
   :done       :done
   :cancelled  :cancelled})

(def status->progress-index
  {:open       0
   :claimed    1
   :on-the-way 2
   :done       3})

(def allowed-transitions
  {[:open :claim]              :claimed
   [:claimed :unclaim]         :open
   [:claimed :mark-on-the-way] :on-the-way
   [:open :cancel]             :cancelled
   [:claimed :cancel]          :cancelled
   [:on-the-way :cancel]       :cancelled
   [:open :done]               :done
   [:claimed :done]            :done
   [:on-the-way :done]         :done})

(def action-error-messages
  {:request/not-found
   "That request no longer exists."

   :request/not-owner
   "You do not have permission to change this request."

   :request/edit-used
   "This request has already been edited."

   :request/not-editable
   "This request can no longer be edited."

   :request/not-cancellable
   "This request can no longer be cancelled."

   :request/not-markable-done
   "This request cannot be marked done from its current state."

   :request/not-claimable
   "This request is no longer available to claim."

   :request/not-unclaimable
   "This request cannot be unclaimed from its current state."

   :request/not-markable-on-the-way
   "Help cannot be marked on the way from the request's current state."

   :request/not-authorized
   "You do not have permission to perform this action."

   :request/stale
   "The request changed before your action completed. Please try again."

   :request/invalid-input
   "Some request information needs to be corrected."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn uuid?
  [value]
  (instance? UUID value))

(defn instant?
  [value]
  (instance? Instant value))

(defn- trim-to-nil
  [value]
  (when (some? value)
    (let [value (str/trim (str value))]
      (when-not (str/blank? value)
        value))))

(defn- without-nils
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- exactly-one?
  [& values]
  (= 1 (count (filter some? values))))

(defn error-message
  [error]
  (get action-error-messages
       error
       "The request could not be updated."))

;; =============================================================================
;; Domain rules
;; =============================================================================

(defn status?
  [value]
  (contains? statuses value))

(defn active?
  [request]
  (contains? active-statuses (:request/status request)))

(defn terminal?
  [request]
  (contains? terminal-statuses (:request/status request)))

(defn open?
  [request]
  (= :open (:request/status request)))

(defn claimed?
  [request]
  (= :claimed (:request/status request)))

(defn on-the-way?
  [request]
  (= :on-the-way (:request/status request)))

(defn done?
  [request]
  (= :done (:request/status request)))

(defn cancelled?
  [request]
  (= :cancelled (:request/status request)))

(defn editable?
  [request]
  (and (active? request)
       (nil? (:request/edited-at request))))

(defn cancellable?
  [request]
  (active? request))

(defn markable-done?
  [request]
  (active? request))

(defn claimable?
  [request]
  (open? request))

(defn unclaimable?
  [request]
  (claimed? request))

(defn markable-on-the-way?
  [request]
  (claimed? request))

(defn progress-stage
  [request]
  (get status->progress-stage (:request/status request)))

(defn progress-index
  [request]
  (get status->progress-index (:request/status request)))

(defn next-status
  [request action]
  (get allowed-transitions
       [(:request/status request) action]))

(defn can-transition?
  [request action]
  (some? (next-status request action)))

(defn owned-by-user?
  [request user-id]
  (and (uuid? user-id)
       (= user-id (:request/user request))))

(defn owned-by-capability?
  [request capability-id]
  (and (uuid? capability-id)
       (= capability-id (:request/capability request))))

(defn owned-by?
  [request {:keys [user-id capability-id]}]
  (or (owned-by-user? request user-id)
      (owned-by-capability? request capability-id)))

;; =============================================================================
;; Schema invariants
;; =============================================================================

(defn- requestor-consistent?
  [request]
  (exactly-one? (:request/user request)
                (:request/capability request)))

(defn- location-consistent?
  [request]
  (exactly-one? (:request/store-area request)
                (:request/store-area-text request)))

(defn- lifecycle-consistent?
  [{:request/keys
    [status
     claimed-by
     claimed-at
     on-the-way-at
     completed-at
     cancelled-at]}]
  (case status
    :open
    (and (nil? claimed-by)
         (nil? claimed-at)
         (nil? on-the-way-at)
         (nil? completed-at)
         (nil? cancelled-at))

    :claimed
    (and (some? claimed-by)
         (some? claimed-at)
         (nil? on-the-way-at)
         (nil? completed-at)
         (nil? cancelled-at))

    :on-the-way
    (and (some? claimed-by)
         (some? claimed-at)
         (some? on-the-way-at)
         (nil? completed-at)
         (nil? cancelled-at))

    :done
    (and (some? completed-at)
         (nil? cancelled-at))

    :cancelled
    (and (some? cancelled-at)
         (nil? completed-at))

    false))

;; =============================================================================
;; Schema
;; =============================================================================

(def request-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:request/store :uuid]

    [:request/user
     {:optional true}
     :uuid]

    [:request/capability
     {:optional true}
     :uuid]

    [:request/store-area
     {:optional true}
     :uuid]

    [:request/store-area-text
     {:optional true}
     :request/store-area-text]

    [:request/title :request/title]

    [:request/details
     {:optional true}
     :request/details]

    [:request/status :request/status]
    [:request/revision :request/revision]

    [:request/claimed-by
     {:optional true}
     :uuid]

    [:request/created-at ::instant]
    [:request/updated-at ::instant]

    [:request/claimed-at
     {:optional true}
     ::instant]

    [:request/on-the-way-at
     {:optional true}
     ::instant]

    [:request/edited-at
     {:optional true}
     ::instant]

    [:request/completed-at
     {:optional true}
     ::instant]

    [:request/cancelled-at
     {:optional true}
     ::instant]]

   [:fn
    {:error/message
     "A request must belong to exactly one user or request capability."}
    requestor-consistent?]

   [:fn
    {:error/message
     "A request must have exactly one selected or written in-store location."}
    location-consistent?]

   [:fn
    {:error/message
     "The request lifecycle fields are inconsistent with its status."}
    lifecycle-consistent?]])

(def schema
  {::instant
   [:fn instant?]

   :request/store-area-text
   [:string {:min 1
             :max store-area-text-max}]

   :request/title
   [:string {:min 1
             :max title-max}]

   :request/details
   [:string {:min 1
             :max details-max}]

   :request/status
   (into [:enum] status-order)

   :request/revision
   [:int {:min 0}]

   :request/progress-stage
   [:enum :created :claimed :on-the-way :done :cancelled]

   :request/progress-index
   [:int {:min 0 :max 3}]

   :request/found? :boolean
   :request/active? :boolean
   :request/terminal? :boolean
   :request/editable? :boolean
   :request/cancellable? :boolean
   :request/markable-done? :boolean
   :request/claimable? :boolean
   :request/unclaimable? :boolean
   :request/markable-on-the-way? :boolean
   :request/owned-by-current-actor? :boolean
   :request/can-edit? :boolean
   :request/can-cancel? :boolean
   :request/can-mark-done? :boolean

   :request/id :uuid
   :request/store-id :uuid
   :request/store-area-id :uuid
   :request/claimed-by-id :uuid
   :request/doc request-schema

   :request request-schema})

;; =============================================================================
;; Input normalization and validation
;; =============================================================================

(defn normalize-location-input
  [{:keys [store-area-id store-area-text] :as input}]
  (assoc input
         :store-area-id
         (when (uuid? store-area-id)
           store-area-id)

         :store-area-text
         (trim-to-nil store-area-text)))

(defn normalize-create-input
  [input]
  (-> (or input {})
      normalize-location-input
      (update :title trim-to-nil)
      (update :details trim-to-nil)))

(defn normalize-edit-input
  [input]
  (-> (or input {})
      normalize-location-input
      (update :title trim-to-nil)
      (update :details trim-to-nil)))

(defn location-input-errors
  [{:keys [store-area-id store-area-text]}]
  (cond-> {}
    (not (exactly-one? store-area-id store-area-text))
    (assoc :location
           "Choose one store location or select ‘I'll tell you…’ and enter it.")

    (and store-area-text
         (> (count store-area-text) store-area-text-max))
    (assoc :store-area-text
           (str "Use " store-area-text-max " characters or fewer."))))

(defn request-text-errors
  [{:keys [title details]}]
  (cond-> {}
    (nil? title)
    (assoc :title "Briefly describe what you need.")

    (and title
         (> (count title) title-max))
    (assoc :title
           (str "Use " title-max " characters or fewer."))

    (and details
         (> (count details) details-max))
    (assoc :details
           (str "Use " details-max " characters or fewer."))))

(defn create-input-errors
  [input]
  (let [{:keys [store-id user-id capability-id now] :as input}
        (normalize-create-input input)]
    (merge
     (location-input-errors input)
     (request-text-errors input)
     (cond-> {}
       (not (uuid? store-id))
       (assoc :store-id "Choose a valid location.")

       (not (exactly-one? user-id capability-id))
       (assoc :requestor
              "Exactly one signed-in user or request capability is required.")

       (and user-id
            (not (uuid? user-id)))
       (assoc :user-id "The signed-in user is invalid.")

       (and capability-id
            (not (uuid? capability-id)))
       (assoc :capability-id "The request capability is invalid.")

       (not (instant? now))
       (assoc :now "A valid creation time is required.")))))

(defn edit-input-errors
  [input]
  (let [input (normalize-edit-input input)]
    (merge
     (location-input-errors input)
     (request-text-errors input))))

(defn valid-create-input?
  [input]
  (empty? (create-input-errors input)))

(defn valid-edit-input?
  [input]
  (empty? (edit-input-errors input)))

(defn- throw-invalid!
  [message errors input]
  (throw
   (ex-info
    message
    {:error/type :request/invalid-input
     :errors errors
     :input input})))

;; =============================================================================
;; Request construction and state changes
;; =============================================================================

(defn new-request
  [{:keys
    [id
     store-id
     user-id
     capability-id
     store-area-id
     store-area-text
     title
     details
     now]
    :as input}]
  (let [{:keys [store-area-id store-area-text title details] :as normalized}
        (normalize-create-input input)
        errors
        (merge
         (create-input-errors normalized)
         (cond-> {}
           (not (uuid? id))
           (assoc :id "A request UUID is required.")))]
    (when (seq errors)
      (throw-invalid! "Cannot create request." errors input))

    (cond->
     {:xt/id id
      :request/store store-id
      :request/title title
      :request/status :open
      :request/revision 0
      :request/created-at now
      :request/updated-at now}

      user-id
      (assoc :request/user user-id)

      capability-id
      (assoc :request/capability capability-id)

      store-area-id
      (assoc :request/store-area store-area-id)

      store-area-text
      (assoc :request/store-area-text store-area-text)

      details
      (assoc :request/details details))))

(defn- bump-revision
  [request now]
  (-> request
      (update :request/revision (fnil inc 0))
      (assoc :request/updated-at now)))

(defn edit-request-doc
  [request input now]
  (let [{:keys [store-area-id store-area-text title details] :as input}
        (normalize-edit-input input)
        errors
        (edit-input-errors input)]
    (cond
      (not (editable? request))
      {:ok? false
       :error
       (if (:request/edited-at request)
         :request/edit-used
         :request/not-editable)}

      (seq errors)
      {:ok? false
       :error :request/invalid-input
       :errors errors}

      (not (instant? now))
      {:ok? false
       :error :request/invalid-input
       :errors {:now "A valid edit time is required."}}

      :else
      {:ok? true
       :request
       (cond->
        (-> request
            (dissoc :request/store-area
                    :request/store-area-text
                    :request/details)
            (assoc :request/title title
                   :request/edited-at now)
            (bump-revision now))

         store-area-id
         (assoc :request/store-area store-area-id)

         store-area-text
         (assoc :request/store-area-text store-area-text)

         details
         (assoc :request/details details))})))

(defn transition-request
  [request action {:keys [actor-id now]}]
  (let [status' (next-status request action)]
    (cond
      (nil? status')
      {:ok? false
       :error
       (case action
         :claim :request/not-claimable
         :unclaim :request/not-unclaimable
         :mark-on-the-way :request/not-markable-on-the-way
         :cancel :request/not-cancellable
         :done :request/not-markable-done
         :request/not-authorized)}

      (not (instant? now))
      {:ok? false
       :error :request/invalid-input
       :errors {:now "A valid transition time is required."}}

      (and (= action :claim)
           (not (uuid? actor-id)))
      {:ok? false
       :error :request/invalid-input
       :errors {:actor-id "A valid employee UUID is required."}}

      :else
      {:ok? true
       :request
       (case action
         :claim
         (-> request
             (assoc :request/status :claimed
                    :request/claimed-by actor-id
                    :request/claimed-at now)
             (dissoc :request/on-the-way-at
                     :request/completed-at
                     :request/cancelled-at)
             (bump-revision now))

         :unclaim
         (-> request
             (assoc :request/status :open)
             (dissoc :request/claimed-by
                     :request/claimed-at
                     :request/on-the-way-at
                     :request/completed-at
                     :request/cancelled-at)
             (bump-revision now))

         :mark-on-the-way
         (-> request
             (assoc :request/status :on-the-way
                    :request/on-the-way-at now)
             (dissoc :request/completed-at
                     :request/cancelled-at)
             (bump-revision now))

         :cancel
         (-> request
             (assoc :request/status :cancelled
                    :request/cancelled-at now)
             (dissoc :request/completed-at)
             (bump-revision now))

         :done
         (-> request
             (assoc :request/status :done
                    :request/completed-at now)
             (dissoc :request/cancelled-at)
             (bump-revision now)))})))

;; =============================================================================
;; Request-specific command data
;; =============================================================================

(defn create-request-tx
  [request]
  [[:put-docs table request]])

(defn update-request-tx
  [request]
  [[:put-docs table request]])

(defn expected-version
  [request]
  {:request/id (:xt/id request)
   :request/revision (:request/revision request)
   :request/status (:request/status request)
   :request/updated-at (:request/updated-at request)})

(defn create-command
  [request]
  {:request/operation :create
   :request/id (:xt/id request)
   :request/doc request
   :tx (create-request-tx request)})

(defn transition-command
  [before after action]
  {:request/operation action
   :request/id (:xt/id before)
   :request/expected (expected-version before)
   :request/doc after
   :tx (update-request-tx after)})

(defn request-change
  [request operation]
  (without-nils
   {:request/id (:xt/id request)
    :request/store-id (:request/store request)
    :request/user-id (:request/user request)
    :request/capability-id (:request/capability request)
    :request/status (:request/status request)
    :request/revision (:request/revision request)
    :request/operation operation}))

;; =============================================================================
;; Injected request services
;; =============================================================================

(def load-request-service
  ::load-request)

(def submit-create-service
  ::submit-create!)

(def submit-transition-service
  ::submit-transition!)

(def publish-service
  ::publish!)

(defn- required-service
  [ctx service-key]
  (or (get ctx service-key)
      (throw
       (ex-info
        (str "Missing request model service " service-key)
        {:error/type :request/missing-service
         :service service-key}))))

;; =============================================================================
;; Graph resolvers
;; =============================================================================

(graph/defresolver request-by-id
  {:input [:request/id]
   :output [:request/found?
            :request/doc]}
  [ctx {:request/keys [id]}]
  (let [request
        ((required-service ctx load-request-service) ctx id)]
    (cond->
     {:request/found? (some? request)}
      request
      (assoc :request/doc request))))

(def request-field-query
  [:request/id
   :request/store-id
   :request/store-area-id
   :request/store-area-text
   :request/title
   :request/details
   :request/status
   :request/revision
   :request/claimed-by-id
   :request/created-at
   :request/updated-at
   :request/claimed-at
   :request/on-the-way-at
   :request/edited-at
   :request/completed-at
   :request/cancelled-at])

(graph/defresolver request-fields
  {:input [:request/doc]
   :output request-field-query}
  [_ctx {:request/keys [doc]}]
  (without-nils
   {:request/id (:xt/id doc)
    :request/store-id (:request/store doc)
    :request/store-area-id (:request/store-area doc)
    :request/store-area-text (:request/store-area-text doc)
    :request/title (:request/title doc)
    :request/details (:request/details doc)
    :request/status (:request/status doc)
    :request/revision (:request/revision doc)
    :request/claimed-by-id (:request/claimed-by doc)
    :request/created-at (:request/created-at doc)
    :request/updated-at (:request/updated-at doc)
    :request/claimed-at (:request/claimed-at doc)
    :request/on-the-way-at (:request/on-the-way-at doc)
    :request/edited-at (:request/edited-at doc)
    :request/completed-at (:request/completed-at doc)
    :request/cancelled-at (:request/cancelled-at doc)}))

(graph/defresolver request-lifecycle-facts
  {:input [:request/doc]
   :output [:request/active?
            :request/terminal?
            :request/editable?
            :request/cancellable?
            :request/markable-done?
            :request/claimable?
            :request/unclaimable?
            :request/markable-on-the-way?
            :request/progress-stage
            :request/progress-index]}
  [_ctx {:request/keys [doc]}]
  (without-nils
   {:request/active? (active? doc)
    :request/terminal? (terminal? doc)
    :request/editable? (editable? doc)
    :request/cancellable? (cancellable? doc)
    :request/markable-done? (markable-done? doc)
    :request/claimable? (claimable? doc)
    :request/unclaimable? (unclaimable? doc)
    :request/markable-on-the-way? (markable-on-the-way? doc)
    :request/progress-stage (progress-stage doc)
    :request/progress-index (progress-index doc)}))

(graph/defresolver request-owned-by-current-actor
  {:input [:request/doc
           [:? :current-user/id]
           [:? :request/capability-id]]
   :output [:request/owned-by-current-actor?]}
  [_ctx {:request/keys [doc capability-id]
         :current-user/keys [id]}]
  {:request/owned-by-current-actor?
   (owned-by? doc
              {:user-id id
               :capability-id capability-id})})

(graph/defresolver request-customer-permissions
  {:input [:request/doc
           :request/owned-by-current-actor?]
   :output [:request/can-edit?
            :request/can-cancel?
            :request/can-mark-done?]}
  [_ctx {:request/keys [doc owned-by-current-actor?]}]
  {:request/can-edit?
   (and owned-by-current-actor?
        (editable? doc))

   :request/can-cancel?
   (and owned-by-current-actor?
        (cancellable? doc))

   :request/can-mark-done?
   (and owned-by-current-actor?
        (markable-done? doc))})

(def resolvers
  [request-by-id
   request-fields
   request-lifecycle-facts
   request-owned-by-current-actor
   request-customer-permissions])

;; =============================================================================
;; FX effect handlers
;; =============================================================================

(def submit-create-effect
  ::submit-create)

(def submit-transition-effect
  ::submit-transition)

(def publish-effect
  ::publish)

(defn- handle-submit-create
  [ctx command]
  ((required-service ctx submit-create-service) ctx command))

(defn- handle-submit-transition
  [ctx command]
  ((required-service ctx submit-transition-service) ctx command))

(defn- handle-publish
  [ctx change]
  ((required-service ctx publish-service) ctx change))

(def fx-handlers
  {submit-create-effect handle-submit-create
   submit-transition-effect handle-submit-transition
   publish-effect handle-publish})

;; =============================================================================
;; Shared FX state functions
;; =============================================================================

(def owner-command-query
  [:request/found?
   [:? :request/doc]
   [:? :request/owned-by-current-actor?]
   [:? :request/editable?]
   [:? :request/cancellable?]
   [:? :request/markable-done?]])

(defn- actor-input
  [ctx]
  (cond->
   {:request/id (:request/id ctx)}

    (uuid? (:current-user/id ctx))
    (assoc :current-user/id (:current-user/id ctx))

    (uuid? (:request/capability-id ctx))
    (assoc :request/capability-id (:request/capability-id ctx))))

(defn- prepare-transition
  [before action result]
  (if-not (:ok? result)
    {:biff.fx/return result}
    (let [after (:request result)]
      {:request/before before
       :request/after after
       :request/command (transition-command before after action)
       :request/change (request-change after action)
       :biff.fx/next :write})))

(defn- write-create-state
  [{:request/keys [command]}]
  {:request/write-result
   [submit-create-effect command]
   :biff.fx/next :publish})

(defn- write-transition-state
  [{:request/keys [command]}]
  {:request/write-result
   [submit-transition-effect command]
   :biff.fx/next :publish})

(defn- publish-state
  [{:request/keys [change]}]
  {:request/publish-result
   [publish-effect change]
   :biff.fx/next :finish})

(defn- finish-create-state
  [{:request/keys [doc write-result publish-result]}]
  {:biff.fx/return
   {:ok? true
    :request doc
    :write-result write-result
    :publish-result publish-result}})

(defn- finish-transition-state
  [{:request/keys [after write-result publish-result]}]
  {:biff.fx/return
   {:ok? true
    :request after
    :write-result write-result
    :publish-result publish-result}})

;; =============================================================================
;; FX machine: create request
;; =============================================================================

(fx/defmachine create-request
  :start
  (fn [ctx]
    (let [now (:biff.fx/now ctx)
          seed (:biff.fx/seed ctx)
          [request-id next-seed] (fx/uuid7 seed now)
          input (-> (:request/create-input ctx)
                    (assoc :id request-id
                           :now now)
                    normalize-create-input)]
      {:request/id request-id
       :request/next-seed next-seed
       :request/input input
       :biff.fx/next :validate}))

  :validate
  (fn [{:request/keys [input]}]
    (let [errors (create-input-errors input)]
      (if (seq errors)
        {:biff.fx/return
         {:ok? false
          :error :request/invalid-input
          :errors errors}}
        {:biff.fx/next :build})))

  :build
  (fn [{:request/keys [input]}]
    (let [request (new-request input)]
      {:request/doc request
       :request/command (create-command request)
       :request/change (request-change request :create)
       :biff.fx/next :write}))

  :write
  write-create-state

  :publish
  publish-state

  :finish
  finish-create-state)

;; =============================================================================
;; FX machine: edit request once
;; =============================================================================

(fx/defmachine edit-request
  :start
  (fn [ctx]
    {:request/input
     (normalize-edit-input (:request/edit-input ctx))

     :request/facts
     [:biff.graph.fx/query
      (actor-input ctx)
      owner-command-query]

     :biff.fx/next
     :validate})

  :validate
  (fn [{:request/keys [facts input]
        :biff.fx/keys [now]}]
    (cond
      (not (:request/found? facts))
      {:biff.fx/return
       {:ok? false
        :error :request/not-found}}

      (not (:request/owned-by-current-actor? facts))
      {:biff.fx/return
       {:ok? false
        :error :request/not-owner}}

      (not (:request/editable? facts))
      {:biff.fx/return
       {:ok? false
        :error
        (if (:request/edited-at (:request/doc facts))
          :request/edit-used
          :request/not-editable)}}

      :else
      (prepare-transition
       (:request/doc facts)
       :edit
       (edit-request-doc (:request/doc facts)
                         input
                         now))))

  :write
  write-transition-state

  :publish
  publish-state

  :finish
  finish-transition-state)

;; =============================================================================
;; FX machines: customer lifecycle actions
;; =============================================================================

(defn- owner-transition-machine
  [machine-name action permission-key error]
  (fx/machine
   machine-name

   :start
   (fn [ctx]
     {:request/facts
      [:biff.graph.fx/query
       (actor-input ctx)
       owner-command-query]

      :biff.fx/next
      :validate})

   :validate
   (fn [{:request/keys [facts]
         :biff.fx/keys [now]}]
     (cond
       (not (:request/found? facts))
       {:biff.fx/return
        {:ok? false
         :error :request/not-found}}

       (not (:request/owned-by-current-actor? facts))
       {:biff.fx/return
        {:ok? false
         :error :request/not-owner}}

       (not (get facts permission-key))
       {:biff.fx/return
        {:ok? false
         :error error}}

       :else
       (prepare-transition
        (:request/doc facts)
        action
        (transition-request (:request/doc facts)
                            action
                            {:now now}))))

   :write
   write-transition-state

   :publish
   publish-state

   :finish
   finish-transition-state))

(def cancel-request
  (owner-transition-machine
   ::cancel-request
   :cancel
   :request/cancellable?
   :request/not-cancellable))

(def mark-request-done
  (owner-transition-machine
   ::mark-request-done
   :done
   :request/markable-done?
   :request/not-markable-done))

;; =============================================================================
;; Module contribution
;; =============================================================================

(def module
  {:biff.graph/resolvers resolvers
   :biff.fx/handlers fx-handlers})
