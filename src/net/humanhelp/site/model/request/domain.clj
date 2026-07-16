(ns net.humanhelp.site.model.request.domain
  (:require
   [clojure.string :as str])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Identity and limits
;; =============================================================================

(def entity-type :request)

(def store-area-text-max 120)
(def title-max 60)
(def details-max 500)

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def status-order
  [:open
   :claimed
   :on-the-way
   :done
   :cancelled])

(def statuses
  (set status-order))

(def active-statuses
  #{:open
    :claimed
    :on-the-way})

(def terminal-statuses
  #{:done
    :cancelled})

(def progress-stages
  [:created
   :claimed
   :on-the-way
   :done])

(def status->progress-stage
  {:open :created
   :claimed :claimed
   :on-the-way :on-the-way
   :done :done
   :cancelled :cancelled})

(def status->progress-index
  {:open 0
   :claimed 1
   :on-the-way 2
   :done 3
   :cancelled nil})

(def allowed-transitions
  {[:open :claim] :claimed
   [:claimed :unclaim] :open
   [:claimed :mark-on-the-way] :on-the-way

   [:open :cancel] :cancelled
   [:claimed :cancel] :cancelled
   [:on-the-way :cancel] :cancelled

   [:open :done] :done
   [:claimed :done] :done
   [:on-the-way :done] :done})

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

   :request/not-assignee
   "Only the employee assigned to this request can perform that action."

   :request/not-authorized
   "You do not have permission to perform this action."

   :request/invalid-input
   "Some request information needs to be corrected."

   :request/invalid-time
   "The request could not be changed because its timestamp was invalid."

   :request/stale
   "The request changed before your action completed. Please try again."})

;; =============================================================================
;; General helpers
;; =============================================================================

(defn uuid-value?
  [value]
  (instance? UUID value))

(defn instant-value?
  [value]
  (instance? Instant value))

(defn error-message
  [error]
  (get action-error-messages
       error
       "The request could not be updated."))

(defn without-nils
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn exactly-one-present?
  [& values]
  (= 1
     (count
      (filter some? values))))

(defn- trim-to-nil
  [value]
  (if (string? value)
    (let [value (str/trim value)]
      (when-not (str/blank? value)
        value))
    value))

(defn- instant<=
  [a b]
  (and (instant-value? a)
       (instant-value? b)
       (not (.isAfter ^Instant a ^Instant b))))

(defn- optional-between?
  [start value end]
  (or (nil? value)
      (and (instant<= start value)
           (instant<= value end))))

(defn valid-change-time?
  [request now]
  (and (instant-value? now)
       (instant<= (:request/created-at request) now)
       (instant<= (:request/updated-at request) now)))

;; =============================================================================
;; Domain predicates
;; =============================================================================

(defn status?
  [value]
  (contains? statuses value))

(defn active?
  [request]
  (contains? active-statuses
             (:request/status request)))

(defn terminal?
  [request]
  (contains? terminal-statuses
             (:request/status request)))

(defn open?
  [request]
  (= :open
     (:request/status request)))

(defn claimed?
  [request]
  (= :claimed
     (:request/status request)))

(defn on-the-way?
  [request]
  (= :on-the-way
     (:request/status request)))

(defn done?
  [request]
  (= :done
     (:request/status request)))

(defn cancelled?
  [request]
  (= :cancelled
     (:request/status request)))

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

(defn assigned-to?
  [request employee-id]
  (and (uuid-value? employee-id)
       (= employee-id
          (:request/claimed-by request))))

(defn progress-stage
  [request]
  (get status->progress-stage
       (:request/status request)))

(defn progress-index
  [request]
  (get status->progress-index
       (:request/status request)))

(defn next-status
  [request action]
  (get allowed-transitions
       [(:request/status request)
        action]))

(defn can-transition?
  [request action]
  (some? (next-status request action)))

(defn owned-by-user?
  [request user-id]
  (and (uuid-value? user-id)
       (= user-id
          (:request/user request))))

(defn owned-by-capability?
  [request capability-id]
  (and (uuid-value? capability-id)
       (= capability-id
          (:request/capability request))))

(defn owned-by?
  [request {:keys [user-id capability-id]}]
  (or (owned-by-user? request user-id)
      (owned-by-capability? request capability-id)))

;; =============================================================================
;; Document invariants
;; =============================================================================

(defn requestor-consistent?
  [request]
  (exactly-one-present?
   (:request/user request)
   (:request/capability request)))

(defn location-consistent?
  [request]
  (exactly-one-present?
   (:request/store-area request)
   (:request/store-area-text request)))

(defn- claim-history-consistent?
  [{:request/keys [claimed-by claimed-at on-the-way-at]}]
  (and (= (some? claimed-by)
          (some? claimed-at))
       (or (nil? on-the-way-at)
           (and (some? claimed-by)
                (some? claimed-at)))))

(defn- lifecycle-times-consistent?
  [{:request/keys
    [created-at
     updated-at
     claimed-at
     on-the-way-at
     edited-at
     completed-at
     cancelled-at]}]
  (and (instant<= created-at updated-at)
       (every? #(optional-between? created-at % updated-at)
               [claimed-at
                on-the-way-at
                edited-at
                completed-at
                cancelled-at])
       (or (nil? on-the-way-at)
           (instant<= claimed-at on-the-way-at))
       (or (nil? completed-at)
           (and (or (nil? claimed-at)
                    (instant<= claimed-at completed-at))
                (or (nil? on-the-way-at)
                    (instant<= on-the-way-at completed-at))))
       (or (nil? cancelled-at)
           (and (or (nil? claimed-at)
                    (instant<= claimed-at cancelled-at))
                (or (nil? on-the-way-at)
                    (instant<= on-the-way-at cancelled-at))))))

(defn lifecycle-consistent?
  [{:request/keys
    [status
     claimed-by
     claimed-at
     on-the-way-at
     completed-at
     cancelled-at]
    :as request}]
  (and (status? status)
       (claim-history-consistent? request)
       (lifecycle-times-consistent? request)
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

         false)))

;; =============================================================================
;; Input normalization
;; =============================================================================

(defn normalize-location-input
  [input]
  (update (or input {})
          :store-area-text
          trim-to-nil))

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

;; =============================================================================
;; Input validation
;; =============================================================================

(defn location-input-errors
  [{:keys [store-area-id store-area-text]}]
  (cond-> {}
    (not (exactly-one-present?
          store-area-id
          store-area-text))
    (assoc :location
           "Choose one store location or select 'I'll tell you...' and enter it.")

    (and (some? store-area-id)
         (not (uuid-value? store-area-id)))
    (assoc :store-area-id
           "Choose a valid store location.")

    (and (some? store-area-text)
         (not (string? store-area-text)))
    (assoc :store-area-text
           "Enter a valid store location.")

    (and (string? store-area-text)
         (> (count store-area-text)
            store-area-text-max))
    (assoc :store-area-text
           (str "Use "
                store-area-text-max
                " characters or fewer."))))

(defn request-text-errors
  [{:keys [title details]}]
  (cond-> {}
    (nil? title)
    (assoc :title
           "Briefly describe what you need.")

    (and (some? title)
         (not (string? title)))
    (assoc :title
           "Enter a valid request title.")

    (and (string? title)
         (> (count title) title-max))
    (assoc :title
           (str "Use "
                title-max
                " characters or fewer."))

    (and (some? details)
         (not (string? details)))
    (assoc :details
           "Enter valid additional information.")

    (and (string? details)
         (> (count details) details-max))
    (assoc :details
           (str "Use "
                details-max
                " characters or fewer."))))

(defn create-input-errors
  [input]
  (let [{:keys [id store-id user-id capability-id now]
         :as input}
        (normalize-create-input input)]
    (merge
     (location-input-errors input)
     (request-text-errors input)
     (cond-> {}
       (not (uuid-value? id))
       (assoc :id
              "A request UUID is required.")

       (not (uuid-value? store-id))
       (assoc :store-id
              "Choose a valid store.")

       (not (exactly-one-present?
             user-id
             capability-id))
       (assoc :requestor
              "Exactly one signed-in user or request capability is required.")

       (and (some? user-id)
            (not (uuid-value? user-id)))
       (assoc :user-id
              "The signed-in user is invalid.")

       (and (some? capability-id)
            (not (uuid-value? capability-id)))
       (assoc :capability-id
              "The request capability is invalid.")

       (not (instant-value? now))
       (assoc :now
              "A valid creation time is required.")))))

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
;; Request construction
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
  (let [{:keys [store-area-id store-area-text title details]
         :as normalized}
        (normalize-create-input input)
        errors
        (create-input-errors normalized)]
    (when (seq errors)
      (throw-invalid!
       "Cannot create request."
       errors
       input))

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
  (let [{:keys [store-area-id store-area-text title details]
         :as input}
        (normalize-edit-input input)
        errors
        (edit-input-errors input)]
    (cond
      (not (editable? request))
      {:ok? false
       :error (if (:request/edited-at request)
                :request/edit-used
                :request/not-editable)}

      (seq errors)
      {:ok? false
       :error :request/invalid-input
       :errors errors}

      (not (valid-change-time? request now))
      {:ok? false
       :error :request/invalid-time}

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

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn- transition-error
  [action]
  (case action
    :claim :request/not-claimable
    :unclaim :request/not-unclaimable
    :mark-on-the-way :request/not-markable-on-the-way
    :cancel :request/not-cancellable
    :done :request/not-markable-done
    :request/not-authorized))

(defn transition-request
  [request action {:keys [actor-id now]}]
  (let [status' (next-status request action)]
    (cond
      (nil? status')
      {:ok? false
       :error (transition-error action)}

      (not (valid-change-time? request now))
      {:ok? false
       :error :request/invalid-time}

      (and (= action :claim)
           (not (uuid-value? actor-id)))
      {:ok? false
       :error :request/invalid-input
       :errors {:actor-id
                "A valid employee UUID is required."}}

      (and (contains? #{:unclaim :mark-on-the-way}
                      action)
           (not (assigned-to? request actor-id)))
      {:ok? false
       :error :request/not-assignee}

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
;; Pure command descriptions
;; =============================================================================

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
   :request/after request})

(defn update-command
  [before after operation]
  {:request/operation operation
   :request/id (:xt/id before)
   :request/expected (expected-version before)
   :request/before before
   :request/after after})

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
;; Public domain description
;; =============================================================================

(def model
  {:entity-type entity-type
   :limits {:store-area-text store-area-text-max
            :title title-max
            :details details-max}
   :statuses status-order
   :active-statuses active-statuses
   :terminal-statuses terminal-statuses
   :progress-stages progress-stages
   :allowed-transitions allowed-transitions})
