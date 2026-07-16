(ns net.humanhelp.site.model.request.domain.core
  "Public facade for the Request domain.

   Request schema, Graph, FX, and the outer Request model facade should depend
   on this namespace rather than on the internal domain namespaces.

   This facade exposes:

   - Requestor value construction and inspection;
   - customer-editable content predicates and limits;
   - Request identity and location facts;
   - lifecycle facts and transitions;
   - complete Request consistency;
   - Request construction and content editing;
   - shared model commands.

   Authorization remains outside the pure domain. FX must establish valid
   Organization location context, User access, authenticated capability
   control, and action authority before invoking these operations."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.domain.content :as request-content]
   [net.humanhelp.site.model.request.domain.entity :as entity]
   [net.humanhelp.site.model.request.domain.lifecycle :as lifecycle]
   [net.humanhelp.site.model.request.domain.requestor :as requestor]))

;; =============================================================================
;; Model identity and versioning
;; =============================================================================

(def entity-type
  entity/entity-type)

(def version
  entity/version)

(def operation-order
  (into
   [:create
    :edit]
   lifecycle/operation-order))

(def operations
  (set operation-order))

(defn operation?
  [value]
  (contains?
   operations
   value))

;; =============================================================================
;; Requestor values
;; =============================================================================

(def requestor-types
  requestor/requestor-types)

(def requestor-type?
  requestor/requestor-type?)

(def requestor-reference?
  requestor/requestor-reference?)

(def user-requestor
  requestor/user-requestor)

(def capability-requestor
  requestor/capability-requestor)

(def user-requestor?
  requestor/user-requestor?)

(def capability-requestor?
  requestor/capability-requestor?)

(def requestor
  requestor/requestor)

(def requestor-type
  requestor/requestor-type)

(def requestor-id
  requestor/requestor-id)

(def requested-by-user?
  requestor/requested-by-user?)

(def requested-by-capability?
  requestor/requested-by-capability?)

(def requested-by?
  requestor/requested-by?)

(def controlled-by?
  requestor/controlled-by?)

;; =============================================================================
;; Request content
;; =============================================================================

(def title-max
  request-content/title-max)

(def details-max
  request-content/details-max)

(def location-detail-max
  request-content/location-detail-max)

(def title?
  request-content/title?)

(def details?
  request-content/details?)

(def location-detail?
  request-content/location-detail?)

(def content?
  request-content/content?)

(def content
  request-content/content)

(def normalize-content
  request-content/normalize-content)

(def content-errors
  request-content/content-errors)

(def valid-content?
  request-content/valid-content?)

(def same-content?
  request-content/same-content?)

;; =============================================================================
;; Request identity and location
;; =============================================================================

(def request-id
  entity/request-id)

(def organization-id
  entity/organization-id)

(def location-id
  entity/location-id)

(def revision
  entity/revision)

(def created-at
  entity/created-at)

(def updated-at
  entity/updated-at)

(def belongs-to-organization?
  entity/belongs-to-organization?)

(def at-location?
  entity/at-location?)

(def belongs-to-location?
  entity/belongs-to-location?)

;; =============================================================================
;; Lifecycle vocabulary and facts
;; =============================================================================

(def status-order
  lifecycle/status-order)

(def statuses
  lifecycle/statuses)

(def active-statuses
  lifecycle/active-statuses)

(def assigned-statuses
  lifecycle/assigned-statuses)

(def terminal-statuses
  lifecycle/terminal-statuses)

(def transitions
  lifecycle/transitions)

(def status?
  lifecycle/status?)

(def status
  lifecycle/status)

(def open?
  lifecycle/open?)

(def claimed?
  lifecycle/claimed?)

(def on-the-way?
  lifecycle/on-the-way?)

(def done?
  lifecycle/done?)

(def cancelled?
  lifecycle/cancelled?)

(def active?
  lifecycle/active?)

(def terminal?
  lifecycle/terminal?)

(def next-status
  lifecycle/next-status)

(def transition-allowed?
  lifecycle/transition-allowed?)

(def claimable?
  lifecycle/claimable?)

(def unclaimable?
  lifecycle/unclaimable?)

(def markable-on-the-way?
  lifecycle/markable-on-the-way?)

(def completable?
  lifecycle/completable?)

(def cancellable?
  lifecycle/cancellable?)

(def helper-id
  lifecycle/helper-id)

(def has-helper?
  lifecycle/has-helper?)

(def actively-assigned?
  lifecycle/actively-assigned?)

(def assigned-to?
  lifecycle/assigned-to?)

;; =============================================================================
;; Complete Request consistency
;; =============================================================================

(defn request-consistent?
  "Returns true when Request structure and lifecycle are both valid."
  [request]
  (and
   (entity/structurally-consistent?
    request)
   (lifecycle/lifecycle-consistent?
    request)))

(defn- request-context
  [request]
  {:request/id
   (:xt/id request)

   :request/organization
   (:request/organization request)

   :request/location
   (:request/location request)

   :request/requestor
   (requestor/requestor request)

   :request/status
   (:request/status request)

   :request/helper
   (:request/helper request)

   :request/revision
   (:request/revision request)})

(defn require-request-consistent
  [request]
  (when-not
   (request-consistent?
    request)
    (model.common/throw-invalid!
     :request/invalid-request
     "The Request is invalid."
     {:request
      "The Request structure or lifecycle is inconsistent."}
     (request-context request)))

  request)

;; =============================================================================
;; Construction
;; =============================================================================

(def normalize-create-input
  entity/normalize-create-input)

(def create-input-errors
  entity/create-input-errors)

(def valid-create-input?
  entity/valid-create-input?)

(defn new-request
  "Constructs a new open Request from canonical nested domain values.

   Expected shape:

     {:id              uuid
      :organization-id uuid
      :location-id     uuid
      :requestor       {:requestor/type :user|:capability
                        :requestor/id   uuid}
      :content         {:title ...
                        :details ...
                        :location-detail ...}
      :now             instant}"
  [input]
  (-> (entity/new-request input)
      require-request-consistent))

;; =============================================================================
;; Content editing
;; =============================================================================

(defn editable?
  "Returns true while customer-editable Request content can still change.

   Content may change throughout the active lifecycle so corrections and
   within-location directions can reach an already assigned helper through
   normal Request invalidation. Terminal Requests are immutable."
  [request]
  (lifecycle/active?
   request))

(defn normalize-edit-input
  [input]
  (let [input
        (or input {})]
    {:content
     (request-content/normalize-content
      (:content input))

     :now
     (:now input)}))

(defn edit-input-errors
  [input]
  (let [{:keys
         [content
          now]}
        (normalize-edit-input input)

        content-errors
        (request-content/content-errors
         content)]
    (cond-> {}
      (seq content-errors)
      (assoc
       :content
       content-errors)

      (not
       (model.common/timestamp-value?
        now))
      (assoc
       :now
       "A valid Request edit time is required."))))

(defn valid-edit-input?
  [input]
  (empty?
   (edit-input-errors input)))

(defn edit-request
  [request input]
  (require-request-consistent
   request)

  (when-not
   (editable?
    request)
    (model.common/throw-invalid!
     :request/not-editable
     "The Request cannot be edited."
     {:status
      "Only an active Request can be edited."}
     (request-context request)))

  (let [{:keys
         [content
          now]
         :as normalized}
        (normalize-edit-input input)

        errors
        (edit-input-errors
         normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :request/invalid-edit-input
       "The Request edit is invalid."
       errors
       (request-context request)))

    (-> (entity/revise
         request
         now
         #(request-content/apply-content
           %
           content))
        require-request-consistent)))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(def claim-request
  lifecycle/claim-request)

(def unclaim-request
  lifecycle/unclaim-request)

(def mark-request-on-the-way
  lifecycle/mark-request-on-the-way)

(def complete-request
  lifecycle/complete-request)

(def cancel-request
  lifecycle/cancel-request)

;; =============================================================================
;; Shared model commands
;; =============================================================================

(defn create-command
  [input]
  (model.common/create-command
   entity-type
   (new-request input)
   version))

(defn- change-command
  [operation before after]
  (model.common/update-command
   entity-type
   operation
   before
   after
   version))

(defn edit-command
  [request input]
  (change-command
   :edit
   request
   (edit-request
    request
    input)))

(defn claim-command
  [request input]
  (change-command
   :claim
   request
   (claim-request
    request
    input)))

(defn unclaim-command
  [request input]
  (change-command
   :unclaim
   request
   (unclaim-request
    request
    input)))

(defn mark-on-the-way-command
  [request input]
  (change-command
   :mark-on-the-way
   request
   (mark-request-on-the-way
    request
    input)))

(defn complete-command
  [request input]
  (change-command
   :complete
   request
   (complete-request
    request
    input)))

(defn cancel-command
  [request input]
  (change-command
   :cancel
   request
   (cancel-request
    request
    input)))
