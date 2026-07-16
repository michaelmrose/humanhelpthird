(ns net.humanhelp.site.model.request.domain.lifecycle
  "Pure lifecycle and helper-assignment rules for assistance requests.

   Request lifecycle is:

     :open
       -> :claimed
       -> :on-the-way
       -> :done

   An active request may also be cancelled. A claimed request may be returned
   to :open by its assigned helper.

   The Request document keeps the helper associated with a terminal request,
   but `actively-assigned?` is true only while the request is :claimed or
   :on-the-way. Unclaiming removes current assignment fields; prior assignment
   remains available through XTDB history and semantic model changes.

   This namespace does not determine whether a User is an eligible helper,
   whether a customer or manager may cancel, or whether a location is
   operational. FX must establish those facts before invoking these pure
   transitions."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.domain.entity :as entity]))

;; =============================================================================
;; Lifecycle vocabulary
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

(def assigned-statuses
  #{:claimed
    :on-the-way})

(def terminal-statuses
  #{:done
    :cancelled})

(def operation-order
  [:claim
   :unclaim
   :mark-on-the-way
   :complete
   :cancel])

(def operations
  (set operation-order))

(def transitions
  {[:open :claim] :claimed
   [:claimed :unclaim] :open
   [:claimed :mark-on-the-way] :on-the-way
   [:claimed :complete] :done
   [:on-the-way :complete] :done
   [:open :cancel] :cancelled
   [:claimed :cancel] :cancelled
   [:on-the-way :cancel] :cancelled})

(def lifecycle-fields
  #{:request/helper
    :request/claimed-at
    :request/on-the-way-at
    :request/completed-at
    :request/cancelled-at
    :request/cancellation-reason})

(defn status?
  [value]
  (contains? statuses value))

(defn operation?
  [value]
  (contains? operations value))

(defn status
  [request]
  (:request/status request))

(defn open?
  [request]
  (= :open (status request)))

(defn claimed?
  [request]
  (= :claimed (status request)))

(defn on-the-way?
  [request]
  (= :on-the-way (status request)))

(defn done?
  [request]
  (= :done (status request)))

(defn cancelled?
  [request]
  (= :cancelled (status request)))

(defn active?
  [request]
  (contains? active-statuses (status request)))

(defn terminal?
  [request]
  (contains? terminal-statuses (status request)))

(defn next-status
  [request operation]
  (get transitions [(status request) operation]))

(defn transition-allowed?
  [request operation]
  (some? (next-status request operation)))

(defn claimable?
  [request]
  (transition-allowed? request :claim))

(defn unclaimable?
  [request]
  (transition-allowed? request :unclaim))

(defn markable-on-the-way?
  [request]
  (transition-allowed? request :mark-on-the-way))

(defn completable?
  [request]
  (transition-allowed? request :complete))

(defn cancellable?
  [request]
  (transition-allowed? request :cancel))

;; =============================================================================
;; Helper assignment
;; =============================================================================

(defn helper-id
  "Returns the helper associated with the current or terminal request."
  [request]
  (:request/helper request))

(defn has-helper?
  "Returns true when a helper is recorded, including on terminal requests."
  [request]
  (uuid? (helper-id request)))

(defn actively-assigned?
  "Returns true only while a helper is actively responsible for the request."
  [request]
  (and
   (contains? assigned-statuses (status request))
   (has-helper? request)))

(defn assigned-to?
  [request user-id]
  (and
   (uuid? user-id)
   (actively-assigned? request)
   (= user-id (helper-id request))))

;; =============================================================================
;; Lifecycle consistency
;; =============================================================================

(defn- optional-timestamp?
  [value]
  (or
   (nil? value)
   (model.common/timestamp-value? value)))

(defn- optional-reason?
  [value]
  (or
   (nil? value)
   (qualified-keyword? value)))

(defn- absent?
  [request keys]
  (every? #(nil? (get request %)) keys))

(defn- within-request-time?
  [request value]
  (model.common/optional-between?
   (:request/created-at request)
   value
   (:request/updated-at request)))

(defn- ordered?
  [earlier later]
  (or
   (nil? earlier)
   (nil? later)
   (not (.isAfter
         ^java.time.Instant earlier
         ^java.time.Instant later))))

(defn- lifecycle-values-consistent?
  [request]
  (let [{:request/keys
         [helper
          claimed-at
          on-the-way-at
          completed-at
          cancelled-at
          cancellation-reason]}
        request]
    (and
     (or (nil? helper) (uuid? helper))
     (optional-timestamp? claimed-at)
     (optional-timestamp? on-the-way-at)
     (optional-timestamp? completed-at)
     (optional-timestamp? cancelled-at)
     (optional-reason? cancellation-reason)

     (every?
      #(within-request-time? request %)
      [claimed-at on-the-way-at completed-at cancelled-at])

     (ordered? claimed-at on-the-way-at)
     (ordered? claimed-at completed-at)
     (ordered? claimed-at cancelled-at)
     (ordered? on-the-way-at completed-at)
     (ordered? on-the-way-at cancelled-at))))

(defn- open-consistent?
  [request]
  (absent?
   request
   [:request/helper
    :request/claimed-at
    :request/on-the-way-at
    :request/completed-at
    :request/cancelled-at
    :request/cancellation-reason]))

(defn- claimed-consistent?
  [request]
  (and
   (uuid? (:request/helper request))
   (some? (:request/claimed-at request))
   (absent?
    request
    [:request/on-the-way-at
     :request/completed-at
     :request/cancelled-at
     :request/cancellation-reason])))

(defn- on-the-way-consistent?
  [request]
  (and
   (uuid? (:request/helper request))
   (some? (:request/claimed-at request))
   (some? (:request/on-the-way-at request))
   (absent?
    request
    [:request/completed-at
     :request/cancelled-at
     :request/cancellation-reason])))

(defn- done-consistent?
  [request]
  (and
   (uuid? (:request/helper request))
   (some? (:request/claimed-at request))
   (some? (:request/completed-at request))
   (absent?
    request
    [:request/cancelled-at
     :request/cancellation-reason])))

(defn- cancelled-consistent?
  [request]
  (and
   (some? (:request/cancelled-at request))
   (nil? (:request/completed-at request))
   (or
    (and
     (nil? (:request/helper request))
     (nil? (:request/claimed-at request))
     (nil? (:request/on-the-way-at request)))
    (and
     (uuid? (:request/helper request))
     (some? (:request/claimed-at request))))))

(defn lifecycle-consistent?
  "Returns true when status, assignment, timestamps, and terminal fields form
   one valid lifecycle state.

   Call domain.core/request-consistent? when structural Request validation is
   also required."
  [request]
  (and
   (entity/structurally-consistent? request)
   (status? (status request))
   (lifecycle-values-consistent? request)
   (case (status request)
     :open
     (open-consistent? request)

     :claimed
     (claimed-consistent? request)

     :on-the-way
     (on-the-way-consistent? request)

     :done
     (done-consistent? request)

     :cancelled
     (cancelled-consistent? request)

     false)))

;; =============================================================================
;; Failure helpers
;; =============================================================================

(defn- request-context
  [request]
  {:request/id
   (:xt/id request)

   :request/organization
   (:request/organization request)

   :request/location
   (:request/location request)

   :request/status
   (:request/status request)

   :request/helper
   (:request/helper request)

   :request/revision
   (:request/revision request)})

(defn- fail!
  [error-type message errors request]
  (model.common/throw-invalid!
   error-type
   message
   errors
   (request-context request)))

(defn require-lifecycle-consistent
  [request]
  (when-not
   (lifecycle-consistent? request)
    (fail!
     :request/invalid-lifecycle
     "The Request lifecycle is invalid."
     {:request
      "The status, helper assignment, or lifecycle timestamps are inconsistent."}
     request))

  request)

(defn- require-transition!
  [request operation]
  (when-not
   (transition-allowed? request operation)
    (fail!
     :request/invalid-transition
     "The Request lifecycle transition is invalid."
     {:operation
      (str
       "Operation "
       operation
       " is not allowed from status "
       (status request)
       ".")}
     request)))

(defn- require-helper-id!
  [request user-id]
  (when-not
   (uuid? user-id)
    (fail!
     :request/invalid-helper
     "The Request helper is invalid."
     {:helper-id
      "A helper User UUID is required."}
     request))

  user-id)

(defn- require-assigned-helper!
  [request user-id]
  (require-helper-id! request user-id)

  (when-not
   (assigned-to? request user-id)
    (fail!
     :request/not-assigned-helper
     "The Request lifecycle transition is invalid."
     {:helper-id
      "Only the currently assigned helper may perform this transition."}
     request))

  user-id)

(defn- require-reason!
  [request reason]
  (when-not
   (optional-reason? reason)
    (fail!
     :request/invalid-cancellation-reason
     "The Request cancellation reason is invalid."
     {:reason
      "A cancellation reason must be a qualified keyword when supplied."}
     request))

  reason)

(defn- revise-lifecycle
  [request now mutation-fn]
  (require-lifecycle-consistent request)

  (-> (entity/revise request now mutation-fn)
      require-lifecycle-consistent))

;; =============================================================================
;; Transitions
;; =============================================================================

(defn claim-request
  [request
   {:keys [helper-id now]}]
  (require-lifecycle-consistent request)
  (require-transition! request :claim)
  (require-helper-id! request helper-id)

  (revise-lifecycle
   request
   now
   #(assoc %
           :request/status :claimed
           :request/helper helper-id
           :request/claimed-at now)))

(defn unclaim-request
  [request
   {:keys [helper-id now]}]
  (require-lifecycle-consistent request)
  (require-transition! request :unclaim)
  (require-assigned-helper! request helper-id)

  (revise-lifecycle
   request
   now
   #(-> %
        (assoc :request/status :open)
        (dissoc
         :request/helper
         :request/claimed-at
         :request/on-the-way-at))))

(defn mark-request-on-the-way
  [request
   {:keys [helper-id now]}]
  (require-lifecycle-consistent request)
  (require-transition! request :mark-on-the-way)
  (require-assigned-helper! request helper-id)

  (revise-lifecycle
   request
   now
   #(assoc %
           :request/status :on-the-way
           :request/on-the-way-at now)))

(defn complete-request
  [request
   {:keys [helper-id now]}]
  (require-lifecycle-consistent request)
  (require-transition! request :complete)
  (require-assigned-helper! request helper-id)

  (revise-lifecycle
   request
   now
   #(assoc %
           :request/status :done
           :request/completed-at now)))

(defn cancel-request
  [request
   {:keys [now reason]}]
  (require-lifecycle-consistent request)
  (require-transition! request :cancel)
  (require-reason! request reason)

  (revise-lifecycle
   request
   now
   #(cond->
     (assoc %
            :request/status :cancelled
            :request/cancelled-at now)

     reason
     (assoc :request/cancellation-reason reason))))
