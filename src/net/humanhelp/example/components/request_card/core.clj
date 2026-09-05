(ns net.humanhelp.example.components.request-card.core
  "Request card for the HumanHelp example app, backed directly by production models.

   This component renders the real HumanHelp Request/User/RequestAssignment
   projections composed by net.humanhelp.example.board. It intentionally has no
   dependency on net.humanhelp.example.model and does not translate production
   lifecycle operations back into the former demo vocabulary.

   Optimistic operation identity and browser execution policy come from the
   production Request choreography capability carried by each board affordance.
   Per-render authority context comes from board/optimistic-binding, whose
   observed basis is derived only from Gesso Live's authoritative XTDB
   progression. Request's model revision remains only a fact version.

   Rendering an affordance is never authorization. The trusted example
   optimistic boundary resolves the same production operation entry and the
   public Request model rereads/revalidates current authority before committing."
  (:require
   [clojure.string :as str]
   [gesso.core :as g]
   [gesso.live.core :as live]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.components.request-card.attr :as attr]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.time Duration Instant]))

;; =============================================================================
;; Presentation vocabulary
;; =============================================================================

(defn status-label
  [request-document]
  (case (request/status request-document)
    :open "Open"
    :claimed "Claimed"
    :on-the-way "On the way"
    :done "Done"
    :cancelled "Cancelled"
    "Unknown"))

(defn status-pill-status
  [request-document]
  (case (request/status request-document)
    :open :waiting
    :claimed :active
    :on-the-way :active
    :done :success
    :cancelled :muted
    :destructive))

(defn request-status-pill
  [request-document]
  (g/status-pill
   {:status (status-pill-status request-document)
    :text   (status-label request-document)
    :dot?   true}))

(defn action-label
  [operation]
  (case operation
    :request/claim "Claim"
    :request/unclaim "Unclaim"
    :request/mark-on-the-way "On the way"
    :request/complete "Done"
    :request/cancel "Cancel"
    :request/reassign "Reassign"
    (-> operation name (str/replace "-" " ") str/capitalize)))

(defn- action-variant
  [operation]
  (case operation
    :request/claim :primary
    :request/mark-on-the-way :primary
    :request/complete :primary
    :request/unclaim :outline
    :request/cancel :outline
    :request/reassign :outline
    :default))

(defn- action-button-class
  [operation]
  (case (action-variant operation)
    :primary "btn-sm-primary"
    :outline "btn-sm-outline"
    "btn-sm"))

(defn- action-button-attrs
  [operation]
  {:class                            (action-button-class operation)
   :data-humanhelp-request-operation (name operation)})

;; =============================================================================
;; Stable DOM identity and authoritative optimistic binding
;; =============================================================================

(defn request-target-id
  [row]
  (str "humanhelp-request-" (board/row-request-id row)))

(defn- capability-binding
  "Return the closed per-render binding accepted by Gesso.

   board/optimistic-binding currently carries the production capability as a
   redundant convenience field. The capability is already owned by the board
   affordance, and Gesso deliberately rejects capability-owned fields inside the
   closed binding map. Keep that separation explicit here; a following cleanup
   revision will remove the redundant field from board/optimistic-binding."
  [binding]
  (some-> binding (dissoc :capability)))

(defn action-button
  [ctx row {:keys [operation capability arguments]} board-state-selector]
  (let [target-id (request-target-id row)
        binding   (board/optimistic-binding
                   ctx
                   row
                   operation
                   arguments
                   target-id)
        binding'  (capability-binding binding)]
    (live/post-button
     ctx
     (cond->
      {:to      (routes/operation-url (board/row-request-id row) operation)
       :swap    "none"
       :include board-state-selector
       :form-attrs
       {:class                              "inline-flex"
        :data-humanhelp-request-action-form true}
       :button-attrs
       (action-button-attrs operation)
       :children
       [[:span {:data-gesso-button-label true}
         (action-label operation)]]}
       binding'
       (assoc :optimistic capability
              :optimistic-binding binding')))))

;; =============================================================================
;; Request/User display composition
;; =============================================================================

(defn- display-user-label
  [user-document]
  (when user-document
    (or
     (some-> (user/user-display-name user-document) str/trim not-empty)
     (some-> (user/user-email user-document) str/trim not-empty)
     (some-> (user/user-phone user-document) str/trim not-empty))))

(defn- requestor-label
  [row]
  (or
   (display-user-label (:requestor-user row))
   (when
    (= :capability
       (request/requestor-type (board/row-request row)))
     "Capability request")
   "Requestor"))

(defn- helper-label
  [row viewer-id]
  (when-let [assignment (board/row-primary-assignment row)]
    (if
     (= viewer-id (request/assignment-helper-id assignment))
      "you"
      (or
       (display-user-label (:primary-helper-user row))
       "another helper"))))

(defn- elapsed-label
  [^Instant created-at]
  (when created-at
    (let [seconds (max 0 (.getSeconds (Duration/between created-at (Instant/now))))]
      (cond
        (< seconds 60)
        "less than a minute"

        (< seconds 3600)
        (let [minutes (quot seconds 60)]
          (str minutes " minute" (when (not= 1 minutes) "s")))

        (< seconds 86400)
        (let [hours (quot seconds 3600)]
          (str hours " hour" (when (not= 1 hours) "s")))

        :else
        (let [days (quot seconds 86400)]
          (str days " day" (when (not= 1 days) "s")))))))

(defn request-meta
  [request-document]
  (let [{:keys [location-detail]} (request/content request-document)]
    [:div (attr/meta-attrs)
     (request-status-pill request-document)

     (when (seq location-detail)
       (g/muted-text
        {:as    :span
         :class "text-xs-theme"
         :text  location-detail}))

     (when (seq location-detail)
       (g/muted-text
        {:as    :span
         :class "text-xs-theme"
         :text  "·"}))

     (when-let [elapsed (elapsed-label (request/created-at request-document))]
       (g/muted-text
        {:as    :span
         :class "text-xs-theme"
         :text  (str "waiting " elapsed)}))]))

(defn request-card-actions
  [ctx row viewer-id board-state-selector]
  (let [affordances (board/operation-affordances row viewer-id)]
    (when (seq affordances)
      (into
       [:div (attr/actions-attrs)]
       (map
        #(action-button
          ctx
          row
          %
          board-state-selector))
       affordances))))

(defn request-summary
  [row viewer-id open?]
  (let [request-document (board/row-request row)
        {:keys [title]}  (request/content request-document)]
    [:summary (attr/summary-attrs)
     [:div (attr/header-stack-attrs)
      [:h3 (attr/title-attrs)
       title]

      (request-meta request-document)

      [:div (attr/customer-row-attrs)
       (g/text
        {:as      :span
         :variant :small
         :class   "weight-medium-theme"
         :text    (requestor-label row)})

       (when-let [claimed-by (helper-label row viewer-id)]
         (g/muted-text
          {:as    :span
           :class "text-xs-theme leading-body"
           :text  (str "claimed by " claimed-by)}))]]

     (g/icon "chevron-down"
             {:size  :sm
              :class "shrink-0 transition-transform duration-200 ease-in-out"
              :attrs (attr/chevron-attrs open?)})]))

(defn request-content
  [ctx row viewer-id board-state-selector]
  (let [{:keys [details]} (request/content (board/row-request row))]
    (g/accordion-content
     (attr/details-attrs)
     (when (seq details)
       (g/text
        {:as      :p
         :variant :small
         :class   "leading-body"
         :text    details}))
     (request-card-actions
      ctx
      row
      viewer-id
      board-state-selector))))

;; =============================================================================
;; Card shell
;; =============================================================================

(defn- item-attrs
  [row open?]
  (let [request-document (board/row-request row)]
    {:id                              (request-target-id row)
     :data-humanhelp-request-card     true
     :data-humanhelp-request-selected (when open? "true")
     :data-humanhelp-request-terminal (when (request/terminal? request-document) "true")
     :style                           (attr/card-style request-document)}))

(defn request-card
  "Render one production HumanHelp Request board row.

   Required input keys:

     :row
       Row produced by net.humanhelp.example.board.

     :viewer
       Authenticated production User document used only for presentation-level
       affordance filtering/labels. It never establishes server authority.

     :board-state-selector
       Stable hx-include selector for the example's presentation-state form.

   The component renders only authoritative production documents and inert
   production Choreo capabilities. When ctx has no justified XTDB progression,
   the same action remains an ordinary HTMX POST and optimism is omitted rather
   than fabricating an observed basis."
  [ctx {:keys [row viewer board-state-selector open?]
        :or   {open? false}}]
  (let [request-document (board/row-request row)
        viewer-id        (user/user-id viewer)]
    (when-not (request/request-document? request-document)
      (throw
       (ex-info
        "HumanHelp example request card requires a production Request document."
        {:request request-document})))
    (when-not (uuid? viewer-id)
      (throw
       (ex-info
        "HumanHelp example request card requires a production User viewer."
        {:viewer    viewer
         :viewer-id viewer-id})))
    (g/accordion-item
     {:value (board/row-request-id row)
      :open? open?
      :class (attr/item-class request-document open?)
      :attrs (item-attrs row open?)}
     (request-summary row viewer-id open?)
     (request-content
      ctx
      row
      viewer-id
      board-state-selector))))
