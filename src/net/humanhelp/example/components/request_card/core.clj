(ns net.humanhelp.example.components.request-card.core
  (:require
   [gesso.core :as g]
   [gesso.live.core :as live]
   [net.humanhelp.example.components.request-card.attr :as attr]
   [net.humanhelp.example.model :as model]
   [net.humanhelp.example.routes :as routes]))

;; -----------------------------------------------------------------------------
;; Status Pill Rendering
;; -----------------------------------------------------------------------------

(defn status-label
  [request]
  (model/request-status-label request))

(defn status-pill-status
  [request]
  (case (:request/status request)
    :open :waiting
    :claimed :active
    :done :success
    :cancelled :muted
    :destructive))

(defn request-status-pill
  [request]
  (g/status-pill
   {:status (status-pill-status request)
    :text   (status-label request)
    :dot?   true}))

;; -----------------------------------------------------------------------------
;; Hidden Parameter Helpers
;; -----------------------------------------------------------------------------

(defn hidden-input
  [name value]
  (when (some? value)
    [:input {:type  "hidden"
             :name  name
             :value value}]))

(defn- true-input
  [name value]
  (when (true? value)
    (hidden-input name "true")))

(defn- created-order-input
  [created-order]
  (when (and (some? created-order)
             (not= :newest created-order))
    (hidden-input routes/created-order-param (name created-order))))

(defn view-state-hidden-inputs
  "Render board view-state hidden inputs.

   The new Live post-button path normally carries board state by including the
   stable page-level board-state form. This helper remains useful to the
   removable example's other form-shaped UI and can disappear when the example
   is rewritten."
  [{:keys [search
           visible-revision
           created-order
           mine-first?
           unclaimed-first?
           show-terminal?]}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/search-param search)
   (hidden-input routes/visible-revision-param visible-revision)
   (created-order-input created-order)
   (true-input routes/mine-first-param mine-first?)
   (true-input routes/unclaimed-first-param unclaimed-first?)
   (true-input routes/show-terminal-param show-terminal?)])

;; -----------------------------------------------------------------------------
;; Optimistic Action Binding
;; -----------------------------------------------------------------------------

(defn- action-transition
  [action]
  (keyword "request" (name action)))

(defn- request-scope
  [request]
  [:request (:request/id request)])

(defn- request-revision
  [request]
  (or (:request/updated-revision request)
      (:request/created-revision request)))

(defn- request-target-id
  [request]
  (str "humanhelp-request-" (:request/id request)))

(def optimistic-capabilities
  "Protocol-v3 semantic capabilities for the removable example's lifecycle
   affordances.

   These are inert rendering configuration, never authorization. The trusted
   server registry still authenticates the user, resolves the operation, rereads
   current state, and decides whether the transition is allowed."
  (into
   {}
   (map
    (fn [action]
      (let [operation (action-transition action)]
        [action
         (live/optimistic-capability
          {:operation operation
           :plan-key operation})]))
    model/lifecycle-actions)))

(defn- optimistic-capability-for
  [action]
  (or (get optimistic-capabilities action)
      (throw
       (ex-info
        "Human Help request card has no optimistic capability for lifecycle action."
        {:action action
         :known-actions (set (keys optimistic-capabilities))}))))

(defn- optimistic-binding
  "Return protocol-v3 per-render binding data when this request has a known
   authoritative revision.

   A missing revision does not invent a basis. The action remains a normal HTMX
   POST in that case and simply skips optimism."
  [request]
  (when-let [revision (request-revision request)]
    {:arguments      {:request-id (:request/id request)}
     :observed-basis revision
     :scope          (request-scope request)
     :fact-versions  {:request/revision revision}
     :target-id      (request-target-id request)}))

;; -----------------------------------------------------------------------------
;; Action Composition
;; -----------------------------------------------------------------------------

(defn- action-variant
  [action]
  (case action
    :done :primary
    :claim :primary
    :take-over :primary
    :cancel :outline
    :unclaim :outline
    :default))

(defn- action-button-class
  [action]
  ;; Match gesso.components.button's :sm classes so the low-level Live request
  ;; owner remains visually identical to the ordinary Gesso button component.
  (case (action-variant action)
    :primary "btn-sm-primary"
    :outline "btn-sm-outline"
    "btn-sm"))

(defn- action-button-attrs
  [action]
  {:class                         (action-button-class action)
   :data-humanhelp-request-action (name action)})

(defn action-button
  [ctx request action board-state-selector]
  (let [text       (model/action-label action)
        capability (optimistic-capability-for action)
        binding    (optimistic-binding request)]
    (live/post-button
     ctx
     (cond->
      {:to      (routes/action-url (:request/id request) action)
       :swap    "none"
       :include board-state-selector
       :form-attrs
       {:class                              "inline-flex"
        :data-humanhelp-request-action-form true}

       ;; post-button owns the real clicked button and the HTMX request. Keep
       ;; styling/application identity on that actual protocol source.
       :button-attrs
       (action-button-attrs action)

       :children
       [[:span {:data-gesso-button-label true}
         text]]}
       binding
       (assoc :optimistic capability
              :optimistic-binding binding)))))

;; -----------------------------------------------------------------------------
;; Card Content
;; -----------------------------------------------------------------------------

(defn request-meta
  [request]
  [:div (attr/meta-attrs)
   (request-status-pill request)

   (g/muted-text
    {:as    :span
     :class "text-xs-theme"
     :text  (:request/area request)})

   (g/muted-text
    {:as    :span
     :class "text-xs-theme"
     :text  "·"})

   (g/muted-text
    {:as    :span
     :class "text-xs-theme"
     :text  (str "waiting " (model/waiting-label request))})])

(defn request-card-actions
  [ctx request user board-state-selector]
  (let [actions (model/available-actions request user)]
    (when (seq actions)
      (into
       [:div (attr/actions-attrs)]
       (map #(action-button
              ctx
              request
              %
              board-state-selector))
       actions))))

(defn- claimed-by-label
  [request user]
  (cond
    (= (:request/claimed-by request) (:user/id user))
    "you"

    (:request/claimed-by-email request)
    (:request/claimed-by-email request)

    :else
    nil))

(defn request-summary
  [request user open?]
  [:summary (attr/summary-attrs)
   [:div (attr/header-stack-attrs)
    [:h3 (attr/title-attrs)
     (:request/title request)]

    (request-meta request)

    [:div (attr/customer-row-attrs)
     (g/text
      {:as      :span
       :variant :small
       :class   "weight-medium-theme"
       :text    (:request/customer-name request)})

     (when-let [claimed-by (claimed-by-label request user)]
       (g/muted-text
        {:as    :span
         :class "text-xs-theme leading-body"
         :text  (str "claimed by " claimed-by)}))]]

   (g/icon "chevron-down"
           {:size  :sm
            :class "shrink-0 transition-transform duration-200 ease-in-out"
            :attrs (attr/chevron-attrs open?)})])

(defn request-content
  [ctx request user board-state-selector]
  (g/accordion-content
   (attr/details-attrs)
   (when (model/present? (:request/details request))
     (g/text
      {:as      :p
       :variant :small
       :class   "leading-body"
       :text    (:request/details request)}))
   (request-card-actions
    ctx
    request
    user
    board-state-selector)))

;; -----------------------------------------------------------------------------
;; Card Shell
;; -----------------------------------------------------------------------------

(defn- base-request-card
  [ctx {:keys [request
               user
               board-state-selector
               open?]}]
  (g/accordion-item
   {:value (:request/id request)
    :open? open?
    :class (attr/item-class request open?)
    :attrs (attr/item-attrs request open?)}
   (request-summary request user open?)
   (request-content
    ctx
    request
    user
    board-state-selector)))

;; -----------------------------------------------------------------------------
;; Public Card
;; -----------------------------------------------------------------------------

(defn request-card
  "Render one authoritative request card with protocol-v3 lifecycle actions.

   The server-rendered card never embeds provisional HTML. Each available action
   emits only inert semantic binding data on live/post-button: operation,
   arguments, observed authoritative revision, request scope/fact version, and
   the stable target id. The browser Choreo runtime owns command/execution
   identity, provisional derivation/rendering, timeout/replacement policy, and
   settlement realization.

   The request-list Live panel owns client continuity, so scroll/focus/details
   state survives authoritative refreshes without this component encoding that
   browser interaction state."
  [ctx {:keys [request
               user
               board-state-selector
               open?]
        :or   {open? false}}]
  (base-request-card
   ctx
   {:request              request
    :user                 user
    :board-state-selector board-state-selector
    :open?                open?}))
