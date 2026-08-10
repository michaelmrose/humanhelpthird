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
  (or (:ui/pending-label request)
      (model/request-status-label request)))

(defn status-pill-status
  [request]
  (if (:ui/pending? request)
    :active
    (case (:request/status request)
      :open :waiting
      :claimed :active
      :done :success
      :cancelled :muted
      :destructive)))

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
;; Optimistic State Generation
;; -----------------------------------------------------------------------------

(defn- action-pending-label
  [action]
  (case action
    :claim "Claiming…"
    :take-over "Taking over…"
    :unclaim "Unclaiming…"
    :done "Marking done…"
    :cancel "Canceling…"
    "Updating…"))

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

(defn- pending-request
  [request action pending-label]
  (assoc request
         :ui/pending? true
         :ui/optimistic? true
         :ui/pending-action action
         :ui/pending-label pending-label
         :ui/disable-actions? true))

(defn- optimistic-request
  [request user action]
  (let [pending-label (action-pending-label action)
        user-id       (:user/id user)
        user-email    (:user/email user)]
    (case action
      :claim
      (assoc (pending-request request action pending-label)
             :request/status :claimed
             :request/claimed-by user-id
             :request/claimed-by-email user-email
             :ui/claimed-by-me? true)

      :take-over
      (assoc (pending-request request action pending-label)
             :request/status :claimed
             :request/claimed-by user-id
             :request/claimed-by-email user-email
             :ui/claimed-by-me? true)

      :unclaim
      (assoc (pending-request request action pending-label)
             :request/status :open
             :request/claimed-by nil
             :request/claimed-by-email nil
             :ui/claimed-by-me? false)

      :done
      (pending-request request action pending-label)

      :cancel
      (pending-request request action pending-label)

      (pending-request request action pending-label))))

;; base-request-card is also the projection renderer. Projected requests carry
;; :ui/disable-actions?, so their action buttons deliberately do not attach
;; another optimistic descriptor.
(declare base-request-card)

(defn- optimistic-config
  [ctx request user action view-state board-state-selector]
  (when-not (:ui/disable-actions? request)
    {:transition      (action-transition action)
     :scope           (request-scope request)
     :base-revision   (request-revision request)
     :target          "closest [data-humanhelp-request-card]"
     :pending-label   (action-pending-label action)
     :projection-mode :provisional
     :content
     (base-request-card
      ctx
      {:request              (optimistic-request request user action)
       :user                 user
       :view-state           view-state
       :board-state-selector board-state-selector
       :open?                true})}))

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

(defn- projected-action-button
  "Render an inert action control inside optimistic projection markup.

   Do not use live/post-button here. An optimistic template is itself rendered
   inside the authoritative action's lightweight wrapper form. Putting more Live
   wrapper forms inside that template creates nested-form HTML, which browsers
   are allowed to repair while parsing and can change the projection DOM shape
   before Gesso validates it."
  [request action]
  (g/button
   {:variant (action-variant action)
    :size    :sm
    :text    (model/action-label action)
    :attrs   {:type                            "button"
              :disabled                        true
              :aria-disabled                   "true"
              :data-humanhelp-request-action   (name action)
              :data-humanhelp-projected-action true}}))

(defn action-button
  [ctx request user action view-state board-state-selector]
  (if (:ui/disable-actions? request)
    (projected-action-button request action)
    (let [text (model/action-label action)]
      (live/post-button
       ctx
       {:to      (routes/action-url (:request/id request) action)
        :swap    "none"
        :include board-state-selector
        :form-attrs
        {:class                              "inline-flex"
         :data-humanhelp-request-action-form true}

        ;; post-button owns the actual clicked <button>. Keep component styling
        ;; and application identity on that real HTMX/protocol source.
        :button-attrs
        (action-button-attrs action)

        ;; The optimistic runtime can swap this label to :pending-label after the
        ;; click has been accepted, while the whole-card projection is installed.
        :children
        [[:span {:data-gesso-button-label true}
          text]]

        :optimistic
        (optimistic-config
         ctx
         request
         user
         action
         view-state
         board-state-selector)}))))

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

(defn- pending-note
  [request]
  (when (:ui/pending? request)
    (g/muted-text
     {:as    :span
      :class "text-xs-theme leading-body weight-medium-theme"
      :attrs {:data-humanhelp-request-pending-note true
              :aria-live                           "polite"
              :style                               {:position          "absolute"
                                                    :inset-block-start "var(--space-3)"
                                                    :inset-inline-end  "calc(var(--space-5) + var(--icon-size-lg))"
                                                    :z-index           2
                                                    :pointer-events    "none"
                                                    :padding           "0.125rem 0.4rem"
                                                    :border-radius     "var(--radius-sm)"
                                                    :background        "color-mix(in srgb, var(--card) 88%, transparent)"
                                                    :box-shadow        "var(--shadow-xs)"}}
      :text  "confirming…"})))

(defn request-card-actions
  [ctx request user view-state board-state-selector]
  (let [actions (model/available-actions request user)]
    (when (seq actions)
      (into
       [:div (attr/actions-attrs)]
       (map #(action-button
              ctx
              request
              user
              %
              view-state
              board-state-selector))
       actions))))

(defn- claimed-by-label
  [request user]
  (cond
    (:ui/claimed-by-me? request)
    "you"

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
  [ctx request user view-state board-state-selector]
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
    view-state
    board-state-selector)))

;; -----------------------------------------------------------------------------
;; Card Shell
;; -----------------------------------------------------------------------------

(defn- request-item-attrs
  [request open?]
  (merge
   (attr/item-attrs request open?)
   (cond-> {}
     (:ui/pending? request)
     (assoc :data-humanhelp-request-pending "true")

     (:ui/optimistic? request)
     (assoc :data-humanhelp-request-optimistic "true")

     (:ui/pending-action request)
     (assoc :data-humanhelp-request-pending-action
            (name (:ui/pending-action request))))))

(defn- base-request-card
  [ctx {:keys [request
               user
               view-state
               board-state-selector
               open?]}]
  (let [view-state (or view-state {})]
    (g/accordion-item
     {:value (:request/id request)
      :open? open?
      :class (attr/item-class request open?)
      :attrs (request-item-attrs request open?)}
     (request-summary request user open?)
     (pending-note request)
     (request-content
      ctx
      request
      user
      view-state
      board-state-selector))))

;; -----------------------------------------------------------------------------
;; Public Card
;; -----------------------------------------------------------------------------

(defn request-card
  "Render one request card using Gesso Live protocol-v2 optimistic actions.

   The authoritative card contains ordinary application state. Each available
   lifecycle action uses live/post-button, which owns:
   - the clicked HTMX request source
   - execution correlation
   - protocol attrs
   - the matched projection template

   The request-list Live panel owns client continuity, so scroll/focus/details
   state can survive both optimistic replacement and later authoritative/OOB
   replacement without this component encoding selection state."
  [ctx {:keys [request
               user
               view-state
               board-state-selector
               open?]
        :or   {open? false}}]
  (base-request-card
   ctx
   {:request              request
    :user                 user
    :view-state           view-state
    :board-state-selector board-state-selector
    :open?                open?}))
