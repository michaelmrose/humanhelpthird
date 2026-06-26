(ns net.humanhelp.humanhelp.components.request-card.core
  (:require
   [gesso.core :as g]
   [net.humanhelp.humanhelp.components.request-card.attr :as attr]
   [net.humanhelp.humanhelp.model :as model]
   [net.humanhelp.humanhelp.routes :as routes]))

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
    :text (status-label request)
    :dot? true}))

;; -----------------------------------------------------------------------------
;; Hidden Parameter Sync
;; -----------------------------------------------------------------------------

(defn hidden-input
  [name value]
  (when (some? value)
    [:input {:type "hidden"
             :name name
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
  "Render board view-state hidden inputs for request action forms.

   Action forms do not have their own visible search or board-option controls,
   so they preserve the current normalized board state.

   Open/selected request-card state is intentionally not preserved here.
   Preserving local accordion open state across fragment replacement belongs to
   Gesso Live continuity."
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

(defn- optimistic-template-id
  [request action]
  (str "humanhelp-request-"
       (:request/id request)
       "-"
       (name action)
       "-optimistic"))

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

;; -----------------------------------------------------------------------------
;; Action Form Composition
;; -----------------------------------------------------------------------------

(defn form-action
  [ctx {:keys [to
               text
               variant
               size
               view-state
               board-state-selector
               optimistic-template-id
               disabled?
               attrs]}]
  [:form
   (attr/action-form-attrs
    {:to to
     :board-state-selector board-state-selector
     :attrs (merge
             (when optimistic-template-id
               {:data-gesso-optimistic-template optimistic-template-id
                :data-gesso-optimistic-target "closest [data-humanhelp-request-card]"})
             attrs)})
   (g/anti-forgery-input ctx)
   (view-state-hidden-inputs view-state)
   (g/button
    {:variant (or variant :default)
     :size (or size :sm)
     :text text
     :attrs (cond-> {:type "submit"}
              disabled?
              (assoc :disabled true
                     :aria-disabled "true"))})])

(defn action-button
  [ctx request action view-state board-state-selector]
  (form-action
   ctx
   {:to (routes/action-url (:request/id request) action)
    :text (model/action-label action)
    :variant (case action
               :done :primary
               :claim :primary
               :take-over :primary
               :cancel :outline
               :unclaim :outline
               :default)
    :view-state view-state
    :board-state-selector board-state-selector
    :optimistic-template-id (optimistic-template-id request action)
    :disabled? (:ui/disable-actions? request)
    :attrs {:data-humanhelp-request-action (name action)
            :data-gesso-optimistic-label (action-pending-label action)}}))

;; -----------------------------------------------------------------------------
;; Card Content
;; -----------------------------------------------------------------------------

(defn request-meta
  [request]
  [:div (attr/meta-attrs)
   (request-status-pill request)

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text (:request/area request)})

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text "·"})

   (g/muted-text
    {:as :span
     :class "text-xs-theme"
     :text (str "waiting " (model/waiting-label request))})])

(defn- pending-note
  [request]
  (when (:ui/pending? request)
    (g/muted-text
     {:as :span
      :class "text-xs-theme leading-body weight-medium-theme"
      :attrs {:data-humanhelp-request-pending-note true
              :aria-live "polite"
              :style {:position "absolute"
                      :inset-block-start "var(--space-3)"
                      :inset-inline-end "calc(var(--space-5) + var(--icon-size-lg))"
                      :z-index 2
                      :pointer-events "none"
                      :padding "0.125rem 0.4rem"
                      :border-radius "var(--radius-sm)"
                      :background "color-mix(in srgb, var(--card) 88%, transparent)"
                      :box-shadow "var(--shadow-xs)"}}
      :text "confirming…"})))

(defn request-card-actions
  [ctx request user view-state board-state-selector]
  (let [actions (model/available-actions request user)]
    (when (seq actions)
      (into
       [:div (attr/actions-attrs)]
       (map #(action-button ctx request % view-state board-state-selector))
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
      {:as :span
       :variant :small
       :class "weight-medium-theme"
       :text (:request/customer-name request)})

     (when-let [claimed-by (claimed-by-label request user)]
       (g/muted-text
        {:as :span
         :class "text-xs-theme leading-body"
         :text (str "claimed by " claimed-by)}))]]

   (g/icon "chevron-down"
           {:size :sm
            :class "shrink-0 transition-transform duration-200 ease-in-out"
            :attrs (attr/chevron-attrs open?)})])

(defn request-content
  [ctx request user view-state board-state-selector]
  (g/accordion-content
   (attr/details-attrs)
   (when (model/present? (:request/details request))
     (g/text
      {:as :p
       :variant :small
       :class "leading-body"
       :text (:request/details request)}))
   (request-card-actions ctx request user view-state board-state-selector)))

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
  [ctx {:keys [request user view-state board-state-selector open?]}]
  (let [view-state (or view-state {})]
    (g/accordion-item
     {:value (:request/id request)
      :open? open?
      :class (attr/item-class request open?)
      :attrs (request-item-attrs request open?)}
     (request-summary request user open?)
     (pending-note request)
     (request-content ctx request user view-state board-state-selector))))

;; -----------------------------------------------------------------------------
;; Optimistic Templates
;; -----------------------------------------------------------------------------

(defn- optimistic-template
  [ctx {:keys [request user view-state board-state-selector action]}]
  (let [template-id (optimistic-template-id request action)]
    [:template {:data-gesso-optimistic-template template-id}
     (base-request-card
      ctx
      {:request (optimistic-request request user action)
       :user user
       :view-state view-state
       :board-state-selector board-state-selector
       :open? true})]))

(defn- optimistic-templates
  [ctx {:keys [request user view-state board-state-selector optimistic-templates?]}]
  (when-not (= false optimistic-templates?)
    (let [actions (model/available-actions request user)]
      (when (seq actions)
        (doall
         (for [action actions]
           (optimistic-template
            ctx
            {:request request
             :user user
             :view-state view-state
             :board-state-selector board-state-selector
             :action action})))))))

;; -----------------------------------------------------------------------------
;; Public Card
;; -----------------------------------------------------------------------------

(defn request-card
  "Render a model-backed request accordion row with embedded optimistic template hooks."
  [ctx {:keys [request
               user
               view-state
               board-state-selector
               open?
               optimistic-templates?]
        :or {open? false
             optimistic-templates? true}}]
  (let [card (base-request-card
              ctx
              {:request request
               :user user
               :view-state view-state
               :board-state-selector board-state-selector
               :open? open?})
        templates (optimistic-templates
                   ctx
                   {:request request
                    :user user
                    :view-state view-state
                    :board-state-selector board-state-selector
                    :optimistic-templates? optimistic-templates?})]
    (if (seq templates)
      (into card templates)
      card)))
