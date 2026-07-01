(ns net.humanhelp.example.views
  "Human Help analogue UI.

   This namespace owns Hiccup rendering only.

   It intentionally does not know about:
   - XTDB
   - Gesso Live model compilation
   - Ring route tables
   - lifecycle mutation

   Data comes in from app/live/model boundary namespaces."
  (:require
   [clojure.string :as str]
   [gesso.core :as g]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.components.refresh-button.core :refer [refresh-button]]
   [net.humanhelp.example.components.request-card.core :refer [request-card]]
   [net.humanhelp.example.model :as model]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.ui :as ui]))

;; -----------------------------------------------------------------------------
;; DOM ids shared with app/live
;; -----------------------------------------------------------------------------

(def request-toolbar-dom-id
  "humanhelp-request-toolbar-fragment")

(def request-list-dom-id
  "humanhelp-request-list-fragment")

(def create-request-dialog-id
  "humanhelp-create-request-dialog")

(def create-request-dialog-body-id
  "humanhelp-create-request-dialog-body")

(def board-options-dialog-id
  "humanhelp-board-options-dialog")

(def board-options-dialog-body-id
  "humanhelp-board-options-dialog-body")

(def search-input-dom-id
  "humanhelp-search")

(def board-state-form-id
  "humanhelp-board-state")

;; -----------------------------------------------------------------------------
;; Stable board-state selectors
;; -----------------------------------------------------------------------------

(defn board-state-selector
  []
  (str "#" board-state-form-id))

(defn board-state-input-selector
  [name]
  (str (board-state-selector)
       " input[name="
       name
       "]"))

(defn board-options-preserved-state-selector
  "Return the live DOM fields that board-options submits but does not edit.

   Do not include the whole board-state form here: it contains hidden board-option
   inputs, and the options form has visible controls for those same params. The
   options form should include only current search and visible revision from the
   stable board-state/search form."
  []
  (str "#" search-input-dom-id
       ", " (board-state-input-selector routes/visible-revision-param)))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn user-email
  "Return a real display email for a Human Help user map.

   Do not treat :user/id as an email. If this returns nil, the caller does not
   currently have a displayable email address."
  [user]
  (let [email (:user/email user)]
    (when (and (string? email)
               (str/includes? email "@"))
      email)))

(defn account-email
  "Backward-compatible name for create-request form defaults."
  [user]
  (user-email user))

(defn muted
  [text]
  (g/muted-text
   {:as :p
    :class "text-sm-theme leading-body"
    :text text}))

(defn hidden-input
  [name value]
  (when (some? value)
    [:input {:type "hidden"
             :name name
             :value value}]))

(defn boolean-param-value
  [x]
  (if x "true" "false"))

(defn created-order-param-value
  [view-state]
  (name (or (:created-order view-state)
            model/default-created-order
            :newest)))

(defn board-state-option-hidden-inputs
  "Render hidden inputs for board options when a form is only preserving state.

   These forms are not visibly editing board options, so checkbox state must be
   explicit. Otherwise unchecked checkboxes would be indistinguishable from
   missing state after a later request."
  [{:keys [mine-first? unclaimed-first? show-terminal?] :as view-state}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/created-order-param
                 (created-order-param-value view-state))
   (hidden-input routes/mine-first-param
                 (boolean-param-value mine-first?))
   (hidden-input routes/unclaimed-first-param
                 (boolean-param-value unclaimed-first?))
   (hidden-input routes/show-terminal-param
                 (boolean-param-value show-terminal?))])

(defn view-state-hidden-inputs
  "Render full view-state hidden inputs.

   Use this for forms that do not have their own visible search input.
   Do not use this inside search-control, because that form already has the
   visible search input named q."
  [{:keys [search visible-revision] :as view-state}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/search-param search)
   (hidden-input routes/visible-revision-param visible-revision)
   (board-state-option-hidden-inputs view-state)])

(defn board-state-hidden-inputs
  "Render board-state hidden inputs for the search/board-state form.

   Deliberately omits q/search because the visible search input is the source
   of truth for q. Rendering both a hidden q and visible q causes repeated
   params such as [\"\" \"test\"]."
  [{:keys [visible-revision] :as view-state}]
  [:div {:style {:display "contents"}}
   (hidden-input routes/visible-revision-param visible-revision)
   (board-state-option-hidden-inputs view-state)])

(defn oob-response
  [& nodes]
  (into [:div {:style {:display "contents"}}]
        (remove nil? nodes)))

;; -----------------------------------------------------------------------------
;; Page shell bits
;; -----------------------------------------------------------------------------

(defn hero
  []
  [:div {:class "title-stack-theme text-center"}
   (g/page-title
    {:text "Human Help Fast."
     :class "text-3xl-theme py-6"})])

;; -----------------------------------------------------------------------------
;; Create request dialog
;; -----------------------------------------------------------------------------

(defn create-field
  [{:keys [id label name value placeholder errors error-key]}]
  (g/field
   {:for id
    :label-text label
    :error (get errors error-key)
    :control
    (g/input
     {:id id
      :name name
      :value value
      :placeholder placeholder})}))

(defn create-textarea-field
  [{:keys [id label name value placeholder errors error-key]}]
  (g/field
   {:for id
    :label-text label
    :error (get errors error-key)
    :control
    (g/textarea
     {:id id
      :name name
      :rows 4
      :value value
      :placeholder placeholder})}))

(defn create-request-form
  [ctx {:keys [user values errors]}]
  (let [values (or values {})
        errors (or errors {})]
    (g/form
     ctx
     {:post (routes/create-request-url)
      :swap "none"
      :attrs {:hx-include (board-state-selector)}}
     (create-field
      {:id "humanhelp-create-customer-name"
       :label "Your name"
       :name "customer-name"
       :value (or (:customer-name values)
                  (account-email user)
                  "")
       :errors errors
       :error-key :customer-name})

     (create-field
      {:id "humanhelp-create-area"
       :label "Area"
       :name "area"
       :value (or (:area values) "")
       :placeholder "Garden"
       :errors errors
       :error-key :area})

     (create-field
      {:id "humanhelp-create-title"
       :label "Request"
       :name "title"
       :value (or (:title values) "")
       :placeholder "Need help finding a rake"
       :errors errors
       :error-key :title})

     (create-textarea-field
      {:id "humanhelp-create-details"
       :label "Details"
       :name "details"
       :rows 4
       :value (or (:details values) "")
       :placeholder "Add item, aisle, or context."
       :errors errors
       :error-key :details})

     (g/group
      {:align :end}
      (g/dialog-close
       {:text "Cancel"})
      (g/button
       {:variant :primary
        :text "Create"
        :attrs {:type "submit"}})))))

(defn create-request-dialog-body
  [ctx opts]
  [:div {:id create-request-dialog-body-id}
   (create-request-form ctx opts)])

(defn create-request-button
  []
  (g/dialog-trigger
   {:class "btn-icon-primary"
    :attrs {:aria-label "Create request"}}
   "+"))

(defn create-request-dialog
  [ctx {:keys [open?] :as opts}]
  (g/dialog
   {:open? open?
    :attrs {:id create-request-dialog-id}}
   (create-request-button)
   (g/dialog-overlay
    {:open? open?})
   (g/dialog-content
    {:open? open?
     :title "Create request"
     :description "Everyone can make and service requests in this Human Help analogue."
     :body [(create-request-dialog-body ctx opts)]})))

(defn create-request-dialog-fragment
  [ctx {:keys [user values errors open?]}]
  (create-request-dialog
   ctx
   {:user user
    :values values
    :errors errors
    :open? open?}))

;; -----------------------------------------------------------------------------
;; Board options dialog
;; -----------------------------------------------------------------------------

(defn option-param-name
  [enabled-key]
  (case enabled-key
    :mine-first? routes/mine-first-param
    :unclaimed-first? routes/unclaimed-first-param
    :show-terminal? routes/show-terminal-param
    (name enabled-key)))

(defn board-options-button
  []
  (g/dialog-trigger
   {:class "btn-icon-secondary"
    :attrs {:aria-label "Board options"
            :title "Board options"
            :data-humanhelp-board-options-trigger true}}
   (g/icon "settings" {:size :sm})))

(defn created-order-select
  [{:keys [created-order-options]}]
  (let [active-id (or (some (fn [{:keys [id active?]}]
                              (when active? id))
                            created-order-options)
                      model/default-created-order)]
    (g/field
     {:for "humanhelp-created-order"
      :label-text "Sort order"
      :class "content-stack-theme gap-1"
      :control
      (g/select
       {:id "humanhelp-created-order"
        :name routes/created-order-param
        :value (name active-id)
        :options (mapv (fn [{:keys [id label]}]
                         {:value (name id)
                          :label label})
                       created-order-options)
        :class "w-full"
        :attrs {:data-humanhelp-created-order-select true}})})))

(defn checkbox-option
  [{:keys [id label enabled-key checked?]}]
  (let [input-id (str "humanhelp-board-option-" (name id))]
    [:label {:for input-id
             :class "flex items-center gap-inline rounded-md py-1"
             :data-humanhelp-board-option true
             :data-humanhelp-board-option-id (name id)}
     (g/checkbox
      {:id input-id
       :name (option-param-name enabled-key)
       :value "true"
       :checked (boolean checked?)})
     (g/text
      {:as :span
       :variant :small
       :class "weight-medium-theme"
       :text label})]))

(defn board-options-form
  [ctx {:keys [view-state
               created-order-options
               priority-sort-options
               terminal-visibility-option]}]
  (let [metadata (model/board-option-metadata (or view-state {}))
        created-order-options (or created-order-options
                                  (:created-order-options metadata))
        priority-sort-options (or priority-sort-options
                                  (:priority-sort-options metadata))
        terminal-visibility-option (or terminal-visibility-option
                                       (:terminal-visibility-option metadata))]
    (g/form
     ctx
     {:post (routes/apply-board-options-url)
      :swap "none"
      :class "form-theme content-stack-theme gap-form"
      :attrs {:data-humanhelp-board-options-form true
              :hx-include (board-options-preserved-state-selector)}}

     (created-order-select
      {:created-order-options created-order-options})

     (into
      [:div {:class "content-stack-theme gap-field"
             :data-humanhelp-priority-options true}]
      (map checkbox-option)
      priority-sort-options)

     [:div {:class "content-stack-theme gap-field"
            :data-humanhelp-visibility-options true}
      (checkbox-option terminal-visibility-option)]

     (g/group
      {:align :end}
      (g/dialog-close
       {:text "Cancel"})
      (g/button
       {:variant :primary
        :text "Done"
        :attrs {:type "submit"}})))))

(defn board-options-dialog-body
  [ctx opts]
  [:div {:id board-options-dialog-body-id
        :class "pt-4"}
   (board-options-form ctx opts)])

(defn board-options-dialog
  [ctx {:keys [open?] :as opts}]
  (g/dialog
   {:open? open?
    :attrs {:id board-options-dialog-id}}
   (board-options-button)
   (g/dialog-overlay
    {:open? open?})
   (g/dialog-content
    {:open? open?
     :title "Board options"
     :body [(board-options-dialog-body ctx opts)]})))

;; -----------------------------------------------------------------------------
;; Request toolbar
;; -----------------------------------------------------------------------------

(defn refresh-form
  [ctx view-state stale?]
  (g/form
   ctx
   {:post (routes/refresh-requests-url)
    :swap "none"
    :inline? true
    :attrs {:hx-include (board-state-selector)}}
   (refresh-button {:stale? stale?})))

(defn request-toolbar-heading
  [{:keys [open-count pending-open-count]}]
  [:div {:class "content-stack-theme gap-field"}
   (g/section-title
    {:text "Requests"
     :class "text-lg-theme weight-semibold-theme"})

   (g/group
    {}
    (g/status-pill
     {:status (if (pos? (or open-count 0)) :active :muted)
      :dot? true
      :text "Open"})

    (g/muted-text
     {:as :span
      :class "text-sm-theme leading-body"
      :text (str (or open-count 0) " open")})

    (when (pos? (or pending-open-count 0))
      (g/badge
       {:variant :secondary
        :text (str "+" pending-open-count " new")})))])

(defn request-toolbar-fragment
  [{:keys [ctx
           user
           view-state
           open-count
           pending-open-count
           stale?
           latest-revision
           created-order-options
           priority-sort-options
           terminal-visibility-option]}]
  (let [view-state (or view-state {})
        stale?     (boolean stale?)]
    [:div {:id request-toolbar-dom-id
           :data-humanhelp-fragment "request-toolbar"
           :data-latest-revision latest-revision
           :class "content-stack-theme"}
     (g/toolbar
      {:start [(request-toolbar-heading
                {:open-count open-count
                 :pending-open-count pending-open-count})]
       :end [(g/group
              {:orientation :vertical
               :wrap? false
               :class "gap-field"
               :attrs {:style {:align-items "flex-end"}}}
              (refresh-form ctx view-state stale?)
              (create-request-dialog
               ctx
               {:user user
                :values {}
                :errors {}
                :open? false})
              (board-options-dialog
               ctx
               {:view-state view-state
                :created-order-options created-order-options
                :priority-sort-options priority-sort-options
                :terminal-visibility-option terminal-visibility-option
                :open? false}))]})

     (when stale?
       (muted "New request data is available. Refresh when you are ready."))]))

;; -----------------------------------------------------------------------------
;; Search / board-state form
;; -----------------------------------------------------------------------------

(defn search-control
  ([opts]
   (search-control nil opts))
  ([ctx {:keys [view-state]}]
   (let [view-state (or view-state {})]
     (g/form
      ctx
      {:get (routes/search-requests-url)
       :target (str "#" request-list-dom-id)
       :swap "outerHTML"
       :trigger (str "keyup changed delay:250ms from:#" search-input-dom-id
                     ", search from:#" search-input-dom-id)
       :class "content-stack-theme"
       :attrs {:id board-state-form-id}}
      (board-state-hidden-inputs view-state)
      (g/field
       {:for search-input-dom-id
        :label-text "Search requests"
        :control
        (g/input
         {:type "search"
          :id search-input-dom-id
          :name routes/search-param
          :value (or (:search view-state) "")
          :placeholder "Search by person, request, area, or status"})})))))

;; -----------------------------------------------------------------------------
;; Request list
;; -----------------------------------------------------------------------------

(defn empty-request-list
  [{:keys [view-state]}]
  (g/empty-state
   {:title (if (model/present? (:search view-state))
             "No matching requests"
             "No requests yet")
    :description (if (model/present? (:search view-state))
                   "Try fewer words or a different person, area, request, or status."
                   "Create a request with the plus button to start the demo.")
    :icon (g/empty-state-icon)}))

(defn request-accordion
  [{:keys [ctx user view-state requests]}]
  (apply
   g/accordion
   {:type :single
    :collapsible? true
    :class "content-stack-theme shadow-none"
    :attrs {:data-humanhelp-request-accordion true}}
   (map
    (fn [request]
      (request-card
       ctx
       {:request request
        :user user
        :view-state view-state}))
    requests)))

(defn request-list-prune-attrs
  [next-prune-ms]
  (when (and (integer? next-prune-ms)
             (pos? next-prune-ms))
    {:hx-get (routes/request-list-fragment-url)
     :hx-trigger (str "load delay:" next-prune-ms "ms")
     :hx-swap "outerHTML"
     :hx-include (board-state-selector)
     :data-humanhelp-next-prune-ms next-prune-ms}))

(defn request-list-fragment
  [{:keys [ctx user view-state requests latest-revision next-prune-ms]}]
  [:div (merge
         {:id request-list-dom-id
          :data-humanhelp-fragment "request-list"
          :data-latest-revision latest-revision
          :class "content-stack-theme"}
         (request-list-prune-attrs next-prune-ms))
   (if (seq requests)
     (request-accordion
      {:ctx ctx
       :user user
       :view-state view-state
       :requests requests})
     (empty-request-list {:view-state view-state}))])

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn board-card
  [ctx {:keys [view-state request-toolbar-panel request-list-panel]}]
  (g/card
   {:class "shadow-lg"
    :attrs {:data-humanhelp-board true}}
   (g/card-content
    {:class "content-stack-theme"}
    (or request-toolbar-panel
        [:div {:id request-toolbar-dom-id}
         (muted "Request toolbar loading...")])

    (search-control ctx {:view-state view-state})

    (or request-list-panel
        [:div {:id request-list-dom-id}
         (muted "Request list loading...")]))))

(defn page
  [ctx {:keys [user
               view-state
               request-toolbar-panel
               request-list-panel]}]
  (ui/page-shell
   ctx
   {:user user}

   (client-plumbing/listener
    ctx
    {:trigger-attrs {:hx-include (board-state-selector)}})

   (ui/container
    [:div {:class "content-stack-theme gap-section"}
     (hero)

     (board-card
      ctx
      {:view-state view-state
       :request-toolbar-panel request-toolbar-panel
       :request-list-panel request-list-panel})])))

;; -----------------------------------------------------------------------------
;; OOB / response helpers
;; -----------------------------------------------------------------------------

(defn replace-toolbar-oob
  [toolbar]
  (g/oob-outer-html request-toolbar-dom-id toolbar))

(defn replace-request-list-oob
  [request-list]
  (g/oob-outer-html request-list-dom-id request-list))

(defn replace-dialog-oob
  [dialog]
  (g/oob-outer-html create-request-dialog-id dialog))

(defn replace-board-state-oob
  "Render an OOB replacement for the stable board-state/search form.

   The app should use this whenever a response changes visible revision or board
   options outside the search form itself."
  ([view-state]
   (replace-board-state-oob nil view-state))
  ([ctx view-state]
   (g/oob-outer-html
    board-state-form-id
    (search-control ctx {:view-state view-state}))))

(defn with-board-state-oob
  "Wrap nodes with a board-state OOB replacement first.

   Putting board-state first means the browser updates hidden state before
   processing other OOB fragments from the same response."
  [ctx view-state & nodes]
  (apply oob-response
         (replace-board-state-oob ctx view-state)
         nodes))

(defn fragments-oob
  [{:keys [toolbar request-list]}]
  (oob-response
   (when toolbar
     (replace-toolbar-oob toolbar))
   (when request-list
     (replace-request-list-oob request-list))))

(defn create-request-validation-error
  [ctx {:keys [user values errors]}]
  (replace-dialog-oob
   (create-request-dialog
    ctx
    {:user user
     :values values
     :errors errors
     :open? true})))

(defn create-request-success
  [ctx {:keys [request toolbar request-list]}]
  (oob-response
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :success
     :duration 5000
     :title "Request created"
     :description (if request
                    (str "Request #"
                         (:request/number request)
                         " is now on the board.")
                    "The request is now on the board.")})))

(defn refreshed-request-board-fragments
  [{:keys [toolbar request-list]}]
  (fragments-oob
   {:toolbar toolbar
    :request-list request-list}))

(defn request-lifecycle-result
  [{:keys [action request toolbar request-list]}]
  (oob-response
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (when (and action request)
     (g/render-toast-oob
      {:variant :success
       :duration 2500
       :title (model/action-label action)
       :description (model/action-result-message action request)}))))

(defn request-action-error
  [{:keys [result]}]
  (g/render-toast-oob
   {:variant :danger
    :duration 7000
    :title "Request not updated"
    :description (or (get-in result [:error :message])
                     (:message result)
                     (:reason result)
                     "That request action could not be completed.")}))

(defn reset-demo-result
  [{:keys [toolbar request-list]}]
  (oob-response
   (fragments-oob
    {:toolbar toolbar
     :request-list request-list})
   (g/render-toast-oob
    {:variant :info
     :duration 5000
     :title "Demo reset"
     :description "The Human Help request board was reset."})))
