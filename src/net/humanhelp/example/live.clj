(ns net.humanhelp.example.live
  "Production-model-backed Gesso Live wiring for the HumanHelp example app.

   The example app is the immediate proving surface for the production
   HumanHelp models.  This namespace therefore queries the production-backed
   net.humanhelp.example.board projection and listens to the semantic :request
   changes emitted by the production Request model.

   It deliberately does not depend on net.humanhelp.example.model.

   Responsibilities here are limited to the Live adapter boundary:

   - one production Location-backed example scope;
   - Request change -> fragment invalidation routing;
   - fragment query/render descriptors;
   - fragment panel/response/stream helpers;
   - client-only continuity;
   - a few temporary app-facing change/toast helpers kept only until the old
     example.app mutation handlers are removed.

   Domain truth, lifecycle transitions, persistence, authorization, Choreo
   capabilities, and Request read composition live in production model/Choreo
   namespaces and net.humanhelp.example.board.

   Important load-boundary rule:
   this namespace must not statically require net.humanhelp.example.views.
   View renderer Vars and view-owned DOM id Vars are resolved lazily so app/live
   reload order remains acyclic."
  (:require
   [gesso.core :as g]
   [gesso.live.continuity :as continuity]
   [gesso.live.core :as live]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.site.mock-data :as mock-data]))

;; =============================================================================
;; Production example scope / render context
;; =============================================================================

(def location-id
  "Production development-fixture Location displayed by the example board."
  mock-data/default-location-id)

(def store-id
  "Temporary source-compatibility alias for callers that still use the old
   example's store terminology.  The value is now the production Location UUID,
   not the former demo-store string."
  location-id)

(def notification-scope
  "Connected-browser scope used for page-global HumanHelp notifications."
  client-plumbing/app-scope)

(def ^:private render-options-key
  ::render-options)

(defn- with-render-options
  [ctx render-options]
  (assoc ctx render-options-key (or render-options {})))

(defn- render-options
  [ctx]
  (get ctx render-options-key {}))

(defn- render-viewer
  "Return the production User projection supplied by the HTTP/app boundary.

   :user remains accepted temporarily because the currently installed app uses
   that key.  Both values are expected to be production User-shaped data; this
   namespace never falls back to example.model identity."
  [ctx]
  (or (:viewer (render-options ctx))
      (:user (render-options ctx))))

(defn- render-view-state
  [ctx]
  (:view-state (render-options ctx)))

(defn- normalized-render-view-state
  [ctx]
  (board/normalize-view-state
   (render-view-state ctx)))

(defn- board-input
  [ctx id]
  {:location-id id
   :viewer (render-viewer ctx)
   :view-state (normalized-render-view-state ctx)})

;; =============================================================================
;; Lazy view boundary
;; =============================================================================

(defn- resolve-view-var
  [sym]
  (or (requiring-resolve sym)
      (throw
       (ex-info
        "Could not resolve HumanHelp example view var."
        {:symbol sym}))))

(defn- view-value
  [sym]
  @(resolve-view-var sym))

(defn- call-view
  [sym & args]
  (apply @(resolve-view-var sym) args))

(defn request-toolbar-dom-id
  []
  (view-value 'net.humanhelp.example.views/request-toolbar-dom-id))

(defn request-list-dom-id
  []
  (view-value 'net.humanhelp.example.views/request-list-dom-id))

(defn board-state-form-id
  []
  (view-value 'net.humanhelp.example.views/board-state-form-id))

(defn board-state-selector
  []
  (str "#" (board-state-form-id)))

;; =============================================================================
;; Client continuity
;; =============================================================================

(def request-list-client-continuity
  "Browser-local interaction state that survives authoritative list replacement.

   Search/sort/filter state is still HTTP-visible board state carried through
   hx-include.  Only scroll, focus/caret, and native <details> state live here."
  (continuity/preserve
   {:scroll {:selector "[data-humanhelp-request-card]"}
    :focus true
    :boxes [(continuity/details-open
             {:selector
              "details[data-humanhelp-request-card][data-accordion-value]"
              :key-attr "data-accordion-value"
              :single? true})]}))

;; =============================================================================
;; Live scope authorization
;; =============================================================================

(defn allow-example-location?
  "The proving UI currently exposes exactly the fixed production development
   Location.  This is presentation/scope selection, not Request authorization;
   every Request operation still authenticates and revalidates in the model."
  [_ctx id]
  (= location-id id))

(def allow-demo-store?
  "Temporary source-compatibility alias."
  allow-example-location?)

;; =============================================================================
;; Fragment queries
;; =============================================================================

(defn request-toolbar-query
  [ctx id]
  (let [data (board/board-data ctx (board-input ctx id))
        viewer (:viewer data)
        view-state (:view-state data)
        basis (:observed-basis data)]
    {:ctx ctx
     :location-id id
     :viewer viewer
     ;; Temporary view key until all non-Request example UI says :viewer.
     :user viewer
     :view-state view-state
     :open-count (:active-count data)
     ;; Production Live progression replaces the old demo revision/pending
     ;; revision scheme.  There is no second application revision frontier.
     :pending-open-count 0
     :stale? false
     :latest-revision basis}))

(defn request-list-query
  [ctx id]
  (let [data (board/board-data ctx (board-input ctx id))]
    (assoc data
           :ctx ctx
           :store/id id
           :user (:viewer data)
           :latest-revision (:observed-basis data))))

;; =============================================================================
;; Fragment renders
;; =============================================================================

(defn request-toolbar-render
  [data]
  (call-view
   'net.humanhelp.example.views/request-toolbar-fragment
   data))

(defn request-list-render
  [data]
  (call-view
   'net.humanhelp.example.views/request-list-fragment
   data))

;; =============================================================================
;; Compiled Live model
;; =============================================================================

(def compiled-live
  (live/compile-live-app
   {:response g/html-response

    :scopes
    {:request-toolbar
     {:topic :humanhelp/request-toolbar
      :id-key :request/location-id
      :label "Request toolbar"
      :authorized? allow-example-location?}

     :request-list
     {:topic :humanhelp/request-list
      :id-key :request/location-id
      :label "Request list"
      :authorized? allow-example-location?}}

    ;; Production Request FX emits semantic :request changes carrying
    ;; :request/location-id.  Assignment changes deliberately coalesce onto the
    ;; owning Request and use the same topic/location identity, so one rule
    ;; invalidates the complete aggregate projection.
    :graph
    {:request
     [{:scope :request-toolbar
       :id-key :request/location-id}
      {:scope :request-list
       :id-key :request/location-id}]}

    :fragments
    {:request-toolbar
     {:scope :request-toolbar
      :id-fn (fn [_id]
               (request-toolbar-dom-id))
      :query request-toolbar-query
      :render request-toolbar-render
      :swap :outerHTML}

     :request-list
     {:scope :request-list
      :id-fn (fn [_id]
               (request-list-dom-id))
      :query request-list-query
      :render request-list-render
      :swap :outerHTML}}}))

(def live-rules
  "Compiled invalidation rules exported for the app module."
  (live/model-live-rules compiled-live))

;; =============================================================================
;; Fragment URL options
;; =============================================================================

(defn fragment-options
  "Return production-backed fragment panel options.

   Board presentation state is included from #humanhelp-board-state rather than
   encoded into the stable fragment/stream URLs."
  ([fragment-name]
   (fragment-options fragment-name nil))
  ([fragment-name _view-state]
   (case fragment-name
     :request-toolbar
     {:fragment-url (routes/request-toolbar-fragment-url)
      :stream-url (routes/request-toolbar-stream-url)
      :root-attrs {:hx-include (board-state-selector)}}

     :request-list
     {:fragment-url (routes/request-list-fragment-url)
      :stream-url (routes/request-list-stream-url)
      :swap "outerHTML show:none focus-scroll:false"
      :root-attrs {:hx-include (board-state-selector)}
      :client-continuity request-list-client-continuity}

     (throw
      (ex-info
       "Unknown HumanHelp live fragment."
       {:fragment fragment-name
        :known-fragments [:request-toolbar :request-list]})))))

;; =============================================================================
;; Initial panels
;; =============================================================================

(defn request-toolbar-panel
  ([]
   (request-toolbar-panel nil))
  ([_view-state]
   (live/model-fragment-panel
    compiled-live
    :request-toolbar
    location-id
    (fragment-options :request-toolbar))))

(defn request-list-panel
  ([]
   (request-list-panel nil))
  ([_view-state]
   (live/model-fragment-panel
    compiled-live
    :request-list
    location-id
    (fragment-options :request-list))))

(defn page-panels
  "Return the production-backed Live panels needed for the example page."
  ([]
   (page-panels nil))
  ([_view-state]
   {:request-toolbar-panel (request-toolbar-panel)
    :request-list-panel (request-list-panel)}))

;; =============================================================================
;; Fragment render / response helpers
;; =============================================================================

(defn render-fragment-node
  "Render one HumanHelp Live fragment to Hiccup.

   render-options should contain :viewer (preferred) or the temporary :user
   compatibility key, plus :view-state."
  [ctx fragment-name render-options]
  (live/render-fragment-node
   compiled-live
   (with-render-options ctx render-options)
   fragment-name
   location-id))

(defn render-fragment-response
  [ctx fragment-name render-options]
  (live/render-fragment-response
   compiled-live
   (with-render-options ctx render-options)
   fragment-name
   location-id))

(defn stream-response
  "Start an SSE stream for one production-backed HumanHelp fragment."
  ([live-system ctx fragment-name render-options]
   (stream-response live-system ctx fragment-name render-options nil))
  ([live-system ctx fragment-name render-options options]
   (:response
    (live/start-fragment-stream!
     live-system
     compiled-live
     (with-render-options ctx render-options)
     fragment-name
     location-id
     (merge
      {:flow-options {:relieve? true}}
      options)))))

;; =============================================================================
;; Temporary app-facing semantic change helpers
;; =============================================================================
;;
;; Production Request FX already emits :request changes.  These constructors are
;; retained only so the not-yet-replaced example.app demo mutation handlers keep
;; loading between revisions.  They deliberately emit the same production
;; :request topic/location routing shape, and can be deleted with those handlers.

(defn- canonical-operation
  [action]
  (case action
    :claim :request/claim
    :unclaim :request/unclaim
    :take-over :request/mark-on-the-way
    :done :request/complete
    :cancel :request/cancel
    :request/claim :request/claim
    :request/unclaim :request/unclaim
    :request/mark-on-the-way :request/mark-on-the-way
    :request/complete :request/complete
    :request/cancel :request/cancel
    :request/reassign :request/reassign
    action))

(defn- request-id-value
  [request-document]
  (or (:xt/id request-document)
      (:request/id request-document)))

(defn- request-status-value
  [request-document]
  (:request/status request-document))

(defn- request-change-base
  [request-document operation]
  {:topic :request
   :id (request-id-value request-document)
   :request/operation (canonical-operation operation)
   :request/id (request-id-value request-document)
   :request/location-id location-id
   :request/status (request-status-value request-document)})

(defn request-created-change
  [{:keys [request revision actor]}]
  (cond->
   (assoc
    (request-change-base request :request/create)
    :change/kind :created)
    revision
    (assoc :request/revision revision)

    (:user/id actor)
    (assoc :actor/id (:user/id actor))))

(defn request-transition-topic
  "Return the production Choreo outcome keyword for one lifecycle action.

   Legacy unqualified action names are accepted only until example.app is cut
   over; callers should use production :request/* operation identities."
  [action]
  (case (canonical-operation action)
    :request/claim :request/claimed
    :request/unclaim :request/unclaimed
    :request/mark-on-the-way :request/on-the-way
    :request/complete :request/completed
    :request/cancel :request/cancelled
    :request/reassign :request/reassigned
    (throw
     (ex-info
      "Unknown HumanHelp Request transition operation."
      {:action action}))))

(defn request-transition-change
  [{:keys [action request previous revision actor]}]
  (cond->
   (merge
    (request-change-base request action)
    {:change/kind :updated
     :request/previous-status (request-status-value previous)})
    revision
    (assoc :request/revision revision)

    (:user/id actor)
    (assoc :actor/id (:user/id actor))))

(defn demo-reset-change
  "Temporary wake-up used only by the old demo reset handler."
  [{:keys [revision actor]}]
  (cond->
   {:topic :request
    :id location-id
    :change/kind :updated
    :request/operation :request/demo-reset
    :request/id location-id
    :request/location-id location-id}
    revision
    (assoc :request/revision revision)

    (:user/id actor)
    (assoc :actor/id (:user/id actor))))

;; =============================================================================
;; Page-global notifications
;; =============================================================================

(defn request-toast-description
  [request-document]
  (let [reference
        (or (:request/number request-document)
            (request-id-value request-document))
        title (:request/title request-document)]
    (str
     "New request "
     reference
     (when (board/present? title)
       (str ": " title)))))

(defn send-new-request-toast!
  ([request-document]
   (send-new-request-toast! request-document {}))
  ([request-document {:keys [actor exclude-user-id]}]
   (let [excluded-user-id (or exclude-user-id
                              (:user/id actor))
         toast {:variant :info
                :title "New request received"
                :description (request-toast-description request-document)}]
     (if excluded-user-id
       (client-plumbing/send-toast-to-scope-except-user!
        notification-scope
        excluded-user-id
        toast)
       (client-plumbing/send-toast-to-scope!
        notification-scope
        toast)))))

(defn send-reset-toast!
  "Temporary notification retained for the old demo reset route."
  []
  (client-plumbing/send-toast-to-scope!
   notification-scope
   {:variant :info
    :title "Demo reset"
    :description "The HumanHelp request board was reset."}))

(defn send-request-action-error-toast!
  [message]
  (client-plumbing/send-toast-to-scope!
   notification-scope
   {:variant :danger
    :title "Request not updated"
    :description (or message
                     "That request action could not be completed.")}))

;; =============================================================================
;; Notification adapter
;; =============================================================================

(defn notify!
  "Submit a primary change to the app-wide Gesso Live system.

   Production Request model transactions normally publish their own semantic
   changes.  This adapter remains useful to the temporary old example handlers
   and for explicit non-model page-global events while the cutover completes."
  [live-system ctx change]
  (live/submit-expanded!
   live-system
   ctx
   change))
