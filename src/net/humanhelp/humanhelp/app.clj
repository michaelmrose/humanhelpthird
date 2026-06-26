(ns net.humanhelp.humanhelp.app
  "HTTP boundary for the removable Human Help analogue app.

   This namespace assembles HTTP handlers from:

   - net.humanhelp.humanhelp.live
   - net.humanhelp.humanhelp.model
   - net.humanhelp.humanhelp.routes
   - net.humanhelp.humanhelp.views

   It should not own generic Gesso Live plumbing. Human Help live panels,
   fragment rendering, stream responses, change constructors, and toast helpers
   are delegated to net.humanhelp.humanhelp.live.

   It should not own Hiccup/OOB response shape beyond choosing which view helper
   to return. Board-state OOB rendering is delegated to views.clj."
  (:require
   [com.biffweb.experimental :as biffx]
   [clojure.string :as str]
   [gesso.core :as g]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.humanhelp.live :as app-live]
   [net.humanhelp.humanhelp.model :as model]
   [net.humanhelp.humanhelp.routes :as routes]
   [net.humanhelp.humanhelp.views :as views]
   [net.humanhelp.middleware :as mid])
  (:import
   [java.util UUID]))

;; -----------------------------------------------------------------------------
;; Request boundary helpers
;; -----------------------------------------------------------------------------

(defn- scalar-param-value
  "Normalize a request param value to the scalar value the app expects.

   Repeated browser params can arrive as vectors, for example when a form has
   two fields with the same name:

     [\"\" \"test\"]

   The last submitted value is treated as authoritative. This preserves normal
   form-clearing behavior:

     [\"old\" \"\"] => \"\""
  [x]
  (cond
    (nil? x)
    nil

    (and (sequential? x)
         (not (map? x)))
    (last x)

    :else
    x))

(defn- param
  "Read a Ring/Biff request param by keyword or string key.

   This stays here temporarily because we do not yet have a dedicated request
   boundary/view-state namespace. It supports plain Ring-style maps as well as
   common Reitit match placement."
  [ctx k]
  (scalar-param-value
   (or (get-in ctx [:params k])
       (get-in ctx [:params (name k)])
       (get-in ctx [:form-params k])
       (get-in ctx [:form-params (name k)])
       (get-in ctx [:query-params k])
       (get-in ctx [:query-params (name k)])
       (get-in ctx [:path-params k])
       (get-in ctx [:path-params (name k)])
       (get-in ctx [:reitit.core/match :path-params k])
       (get-in ctx [:reitit.core/match :path-params (name k)]))))

(defn- request-id
  [ctx]
  (param ctx :request-id))

(defn- request-view-state
  "Extract request-board view state from request params.

   app.clj owns HTTP parameter extraction only. It does not validate sort/filter
   values or decide their semantics; model/normalize-view-state fills defaults
   and normalizes option values against current persisted Human Help data."
  [ctx]
  {:search (or (param ctx routes/search-param) "")
   :visible-revision (model/parse-visible-revision
                      (param ctx routes/visible-revision-param))
   :created-order (param ctx routes/created-order-param)
   :mine-first? (param ctx routes/mine-first-param)
   :unclaimed-first? (param ctx routes/unclaimed-first-param)
   :show-terminal? (param ctx routes/show-terminal-param)})

(defn- normalized-view-state
  [ctx view-state]
  (model/normalize-view-state ctx view-state))

(defn- create-request-input
  "Extract create-request form input from request params.

   This deliberately uses the HTTP-boundary param helper so repeated browser
   params are normalized before reaching the model parser."
  [ctx]
  (model/parse-create-request-input
   {:title (param ctx :title)
    :area (param ctx :area)
    :details (param ctx :details)
    :customer-name (param ctx :customer-name)}))

;; -----------------------------------------------------------------------------
;; Current user
;; -----------------------------------------------------------------------------

(defn- session-uid
  "Return the signed-in user's session id, when present."
  [ctx]
  (or (get-in ctx [:session :uid])
      (get-in ctx [:session :user])
      (:user/id ctx)
      (get-in ctx [:user :xt/id])))

(defn- ->uuid
  [x]
  (cond
    (uuid? x)
    x

    (string? x)
    (try
      (UUID/fromString x)
      (catch Exception _
        nil))

    :else
    nil))

(defn- emailish?
  [x]
  (and (string? x)
       (str/includes? x "@")))

(defn- user-email-from-ctx
  [ctx]
  (some
   (fn [x]
     (when (emailish? x)
       x))
   [(:user/email ctx)
    (:user/email (:user ctx))
    (get-in ctx [:user :email])
    (get-in ctx [:session :email])
    (get-in ctx [:identity :email])]))

(defn- user-email-from-db
  [ctx]
  (let [conn (:biff/conn ctx)
        uid  (->uuid (session-uid ctx))]
    (when (and conn uid)
      (try
        (some-> (biffx/q conn
                         {:select [:user/email]
                          :from :user
                          :where [:= :xt/id uid]})
                first
                :user/email)
        (catch Exception _
          nil)))))

(defn- current-user-email
  "Return only a real display email, never a UUID/id fallback."
  [ctx]
  (or (user-email-from-ctx ctx)
      (user-email-from-db ctx)))

(defn- current-user
  [ctx]
  (let [email (current-user-email ctx)]
    (cond-> {:user/id (client-plumbing/current-user-id ctx)}
      email
      (assoc :user/email email))))

;; -----------------------------------------------------------------------------
;; Live boundary
;; -----------------------------------------------------------------------------

(defn- live-system
  [ctx]
  (or (:gesso.live/system ctx)
      (throw
       (ex-info "Human Help requires :gesso.live/system in ctx."
                {:ctx-keys (when (map? ctx)
                             (set (keys ctx)))}))))

(defn- board-render-options
  [ctx view-state]
  {:user (current-user ctx)
   :view-state view-state})

(defn- fragment-render-options
  [ctx]
  (board-render-options ctx (request-view-state ctx)))

(defn- render-toolbar-node
  [ctx view-state]
  (app-live/render-fragment-node
   ctx
   :request-toolbar
   (board-render-options ctx view-state)))

(defn- render-list-node
  [ctx view-state]
  (app-live/render-fragment-node
   ctx
   :request-list
   (board-render-options ctx view-state)))

(defn- board-fragments
  [ctx view-state]
  {:toolbar (render-toolbar-node ctx view-state)
   :request-list (render-list-node ctx view-state)})

(defn- notify!
  [ctx change]
  (app-live/notify!
   (live-system ctx)
   ctx
   change))

;; -----------------------------------------------------------------------------
;; HTML / OOB helpers
;; -----------------------------------------------------------------------------

(defn- html
  [node]
  (g/html-response node))

(defn- with-board-state-oob
  [ctx view-state & nodes]
  (apply views/with-board-state-oob
         ctx
         (normalized-view-state ctx view-state)
         nodes))

;; -----------------------------------------------------------------------------
;; Page props
;; -----------------------------------------------------------------------------

(defn- page-props
  [ctx]
  (let [view-state (normalized-view-state
                    ctx
                    (request-view-state ctx))]
    (merge
     {:user (current-user ctx)
      :view-state view-state}
     (app-live/page-panels view-state))))

;; -----------------------------------------------------------------------------
;; Receiver-specific connected-client side effects
;; -----------------------------------------------------------------------------

(defn- previous-revision
  [revision]
  (when (number? revision)
    (max 0 (dec revision))))

(defn- receiver-view-state-for-new-request
  "Return the receiving browser's view-state for a new-request notification.

   The pending client-plumbing request normally includes #humanhelp-board-state,
   so this uses the receiver browser's own q/visible-revision/options state.

   If visible-revision is absent, fall back to a definitely-stale revision so
   the receiver gets the stale toolbar affordance instead of accidentally
   rendering as current."
  [ctx revision]
  (let [view-state (request-view-state ctx)]
    (normalized-view-state
     ctx
     (cond-> view-state
       (nil? (:visible-revision view-state))
       (assoc :visible-revision (previous-revision revision))))))

(defn- new-request-client-oob
  "Return a receiver-specific pending fragment for a newly-created request.

   This renders only observer UI:
   - stale toolbar/count/refresh affordance
   - new-request toast

   It deliberately does not replace the request list. Observers should not have
   their visible list jump on another user's create."
  [request revision]
  (fn [receiver-ctx]
    (let [view-state (receiver-view-state-for-new-request
                      receiver-ctx
                      revision)
          toolbar    (render-toolbar-node receiver-ctx view-state)]
      (views/oob-response
       (views/replace-toolbar-oob toolbar)
       (g/render-toast-oob
        {:variant :info
         :duration 5000
         :title "New request received"
         :description (app-live/request-toast-description request)})))))

(defn- send-new-request-ui-safely!
  [request revision user]
  (try
    (client-plumbing/send-to-scope-except-user!
     app-live/notification-scope
     (:user/id user)
     (new-request-client-oob request revision))
    (catch Exception e
      (println "[humanhelp] send-new-request-ui! failed"
               {:message (.getMessage e)
                :request/id (:request/id request)}))))

(defn- send-reset-toast-safely!
  []
  (try
    (app-live/send-reset-toast!)
    (catch Exception e
      (println "[humanhelp] send-reset-toast! failed"
               {:message (.getMessage e)}))))

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn app-page
  "Render /app."
  [ctx]
  (views/page ctx (page-props ctx)))

;; -----------------------------------------------------------------------------
;; Fragment handlers
;; -----------------------------------------------------------------------------

(defn request-toolbar-fragment
  [ctx]
  (app-live/render-fragment-response
   ctx
   :request-toolbar
   (fragment-render-options ctx)))

(defn request-list-fragment
  [ctx]
  (app-live/render-fragment-response
   ctx
   :request-list
   (fragment-render-options ctx)))

(defn create-request-dialog-fragment
  [ctx]
  (html
   (views/create-request-dialog
    ctx
    {:user (current-user ctx)
     :values {}
     :errors {}
     :open? true})))

;; -----------------------------------------------------------------------------
;; Stream handlers
;; -----------------------------------------------------------------------------

(defn request-toolbar-stream
  [ctx]
  (app-live/stream-response
   (live-system ctx)
   ctx
   :request-toolbar
   (fragment-render-options ctx)))

(defn request-list-stream
  [ctx]
  (app-live/stream-response
   (live-system ctx)
   ctx
   :request-list
   (fragment-render-options ctx)))

;; -----------------------------------------------------------------------------
;; Request creation
;; -----------------------------------------------------------------------------

(defn- create-request-success-response
  "Return the creator's authoritative post-create board update.

   This is the refresh-equivalent actor path:
   - advance visible-revision to the create revision
   - render toolbar from the model
   - render list from the model
   - close/reset the dialog through views/create-request-success

   This is intentionally synchronous in the POST response so it cannot race a
   separate client-side refresh request."
  [ctx {:keys [request revision view-state]}]
  (let [user        (current-user ctx)
        view-state' (assoc view-state
                            :visible-revision revision)
        fragments   (board-fragments ctx view-state')]
    (html
     (with-board-state-oob
       ctx
       view-state'
       (views/create-request-success
        ctx
        (merge
         {:user user
          :request request}
         fragments))))))

(defn create-request!
  "Create a new request from the modal dialog.

   Creator behavior:
   - request is created
   - dialog closes
   - visible list refreshes immediately to include the new request
   - visible revision advances to the create revision

   Other connected users:
   - receive receiver-specific connected-client OOB, excluding the creator
   - see stale toolbar/count/toast
   - their list does not jump until they refresh.

   Important: create does not submit the model-backed :request/created live
   invalidation. That live graph wakes toolbar only, which is observer behavior
   and can race the creator's POST response."
  [ctx]
  (let [user       (current-user ctx)
        view-state (request-view-state ctx)
        input      (create-request-input ctx)
        errors     (model/create-request-errors input)]
    (if (seq errors)
      (html
       (views/create-request-validation-error
        ctx
        {:user user
         :values input
         :errors errors}))

      (let [{:keys [request revision]}
            (model/create-request!
             ctx
             {:user user
              :input input})]

        (send-new-request-ui-safely!
         request
         revision
         user)

        (create-request-success-response
         ctx
         {:request request
          :revision revision
          :view-state view-state})))))

;; -----------------------------------------------------------------------------
;; Request list interactions
;; -----------------------------------------------------------------------------

(defn refresh-requests!
  "Commit the visible request board to the latest revision."
  [ctx]
  (let [view-state (assoc (request-view-state ctx)
                          :visible-revision
                          (model/latest-revision ctx))]
    (html
     (with-board-state-oob
       ctx
       view-state
       (views/refreshed-request-board-fragments
        (board-fragments ctx view-state))))))

(defn search-requests
  "Render the request list for a search input change."
  [ctx]
  (request-list-fragment ctx))

(defn apply-board-options
  "Apply request-board sort/filter options without mutating persisted state.

   The board-options dialog submits current search/revision from the stable
   board-state form plus its own visible option controls. The response replaces
   board state first, then toolbar and request list, so subsequent requests
   preserve the newly-applied options."
  [ctx]
  (let [view-state (normalized-view-state ctx (request-view-state ctx))]
    (html
     (with-board-state-oob
       ctx
       view-state
       (views/refreshed-request-board-fragments
        (board-fragments ctx view-state))))))

;; -----------------------------------------------------------------------------
;; Request lifecycle actions
;; -----------------------------------------------------------------------------

(defn- lifecycle-action!
  "Shared request lifecycle action boundary.

   action is one of:
     :claim
     :unclaim
     :take-over
     :done
     :cancel"
  [ctx action transition-fn]
  (let [user       (current-user ctx)
        view-state (request-view-state ctx)
        id         (request-id ctx)
        result     (transition-fn
                    ctx
                    {:request-id id
                     :user user})]
    (if (= :ok (:status result))
      (let [{:keys [request revision previous]} result]
        (notify!
         ctx
         (app-live/request-transition-change
          {:action action
           :request request
           :previous previous
           :revision revision
           :actor user}))

        (html
         (with-board-state-oob
           ctx
           view-state
           (views/request-lifecycle-result
            (merge
             {:user user
              :action action
              :request request
              :previous previous
              :revision revision
              :view-state view-state}
             (board-fragments ctx view-state))))))

      (html
       (views/request-action-error
        {:user user
         :request-id id
         :action action
         :result result
         :view-state view-state})))))

(defn claim-request!
  [ctx]
  (lifecycle-action!
   ctx
   :claim
   model/claim-request!))

(defn unclaim-request!
  [ctx]
  (lifecycle-action!
   ctx
   :unclaim
   model/unclaim-request!))

(defn take-over-request!
  [ctx]
  (lifecycle-action!
   ctx
   :take-over
   model/take-over-request!))

(defn mark-request-done!
  [ctx]
  (lifecycle-action!
   ctx
   :done
   model/mark-request-done!))

(defn cancel-request!
  [ctx]
  (lifecycle-action!
   ctx
   :cancel
   model/cancel-request!))

;; -----------------------------------------------------------------------------
;; Dev/demo reset
;; -----------------------------------------------------------------------------

(defn reset-demo!
  [ctx]
  (let [user       (current-user ctx)
        result     (model/reset-demo-state! ctx)
        view-state (assoc (request-view-state ctx)
                          :visible-revision
                          (:revision result))]
    (notify!
     ctx
     (app-live/demo-reset-change
      {:revision (:revision result)
       :actor user}))

    (send-reset-toast-safely!)

    (html
     (with-board-state-oob
       ctx
       view-state
       (views/reset-demo-result
        (merge
         {:user user
          :result result
          :view-state view-state}
         (board-fragments ctx view-state)))))))

;; -----------------------------------------------------------------------------
;; Route handler map
;; -----------------------------------------------------------------------------

(def handlers
  {routes/page-id app-page

   routes/request-toolbar-fragment-id request-toolbar-fragment
   routes/request-list-fragment-id request-list-fragment
   routes/create-request-dialog-fragment-id create-request-dialog-fragment

   routes/request-toolbar-stream-id request-toolbar-stream
   routes/request-list-stream-id request-list-stream

   routes/create-request-id create-request!
   routes/refresh-requests-id refresh-requests!
   routes/search-requests-id search-requests
   routes/apply-board-options-id apply-board-options

   routes/claim-request-id claim-request!
   routes/unclaim-request-id unclaim-request!
   routes/take-over-request-id take-over-request!
   routes/done-request-id mark-request-done!
   routes/cancel-request-id cancel-request!

   routes/reset-demo-id reset-demo!})

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:live-rules app-live/live-rules
   :routes (routes/route-table
            handlers
            {:middleware [mid/wrap-signed-in]})})
