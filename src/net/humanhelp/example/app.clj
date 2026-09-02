(ns net.humanhelp.example.app
  "HTTP boundary for the removable Human Help analogue app.

   This namespace assembles HTTP handlers from:

   - net.humanhelp.example.live
   - net.humanhelp.example.model
   - net.humanhelp.example.routes
   - net.humanhelp.example.views

   It should not own generic Gesso Live plumbing. Human Help live panels,
   fragment rendering, stream responses, change constructors, and toast helpers
   are delegated to net.humanhelp.example.live.

   It should not own Hiccup/OOB response shape beyond choosing which view helper
   to return. Board-state OOB rendering is delegated to views.clj."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [com.biffweb.xtdb :as biff.xtdb]
   [gesso.choreo.identity :as choreo.identity]
   [gesso.core :as g]
   [gesso.live.core :as live]
   [gesso.live.optimistic.protocol :as optimistic.protocol]
   [gesso.live.ui :as live.ui]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.live :as app-live]
   [net.humanhelp.example.model :as model]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.example.views :as views]
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
  {:search           (or (param ctx routes/search-param) "")
   :visible-revision (model/parse-visible-revision
                      (param ctx routes/visible-revision-param))
   :created-order    (param ctx routes/created-order-param)
   :mine-first?      (param ctx routes/mine-first-param)
   :unclaimed-first? (param ctx routes/unclaimed-first-param)
   :show-terminal?   (param ctx routes/show-terminal-param)})

(defn- normalized-view-state
  [ctx view-state]
  (model/normalize-view-state ctx view-state))

(defn- create-request-input
  "Extract create-request form input from request params.

   This deliberately uses the HTTP-boundary param helper so repeated browser
   params are normalized before reaching the model parser."
  [ctx]
  (model/parse-create-request-input
   {:title         (param ctx :title)
    :area          (param ctx :area)
    :details       (param ctx :details)
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

(defn- user-phone-from-ctx
  [ctx]
  (some
   identity
   [(:user/phone ctx)
    (:user/phone (:user ctx))
    (get-in ctx [:user :phone])
    (get-in ctx [:session :phone])]))

(defn- user-phone-display-from-ctx
  [ctx]
  (some
   identity
   [(:user/phone-display ctx)
    (:user/phone-display (:user ctx))
    (get-in ctx [:user :phone-display])
    (get-in ctx [:session :phone-display])]))

(defn- user-record-from-db
  [ctx]
  (let [uid
        (->uuid
         (session-uid ctx))]
    (when
     (and
      uid
      (or
       (:biff.xtdb/connection-pool ctx)
       (:biff.xtdb/node ctx)))
      (try
        (first
         (biff.xtdb/q
          ctx
          {:select
           [:xt/id
            :user/email
            :user/phone
            :user/phone-display
            :user/phone-verified-at
            :user/joined-at]

           :from
           [:user]

           :where
           [:= :xt/id uid]}))
        (catch
         Exception e
          (println
           "[humanhelp] current-user lookup failed"
           {:message
            (.getMessage e)

            :uid
            uid})
          nil)))))

(defn- current-user-id
  [ctx user-record]
  (or (:xt/id user-record)
      (session-uid ctx)
      (client-plumbing/current-user-id ctx)))

(defn- current-user
  [ctx]
  (let [user-record   (user-record-from-db ctx)
        user-id       (current-user-id ctx user-record)
        email         (or (user-email-from-ctx ctx)
                          (:user/email user-record))
        phone         (or (user-phone-from-ctx ctx)
                          (:user/phone user-record))
        phone-display (or (user-phone-display-from-ctx ctx)
                          (:user/phone-display user-record))]
    (cond-> {:user/id user-id}
      (:xt/id user-record)
      (assoc :xt/id (:xt/id user-record))

      email
      (assoc :user/email email)

      phone
      (assoc :user/phone phone)

      phone-display
      (assoc :user/phone-display phone-display))))

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
  {:user       (current-user ctx)
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
  {:toolbar      (render-toolbar-node ctx view-state)
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

(defn- optimistic-html
  "Render one protocol-v3 settlement marker plus response nodes, then complete
   the trusted authority projection's direct settlement send.

   The settlement marker is inert transport correlation. It carries only the
   trusted protocol-v3 settlement already produced by Gesso's server boundary.
   Canonical DOM installation remains owned by the ordinary Live/HTMX authority
   path; actor-specific OOB extras may accompany the marker in this response.

   Completing the projected send happens only after response construction
   succeeds. If rendering throws, the send remains incomplete and the browser
   observes transport uncertainty rather than a fabricated semantic failure."
  [prepared & nodes]
  (let [settlement (:settlement prepared)
        response
        (html
         (apply views/oob-response
                (live.ui/optimistic-settlement-marker settlement)
                nodes))]
    (live/complete-optimistic-send prepared)
    response))

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
     {:user       (current-user ctx)
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
        {:variant     :info
         :duration    5000
         :title       "New request received"
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
               {:message    (.getMessage e)
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
    {:user   (current-user ctx)
     :values {}
     :errors {}
     :open?  true})))

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
         {:user    user
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
        {:user   user
         :values input
         :errors errors}))

      (let [{:keys [request revision]}
            (model/create-request!
             ctx
             {:user  user
              :input input})]

        (send-new-request-ui-safely!
         request
         revision
         user)

        (create-request-success-response
         ctx
         {:request    request
          :revision   revision
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

(defn- lifecycle-transition
  [action]
  (keyword "request" (name action)))

(defn- request-revision
  [request]
  (or (:request/updated-revision request)
      (:request/created-revision request)))

(defn- rejection-reason
  [result]
  (or (get-in result [:error :error/type])
      (get-in result [:error :message])
      (:reason result)
      :request/rejected))

(defn- notify-transition-safely!
  [ctx {:keys [action request previous revision actor]}]
  (try
    (notify!
     ctx
     (app-live/request-transition-change
      {:action   action
       :request  request
       :previous previous
       :revision revision
       :actor    actor}))
    (catch Exception e
      ;; Observer delivery must not convert an already-committed model mutation
      ;; into an HTTP failure. The actor still receives its canonical settlement.
      (println
       "[humanhelp] request transition notification failed"
       {:message    (.getMessage e)
        :action     action
        :request/id (:request/id request)
        :revision   revision}))))

(def optimistic-command-param
  "HTMX parameter installed by Gesso's protocol-v3 optimistic browser bridge."
  "__gesso_live_optimistic_command")

(defn- optimistic-command-wire
  "Read one protocol-v3 optimistic command wire value from the HTMX request.

   The browser bridge encodes portable EDN into a normal request parameter.
   EDN parsing establishes only shape. Authentication, operation resolution,
   authorization, current-state reread, and mutation remain trusted server/model
   responsibilities."
  [ctx]
  (let [encoded (param ctx optimistic-command-param)]
    (when-not (and (string? encoded)
                   (not (str/blank? encoded)))
      (throw
       (ex-info
        "Human Help optimistic lifecycle request is missing its Gesso protocol-v3 command."
        {:parameter optimistic-command-param})))
    (try
      (edn/read-string encoded)
      (catch Exception e
        (throw
         (ex-info
          "Human Help optimistic lifecycle request contains unreadable command EDN."
          {:parameter optimistic-command-param}
          e))))))

(defn- lifecycle-authoritative
  "Build the trusted protocol-v3 authoritative observation for one demo request.

   The removable example uses its monotonic store revision as the authority
   basis. The projection is the authoritative request value itself; Gesso treats
   that projection as opaque application data and never infers authorization
   from it."
  [request revision]
  (when request
    (optimistic.protocol/authoritative
     {:presence      :present
      :basis         revision
      :projection    request
      :fact-versions {:request/revision (request-revision request)}})))

(defn- lifecycle-operation-result
  [ctx action result]
  (case (:status result)
    :ok
    {:resolution    :confirmed
     :authoritative (lifecycle-authoritative
                     (:request result)
                     (:revision result))
     :outcome       (app-live/request-transition-topic action)}

    :error
    (let [request  (:request result)
          revision (model/latest-revision ctx)]
      (cond->
       {:resolution :rejected
        :reason     (rejection-reason result)}
        request
        (assoc :authoritative
               (lifecycle-authoritative request revision))))

    (throw
     (ex-info
      "Human Help lifecycle transition returned an unsupported result status."
      {:action action
       :status (:status result)
       :result result}))))

(defn- lifecycle-operation
  "Construct one trusted protocol-v3 optimistic operation for the removable
   example.

   Browser arguments remain untrusted. The operation derives the actor from the
   trusted Ring/Biff context, rereads current demo state through the public demo
   model transition, and only then classifies the semantic settlement."
  [action transition-fn]
  (let [operation (lifecycle-transition action)]
    (live/optimistic-operation
     {:name      (keyword "humanhelp.example.optimistic" (name action))
      :operation operation
      :execute!
      (fn [{:keys [ctx arguments]}]
        (let [user       (current-user ctx)
              request-id (:request-id arguments)
              result
              (transition-fn
               ctx
               {:request-id request-id
                :user       user})]
          (when (= :ok (:status result))
            (notify-transition-safely!
             ctx
             {:action   action
              :request  (:request result)
              :previous (:previous result)
              :revision (:revision result)
              :actor    user}))
          (lifecycle-operation-result ctx action result)))})))

(def optimistic-server
  "Trusted protocol-v3 server registry for the removable example's lifecycle
   actions.

   Rendering an optimistic affordance does not grant authority. Principal is
   reconstructed from trusted request/session context on every command, and a
   browser can select only operations present in this registry."
  (live/optimistic-server
   {:principal-fn
    (fn [ctx]
      (let [user-id (:user/id (current-user ctx))]
        (when-not user-id
          (throw
           (ex-info
            "Human Help optimistic lifecycle action requires an authenticated user."
            {})))
        (choreo.identity/principal user-id)))

    :operations
    {:request/claim
     (lifecycle-operation :claim model/claim-request!)

     :request/unclaim
     (lifecycle-operation :unclaim model/unclaim-request!)

     :request/take-over
     (lifecycle-operation :take-over model/take-over-request!)

     :request/done
     (lifecycle-operation :done model/mark-request-done!)

     :request/cancel
     (lifecycle-operation :cancel model/cancel-request!)}}))

(defn- require-route-command!
  "Fail closed when browser command semantics do not match the HTTP route.

   The browser is allowed to propose a semantic command, but it may not turn a
   /claim endpoint into some other registered operation or retarget the route to
   a different request id by rewriting the optimistic command parameter."
  [ctx action command]
  (let [expected-operation (lifecycle-transition action)
        expected-request-id (request-id ctx)
        actual-operation (:operation command)
        actual-request-id (get-in command [:arguments :request-id])]
    (when-not (= expected-operation actual-operation)
      (throw
       (ex-info
        "Human Help optimistic command does not match the lifecycle route operation."
        {:expected-operation expected-operation
         :actual-operation   actual-operation})))
    (when-not (= expected-request-id actual-request-id)
      (throw
       (ex-info
        "Human Help optimistic command request id does not match the lifecycle route."
        {:expected-request-id expected-request-id
         :actual-request-id   actual-request-id
         :operation           actual-operation})))
    command))

(defn- settlement-request
  [settlement]
  (get-in settlement [:authoritative :projection]))

(defn- lifecycle-response-extra
  [ctx {:keys [action view-state settlement]}]
  (let [request    (settlement-request settlement)
        resolution (:resolution settlement)]
    (cond
      (contains? #{:confirmed :reconciled :already-incorporated}
                 resolution)
      (views/request-lifecycle-extras
       ctx
       {:action     action
        :request    request
        :toolbar    (render-toolbar-node ctx view-state)
        :view-state view-state})

      (contains? #{:rejected :failed} resolution)
      (views/request-action-error
       {:result
        {:reason (:reason settlement)}})

      :else
      (throw
       (ex-info
        "Human Help optimistic lifecycle response received an unsupported settlement resolution."
        {:action     action
         :settlement settlement})))))

(defn- lifecycle-action!
  "Run one request lifecycle endpoint through Gesso Live optimistic protocol v3.

   The browser supplies only an encoded semantic command. This route binds that
   command to the route's operation and request id before the trusted Gesso
   server registry authenticates principal, resolves the operation, invokes the
   demo model transition, and constructs the settlement.

   The HTTP response carries one inert settlement marker plus actor-specific OOB
   extras. Canonical card installation/reconciliation belongs to the ordinary
   Live/HTMX authoritative refresh path, not to a server-supplied Hiccup payload
   embedded inside the settlement."
  [ctx action]
  (let [view-state
        (normalized-view-state
         ctx
         (request-view-state ctx))

        command
        (->> (optimistic-command-wire ctx)
             live/decode-optimistic-command
             (require-route-command! ctx action))

        prepared
        (live/run-optimistic-command
         optimistic-server
         ctx
         command)

        settlement
        (:settlement prepared)

        extra
        (lifecycle-response-extra
         ctx
         {:action     action
          :view-state view-state
          :settlement settlement})]
    (optimistic-html
     prepared
     extra)))

(defn claim-request!
  [ctx]
  (lifecycle-action! ctx :claim))

(defn unclaim-request!
  [ctx]
  (lifecycle-action! ctx :unclaim))

(defn take-over-request!
  [ctx]
  (lifecycle-action! ctx :take-over))

(defn mark-request-done!
  [ctx]
  (lifecycle-action! ctx :done))

(defn cancel-request!
  [ctx]
  (lifecycle-action! ctx :cancel))

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
       :actor    user}))

    (send-reset-toast-safely!)

    (html
     (with-board-state-oob
       ctx
       view-state
       (views/reset-demo-result
        (merge
         {:user       user
          :result     result
          :view-state view-state}
         (board-fragments ctx view-state)))))))

;; -----------------------------------------------------------------------------
;; Route handler map
;; -----------------------------------------------------------------------------

(def handlers
  {routes/page-id app-page

   routes/request-toolbar-fragment-id       request-toolbar-fragment
   routes/request-list-fragment-id          request-list-fragment
   routes/create-request-dialog-fragment-id create-request-dialog-fragment

   routes/request-toolbar-stream-id request-toolbar-stream
   routes/request-list-stream-id    request-list-stream

   routes/create-request-id      create-request!
   routes/refresh-requests-id    refresh-requests!
   routes/search-requests-id     search-requests
   routes/apply-board-options-id apply-board-options

   routes/claim-request-id     claim-request!
   routes/unclaim-request-id   unclaim-request!
   routes/take-over-request-id take-over-request!
   routes/done-request-id      mark-request-done!
   routes/cancel-request-id    cancel-request!

   routes/reset-demo-id reset-demo!})

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:live-rules app-live/live-rules
   :routes     (routes/route-table
                handlers
                {:middleware [mid/wrap-signed-in]})})
