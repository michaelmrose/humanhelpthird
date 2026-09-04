(ns net.humanhelp.example.app
  "HTTP boundary for the removable Human Help analogue app.

   This namespace assembles HTTP handlers from the production-backed example
   board/Live/optimistic/view seams. net.humanhelp.example.model remains only as
   temporary legacy support for demo reset; production Request creation and
   lifecycle execution now go through the public production Request model.

   It should not own generic Gesso Live plumbing. Human Help live panels,
   fragment rendering, stream responses, change constructors, and toast helpers
   are delegated to net.humanhelp.example.live.

   It should not own Hiccup/OOB response shape beyond choosing which view helper
   to return. Board-state OOB rendering is delegated to views.clj."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [gesso.core :as g]
   [gesso.live.core :as live]
   [gesso.live.progression :as progression]
   [gesso.live.ui :as live.ui]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.live :as app-live]
   [net.humanhelp.example.model :as model]
   [net.humanhelp.example.optimistic :as optimistic]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.example.views :as views]
   [net.humanhelp.middleware :as mid]
   [net.humanhelp.site.mock-data :as mock-data]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user])
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
  "Extract request-board presentation state from request params.

   app.clj owns HTTP parameter extraction only. Production example-board
   normalization owns the presentation vocabulary. There is intentionally no
   demo :visible-revision here: Gesso Live/XTDB progression is the authority
   frontier for the production-backed board."
  [ctx]
  {:search           (or (param ctx routes/search-param) "")
   :created-order    (param ctx routes/created-order-param)
   :mine-first?      (param ctx routes/mine-first-param)
   :unclaimed-first? (param ctx routes/unclaimed-first-param)
   :show-terminal?   (param ctx routes/show-terminal-param)})

(defn- normalized-view-state
  [_ctx view-state]
  (board/normalize-view-state view-state))

(defn- create-request-input
  "Build the production Request create input from HTTP form params.

   Production Request owns authenticated requestor identity and one fixed
   Organization/Location-backed content shape.  The still-installed create form
   uses the old transport name :area for one revision; interpret that value only
   as production :location-detail.  :customer-name is deliberately ignored:
   Requestor identity/display data belongs to the authenticated production User,
   not to mutable Request form input."
  [ctx]
  {:organization-id
   mock-data/organization-id

   :location-id
   mock-data/default-location-id

   :content
   (request/normalize-content
    {:title
     (param ctx :title)

     :details
     (param ctx :details)

     :location-detail
     (or
      (param ctx :location-detail)
      (param ctx :area))})})

(defn- create-request-view-values
  "Return the temporary form-facing values for the currently installed create
   dialog.

   The production semantic input already uses :location-detail.  Until the next
   view revision renames the old :area control and removes :customer-name, keep
   only this transport-level projection at the HTTP/view boundary."
  [ctx input]
  {:title
   (get-in input [:content :title])

   :area
   (get-in input [:content :location-detail])

   :details
   (get-in input [:content :details])

   :customer-name
   (param ctx :customer-name)})

(defn- create-request-errors
  "Return production Request content errors in the temporary current form key
   vocabulary."
  [input]
  (let [errors
        (request/content-errors
         (:content input))]
    (not-empty
     (cond-> errors
       (contains? errors :location-detail)
       (->
        (assoc :area (:location-detail errors))
        (dissoc :location-detail))))))

;; -----------------------------------------------------------------------------
;; Current user
;; -----------------------------------------------------------------------------

(defn- canonical-user-id
  "Return the one authenticated identity used by the removable example.

   Keep this boundary identical to connected-client routing. In particular,
   explicit application/model identity must not be shadowed by stale session
   data, and display fields such as email are never identity sources."
  [ctx]
  (client-plumbing/current-user-id ctx))

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

(defn- production-user
  "Load the authenticated actor through the production HumanHelp User model.

   The example application is a proving surface for the production models, not
   a parallel identity store. The canonical authenticated identity must
   therefore resolve to a production User UUID and the User read must satisfy
   whatever Gesso progression requirement is present on ctx."
  [ctx]
  (let [user-id
        (or
         (->uuid
          (canonical-user-id ctx))
         (throw
          (ex-info
           "Human Help example requires a UUID production User identity."
           {:error/type
            :humanhelp.example/invalid-user-id

            :identity
            (canonical-user-id ctx)})))]
    (user/require-user
     ctx
     user-id)))

(defn- current-user
  "Adapt the authoritative production User document to the small view-facing
   user map consumed by the removable example.

   No session/profile fallback is allowed here. Display data comes from the
   production User model so the example exercises the same authoritative facts
   the eventual site UI will consume."
  [ctx]
  (let [user-document
        (production-user ctx)

        user-id
        (user/user-id
         user-document)

        email
        (user/user-email
         user-document)

        phone
        (user/user-phone
         user-document)

        display-name
        (user/user-display-name
         user-document)]

    (cond->
     {:xt/id
      user-id

      :user/id
      user-id

      :user/status
      (user/user-status
       user-document)}

      email
      (assoc
       :user/email
       email)

      phone
      (assoc
       :user/phone
       phone)

      display-name
      (assoc
       :user/display-name
       display-name))))

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

(defn- committed-transaction
  "Return the authoritative transaction metadata from one successful demo model
   mutation.

   Successful mutations in the removable example are expected to cross Gesso
   Live's transaction boundary. Failing closed here prevents a future model
   regression from silently dropping the commit progression required by actor
   read-your-writes rendering and observer convergence."
  [result]
  (or (:transaction result)
      (throw
       (ex-info
        "Human Help successful mutation is missing authoritative transaction metadata."
        {:status (:status result)
         :result-keys (when (map? result)
                        (set (keys result)))}))))

(defn- committed-ctx
  "Return the transaction-derived request context for a successful mutation."
  [result]
  (or (:ctx (committed-transaction result))
      (throw
       (ex-info
        "Human Help committed mutation is missing its progression-aware context."
        {:transaction-keys
         (set (keys (committed-transaction result)))}))))

(defn- transaction-bound-change
  "Attach trusted commit metadata to one semantic change.

   The transaction-established progression, not an application revision and not
   the incoming request progression, is the authority carried by the primary
   change. This mirrors gesso.live/transact-and-notify! for the removable demo's
   temporary two-stage model boundary."
  [result change]
  (let [{:keys [consistency progression]} (committed-transaction result)]
    (cond-> change
      (seq consistency)
      (assoc :gesso.live/consistency consistency)

      progression
      (assoc :progression progression))))

(defn- production-result-ctx
  "Compose one successful production model result's authoritative progression
   into the incoming request context.

   Public model operations intentionally expose stable commit/progression
   semantics rather than Gesso Live's internal transaction context.  A
   dependent render therefore reconstructs only the justified observation
   frontier through the public progression contract."
  [ctx result]
  (when-not
   (= :committed (:commit/status result))
    (throw
     (ex-info
      "Human Help production Request mutation did not report a committed result."
      {:error/type :humanhelp.example/request-not-committed
       :result result})))
  (let [committed-progression
        (or
         (:progression result)
         (throw
          (ex-info
           "Human Help committed production Request mutation is missing authoritative progression."
           {:error/type :humanhelp.example/missing-request-progression
            :result-keys (when (map? result) (set (keys result)))})))]
    (live/with-progression
     ctx
     (progression/compose
      (live/progression ctx)
      committed-progression))))

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

(defn- send-reset-toast-safely!
  []
  (try
    (app-live/send-reset-toast!)
    (catch Exception e
      (println "[humanhelp] send-reset-toast! failed"
               {:message (.getMessage e)}))))

(defn- notify-reset-safely!
  [result user]
  (try
    (notify!
     (committed-ctx result)
     (transaction-bound-change
      result
      (app-live/demo-reset-change
       {:revision (:revision result)
        :actor    user})))
    (catch Exception e
      ;; Reset is already committed. Delivery failure must not make the HTTP
      ;; boundary report that the mutation itself failed.
      (println
       "[humanhelp] demo reset notification failed"
       {:message     (.getMessage e)
        :revision    (:revision result)
        :progression (get-in result [:transaction :progression])}))))

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

   The committed transaction context carries the new Gesso progression. The
   board presentation state therefore needs no application-local revision
   advancement. The response renders toolbar/list from that committed context
   and closes/resets the dialog synchronously."
  [ctx {:keys [result view-state]}]
  (let [{:keys [request]} result
        ctx'        (production-result-ctx ctx result)
        user        (current-user ctx')
        view-state' (normalized-view-state ctx' view-state)
        fragments   (board-fragments ctx' view-state')]
    (html
     (with-board-state-oob
       ctx'
       view-state'
       (views/create-request-success
        ctx'
        (merge
         {:user    user
          :request request}
         fragments))))))

(defn create-request!
  "Create a production Request from the modal dialog.

   The HTTP boundary performs transport extraction and optional pre-validation
   through the public Request content helpers.  Authoritative identity,
   Organization/Location validity, Request construction, atomic commit, and
   semantic Live publication all belong to request.core/create and its model
   dependencies.

   The creator rereads the production board from the committed progression.
   Other connected browsers learn the new Request through the canonical
   production :request Live change emitted by Request FX; this route does not
   synthesize a second example-specific Request notification."
  [ctx]
  (let [user
        (current-user ctx)

        view-state
        (request-view-state ctx)

        input
        (create-request-input ctx)

        errors
        (create-request-errors input)]
    (if
     (seq errors)
      (html
       (views/create-request-validation-error
        ctx
        {:user
         user

         :values
         (create-request-view-values
          ctx
          input)

         :errors
         errors}))

      (let [actor-ctx
            (assoc
             ctx
             :current-user/id
             (:user/id user))

            result
            (request/create
             actor-ctx
             input)]
        (create-request-success-response
         actor-ctx
         {:result
          result

          :view-state
          view-state})))))

;; -----------------------------------------------------------------------------
;; Request list interactions
;; -----------------------------------------------------------------------------

(defn refresh-requests!
  "Refresh the visible request board from the request's current Live context."
  [ctx]
  (let [view-state (normalized-view-state ctx (request-view-state ctx))]
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

   The board-options dialog submits current search/presentation state from the
   stable board-state form plus its own visible option controls. The response
   replaces board state first, then toolbar and request list, so subsequent requests
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

(def optimistic-command-param
  "HTMX parameter installed by Gesso's protocol-v3 optimistic browser bridge."
  "__gesso_live_optimistic_command")

(defn- optimistic-command-wire
  "Read one protocol-v3 optimistic command wire value from the HTMX request.

   The browser bridge serializes one portable command envelope as EDN in a
   normal request parameter. EDN parsing establishes only transport shape;
   example.optimistic/decode-command performs the protocol-v3 validation and
   example.optimistic/run-command establishes authenticated production
   authority."
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

(defn- route-request-id!
  "Return the production Request UUID selected by the concrete HTTP route.

   Route strings are transport data. Production Request choreography and model
   operations are UUID based, so malformed ids fail at the HTTP boundary rather
   than being compared later against a typed command id."
  [ctx]
  (let [raw (request-id ctx)]
    (or
     (->uuid raw)
     (throw
      (ex-info
       "Human Help Request lifecycle route requires a UUID Request id."
       {:error/type :humanhelp.example/invalid-request-id
        :request-id raw})))))

(defn- settlement-request
  [settlement]
  (get-in settlement [:authoritative :projection]))

(defn- lifecycle-response-extra
  "Render only direct-settlement extras that do not compete with managed Live.

   Production request.core operations already commit and publish their semantic
   Request changes through gesso.model.tx/transact! -> Live. The direct HTTP
   response therefore carries only board-state continuity and user feedback;
   canonical Request rendering belongs to the progression-safe Live refresh."
  [ctx {:keys [operation view-state settlement]}]
  (let [request    (settlement-request settlement)
        resolution (:resolution settlement)]
    (cond
      (contains? #{:confirmed :reconciled :already-incorporated}
                 resolution)
      (views/request-lifecycle-extras
       ctx
       {:action     operation
        :request    request
        :view-state view-state})

      (contains? #{:rejected :failed} resolution)
      (views/request-action-error
       {:result
        {:reason (:reason settlement)}})

      :else
      (throw
       (ex-info
        "Human Help optimistic lifecycle response received an unsupported settlement resolution."
        {:operation  operation
         :settlement settlement})))))

(defn- lifecycle-action!
  "Execute one production Request choreography selected by an HTTP route.

   The route owns transport binding only:

     browser EDN command
       -> protocol-v3 decode
       -> bind semantic operation + Request UUID to this concrete route
       -> example.optimistic/run-command
       -> production request.choreo operation entry
       -> request.core authoritative operation

   No Request policy, principal selection, settlement construction, mutation,
   notification, or authoritative basis construction is implemented here.
   Production Request FX/Gesso model transactions already publish progression-
   bound Live changes after commit, so this route must not emit a duplicate
   application-level lifecycle notification."
  [ctx operation]
  (let [view-state
        (normalized-view-state
         ctx
         (request-view-state ctx))

        request-id
        (route-request-id! ctx)

        command
        (optimistic/decode-command
         (optimistic-command-wire ctx))

        command
        (optimistic/require-request-command!
         command
         {:operation operation
          :request-id request-id})

        prepared
        (optimistic/run-command
         ctx
         command)

        settlement
        (:settlement prepared)

        extra
        (lifecycle-response-extra
         ctx
         {:operation  operation
          :view-state view-state
          :settlement settlement})]
    (optimistic-html
     prepared
     extra)))

(defn claim-request!
  [ctx]
  (lifecycle-action! ctx :request/claim))

(defn unclaim-request!
  [ctx]
  (lifecycle-action! ctx :request/unclaim))

(defn mark-on-the-way-request!
  [ctx]
  (lifecycle-action! ctx :request/mark-on-the-way))

(defn complete-request!
  [ctx]
  (lifecycle-action! ctx :request/complete))

(defn cancel-request!
  [ctx]
  (lifecycle-action! ctx :request/cancel))

(defn reassign-request!
  [ctx]
  (lifecycle-action! ctx :request/reassign))

;; -----------------------------------------------------------------------------
;; Dev/demo reset
;; -----------------------------------------------------------------------------

(defn reset-demo!
  [ctx]
  (let [user       (current-user ctx)
        result     (model/reset-demo-state! ctx)
        ctx'       (committed-ctx result)
        view-state (normalized-view-state ctx' (request-view-state ctx))]
    (notify-reset-safely! result user)

    (send-reset-toast-safely!)

    (html
     (with-board-state-oob
       ctx'
       view-state
       (views/reset-demo-result
        (merge
         {:user       user
          :result     result
          :view-state view-state}
         (board-fragments ctx' view-state)))))))

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

   routes/claim-request-id           claim-request!
   routes/unclaim-request-id         unclaim-request!
   routes/mark-on-the-way-request-id mark-on-the-way-request!
   routes/complete-request-id        complete-request!
   routes/cancel-request-id          cancel-request!
   routes/reassign-request-id        reassign-request!

   routes/reset-demo-id reset-demo!})

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:live-rules app-live/live-rules
   :routes     (routes/route-table
                handlers
                {:middleware [mid/wrap-signed-in]})})
