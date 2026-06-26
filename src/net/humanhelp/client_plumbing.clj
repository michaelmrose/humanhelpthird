(ns net.humanhelp.client-plumbing
  "App-owned adapter for connected-client OOB delivery.

   This namespace owns app policy around connected browser clients:

   - route placement
   - middleware
   - current user identity
   - generic client scopes
   - response wrapping
   - app-friendly OOB send helpers
   - generic toast helpers

   It intentionally does not own:

   - connected-client storage
   - browser client id generation
   - SSE listener mechanics
   - SSE stream lifecycle
   - pending OOB queues
   - low-level target delivery

   Those generic mechanics live in gesso.live.client.

   Feature-specific notification wording belongs in feature namespaces, e.g.
   net.humanhelp.humanhelp.live."
  (:require
   [gesso.core :as g]
   [gesso.live.client :as live-client]
   [net.humanhelp.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; App placement
;; -----------------------------------------------------------------------------

(def endpoint
  {:base-path "/app/client-plumbing"
   :stream-path "/app/client-plumbing/stream"
   :pending-path "/app/client-plumbing/pending"
   :client-id-param :client-id})

;; -----------------------------------------------------------------------------
;; Generic client scopes
;; -----------------------------------------------------------------------------

(def app-scope
  "Generic all-app connected-browser scope.

   Feature namespaces can target this when their demo/app is intentionally
   single-tenant or app-wide. More specific scopes can be added later when we
   design feature-driven client scope registration."
  [:app/all])

(defn user-scope
  [user-id]
  [:user (str user-id)])

;; -----------------------------------------------------------------------------
;; App identity
;; -----------------------------------------------------------------------------

(defn current-user-id
  "Return the app user id used for connected-client targeting.

   Biff auth usually gives us a session uid. We intentionally return a string
   because client targeting keys should be stable and easy to serialize/log."
  [ctx]
  (str
   (or (:user/id ctx)
       (:user/email ctx)
       (get-in ctx [:user :xt/id])
       (get-in ctx [:user :email])
       (get-in ctx [:session :uid])
       (get-in ctx [:session :user])
       (get-in ctx [:session :email])
       "demo-user")))

(defn current-user-email
  "Best-effort display email for app UI.

   This namespace does not query XTDB. If the surrounding app wants an exact
   email address, it can attach it to ctx before rendering."
  [ctx]
  (str
   (or (:user/email ctx)
       (get-in ctx [:user :email])
       (get-in ctx [:session :email])
       (get-in ctx [:params :email])
       (get-in ctx [:params "email"])
       (current-user-id ctx))))

(defn current-client
  "Return the app-defined client descriptor for this connected browser.

   Scopes are opaque to Gesso. This generic adapter registers:
   - a per-user scope
   - an app-wide scope

   Feature-specific scopes can be added later via an explicit registration hook
   rather than hardcoding demo feature names here."
  [ctx]
  (let [user-id (current-user-id ctx)]
    {:client/user-id user-id
     :client/scopes #{(user-scope user-id)
                      app-scope}}))

;; -----------------------------------------------------------------------------
;; Channel
;; -----------------------------------------------------------------------------

(defonce channel
  (live-client/channel
   {:id :net.humanhelp/client-oob
    :event "client-oob"
    :endpoint endpoint
    :client current-client}))

(defn reset-plumbing!
  "Clear connected clients and pending OOB fragments for this channel.

   Useful at the REPL during development."
  []
  (live-client/reset-channel! channel))

;; -----------------------------------------------------------------------------
;; Browser listener
;; -----------------------------------------------------------------------------

(defn new-client-id
  "Return a new opaque browser client id.

   This is a convenience wrapper for tests/dev code. Normal callers should use
   listener and let it allocate the id."
  []
  (live-client/new-client-id))

(defn- listener-options
  [client-id options]
  (let [options   (or options {})
        client-id (or client-id
                      (:client/id options)
                      (new-client-id))
        id        (or (:id options)
                      (str "client-plumbing-listener-" client-id))]
    (-> options
        (assoc :client/id client-id
               :id id)
        (update :attrs
                #(merge {:data-client-plumbing-listener true}
                        %)))))

(defn listener
  "Render the browser-side listener for one connected client.

   Call shapes:

     (listener ctx)

     (listener ctx client-id)

     (listener ctx options)

     (listener ctx client-id options)

   Options are passed through to gesso.live.client/listener after adding the
   Gessokit listener marker. Useful options include:

     :attrs
       Extra attrs for the SSE listener wrapper.

     :trigger-attrs
       Extra attrs for the pending-fetch trigger. Human Help uses this to add
       hx-include=\"#humanhelp-board-state\" so client-plumbing pending requests
       carry the receiving browser's board state."
  ([ctx]
   (listener ctx nil nil))

  ([ctx client-id-or-options]
   (if (map? client-id-or-options)
     (listener ctx nil client-id-or-options)
     (listener ctx client-id-or-options nil)))

  ([ctx client-id options]
   (live-client/listener
    channel
    ctx
    (listener-options client-id options))))

;; -----------------------------------------------------------------------------
;; Route handlers
;; -----------------------------------------------------------------------------

(defn stream
  [ctx]
  (live-client/stream-response channel ctx))

(defn pending
  [ctx]
  (if-let [fragment (live-client/drain-fragment! channel ctx)]
    (g/html-response fragment)
    (g/no-content)))

;; -----------------------------------------------------------------------------
;; Generic app send API
;; -----------------------------------------------------------------------------

(defn send!
  "Send arbitrary OOB fragments to a target.

   Target forms:
     :all
     [:client client-id]
     [:user user-id]
     [:scope scope]

   Fragments may be complete HTMX OOB Hiccup nodes:

     (g/oob-inner-html \"notification-count\" \"3\")
     (g/render-toast-oob toast)

   or functions of receiving ctx -> OOB Hiccup:

     (fn [ctx]
       (render-receiver-specific-oob ctx))

   Function fragments are rendered when the receiving browser drains pending
   work, so they can use that browser's params, session, user, and included
   board state."
  [to & fragments]
  (live-client/send!
   channel
   {:to to
    :fragments fragments}))

(defn send-to-client!
  "Send arbitrary OOB fragments to one connected browser client."
  [client-id & fragments]
  (apply live-client/send-to-client! channel client-id fragments))

(defn send-to-user!
  "Send arbitrary OOB fragments to every connected browser client for user-id."
  [user-id & fragments]
  (apply live-client/send-to-user! channel (str user-id) fragments))

(defn send-to-scope!
  "Send arbitrary OOB fragments to every connected browser client in scope."
  [scope & fragments]
  (apply live-client/send-to-scope! channel scope fragments))

(defn broadcast!
  "Send arbitrary OOB fragments to every connected client.

   This should stay explicit and rare."
  [& fragments]
  (apply live-client/broadcast! channel fragments))

;; -----------------------------------------------------------------------------
;; Scope/user exclusion helpers
;; -----------------------------------------------------------------------------

(defn client-user-id
  "Return the normalized user id from a connected-client descriptor.

   The descriptor shape is produced by current-client and stored by
   gesso.live.client."
  [client]
  (some-> (:client/user-id client) str))

(defn client-scopes
  "Return the scope set from a connected-client descriptor."
  [client]
  (set (:client/scopes client)))

(defn client-in-scope?
  [scope client]
  (contains? (client-scopes client) scope))

(defn target-client-ids-for-scope-except-user
  "Return connected client ids in scope, excluding all clients owned by user-id.

   This is app policy layered over gesso.live.client introspection. The generic
   live-client target forms intentionally stay simple; this adapter composes
   scope membership with app identity."
  [scope user-id]
  (let [excluded-user-id (some-> user-id str)]
    (->> (live-client/connected-clients channel)
         (keep
          (fn [[client-id client]]
            (when (and (client-in-scope? scope client)
                       (not= excluded-user-id
                             (client-user-id client)))
              client-id)))
         vec)))

(defn summarize-send-results
  [{:keys [target scope excluded-user-id client-ids fragment-count results]}]
  (let [sent (reduce + (map #(or (:sent %) 0) results))
        woke (reduce + (map #(or (:woke %) 0) results))]
    {:sent sent
     :woke woke
     :woke? (pos? woke)
     :target target
     :scope scope
     :excluded-user-id excluded-user-id
     :client-ids client-ids
     :fragment-count fragment-count
     :results results}))

(defn send-to-scope-except-user!
  "Send arbitrary OOB fragments to every connected browser client in scope
   except clients owned by excluded-user-id.

   This is app-policy fanout. Gesso scopes remain simple and generic; this
   adapter decides how to combine scope membership with current app identity."
  [scope excluded-user-id & fragments]
  (let [excluded-user-id (some-> excluded-user-id str)
        client-ids       (target-client-ids-for-scope-except-user
                          scope
                          excluded-user-id)
        results          (mapv
                          (fn [client-id]
                            (apply send-to-client! client-id fragments))
                          client-ids)]
    (summarize-send-results
     {:target [:scope-except-user scope excluded-user-id]
      :scope scope
      :excluded-user-id excluded-user-id
      :client-ids client-ids
      :fragment-count (count fragments)
      :results results})))

;; -----------------------------------------------------------------------------
;; Generic toast helpers
;; -----------------------------------------------------------------------------

(def default-toast
  {:variant :info
   :title "Live event"
   :description "The page received a live update."
   :duration 1000})

(defn normalize-toast
  [toast]
  (merge default-toast toast))

(defn toast-oob
  "Render one normalized toast as an HTMX OOB fragment."
  [toast]
  (g/render-toast-oob
   (normalize-toast toast)))

(defn send-toast!
  "Send one toast to an arbitrary connected-client target.

   Target forms are the same as send!:
     :all
     [:client client-id]
     [:user user-id]
     [:scope scope]"
  [to toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send! to (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-client!
  [client-id toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send-to-client! client-id (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-user!
  [user-id toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send-to-user! user-id (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-scope!
  [scope toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send-to-scope! scope (toast-oob toast'))
     {:toast toast'})))

(defn send-toast-to-scope-except-user!
  "Send one normalized toast to every connected browser client in scope except
   clients owned by excluded-user-id."
  [scope excluded-user-id toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (send-to-scope-except-user!
      scope
      excluded-user-id
      (toast-oob toast'))
     {:toast toast'})))

(defn broadcast-toast!
  "Broadcast one toast to every connected client.

   This should stay explicit and rare."
  [toast]
  (let [toast' (normalize-toast toast)]
    (merge
     (broadcast! (toast-oob toast'))
     {:toast toast'})))

;; -----------------------------------------------------------------------------
;; Introspection
;; -----------------------------------------------------------------------------

(defn connected-client-ids
  []
  (live-client/connected-client-ids channel))

(defn latest-client-id
  []
  (live-client/latest-client-id channel))

(defn pending-counts
  []
  (live-client/pending-counts channel))

(defn state-summary
  []
  (live-client/state-summary channel))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(def module
  {:routes
   [[(:base-path endpoint)
     {:middleware [mid/wrap-signed-in]}

     ["/stream" {:get stream}]
     ["/pending" {:get pending}]]]})
