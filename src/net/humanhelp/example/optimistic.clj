(ns net.humanhelp.example.optimistic
  "Production Request Choreo server boundary for the HumanHelp example app.

   This namespace is intentionally small. It exists so the example HTTP/UI
   layers can exercise the exact production Request choreography without
   rebuilding optimistic operations around the obsolete example model.

   Authority flow:

     authenticated HumanHelp request context
       -> production User identity
       -> typed Choreo principal
       -> trusted Gesso optimistic server
       -> request.choreo/operation-entries
       -> request.core authoritative operation

   The operation registry is the production registry verbatim. This namespace
   does not wrap, reinterpret, or duplicate Request lifecycle semantics.

   Browser commands remain untrusted. The browser may propose a semantic
   operation, request id, observed basis, scope, fact versions, and provisional
   state, but it cannot choose a server operation outside the production
   registry and cannot establish its own principal.

   The example HTTP layer may additionally bind a command to the operation and
   Request id encoded by a concrete route. require-request-command! provides
   that fail-closed route check without knowing anything about Request policy.

   Request model operations require :current-user/id in ctx. The example's
   authentication boundary historically accepted the same identity sources as
   connected-client routing, including Biff session uid. production-context
   canonicalizes that already-authenticated identity to the UUID expected by the
   production model before execution, keeping the Choreo principal and Request
   actor identical.

   There is deliberately no dependency on net.humanhelp.example.model."
  (:require
   [com.biffweb.fx :as fx]
   [gesso.choreo.identity :as choreo.identity]
   [gesso.live.core :as live]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.util UUID]))

;; =============================================================================
;; Trusted authenticated identity
;; =============================================================================

(defn- ->uuid
  [value]
  (cond
    (uuid? value)
    value

    (string? value)
    (try
      (UUID/fromString value)
      (catch Exception _
        nil))

    :else
    nil))

(defn authenticated-user-id
  "Return the authenticated production User UUID for ctx.

   Identity-source precedence remains owned by client-plumbing/current-user-id,
   so connected-client routing and optimistic command authority cannot silently
   disagree about which signed-in User one request represents.

   The selected identity must be a UUID because production HumanHelp model
   authority is UUID-based. A malformed higher-precedence identity fails closed
   rather than falling through to another possibly stale identity source."
  [ctx]
  (let [canonical-id
        (client-plumbing/current-user-id ctx)]
    (or
     (->uuid canonical-id)
     (throw
      (ex-info
       "HumanHelp example optimistic authority requires a UUID production User identity."
       {:error/type
        :net.humanhelp.example.optimistic/invalid-user-id

        :user/id
        canonical-id})))))

(defn authenticated-user
  "Read and validate the authoritative production User represented by ctx."
  [ctx]
  (user/require-user
   ctx
   (authenticated-user-id ctx)))

(defn production-context
  "Return ctx with the canonical authenticated production User id installed at
   the model authority key :current-user/id.

   Requiring the User first makes a signed-in identity that no longer resolves
   to a production User an explicit integration/authentication failure rather
   than allowing Choreo principal binding and model actor identity to diverge."
  [ctx]
  (let [user-document
        (authenticated-user ctx)

        user-id
        (user/user-id user-document)]
    (when-not (uuid? user-id)
      (throw
       (ex-info
        "HumanHelp production User resolved to a non-UUID identity."
        {:error/type
         :net.humanhelp.example.optimistic/invalid-production-user

         :user
         user-document

         :user/id
         user-id})))
    (assoc ctx :current-user/id user-id)))

(defn- principal
  "Resolve the trusted typed Choreo principal from a production-context.

   run-command installs and validates :current-user/id before Gesso invokes this
   resolver. Keeping the server private ensures callers cannot bypass that
   canonicalization/read boundary. Browser command data never participates in
   principal selection."
  [ctx]
  (let [user-id (:current-user/id ctx)]
    (when-not (uuid? user-id)
      (throw
       (ex-info
        "HumanHelp example optimistic server received a non-production context."
        {:error/type
         :net.humanhelp.example.optimistic/invalid-production-context

         :current-user/id
         user-id})))
    (choreo.identity/principal user-id)))

;; =============================================================================
;; Exact production Request operation registry
;; =============================================================================

(def operation-entries
  "The exact production Request trusted operation registry.

   This alias is intentionally not a transformed copy. The example app must
   exercise the operation entries owned by site.model.request.choreo itself."
  request.choreo/operation-entries)

(def supported-operations
  "Semantic Request operations accepted by the example optimistic boundary."
  (set
   (keys operation-entries)))

(def ^:private server
  "Trusted protocol-v3 optimistic server for production Request choreography.

   Keep this private so execution cannot bypass production-context. The public
   integration seam is run-command."
  (live/optimistic-server
   {:principal-fn principal
    :operations operation-entries}))

;; =============================================================================
;; HTTP-route command binding
;; =============================================================================

(defn require-request-command!
  "Fail closed unless command matches the operation and Request id selected by
   the concrete HTTP route.

   This is route binding, not authorization. The production operation registry
   and Request model still authenticate/revalidate all semantic authority.

   expected:

     {:operation  :request/claim
      :request-id <uuid>}

   The command may contain additional operation-specific arguments (for example
   :helper-id on reassign); this function only prevents operation substitution
   and Request retargeting."
  [command {:keys [operation request-id] :as expected}]
  (when-not (map? command)
    (throw
     (ex-info
      "HumanHelp example optimistic route expected a decoded command map."
      {:error/type
       :net.humanhelp.example.optimistic/invalid-command

       :command command
       :expected expected})))

  (when-not (contains? supported-operations operation)
    (throw
     (ex-info
      "HumanHelp example route requested an operation outside the production Request choreography registry."
      {:error/type
       :net.humanhelp.example.optimistic/unsupported-route-operation

       :operation operation
       :supported-operations supported-operations})))

  (when-not (= operation (:operation command))
    (throw
     (ex-info
      "HumanHelp example optimistic command does not match the route operation."
      {:error/type
       :net.humanhelp.example.optimistic/route-operation-mismatch

       :expected-operation operation
       :actual-operation (:operation command)})))

  (when-not (= request-id
               (get-in command [:arguments :request-id]))
    (throw
     (ex-info
      "HumanHelp example optimistic command Request id does not match the route Request id."
      {:error/type
       :net.humanhelp.example.optimistic/route-request-mismatch

       :expected-request-id request-id
       :actual-request-id (get-in command [:arguments :request-id])
       :operation operation})))

  command)

;; =============================================================================
;; Execution facade
;; =============================================================================

(def run-command-machine
  (fx/machine
   ::run-command-machine
   :start
   (fn [{::keys [command]
         :as ctx}]
     {:biff.fx/return
      (live/run-optimistic-command
       server
       (production-context
        (dissoc ctx ::command))
       command)})))

(defn decode-command
  "Decode and validate one protocol-v3 browser command wire value."
  [wire-command]
  (live/decode-optimistic-command wire-command))

(defn run-command
  "Execute one decoded protocol-v3 command against production Request Choreo.

   The model-facing ctx is first canonicalized through production-context so
   Request authority and the Choreo principal are bound to the same User UUID.
   The returned value is Gesso's prepared settlement send; this namespace does
   not render HTTP or reinterpret settlement meaning."
  [ctx command]
  (run-command-machine
   (assoc
    ctx
    ::command
    command)))
