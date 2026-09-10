(ns net.humanhelp.example.browser-realization
  "Physical browser realization for the HumanHelp example Request application.

   Semantic browser facts are owned by
   net.humanhelp.example.application-preflight/browser-declaration and embedded
   by Gesso's supported application-artifact builder. This namespace supplies
   only physical callbacks that cannot be derived from portable Choreo data.

   Deliberately, this namespace does not implement a second Request model.
   Provisional projection records only inert command context, provisional
   rendering leaves canonical Request content untouched while Gesso marks the
   stable Request-card target as provisional, and authoritative reacquisition
   re-enters the composed Gesso Live runtime through runtime/notify-fragment!.

   In particular, authoritative refresh must never dispatch gesso:live-refresh
   directly. That event is an adapter-authorized physical effect. HumanHelp
   submits a logical invalidation; Gesso owns refresh admission, generation,
   single-flight, progression, and the eventual HTMX trigger."
  (:require
   [gesso.live.browser.runtime :as browser-runtime]))

;; =============================================================================
;; Managed Live coordinates
;; =============================================================================

(def ^:private managed-fragment-selector
  "[data-gesso-live-fragment]")

(def ^:private managed-fragment-attribute
  "data-gesso-live-fragment")

(def ^:private request-list-target-selector
  "[data-humanhelp-fragment=\"request-list\"]")

(defn- browser-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :net.humanhelp.example.browser-realization/error
     :error/kind kind}
    data)))

(defn- managed-fragment-root
  [target]
  (or
   (when target
     (.closest target managed-fragment-selector))
   (some-> (.querySelector js/document request-list-target-selector)
           (.closest managed-fragment-selector))))

(defn- managed-fragment-id!
  [target target-id reason]
  (let [root (managed-fragment-root target)
        fragment-id
        (some-> root
                (.getAttribute managed-fragment-attribute)
                str
                not-empty)]
    (when-not fragment-id
      (throw
       (browser-error
        :missing-managed-fragment
        "HumanHelp optimistic target is not contained by a Gesso-managed Live fragment."
        {:target-id target-id
         :reason reason})))
    fragment-id))

(defn- default-runtime!
  []
  (or
   (browser-runtime/default-runtime)
   (throw
    (browser-error
     :missing-gesso-runtime
     "HumanHelp optimistic authoritative refresh requires the initialized Gesso browser runtime."
     {}))))

;; =============================================================================
;; Optimistic physical realization
;; =============================================================================

(defn- project-provisional
  "Return inert provisional presentation context for one verified command.

   This does not predict Request domain state. The trusted server remains the
   only authority for Request transitions. Operation/arguments/scope are copied
   only as portable presentation/diagnostic context for the provisional value."
  [{:keys [command arguments scope]}]
  {:operation (:operation command)
   :arguments arguments
   :scope scope})

(defn- render-provisional
  "Leave canonical Request content in place while Gesso marks it provisional.

   gesso.live.browser.optimistic owns provisional markers, rollback snapshots,
   lifecycle correlation, and HTMX reprocessing. Returning the same stable
   target deliberately adds no client-side Request state machine."
  [{:keys [target]}]
  target)

(defn- refresh-authority
  "Submit authoritative reacquisition through the composed Gesso Live adapter.

   The optimistic callback receives the physical Request-card target. HumanHelp
   discovers only the enclosing framework-owned fragment identity, then submits
   a logical invalidation through runtime/notify-fragment!. It never dispatches
   gesso:live-refresh itself and never performs a direct GET."
  [{:keys [target target-id reason]}]
  (let [fragment-id
        (managed-fragment-id!
         target
         target-id
         reason)]
    (browser-runtime/notify-fragment!
     (default-runtime!)
     fragment-id)
    nil))

;; =============================================================================
;; Gesso application-build boundary
;; =============================================================================

(def realization-options
  "Physical realization options consumed by Gesso's generated application entrypoint.

   The embedded BrowserAssemblyManifest owns browser role, operation plans,
   feature selection, and optimistic command transport. HumanHelp supplies only
   callbacks Gesso cannot derive from that manifest. Gesso derives :plan-for for
   the HTMX bridge from the exact embedded PlanRegistry."
  {:optimistic-options
   {:project-provisional project-provisional
    :render-provisional render-provisional
    :refresh-authority refresh-authority}

   :optimistic-htmx-options {}})
