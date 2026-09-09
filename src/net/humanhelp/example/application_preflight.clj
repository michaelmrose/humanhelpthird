(ns net.humanhelp.example.application-preflight
  "Canonical whole-application preflight assembly for the HumanHelp example Request board.

   This namespace is application composition, not model policy. Production Request
   semantics remain in net.humanhelp.site.model.request.*. The example application
   contributes only the physical facts it owns: concrete HTTP route ids, the compiled
   Live graph, and the generated browser artifact chosen by startup/build tooling.

   The central rule is to derive every other fact from those canonical sources. In
   particular, this namespace does not maintain a second operation registry, browser
   plan registry, publication registry, or fragment-obligation registry.

   The closed backbone assembled here is:

     Request browser ExecutablePlans
       -> semantic operation capabilities
       -> trusted Request operation entries
       -> concrete HumanHelp command routes
       -> the exact private prepared optimistic server
       -> settlement/publication contracts
       -> compiled HumanHelp Live graph
       -> managed Request board fragments
       -> concrete authoritative fragment/stream routes.

   require-application-assembly! extends that backbone through the physical generated
   browser artifact and optional rendered Hiccup snapshots. Canonical dynamic render
   enforcement and Biff startup installation remain owned by
   gesso.live.application-preflight and are wired by the application entrypoint rather
   than reimplemented here."
  (:require
   [gesso.live.acquisition-preflight :as acquisition-preflight]
   [gesso.live.application-preflight :as application-preflight]
   [gesso.live.browser.entrypoint :as browser-entrypoint]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [net.humanhelp.example.live :as app-live]
   [net.humanhelp.example.optimistic :as optimistic]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.site.model.request.choreo :as request.choreo]))

;; =============================================================================
;; Canonical semantic/browser slice
;; =============================================================================

(def request-operations
  "The semantic Request operation set exposed by the browser application slice.

   Derived from the production Request browser plans so adding/removing a Choreo
   operation changes this set automatically instead of requiring synchronized config."
  (set (keys request.choreo/browser-plans)))

(def browser-declaration
  "Canonical declaration accepted by Gesso's supported application browser build.

   The declaration embeds the exact production Request browser plans and derives the
   required operation set from those same plans. Build tooling may pass this value
   directly to gesso.live.browser.build/build-application-artifact!."
  {:name :net.humanhelp.example/request-browser
   :plans request.choreo/browser-plans
   :required-plan-keys request-operations
   :browser-role request.choreo/request-client-role
   :optimistic? true
   :optimistic-htmx? true})

(defn require-browser-assembly!
  "Return the exact BrowserAssemblyManifest compiled from browser-declaration.

   This deliberately uses Gesso's supported browser entrypoint compiler rather
   than reconstructing the PlanRegistry/BrowserAssembly in HumanHelp. The same
   declaration is therefore the single source for both generated-artifact stamping
   and the server-side application-preflight backbone."
  []
  (browser-entrypoint/compile-browser-assembly!
   browser-declaration))

(defn require-plan-registry!
  "Return the PlanRegistry embedded in the exact compiled browser declaration."
  []
  (:plan-registry (require-browser-assembly!)))

;; =============================================================================
;; Physical command-route realization
;; =============================================================================

(def request-operation-route-ids
  "The irreducible application-owned mapping from semantic operation to physical route.

   Method and path are deliberately *not* copied here; they are derived from the
   canonical route spec selected by each id."
  {request.choreo/claim-operation routes/claim-request-id
   request.choreo/unclaim-operation routes/unclaim-request-id
   request.choreo/mark-on-the-way-operation routes/mark-on-the-way-request-id
   request.choreo/complete-operation routes/complete-request-id
   request.choreo/cancel-operation routes/cancel-request-id
   request.choreo/reassign-operation routes/reassign-request-id})

(defn- absolute-route-path
  [route-id]
  (routes/path (:route (routes/route-spec route-id))))

(defn request-route-capabilities
  "Derive the exact trusted HTTP route capability for each exposed Request operation."
  []
  (into
   (sorted-map)
   (map
    (fn [[operation route-id]]
      (let [{:keys [method]} (routes/route-spec route-id)]
        [operation
         (route-preflight/route-capability
          {:operation operation
           :method method
           :path (absolute-route-path route-id)
           :transports #{:htmx}})])))
   request-operation-route-ids))

(defn require-operation-assembly!
  "Close Request semantic capabilities against their trusted server operations."
  []
  (operation-preflight/require-operation-assembly!
   {:name :net.humanhelp.example/request-operations
    :browser-assembly (require-browser-assembly!)
    :operation-capabilities request.choreo/capabilities
    :server-operations request.choreo/operation-entries}))

(defn require-route-assembly!
  "Close every exposed Request operation against its concrete HumanHelp command route."
  []
  (route-preflight/require-route-assembly!
   {:name :net.humanhelp.example/request-routes
    :operation-assembly (require-operation-assembly!)
    :route-capabilities (request-route-capabilities)}))

(defn require-execution-assembly!
  "Close the command-route assembly against the exact prepared server used at runtime."
  []
  (optimistic/require-execution-assembly!
   (require-route-assembly!)))

;; =============================================================================
;; Live authoritative reacquisition realization
;; =============================================================================

(def request-fragment-route-ids
  "Application-owned physical route ids for the two managed Request board fragments.

   Live semantic obligations are not repeated here; they are derived from compiled-live.
   This map states only which physical GET and stream routes realize each fragment."
  {:request-toolbar
   {:fragment-route-id routes/request-toolbar-fragment-id
    :stream-route-id routes/request-toolbar-stream-id}

   :request-list
   {:fragment-route-id routes/request-list-fragment-id
    :stream-route-id routes/request-list-stream-id}})

(defn- acquisition-realization
  [fragment {:keys [fragment-route-id stream-route-id]}]
  (acquisition-preflight/require-acquisition-realization!
   {:name (keyword "net.humanhelp.example"
                   (str (name fragment) "-acquisition"))
    :live-app app-live/compiled-live
    :fragment fragment
    :fragment-route
    (acquisition-preflight/fragment-route
     {:fragment fragment
      :path (absolute-route-path fragment-route-id)})
    :stream-route
    (acquisition-preflight/stream-route
     {:fragment fragment
      :path (absolute-route-path stream-route-id)})}))

(defn request-acquisition-realizations
  "Derive the physical authoritative acquisition realization for every managed board fragment."
  []
  (into
   (sorted-map)
   (map
    (fn [[fragment route-ids]]
      [fragment (acquisition-realization fragment route-ids)]))
   request-fragment-route-ids))

(defn require-acquisition-assembly!
  "Close the compiled HumanHelp Live graph against authoritative fragment/stream routes."
  []
  (acquisition-preflight/require-acquisition-assembly!
   {:name :net.humanhelp.example/request-acquisition
    :live-app app-live/compiled-live
    :realizations (request-acquisition-realizations)}))

(defn require-operation-acquisition-assembly!
  "Return the closed Request operation -> publication -> Live -> acquisition backbone."
  []
  (operation-acquisition-preflight/require-operation-acquisition-assembly!
   {:name :net.humanhelp.example/request-operation-acquisition
    :execution-assembly (require-execution-assembly!)
    :acquisition-assembly (require-acquisition-assembly!)}))

;; =============================================================================
;; Physical generated artifact / rendered-surface closure
;; =============================================================================

(defn require-application-assembly!
  "Require the current HumanHelp Request application assembly for one generated artifact.

   browser-artifact-path must name the exact generated JavaScript artifact selected by
   the application build/start path. Optional options are deliberately the same two
   physical additions HumanHelp can legitimately supply here:

     :browser-receipt-path
       Non-default Gesso ArtifactReceipt path.

     :rendered-surfaces
       Non-empty keyword -> rendered Hiccup snapshot map. These snapshots provide
       static affordance evidence; canonical dynamic pre-browser enforcement remains
       the completeness boundary for actual application responses.

   Everything semantic below the artifact is reconstructed from canonical HumanHelp
   sources on every call rather than accepted from a caller-provided assembly."
  ([browser-artifact-path]
   (require-application-assembly! browser-artifact-path {}))
  ([browser-artifact-path {:keys [browser-receipt-path rendered-surfaces]
                           :as options}]
   (let [unknown (seq (remove #{:browser-receipt-path :rendered-surfaces}
                              (keys options)))]
     (when unknown
       (throw
        (ex-info
         "HumanHelp application preflight received unsupported options."
         {:error/type :net.humanhelp.example.application-preflight/error
          :error/kind :unknown-options
          :unknown-keys (set unknown)
          :allowed-keys #{:browser-receipt-path :rendered-surfaces}}))))
   (application-preflight/require-application-assembly!
    (cond->
     {:name :net.humanhelp.example/request-application
      :operation-acquisition-assembly
      (require-operation-acquisition-assembly!)
      :browser-artifact-path browser-artifact-path}
      browser-receipt-path
      (assoc :browser-receipt-path browser-receipt-path)

      rendered-surfaces
      (assoc :rendered-surfaces rendered-surfaces)))))
