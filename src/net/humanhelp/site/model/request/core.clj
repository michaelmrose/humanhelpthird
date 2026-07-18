(ns net.humanhelp.site.model.request.core
  "The supported public facade for the HumanHelp Request model.

   Code outside net.humanhelp.site.model.request should require this namespace
   instead of depending directly on Request domain, Graph, schema, or FX
   implementation namespaces.

   This facade exposes:

   - the Request model's Biff module contribution;
   - stable Graph query contracts and named Request reads;
   - the currently supported effectful Request operations;
   - selected pure Requestor, content, identity, lifecycle, and assignment
     facts useful to other models, handlers, and views.

   It deliberately does not expose domain constructors, model command
   constructors, lifecycle mutation functions, guarded revision machinery,
   Graph resolver implementations, Graph input construction,
   authorization-version construction, or FX transaction planners.

   Capability-owned Request writes and supervisor override operations are not
   public because the current Request FX slice does not implement their
   authentication and policy."
  (:require
   [gesso.graph :as graph]
   [net.humanhelp.site.model.request.domain.core :as request]
   [net.humanhelp.site.model.request.fx :as request.fx]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.request.schema :as request.schema]))

;; =============================================================================
;; Model registration
;; =============================================================================

(def schema
  "Malli schemas contributed by the Request model."
  request.schema/schema)

(def resolvers
  "Gesso Graph resolvers contributed by the Request model."
  request.graph/resolvers)

;; net.humanhelp.site.model.fx/module must be installed separately, exactly once
;; for the application. Request FX uses that shared transaction handler and does
;; not contribute another transaction implementation.
(def module
  "Biff module contribution for the Request model."
  {:schema schema
   :biff.graph/resolvers resolvers})

;; =============================================================================
;; Public Graph query contracts
;; =============================================================================

(def request-document-query
  request.graph/request-document-query)

(def request-command-query
  "Loads one Request lookup result with :request/found?, optional
   :request/doc, and optional :request/expected-version.

   This contract is useful to another model that must compose current Request
   state into one larger authorized operation."
  request.graph/request-command-query)

(def request-query
  "Loads one Request lookup result with the persisted document, Request-owned
   projected fields, lifecycle facts, assignment facts, and expected-version
   metadata.

   User access, Organization hierarchy, and current-actor permissions are
   intentionally absent."
  request.graph/request-facts-query)

(def location-requests-query
  "Loads the canonical Request collection for one Organization Location.

   Collection members contain Request-owned persisted, projected, lifecycle,
   assignment, and expected-version facts. Results are ordered newest first
   with Request ID as a deterministic tiebreaker.

   Organization validity, Location hierarchy, User access, current-actor
   permissions, and identity display enrichment are intentionally absent."
  request.graph/location-requests-query)

;; =============================================================================
;; Named Request reads
;; =============================================================================

(defn request-facts
  "Loads one Request by UUID.

   The result follows request-query and contains :request/found? plus optional
   Request-owned facts. This read does not establish current-actor authority,
   capability ownership, helper eligibility, or Location operational state."
  [ctx request-id]
  (graph/query
   ctx
   (request.graph/request-query-input
    {:request-id request-id})
   request-query))

(defn request-command-facts
  "Loads the minimal current Request facts needed to compose another model
   operation.

   Prefer request-facts for handlers and views."
  [ctx request-id]
  (graph/query
   ctx
   (request.graph/request-query-input
    {:request-id request-id})
   request-command-query))

(defn location-requests
  "Loads canonical Requests for one Organization Location.

   input:

     {:organization-id   organization-id
      :location-id       location-id
      :include-terminal? optional-boolean}

   Terminal Requests are excluded unless :include-terminal? is exactly true.
   The result follows location-requests-query:

     {:request/location-requests [...]}

   This read does not validate the Organization hierarchy or establish that the
   current actor may view the Location. Application code must compose those
   concerns through the Organization and User facades."
  [ctx input]
  (graph/query
   ctx
   (request.graph/location-requests-query-input
    input)
   location-requests-query))

;; =============================================================================
;; Supported Request operations
;; =============================================================================

(defn create-request
  "Creates one User-owned Request at an operational Location.

   input:

     {:organization-id organization-id
      :location-id     location-id
      :content         {:title title
                        :details optional-details
                        :location-detail optional-within-location-detail}}

   The authenticated User must be active. Organization hierarchy and Location
   operational state are reloaded and guarded in the transaction."
  [ctx input]
  (request.fx/create-request
   ctx
   input))

(defn edit-request
  "Edits the content of an active User-owned Request.

   input:

     {:request-id request-id
      :content    {:title title
                   :details optional-details
                   :location-detail optional-within-location-detail}}"
  [ctx input]
  (request.fx/edit-request
   ctx
   input))

(defn claim-request
  "Claims one open Request for the authenticated effective helper.

   input:

     {:request-id request-id}"
  [ctx input]
  (request.fx/claim-request
   ctx
   input))

(defn unclaim-request
  "Returns the authenticated helper's claimed Request to open.

   Current assignment ownership and active User identity are required. The
   Location need not remain operational and the helper role may already have
   been revoked.

   input:

     {:request-id request-id}"
  [ctx input]
  (request.fx/unclaim-request
   ctx
   input))

(defn mark-request-on-the-way
  "Marks the authenticated helper's claimed Request on the way.

   input:

     {:request-id request-id}"
  [ctx input]
  (request.fx/mark-request-on-the-way
   ctx
   input))

(defn complete-request
  "Completes the authenticated helper's claimed or on-the-way Request.

   input:

     {:request-id request-id}"
  [ctx input]
  (request.fx/complete-request
   ctx
   input))

(defn cancel-request
  "Cancels an active User-owned Request.

   The Location must still exist and belong to the stored Organization, but it
   need not remain operational.

   input:

     {:request-id request-id
      :reason     optional-qualified-keyword}"
  [ctx input]
  (request.fx/cancel-request
   ctx
   input))

(def operations
  "Public Request operation registry. Entries point at this facade rather than
   the internal FX namespace."
  {:request/create
   #'create-request

   :request/edit
   #'edit-request

   :request/claim
   #'claim-request

   :request/unclaim
   #'unclaim-request

   :request/mark-on-the-way
   #'mark-request-on-the-way

   :request/complete
   #'complete-request

   :request/cancel
   #'cancel-request})

;; =============================================================================
;; Shared Request values
;; =============================================================================

(def request-entity-type
  request/entity-type)

(def operation-order
  request/operation-order)

(def operations-set
  request/operations)

(def requestor-types
  request/requestor-types)

(def status-order
  request/status-order)

(def statuses
  request/statuses)

(def active-statuses
  request/active-statuses)

(def assigned-statuses
  request/assigned-statuses)

(def terminal-statuses
  request/terminal-statuses)

(def title-max
  request/title-max)

(def details-max
  request/details-max)

(def location-detail-max
  request/location-detail-max)

(defn operation?
  [value]
  (request/operation?
   value))

;; =============================================================================
;; Requestor values and facts
;; =============================================================================

(defn requestor-type?
  [value]
  (request/requestor-type?
   value))

(defn requestor-reference?
  [value]
  (request/requestor-reference?
   value))

(defn user-requestor
  [user-id]
  (request/user-requestor
   user-id))

(defn capability-requestor
  [capability-id]
  (request/capability-requestor
   capability-id))

(defn user-requestor?
  [value]
  (request/user-requestor?
   value))

(defn capability-requestor?
  [value]
  (request/capability-requestor?
   value))

(defn requestor
  [request-document]
  (request/requestor
   request-document))

(defn requestor-type
  [request-document]
  (request/requestor-type
   request-document))

(defn requestor-id
  [request-document]
  (request/requestor-id
   request-document))

(defn requested-by-user?
  [request-document user-id]
  (request/requested-by-user?
   request-document
   user-id))

(defn requested-by-capability?
  [request-document capability-id]
  (request/requested-by-capability?
   request-document
   capability-id))

(defn requested-by?
  [request-document requestor-reference]
  (request/requested-by?
   request-document
   requestor-reference))

(defn controlled-by?
  "Returns true when supplied identity values equal the Request's stored
   requestor.

   This is not authentication or authorization. Callers must independently
   establish ownership of the User identity or Request capability."
  [request-document identity]
  (request/controlled-by?
   request-document
   identity))

;; =============================================================================
;; Request content values and facts
;; =============================================================================

(defn normalize-content
  [value]
  (request/normalize-content
   value))

(defn title?
  [value]
  (request/title?
   value))

(defn details?
  [value]
  (request/details?
   value))

(defn location-detail?
  [value]
  (request/location-detail?
   value))

(defn content?
  [value]
  (request/content?
   value))

(defn content
  [request-document]
  (request/content
   request-document))

(defn content-errors
  [value]
  (request/content-errors
   value))

(defn valid-content?
  [value]
  (request/valid-content?
   value))

(defn same-content?
  [request-document value]
  (request/same-content?
   request-document
   value))

;; =============================================================================
;; Request document identity and ownership
;; =============================================================================

(defn request-document?
  [request-document]
  (request/request-consistent?
   request-document))

(defn request-id
  [request-document]
  (request/request-id
   request-document))

(defn organization-id
  [request-document]
  (request/organization-id
   request-document))

(defn location-id
  [request-document]
  (request/location-id
   request-document))

(defn revision
  [request-document]
  (request/revision
   request-document))

(defn created-at
  [request-document]
  (request/created-at
   request-document))

(defn updated-at
  [request-document]
  (request/updated-at
   request-document))

(defn belongs-to-organization?
  [request-document organization-id]
  (request/belongs-to-organization?
   request-document
   organization-id))

(defn at-location?
  [request-document location-id]
  (request/at-location?
   request-document
   location-id))

(defn belongs-to-location?
  [request-document organization-id location-id]
  (request/belongs-to-location?
   request-document
   organization-id
   location-id))

;; =============================================================================
;; Lifecycle values and facts
;; =============================================================================

(defn status?
  [value]
  (request/status?
   value))

(defn status
  [request-document]
  (request/status
   request-document))

(defn open?
  [request-document]
  (request/open?
   request-document))

(defn claimed?
  [request-document]
  (request/claimed?
   request-document))

(defn on-the-way?
  [request-document]
  (request/on-the-way?
   request-document))

(defn done?
  [request-document]
  (request/done?
   request-document))

(defn cancelled?
  [request-document]
  (request/cancelled?
   request-document))

(defn active?
  [request-document]
  (request/active?
   request-document))

(defn terminal?
  [request-document]
  (request/terminal?
   request-document))

(defn next-status
  [request-document operation]
  (request/next-status
   request-document
   operation))

(defn transition-allowed?
  [request-document operation]
  (request/transition-allowed?
   request-document
   operation))

(defn editable?
  [request-document]
  (request/editable?
   request-document))

(defn claimable?
  [request-document]
  (request/claimable?
   request-document))

(defn unclaimable?
  [request-document]
  (request/unclaimable?
   request-document))

(defn markable-on-the-way?
  [request-document]
  (request/markable-on-the-way?
   request-document))

(defn completable?
  [request-document]
  (request/completable?
   request-document))

(defn cancellable?
  [request-document]
  (request/cancellable?
   request-document))

;; =============================================================================
;; Helper-assignment facts
;; =============================================================================

(defn helper-id
  "Returns the helper associated with the active or terminal Request."
  [request-document]
  (request/helper-id
   request-document))

(defn has-helper?
  "Returns true when the Request records a helper, including after completion
   or cancellation."
  [request-document]
  (request/has-helper?
   request-document))

(defn actively-assigned?
  "Returns true only while a helper is actively responsible for a claimed or
   on-the-way Request."
  [request-document]
  (request/actively-assigned?
   request-document))

(defn assigned-to?
  [request-document user-id]
  (request/assigned-to?
   request-document
   user-id))
