(ns net.humanhelp.site.model.request.core
  "The supported public facade for the HumanHelp Request model.

   Code outside net.humanhelp.site.model.request should require this namespace
   instead of depending directly on Request domain, Graph, schema, or FX
   implementation namespaces.

   This facade exposes:

   - the Request model's Biff module contribution;
   - stable Request-owned Graph query contracts;
   - named reads for one Request and one Location's Request collection;
   - the supported effectful Request operations;
   - selected pure Request values and predicates needed by handlers, views, and
     other models.

   It deliberately does not expose Request constructors, command constructors,
   lifecycle mutation functions, guarded revision machinery, Graph resolver
   implementations, raw XTDB2 query construction, authorization proofs,
   workflow machines, or transaction planners.

   net.humanhelp.site.model.fx/module must be installed separately, exactly once
   for the application. Request FX uses that shared transaction handler and
   publishes committed semantic changes through Gesso Live."
  (:require
   [gesso.graph :as graph]
   [net.humanhelp.site.model.request.domain :as domain]
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

(def module
  "Biff module contribution for the Request model."
  {:schema schema
   :biff.graph/resolvers resolvers})

;; =============================================================================
;; Public Graph query contracts
;; =============================================================================

(def request-document-query
  "Graph shape for one complete persisted Request document."
  request.graph/request-document-query)

(def request-query
  "Graph query for one Request with all Request-owned projections and lifecycle
   facts."
  request.graph/request-facts-query)

(def location-requests-query
  "Graph query for the canonical Request collection at one Location."
  request.graph/location-requests-query)

;; =============================================================================
;; Named reads
;; =============================================================================

(defn request-facts
  "Returns Request-owned facts for request-id.

   A missing Request returns {:request/found? false}. This read does not load
   Organization hierarchy, Location operational state, User profiles, or actor
   authorization."
  [ctx request-id]
  (graph/query
   ctx
   (request.graph/request-query-input
    {:request-id request-id})
   request-query))

(defn request-document
  "Returns the complete persisted Request document for request-id, or nil when
   no current Request exists."
  [ctx request-id]
  (let [facts
        (request-facts
         ctx
         request-id)]
    (when
     (true?
      (:request/found? facts))
      (:request/doc facts))))

(defn location-requests
  "Returns the canonical Request collection for one Organization Location.

   input is:

     {:organization-id  uuid
      :location-id      uuid
      :include-terminal? boolean}

   :include-terminal? defaults to false. Results are newest first with Request
   ID as a deterministic tiebreaker.

   This is a Request-owned collection read. Callers that need Location
   hierarchy, operational state, helper display data, or scoped access must
   compose those through the public Organization and User model facades."
  [ctx input]
  (graph/query
   ctx
   (request.graph/location-requests-query-input
    input)
   location-requests-query))

(defn location-request-items
  "Returns the vector under :request/location-requests from location-requests.

   This convenience keeps consumers from depending on the Graph envelope while
   preserving each item as a Request facts map."
  [ctx input]
  (or
   (:request/location-requests
    (location-requests
     ctx
     input))
   []))

;; =============================================================================
;; Effectful operations
;; =============================================================================

(defn create-request
  "Creates a User-owned Request at an operational Organization Location."
  [ctx input]
  (request.fx/create-request
   ctx
   input))

(defn edit-request
  "Edits an active Request owned by the signed-in User."
  [ctx input]
  (request.fx/edit-request
   ctx
   input))

(defn claim-request
  "Claims an open Request for the signed-in effective Location helper."
  [ctx input]
  (request.fx/claim-request
   ctx
   input))

(defn unclaim-request
  "Returns the signed-in assigned helper's claimed Request to open."
  [ctx input]
  (request.fx/unclaim-request
   ctx
   input))

(defn mark-request-on-the-way
  "Marks the signed-in effective helper's assigned Request on the way."
  [ctx input]
  (request.fx/mark-request-on-the-way
   ctx
   input))

(defn complete-request
  "Completes the signed-in effective helper's claimed or on-the-way Request."
  [ctx input]
  (request.fx/complete-request
   ctx
   input))

(defn cancel-request
  "Cancels an active Request owned by the signed-in User."
  [ctx input]
  (request.fx/cancel-request
   ctx
   input))

(defn perform-action
  "Dispatches one supported existing-Request operation.

   operation is one of:

     :edit
     :claim
     :unclaim
     :mark-on-the-way
     :complete
     :cancel

   Request creation remains explicit through create-request."
  [ctx operation input]
  (case operation
    :edit
    (edit-request
     ctx
     input)

    :claim
    (claim-request
     ctx
     input)

    :unclaim
    (unclaim-request
     ctx
     input)

    :mark-on-the-way
    (mark-request-on-the-way
     ctx
     input)

    :complete
    (complete-request
     ctx
     input)

    :cancel
    (cancel-request
     ctx
     input)

    (throw
     (ex-info
      "The requested Request action is not supported."
      {:error/type :request/unsupported-operation
       :error/details
       {:operation operation}}))))

(def operations
  "Public Request operation registry."
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
;; Stable Request vocabulary
;; =============================================================================

(def request-entity-type
  domain/request-entity-type)

(def statuses
  domain/statuses)

(def active-statuses
  domain/active-statuses)

(def terminal-statuses
  domain/terminal-statuses)

(def operations-set
  domain/operations)

;; =============================================================================
;; Requestor values
;; =============================================================================

(def requestor-reference?
  domain/requestor-reference?)

(def user-requestor
  domain/user-requestor)

(def capability-requestor
  domain/capability-requestor)

(def user-requestor?
  domain/user-requestor?)

(def capability-requestor?
  domain/capability-requestor?)

;; =============================================================================
;; Content values
;; =============================================================================

(def title-max
  domain/title-max)

(def details-max
  domain/details-max)

(def location-detail-max
  domain/location-detail-max)

(def normalize-content
  domain/normalize-content)

(def content?
  domain/content?)

(def content-errors
  domain/content-errors)

(def content
  domain/content)

;; =============================================================================
;; Pure Request projections and predicates
;; =============================================================================

(def request-id
  domain/request-id)

(def organization-id
  domain/organization-id)

(def location-id
  domain/location-id)

(def requestor
  domain/requestor)

(def status
  domain/status)

(def helper-id
  domain/helper-id)

(def revision
  domain/revision)

(def created-at
  domain/created-at)

(def updated-at
  domain/updated-at)

(def belongs-to-organization?
  domain/belongs-to-organization?)

(def at-location?
  domain/at-location?)

(def belongs-to-location?
  domain/belongs-to-location?)

(def requested-by?
  domain/requested-by?)

(def requested-by-user?
  domain/requested-by-user?)

(def requested-by-capability?
  domain/requested-by-capability?)

(def controlled-by?
  domain/controlled-by?)

(def open?
  domain/open?)

(def claimed?
  domain/claimed?)

(def on-the-way?
  domain/on-the-way?)

(def done?
  domain/done?)

(def cancelled?
  domain/cancelled?)

(def active?
  domain/active?)

(def terminal?
  domain/terminal?)

(def editable?
  domain/editable?)

(def has-helper?
  domain/has-helper?)

(def actively-assigned?
  domain/actively-assigned?)

(def assigned-to?
  domain/assigned-to?)

(def claimable?
  domain/claimable?)

(def unclaimable?
  domain/unclaimable?)

(def markable-on-the-way?
  domain/markable-on-the-way?)

(def completable?
  domain/completable?)

(def cancellable?
  domain/cancellable?)

(def request-document?
  domain/request-document-consistent?)
