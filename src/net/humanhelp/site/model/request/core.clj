(ns net.humanhelp.site.model.request.core
  "The supported public facade for the HumanHelp Request model.

   Code outside net.humanhelp.site.model.request should require this namespace
   instead of depending directly on Request domain, Assignment, Graph, schema,
   or FX implementation namespaces.

   Request owns two persisted entity types:

   - Request documents own organization/location, requestor, customer-editable
     content, and lifecycle;
   - Request Assignment documents own primary-helper and collaborator
     participation.

   This facade exposes stable Request-owned reads, the supported effectful
   operations, and selected pure read-only facts. It deliberately does not
   expose constructors, model command constructors, lifecycle mutation
   functions, Graph resolver implementations, Graph input builders,
   authorization proofs, workflow machines, or transaction planners."
  (:require
   [gesso.graph :as graph]
   [net.humanhelp.site.model.request.assignment :as assignment]
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

(def assignment-document-query
  "Graph shape for one complete persisted Request Assignment document."
  request.graph/assignment-document-query)

(def request-command-query
  "Loads one Request plus active Request Assignments and optimistic-concurrency
   metadata. This is useful when another model must compose current Request
   state into a larger authorized operation."
  request.graph/request-command-query)

(def request-query
  "Loads one Request with Request-owned projections, lifecycle facts, active
   assignments, and aggregate primary/collaborator facts."
  request.graph/request-facts-query)

(def assignment-query
  "Loads one Request Assignment with assignment-owned projections and lifecycle
   facts."
  request.graph/assignment-facts-query)

(def active-assignments-query
  "Loads active Request Assignments for one Request."
  request.graph/active-assignments-query)

(def assignment-history-query
  "Loads all current Request Assignment records for one Request, including
   ended assignments."
  request.graph/assignment-history-query)

(def location-requests-query
  "Graph query for the canonical Request collection at one Location."
  request.graph/location-requests-query)

;; =============================================================================
;; Named Request reads
;; =============================================================================

(defn request-facts
  "Returns Request-owned facts for request-id.

   A missing Request returns {:request/found? false}. The result includes active
   Request Assignments but not User profiles, Organization hierarchy, Location
   operational state, or actor authorization."
  [ctx request-id]
  (graph/query
   ctx
   (assoc
    (request.graph/request-query-input
     {:request-id request-id})
    :request-assignment/include-ended?
    false)
   request-query))

(defn request-command-facts
  "Returns the current Request, active assignments, and expected-version facts
   used when composing an authorized cross-model write."
  [ctx request-id]
  (graph/query
   ctx
   (assoc
    (request.graph/request-query-input
     {:request-id request-id})
    :request-assignment/include-ended?
    false)
   request-command-query))

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

   input:
     {:organization-id   uuid
      :location-id       uuid
      :include-terminal? boolean}

   :include-terminal? defaults to false. Results are newest first with Request
   ID as a deterministic tiebreaker. Each item contains active Request
   Assignments and aggregate primary/collaborator facts."
  [ctx input]
  (graph/query
   ctx
   (request.graph/location-requests-query-input
    input)
   location-requests-query))

(defn location-request-items
  "Returns the vector under :request/location-requests."
  [ctx input]
  (or
   (:request/location-requests
    (location-requests
     ctx
     input))
   []))

;; =============================================================================
;; Named Request Assignment reads
;; =============================================================================

(defn assignment-facts
  "Returns Request Assignment facts for assignment-id.

   A missing assignment returns {:request-assignment/found? false}."
  [ctx assignment-id]
  (graph/query
   ctx
   (request.graph/assignment-query-input
    {:assignment-id assignment-id})
   assignment-query))

(defn assignment-document
  "Returns one complete Request Assignment document, or nil when absent."
  [ctx assignment-id]
  (let [facts
        (assignment-facts
         ctx
         assignment-id)]
    (when
     (true?
      (:request-assignment/found? facts))
      (:request-assignment/doc facts))))

(defn request-assignments
  "Returns active Request Assignment facts for request-id."
  [ctx request-id]
  (graph/query
   ctx
   (request.graph/request-assignments-query-input
    {:request-id request-id
     :include-ended? false})
   active-assignments-query))

(defn request-assignment-history
  "Returns all current Request Assignment records for request-id, including
   ended assignments."
  [ctx request-id]
  (graph/query
   ctx
   (request.graph/request-assignments-query-input
    {:request-id request-id
     :include-ended? true})
   assignment-history-query))

(defn request-assignment-items
  "Returns the :request/assignments vector from request-assignments."
  [ctx request-id]
  (or
   (:request/assignments
    (request-assignments
     ctx
     request-id))
   []))

(defn request-assignment-history-items
  "Returns the :request/assignments vector from request-assignment-history."
  [ctx request-id]
  (or
   (:request/assignments
    (request-assignment-history
     ctx
     request-id))
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
  "Claims an open Request.

   With no :helper-id, the signed-in effective helper claims it personally.

   With :helper-id naming another User, the signed-in actor must have effective
   supervisor or administrator authority and the target must be an effective
   helper at the Request Location.

   Both paths perform the same domain action: the Request becomes :claimed and
   one active primary Request Assignment is created atomically."
  [ctx input]
  (request.fx/claim-request
   ctx
   input))

(defn unclaim-request
  "Returns the primary helper's claimed Request to open and ends its active
   Request Assignments atomically."
  [ctx input]
  (request.fx/unclaim-request
   ctx
   input))

(defn mark-request-on-the-way
  "Marks the active primary helper's claimed Request on the way."
  [ctx input]
  (request.fx/mark-request-on-the-way
   ctx
   input))

(defn complete-request
  "Completes the active primary helper's Request and ends all active Request
   Assignments atomically."
  [ctx input]
  (request.fx/complete-request
   ctx
   input))

(defn cancel-request
  "Cancels an active User-owned Request and ends all active Request Assignments
   atomically."
  [ctx input]
  (request.fx/cancel-request
   ctx
   input))

(defn add-collaborator
  "Adds an effective helper as an additional participant on an assigned Request.

   input:
     {:request-id uuid
      :helper-id  uuid
      :skill      optional organization-local skill string}

   The current primary helper authorizes this operation."
  [ctx input]
  (request.fx/add-collaborator
   ctx
   input))

(defn remove-collaborator
  "Ends one active collaborator assignment on a Request."
  [ctx input]
  (request.fx/remove-collaborator
   ctx
   input))

(defn reassign-request
  "Replaces the active primary helper on an already-claimed Request.

   This is distinct from manager assignment of an open Request: assigning an
   open Request uses claim-request with :helper-id because it is still the same
   claim domain event."
  [ctx input]
  (request.fx/reassign-request
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
     :add-collaborator
     :remove-collaborator
     :reassign

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

    :add-collaborator
    (add-collaborator
     ctx
     input)

    :remove-collaborator
    (remove-collaborator
     ctx
     input)

    :reassign
    (reassign-request
     ctx
     input)

    (throw
     (ex-info
      "The requested Request action is not supported."
      {:error/type
       :request/unsupported-operation
       :error/details
       {:operation
        operation}}))))

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
   #'cancel-request

   :request/add-collaborator
   #'add-collaborator

   :request/remove-collaborator
   #'remove-collaborator

   :request/reassign
   #'reassign-request})

;; =============================================================================
;; Stable Request vocabulary
;; =============================================================================

(def request-entity-type
  domain/request-entity-type)

(def assignment-entity-type
  assignment/entity-type)

(def statuses
  domain/statuses)

(def active-statuses
  domain/active-statuses)

(def terminal-statuses
  domain/terminal-statuses)

(def operations-set
  domain/operations)

(def assignment-roles
  assignment/roles)

(def assignment-statuses
  assignment/statuses)

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

(def lifecycle-expects-primary-assignment?
  domain/lifecycle-expects-primary-assignment?)

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

;; =============================================================================
;; Pure Request Assignment projections and predicates
;; =============================================================================

(def assignment-id
  assignment/assignment-id)

(def assignment-request-id
  assignment/request-id)

(def assignment-helper-id
  assignment/helper-id)

(def assignment-role
  assignment/role)

(def assignment-status
  assignment/status)

(def assignment-source
  assignment/source)

(def assignment-assigned-by
  assignment/assigned-by)

(def assignment-assigned-at
  assignment/assigned-at)

(def assignment-ended-at
  assignment/ended-at)

(def assignment-ended-by
  assignment/ended-by)

(def assignment-end-reason
  assignment/end-reason)

(def assignment-revision
  assignment/revision)

(def assignment-created-at
  assignment/created-at)

(def assignment-updated-at
  assignment/updated-at)

(def assignment-role?
  assignment/role?)

(def assignment-status?
  assignment/status?)

(def assignment-active?
  assignment/active?)

(def assignment-ended?
  assignment/ended?)

(def primary-assignment?
  assignment/primary?)

(def collaborator-assignment?
  assignment/collaborator?)

(def active-primary-assignment?
  assignment/active-primary?)

(def active-collaborator-assignment?
  assignment/active-collaborator?)

(def assignment-for-request?
  assignment/for-request?)

(def assignment-for-helper?
  assignment/for-helper?)

(def active-assignment-for-helper?
  assignment/active-for-helper?)

(def active-assignment-for-request?
  assignment/active-for-request?)

(def active-assignments
  assignment/active-assignments)

(def ended-assignments
  assignment/ended-assignments)

(def primary-assignments
  assignment/primary-assignments)

(def collaborator-assignments
  assignment/collaborator-assignments)

(def active-primary-assignments
  assignment/active-primary-assignments)

(def active-collaborator-assignments
  assignment/active-collaborator-assignments)

(def active-assignment-for-helper
  assignment/active-assignment-for-helper)

(def active-primary-assignment
  assignment/active-primary-assignment)

(def active-helper-ids
  assignment/active-helper-ids)

(def active-collaborator-helper-ids
  assignment/active-collaborator-helper-ids)

(def assignment-document?
  assignment/document-consistent?)

;; =============================================================================
;; Assignment facts from Request Graph results
;; =============================================================================

(defn assignment-documents
  "Extracts persisted Request Assignment documents from a Request Graph facts
   map or from one Location Request item."
  [request-facts]
  (mapv
   :request-assignment/doc
   (or
    (:request/assignments
     request-facts)
    [])))

(defn primary-assignment
  "Returns the one active primary Request Assignment from a Request facts map,
   nil when there is none, and throws if the facts contain multiple active
   primaries."
  [request-facts]
  (active-primary-assignment
   (assignment-documents
    request-facts)))

(defn collaborator-assignment-documents
  "Returns active collaborator Request Assignment documents from Request facts."
  [request-facts]
  (active-collaborator-assignments
   (assignment-documents
    request-facts)))
