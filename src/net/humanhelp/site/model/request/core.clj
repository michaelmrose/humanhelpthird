(ns net.humanhelp.site.model.request.core
  "Stable public boundary for the HumanHelp Request model.

   Code outside net.humanhelp.site.model.request should depend on this namespace
   rather than Request domain, schema, Graph, or FX internals.

   Request owns two persisted entity types:

   - Request owns organization/location identity, requestor ownership,
     customer-editable content, and lifecycle;
   - RequestAssignment owns primary-helper and collaborator participation in one
     Request, including assignment history.

   User owns global identity.
   Membership owns Organization-local membership, roles, skills, and
   authorization.
   Organization owns hierarchy, Location scope, and operational state.

   This namespace exposes:

   - model registration;
   - ordinary Request-owned reads;
   - guarded Request dependencies for cross-model composition;
   - Request transaction planners;
   - selected stable pure Request and RequestAssignment facts.

   Constructors, mutation functions, command constructors, schema internals,
   persistence predicates, and workflow implementation helpers remain private
   to the Request model."
  (:require
   [gesso.model.core :as model]
   [net.humanhelp.site.model.request.domain :as domain]
   [net.humanhelp.site.model.request.fx :as request.fx]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.request.schema :as request.schema]))

;; =============================================================================
;; Model registration
;; =============================================================================

(def module
  (model/build-module
   request.schema/descriptors
   {:schema
    request.schema/custom-schema

    :resolvers
    request.graph/custom-resolvers}))

(def schema
  (:schema
   module))

(def resolvers
  (:biff.graph/resolvers
   module))

;; =============================================================================
;; Stable entity vocabulary
;; =============================================================================

(def request-entity-type
  domain/request-entity-type)

(def assignment-entity-type
  domain/assignment-entity-type)

(def request-statuses
  domain/request-statuses)

(def active-request-statuses
  domain/active-request-statuses)

(def assigned-request-statuses
  domain/assigned-request-statuses)

(def terminal-request-statuses
  domain/terminal-request-statuses)

(def request-operations
  domain/request-operations)

(def request-document-operations
  domain/request-document-operations)

(def request-assignment-operations
  domain/request-assignment-operations)

(def assignment-roles
  domain/assignment-roles)

(def assignment-statuses
  domain/assignment-statuses)

;; =============================================================================
;; Requestor values
;; =============================================================================

(def requestor-type?
  domain/requestor-type?)

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
;; Request content values
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

(def valid-content?
  domain/valid-content?)

(def content
  domain/content)

;; =============================================================================
;; Ordinary Request reads
;; =============================================================================

(defn request
  "Returns one current Request by UUID, or nil when absent."
  [ctx request-id]
  (request.graph/request-document
   ctx
   request-id))

(defn require-request
  "Returns one current Request by UUID or throws when absent."
  [ctx request-id]
  (or
   (request
    ctx
    request-id)

   (throw
    (ex-info
     "The Request does not exist."
     {:error/type
      :request/not-found

      :error/details
      {:request/id
       request-id}}))))

(defn requests-for-location
  "Returns Requests persisted at one exact Organization Location.

   input:

     {:organization-id   uuid
      :location-id       uuid
      :include-terminal? boolean}

   :include-terminal? defaults to false.

   Results are newest first with Request UUID as a deterministic tiebreaker.

   This is a Request-owned persistence read. It does not establish that the
   Organization or Location currently exists or is operational."
  [ctx input]
  (request.graph/requests-for-location
   ctx
   input))

;; =============================================================================
;; Ordinary RequestAssignment reads
;; =============================================================================

(defn assignment
  "Returns one current RequestAssignment by UUID, or nil when absent."
  [ctx assignment-id]
  (request.graph/request-assignment-document
   ctx
   assignment-id))

(defn require-assignment
  "Returns one current RequestAssignment by UUID or throws when absent."
  [ctx assignment-id]
  (or
   (assignment
    ctx
    assignment-id)

   (throw
    (ex-info
     "The RequestAssignment does not exist."
     {:error/type
      :request-assignment/not-found

      :error/details
      {:request-assignment/id
       assignment-id}}))))

(defn assignments-for-request
  "Returns complete RequestAssignment history for Request, oldest first."
  [ctx request-id]
  (request.graph/assignments-for-request
   ctx
   request-id))

(defn active-assignments-for-request
  "Returns current active RequestAssignments for Request, oldest first."
  [ctx request-id]
  (request.graph/active-assignments-for-request
   ctx
   request-id))

(defn active-assignment-for-helper
  "Returns one helper's current RequestAssignment on Request, or nil.

   Persisted duplicate active assignments are treated as corruption."
  [ctx request-id helper-id]
  (request.graph/active-assignment-for-helper
   ctx
   request-id
   helper-id))

(defn active-primary-assignment-for-request
  "Returns Request's current primary RequestAssignment, or nil.

   Persisted duplicate active primaries are treated as corruption."
  [ctx request-id]
  (request.graph/active-primary-assignment-for-request
   ctx
   request-id))

;; =============================================================================
;; Request-owned aggregate reads
;; =============================================================================

(defn request-snapshot
  "Returns Request plus its current internally-consistent active
   RequestAssignment state, or nil when Request is absent.

   Result:

     {:request                        Request
      :assignments                    [RequestAssignment ...]
      :primary-assignment             RequestAssignment | nil
      :active-helper-ids              #{uuid ...}
      :active-collaborator-helper-ids #{uuid ...}}

   This snapshot contains only Request-owned facts. It does not establish
   Organization state or current Membership authorization."
  [ctx request-id]
  (request.graph/request-snapshot
   ctx
   request-id))

(defn require-request-snapshot
  "Returns Request's current Request-owned aggregate snapshot or throws when
   Request is absent."
  [ctx request-id]
  (request.graph/require-request-snapshot
   ctx
   request-id))

(defn request-snapshots-for-location
  "Returns canonical Location Requests enriched with current Request-owned
   assignment state."
  [ctx input]
  (request.graph/request-snapshots-for-location
   ctx
   input))

;; =============================================================================
;; Guarded dependencies
;; =============================================================================

(defn request-dependency
  "Returns Request plus a guard-only transaction fragment.

   Returns nil when Request does not exist."
  [ctx request-id]
  (request.fx/request-dependency
   ctx
   request-id))

(defn require-request-dependency
  "Returns request-dependency or throws when Request does not exist."
  [ctx request-id]
  (request.fx/require-request-dependency
   ctx
   request-id))

(defn assignment-dependency
  "Returns RequestAssignment plus a guard-only transaction fragment.

   Returns nil when RequestAssignment does not exist."
  [ctx assignment-id]
  (request.fx/assignment-dependency
   ctx
   assignment-id))

(defn require-assignment-dependency
  "Returns assignment-dependency or throws when RequestAssignment does not
   exist."
  [ctx assignment-id]
  (request.fx/require-assignment-dependency
   ctx
   assignment-id))

(defn request-snapshot-dependency
  "Returns Request's current Request-owned aggregate plus guards for the
   Request and every active RequestAssignment observed.

   Returns nil when Request does not exist."
  [ctx request-id]
  (request.fx/request-snapshot-dependency
   ctx
   request-id))

(defn require-request-snapshot-dependency
  "Returns request-snapshot-dependency or throws when Request does not exist."
  [ctx request-id]
  (request.fx/require-request-snapshot-dependency
   ctx
   request-id))

;; =============================================================================
;; Request transaction planners
;; =============================================================================

(defn plan-create-request
  "Plans creation of a User-owned Request at an operational Location.

   input:

     {:organization-id uuid
      :location-id     uuid
      :content         {:title string
                        :details optional-string
                        :location-detail optional-string}}"
  [ctx input]
  (request.fx/plan-create-request
   ctx
   input))

(defn plan-edit-request
  "Plans editing the active User requestor's Request content."
  [ctx input]
  (request.fx/plan-edit-request
   ctx
   input))

(defn plan-claim-request
  "Plans claiming an open Request.

   Without :helper-id, the signed-in effective helper claims personally.

   With another :helper-id, the actor must hold supervisor or administrator
   authority and the selected User must be an effective helper.

   Optional :skill constrains helper eligibility."
  [ctx input]
  (request.fx/plan-claim-request
   ctx
   input))

(defn plan-unclaim-request
  "Plans returning the primary helper's claimed Request to open and ending all
   active RequestAssignments."
  [ctx input]
  (request.fx/plan-unclaim-request
   ctx
   input))

(defn plan-mark-request-on-the-way
  "Plans moving the active primary helper's claimed Request to :on-the-way."
  [ctx input]
  (request.fx/plan-mark-request-on-the-way
   ctx
   input))

(defn plan-complete-request
  "Plans completing the active primary helper's Request and ending all active
   RequestAssignments."
  [ctx input]
  (request.fx/plan-complete-request
   ctx
   input))

(defn plan-cancel-request
  "Plans cancellation by the active User requestor and ends all active
   RequestAssignments."
  [ctx input]
  (request.fx/plan-cancel-request
   ctx
   input))

(defn plan-add-collaborator
  "Plans adding one effective helper as a collaborator to an assigned Request."
  [ctx input]
  (request.fx/plan-add-collaborator
   ctx
   input))

(defn plan-remove-collaborator
  "Plans ending one active collaborator assignment.

   The actor must own the current primary assignment."
  [ctx input]
  (request.fx/plan-remove-collaborator
   ctx
   input))

(defn plan-reassign-request
  "Plans replacing the primary helper on a claimed Request.

   The actor must hold supervisor or administrator authority and the selected
   User must be an effective helper."
  [ctx input]
  (request.fx/plan-reassign-request
   ctx
   input))

;; =============================================================================
;; Request projections
;; =============================================================================

(def request-id
  domain/request-id)

(def organization-id
  domain/organization-id)

(def location-id
  domain/location-id)

(def requestor-type
  domain/requestor-type)

(def requestor-id
  domain/requestor-id)

(def status
  domain/request-status)

(def revision
  domain/request-revision)

(def created-at
  domain/request-created-at)

(def updated-at
  domain/request-updated-at)

(def requestor
  domain/requestor)

;; =============================================================================
;; Request ownership and location facts
;; =============================================================================

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

;; =============================================================================
;; Request lifecycle facts
;; =============================================================================

(def request-status?
  domain/request-status?)

(def request-operation?
  domain/request-operation?)

(def request-document-operation?
  domain/request-document-operation?)

(def request-assignment-operation?
  domain/request-assignment-operation?)

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
;; RequestAssignment projections
;; =============================================================================

(def assignment-id
  domain/assignment-id)

(def assignment-request-id
  domain/assignment-request-id)

(def assignment-helper-id
  domain/assignment-helper-id)

(def assignment-role
  domain/assignment-role)

(def assignment-status
  domain/assignment-status)

(def assignment-source
  domain/assignment-source)

(def assignment-assigned-by
  domain/assignment-assigned-by)

(def assignment-assigned-at
  domain/assignment-assigned-at)

(def assignment-ended-at
  domain/assignment-ended-at)

(def assignment-ended-by
  domain/assignment-ended-by)

(def assignment-end-reason
  domain/assignment-end-reason)

(def assignment-revision
  domain/assignment-revision)

(def assignment-created-at
  domain/assignment-created-at)

(def assignment-updated-at
  domain/assignment-updated-at)

;; =============================================================================
;; RequestAssignment predicates
;; =============================================================================

(def assignment-role?
  domain/assignment-role?)

(def assignment-status?
  domain/assignment-status?)

(def assignment-active?
  domain/assignment-active?)

(def assignment-ended?
  domain/assignment-ended?)

(def primary-assignment?
  domain/primary-assignment?)

(def collaborator-assignment?
  domain/collaborator-assignment?)

(def active-primary-assignment?
  domain/active-primary-assignment?)

(def active-collaborator-assignment?
  domain/active-collaborator-assignment?)

(def assignment-for-request?
  domain/assignment-for-request?)

(def assignment-for-helper?
  domain/assignment-for-helper?)

(def active-assignment-for-helper?
  domain/active-assignment-for-helper?)

(def active-assignment-for-request?
  domain/active-assignment-for-request?)

(def assignment-document?
  domain/assignment-document-consistent?)

;; =============================================================================
;; RequestAssignment collection facts
;; =============================================================================

(def active-assignments
  domain/active-assignments)

(def ended-assignments
  domain/ended-assignments)

(def primary-assignments
  domain/primary-assignments)

(def collaborator-assignments
  domain/collaborator-assignments)

(def active-primary-assignments
  domain/active-primary-assignments)

(def active-collaborator-assignments
  domain/active-collaborator-assignments)

(def active-assignment-for-helper
  domain/active-assignment-for-helper)

(def active-primary-assignment
  domain/active-primary-assignment)

(def active-helper-ids
  domain/active-helper-ids)

(def active-collaborator-helper-ids
  domain/active-collaborator-helper-ids)
