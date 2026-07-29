(ns net.humanhelp.site.model.request.schema
  "Canonical persisted schemas and gesso.model descriptors for Request.

   Request owns two persisted entity types:

   - :request
   - :request-assignment

   The persisted document schemas are the single declarations of storage shape,
   scalar validation, ordinary Graph projection, and model version metadata.

   Request domain owns semantic validity. gesso.model owns mechanical schema
   introspection, persistence normalization, by-ID reads, and ordinary Graph
   field projection.

   Cross-document facts such as the active primary assignment, collaborator
   sets, and lifecycle/assignment consistency are derived in request.graph and
   enforced in request.fx."
  (:require
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.request.domain :as request]))

;; =============================================================================
;; Request value schemas
;; =============================================================================

(def ^:private requestor-type-schema
  [:fn
   {:error/message
    "must be user or capability"}
   request/requestor-type?])

(def requestor-reference-schema
  [:and

   [:map
    {:closed true}

    [:requestor/type
     requestor-type-schema]

    [:requestor/id
     :uuid]]

   [:fn
    {:error/message
     "must be a valid Request requestor reference"}
    request/requestor-reference?]])

(def ^:private title-schema
  [:fn
   {:error/message
    (str
     "must be canonical non-blank text of at most "
     request/title-max
     " characters")}
   request/title?])

(def ^:private details-schema
  [:fn
   {:error/message
    (str
     "must be canonical non-blank text of at most "
     request/details-max
     " characters")}
   (fn [value]
     (and
      (string?
       value)

      (request/details?
       value)))])

(def ^:private location-detail-schema
  [:fn
   {:error/message
    (str
     "must be canonical non-blank text of at most "
     request/location-detail-max
     " characters")}
   (fn [value]
     (and
      (string?
       value)

      (request/location-detail?
       value)))])

(def content-schema
  [:and

   [:map
    {:closed true}

    [:title
     title-schema]

    [:details
     [:maybe
      details-schema]]

    [:location-detail
     [:maybe
      location-detail-schema]]]

   [:fn
    {:error/message
     "must be canonical Request content"}
    request/content?]])

(def ^:private request-status-schema
  [:fn
   {:error/message
    "must be open, claimed, on-the-way, done, or cancelled"}
   request/request-status?])

(def ^:private reason-schema
  [:fn
   {:error/message
    "must be a qualified keyword"}
   qualified-keyword?])

;; =============================================================================
;; Request Assignment value schemas
;; =============================================================================

(def ^:private assignment-role-schema
  [:fn
   {:error/message
    "must be primary or collaborator"}
   request/assignment-role?])

(def ^:private assignment-status-schema
  [:fn
   {:error/message
    "must be active or ended"}
   request/assignment-status?])

(def ^:private assignment-source-schema
  [:fn
   {:error/message
    "must be a qualified keyword"}
   request/assignment-source?])

;; =============================================================================
;; Persisted-field declarations
;; =============================================================================

(defn- graph-field
  ([key schema]
   [key
    {:gesso.model/graph
     true}
    schema])

  ([key graph-key schema]
   [key
    {:gesso.model/graph
     graph-key}
    schema]))

(defn- optional-graph-field
  ([key schema]
   [key
    {:optional
     true

     :gesso.model/graph
     true}
    schema])

  ([key graph-key schema]
   [key
    {:optional
     true

     :gesso.model/graph
     graph-key}
    schema]))

;; =============================================================================
;; Persisted Request document
;; =============================================================================

(def request-document-schema
  "Complete persisted Request document.

   The structural map is closed. The final domain predicate owns relationships
   among requestor, content, lifecycle, timestamps, and version fields."
  [:and

   [:map
    {:closed true}

    [:xt/id
     :uuid]

    (graph-field
     :request/organization
     :request/organization-id
     :uuid)

    (graph-field
     :request/location
     :request/location-id
     :uuid)

    (graph-field
     :request/requestor-type
     requestor-type-schema)

    (graph-field
     :request/requestor-id
     :uuid)

    (graph-field
     :request/title
     title-schema)

    (optional-graph-field
     :request/details
     details-schema)

    (optional-graph-field
     :request/location-detail
     location-detail-schema)

    (graph-field
     :request/status
     request-status-schema)

    (graph-field
     :request/revision
     model.schema/revision-schema)

    (graph-field
     :request/created-at
     model.schema/instant-schema)

    (graph-field
     :request/updated-at
     model.schema/instant-schema)

    (optional-graph-field
     :request/claimed-at
     model.schema/instant-schema)

    (optional-graph-field
     :request/on-the-way-at
     model.schema/instant-schema)

    (optional-graph-field
     :request/completed-at
     model.schema/instant-schema)

    (optional-graph-field
     :request/cancelled-at
     model.schema/instant-schema)

    (optional-graph-field
     :request/cancellation-reason
     reason-schema)]

   [:fn
    {:error/message
     "The Request ownership, content, lifecycle, or version fields are inconsistent."}
    request/request-document-consistent?]])

;; =============================================================================
;; Persisted Request Assignment document
;; =============================================================================

(def request-assignment-document-schema
  "Complete persisted RequestAssignment document.

   Assignment creation time is the assignment time; there is deliberately no
   duplicate persisted :request-assignment/assigned-at field.

   The structural map is closed. The final domain predicate owns lifecycle,
   audit, and version relationships."
  [:and

   [:map
    {:closed true}

    [:xt/id
     :uuid]

    (graph-field
     :request-assignment/request
     :request-assignment/request-id
     :uuid)

    (graph-field
     :request-assignment/helper
     :request-assignment/helper-id
     :uuid)

    (graph-field
     :request-assignment/role
     assignment-role-schema)

    (graph-field
     :request-assignment/status
     assignment-status-schema)

    (graph-field
     :request-assignment/source
     assignment-source-schema)

    (optional-graph-field
     :request-assignment/assigned-by
     :uuid)

    (graph-field
     :request-assignment/revision
     model.schema/revision-schema)

    (graph-field
     :request-assignment/created-at
     model.schema/instant-schema)

    (graph-field
     :request-assignment/updated-at
     model.schema/instant-schema)

    (optional-graph-field
     :request-assignment/ended-at
     model.schema/instant-schema)

    (optional-graph-field
     :request-assignment/ended-by
     :uuid)

    (optional-graph-field
     :request-assignment/end-reason
     reason-schema)]

   [:fn
    {:error/message
     "The Request Assignment identity, participation, lifecycle, or version fields are inconsistent."}
    request/assignment-document-consistent?]])

;; =============================================================================
;; gesso.model descriptors
;; =============================================================================

(def request-descriptor
  {:entity-type
   request/request-entity-type

   :document-schema
   request-document-schema

   :identity
   {:graph-key
    :request/id}

   :version
   request/request-version})

(def request-assignment-descriptor
  {:entity-type
   request/assignment-entity-type

   :document-schema
   request-assignment-document-schema

   :identity
   {:graph-key
    :request-assignment/id}

   :version
   request/assignment-version})

(def descriptors
  [request-descriptor
   request-assignment-descriptor])

;; =============================================================================
;; Request-specific derived Graph values
;; =============================================================================

(def custom-schema
  "Schemas for Request-owned Graph values that cannot be generated directly
   from persisted fields.

   Ordinary document fields, identity keys, document schemas, and aliases such
   as :request/location-id are generated from the descriptors."
  {:request/requestor
   requestor-reference-schema

   :request/content
   content-schema

   ;; --------------------------------------------------------------------------
   ;; Request lifecycle facts
   ;; --------------------------------------------------------------------------

   :request/open?
   :boolean

   :request/claimed?
   :boolean

   :request/on-the-way?
   :boolean

   :request/done?
   :boolean

   :request/cancelled?
   :boolean

   :request/active?
   :boolean

   :request/terminal?
   :boolean

   :request/editable?
   :boolean

   :request/claimable?
   :boolean

   :request/unclaimable?
   :boolean

   :request/markable-on-the-way?
   :boolean

   :request/completable?
   :boolean

   :request/cancellable?
   :boolean

   :request/expects-primary-assignment?
   :boolean

   ;; --------------------------------------------------------------------------
   ;; Request Assignment derived facts
   ;; --------------------------------------------------------------------------

   :request-assignment/assigned-at
   model.schema/instant-schema

   :request-assignment/active?
   :boolean

   :request-assignment/ended?
   :boolean

   :request-assignment/primary?
   :boolean

   :request-assignment/collaborator?
   :boolean

   :request-assignment/active-primary?
   :boolean

   :request-assignment/active-collaborator?
   :boolean

   ;; --------------------------------------------------------------------------
   ;; Request Assignment collection facts
   ;; --------------------------------------------------------------------------

   :request/has-primary-assignment?
   :boolean

   :request/primary-assignment-id
   :uuid

   :request/primary-helper-id
   :uuid

   :request/active-helper-ids
   [:set
    :uuid]

   :request/active-collaborator-helper-ids
   [:set
    :uuid]})
