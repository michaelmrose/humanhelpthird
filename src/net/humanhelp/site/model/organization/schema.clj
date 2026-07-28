(ns net.humanhelp.site.model.organization.schema
  "Canonical Organization Malli schemas and gesso.model descriptors.

   This namespace declares persisted document shapes and Organization-specific
   Graph values. gesso.model assembles generated schema and resolvers from the
   descriptors at the model boundary."
  (:require
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.organization.domain :as organization]))

;; =============================================================================
;; Shared Organization value schemas
;; =============================================================================

(def reason-schema
  [:fn
   {:error/message "must be a qualified keyword"}
   qualified-keyword?])

(def name-schema
  [:fn
   {:error/message "must be a canonical non-blank name of at most 160 characters"}
   organization/name?])

(def status-schema
  [:fn
   {:error/message "must be active, suspended, or closed"}
   organization/status?])

(def scope-type-schema
  [:enum
   :organization
   :organization-group
   :location])

(def parent-scope-type-schema
  [:enum
   :organization
   :organization-group])

(def scope-schema
  [:and
   [:map
    {:closed true}
    [:scope/type scope-type-schema]
    [:scope/id :uuid]]
   [:fn
    {:error/message "must be a valid Organization scope"}
    organization/scope?]])

(def parent-scope-schema
  [:and
   [:map
    {:closed true}
    [:scope/type parent-scope-type-schema]
    [:scope/id :uuid]]
   [:fn
    {:error/message "must be a valid Organization parent scope"}
    organization/parent-scope?]])

(def scope-context-schema
  [:and
   [:map
    {:closed true}
    [:organization/id :uuid]
    [:scope/target scope-schema]
    [:scope/applicable [:vector {:min 1} scope-schema]]
    [:scope/operational? :boolean]]
   [:fn
    {:error/message "must be a valid Organization scope context"}
    organization/scope-context?]])

;; =============================================================================
;; Persisted document field builders
;; =============================================================================

(defn- graph-field
  ([key schema]
   [key
    {:gesso.model/graph true}
    schema])
  ([key graph-key schema]
   [key
    {:gesso.model/graph graph-key}
    schema]))

(defn- optional-graph-field
  [key schema]
  [key
   {:optional true
    :gesso.model/graph true}
   schema])

(defn- entity-key
  [entity field]
  (keyword
   (name entity)
   (name field)))

(defn- version-fields
  [entity]
  [(graph-field
    (entity-key entity :revision)
    model.schema/revision-schema)

   (graph-field
    (entity-key entity :created-at)
    model.schema/instant-schema)

   (graph-field
    (entity-key entity :updated-at)
    model.schema/instant-schema)])

(defn- audit-fields
  [entity audit reason-field]
  [(optional-graph-field
    (entity-key
     entity
     (keyword
      (str
       (name audit)
       "-at")))
    model.schema/instant-schema)

   (optional-graph-field
    (entity-key
     entity
     (keyword
      (str
       (name audit)
       "-by")))
    :uuid)

   (optional-graph-field
    (entity-key entity reason-field)
    reason-schema)])

(defn- lifecycle-fields
  [entity]
  (into
   []
   cat
   [(audit-fields
     entity
     :suspended
     :suspension-reason)

    (audit-fields
     entity
     :closed
     :closure-reason)]))

(defn- document-schema
  [fields predicate message]
  [:and
   (into
    [:map {:closed true}]
    fields)
   [:fn
    {:error/message message}
    predicate]])

;; =============================================================================
;; Persisted documents
;; =============================================================================

(def organization-document-schema
  (document-schema
   (into
    [[:xt/id :uuid]

     (graph-field
      :organization/name
      name-schema)

     (graph-field
      :organization/status
      status-schema)]

    (concat
     (version-fields :organization)
     (lifecycle-fields :organization)))

   organization/organization-document-consistent?

   "The organization lifecycle fields are inconsistent."))

(def organization-group-document-schema
  (document-schema
   (into
    [[:xt/id :uuid]

     (graph-field
      :organization-group/organization
      :organization-group/organization-id
      :uuid)

     (graph-field
      :organization-group/parent-type
      parent-scope-type-schema)

     (graph-field
      :organization-group/parent-id
      :uuid)

     (graph-field
      :organization-group/name
      name-schema)

     (graph-field
      :organization-group/status
      status-schema)]

    (concat
     (version-fields :organization-group)

     (audit-fields
      :organization-group
      :moved
      :move-reason)

     (lifecycle-fields
      :organization-group)))

   organization/organization-group-document-consistent?

   "The organization-group ownership, parent, or lifecycle fields are inconsistent."))

(def location-document-schema
  (document-schema
   (into
    [[:xt/id :uuid]

     (graph-field
      :location/organization
      :location/organization-id
      :uuid)

     (graph-field
      :location/parent-type
      parent-scope-type-schema)

     (graph-field
      :location/parent-id
      :uuid)

     (graph-field
      :location/name
      name-schema)

     (graph-field
      :location/status
      status-schema)]

    (concat
     (version-fields :location)

     (audit-fields
      :location
      :moved
      :move-reason)

     (lifecycle-fields
      :location)))

   organization/location-document-consistent?

   "The location ownership, parent, or lifecycle fields are inconsistent."))

;; =============================================================================
;; gesso.model descriptors
;; =============================================================================

(def organization-descriptor
  {:entity-type
   organization/organization-entity-type

   :document-schema
   organization-document-schema

   :identity
   {:graph-key :organization/id}

   :version
   organization/organization-version})

(def organization-group-descriptor
  {:entity-type
   organization/organization-group-entity-type

   :document-schema
   organization-group-document-schema

   :identity
   {:graph-key :organization-group/id}

   :version
   organization/organization-group-version})

(def location-descriptor
  {:entity-type
   organization/location-entity-type

   :document-schema
   location-document-schema

   :identity
   {:graph-key :location/id}

   :version
   organization/location-version})

(def descriptors
  [organization-descriptor
   organization-group-descriptor
   location-descriptor])

;; =============================================================================
;; Organization-specific Graph values
;; =============================================================================

(def custom-schema
  {:organization/active?
   :boolean

   :organization/operational?
   :boolean

   :organization/scope
   scope-schema

   :organization/scope-context
   scope-context-schema

   :organization-group/parent-scope
   parent-scope-schema

   :organization-group/scope
   scope-schema

   :organization-group/active?
   :boolean

   :organization-group/operational?
   :boolean

   :organization-group/ancestor-docs
   [:vector organization-group-document-schema]

   :organization-group/scope-context
   scope-context-schema

   :location/parent-scope
   parent-scope-schema

   :location/scope
   scope-schema

   :location/active?
   :boolean

   :location/operational?
   :boolean

   :location/ancestor-group-docs
   [:vector organization-group-document-schema]

   :location/scope-context
   scope-context-schema})
