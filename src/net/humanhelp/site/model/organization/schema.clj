(ns net.humanhelp.site.model.organization.schema
  "Canonical Malli documents, gesso.model descriptors, and custom hierarchy
   Graph schema for Organization.

   Persisted document schemas are the single declaration of storage shape,
   persistence codecs, and ordinary Graph projections. Organization keeps only
   hierarchy-derived Graph values and the temporary legacy authorization-proof
   contract as custom schema."
  (:require
   [gesso.model.core :as model]
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.organization.domain :as organization]))

;; Shared scalar schemas

(def instant-schema
  [:fn
   {:gesso.model/codec :instant
    :error/message "must be a java.time.Instant"}
   model.common/timestamp-value?])

(def revision-schema [:int {:min 0}])

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

(def scope-type-schema authorization-scope/scope-type-schema)
(def parent-scope-type-schema authorization-scope/parent-scope-type-schema)
(def scope-reference-schema authorization-scope/scope-reference-schema)
(def parent-scope-reference-schema authorization-scope/parent-scope-reference-schema)
(def scope-reference-vector-schema [:vector scope-reference-schema])
(def scope-context-schema authorization-scope/scope-context-schema)

;; Temporary legacy authorization-proof schema

;; User and Request still consume this Graph value. Remove it after those models
;; migrate to canonical gesso.model.command guards.
(def expected-version-schema
  [:map {:closed true}
   [:model/id :uuid]
   [:model/revision-key qualified-keyword?]
   [:model/revision revision-schema]
   [:model/updated-at-key qualified-keyword?]
   [:model/updated-at instant-schema]])

(def authorization-version-schema
  [:map {:closed true}
   [:model/entity-type [:enum :organization :organization-group :location]]
   [:model/expected expected-version-schema]])

(def authorization-versions-schema
  [:vector authorization-version-schema])

;; Persisted document field builders

(defn- graph-field
  ([key schema]
   [key {:gesso.model/graph true} schema])
  ([key graph-key schema]
   [key {:gesso.model/graph graph-key} schema]))

(defn- optional-graph-field [key schema]
  [key {:optional true :gesso.model/graph true} schema])

(defn- entity-key [entity field]
  (keyword (name entity) (name field)))

(defn- version-fields [entity]
  [(graph-field (entity-key entity :revision) revision-schema)
   (graph-field (entity-key entity :created-at) instant-schema)
   (graph-field (entity-key entity :updated-at) instant-schema)])

(defn- audit-fields [entity audit reason-field]
  [(optional-graph-field
    (entity-key entity (keyword (str (name audit) "-at")))
    instant-schema)
   (optional-graph-field
    (entity-key entity (keyword (str (name audit) "-by")))
    :uuid)
   (optional-graph-field
    (entity-key entity reason-field)
    reason-schema)])

(defn- lifecycle-fields [entity]
  (into [] cat
        [(audit-fields entity :suspended :suspension-reason)
         (audit-fields entity :closed :closure-reason)]))

(defn- document-schema [fields predicate message]
  [:and
   (into [:map {:closed true}] fields)
   [:fn {:error/message message} predicate]])

;; Persisted documents

(def organization-document-schema
  (document-schema
   (into
    [[:xt/id :uuid]
     (graph-field :organization/name name-schema)
     (graph-field :organization/status status-schema)]
    (concat
     (version-fields :organization)
     (lifecycle-fields :organization)))
   organization/organization-document-consistent?
   "The organization lifecycle fields are inconsistent."))

(def organization-group-document-schema
  (document-schema
   (into
    [[:xt/id :uuid]
     (graph-field :organization-group/organization
                  :organization-group/organization-id
                  :uuid)
     (graph-field :organization-group/parent-type parent-scope-type-schema)
     (graph-field :organization-group/parent-id :uuid)
     (graph-field :organization-group/name name-schema)
     (graph-field :organization-group/status status-schema)]
    (concat
     (version-fields :organization-group)
     (audit-fields :organization-group :moved :move-reason)
     (lifecycle-fields :organization-group)))
   organization/organization-group-document-consistent?
   "The organization-group ownership, parent, or lifecycle fields are inconsistent."))

(def location-document-schema
  (document-schema
   (into
    [[:xt/id :uuid]
     (graph-field :location/organization :location/organization-id :uuid)
     (graph-field :location/parent-type parent-scope-type-schema)
     (graph-field :location/parent-id :uuid)
     (graph-field :location/name name-schema)
     (graph-field :location/status status-schema)]
    (concat
     (version-fields :location)
     (audit-fields :location :moved :move-reason)
     (lifecycle-fields :location)))
   organization/location-document-consistent?
   "The location ownership, parent, or lifecycle fields are inconsistent."))

;; gesso.model descriptors

(def organization-descriptor
  {:entity-type organization/organization-entity-type
   :document-schema organization-document-schema
   :identity {:graph-key :organization/id}
   :version organization/organization-version})

(def organization-group-descriptor
  {:entity-type organization/organization-group-entity-type
   :document-schema organization-group-document-schema
   :identity {:graph-key :organization-group/id}
   :version organization/organization-group-version})

(def location-descriptor
  {:entity-type organization/location-entity-type
   :document-schema location-document-schema
   :identity {:graph-key :location/id}
   :version organization/location-version})

(def descriptors
  [organization-descriptor
   organization-group-descriptor
   location-descriptor])

;; Complete Graph/Malli registry

(defn- merge-schema
  "Merges generated/custom schema without silently hiding future collisions."
  [left right]
  (merge-with
   (fn [existing incoming]
     (if (= existing incoming)
       existing
       (throw
        (ex-info
         "Organization schema declarations collide."
         {:error/type :organization.schema/schema-collision
          :existing existing
          :incoming incoming}))))
   left
   right))

(def generated-schema
  (reduce
   (fn [registry descriptor]
     (merge-schema registry (model/generated-schema descriptor)))
   {}
   descriptors))

(defn- state-schema [entity]
  {(entity-key entity :active?) :boolean
   (entity-key entity :suspended?) :boolean
   (entity-key entity :closed?) :boolean
   (entity-key entity :operational?) :boolean})

(def custom-schema
  (merge
   {::instant instant-schema
    ::revision revision-schema
    ::reason reason-schema
    ::name name-schema
    ::status status-schema
    ::scope-type scope-type-schema
    ::parent-scope-type parent-scope-type-schema
    ::scope-reference scope-reference-schema
    ::parent-scope-reference parent-scope-reference-schema
    ::scope-context scope-context-schema
    ::expected-version expected-version-schema
    ::authorization-version authorization-version-schema
    ::authorization-versions authorization-versions-schema}

   (state-schema :organization)
   {:organization/scope scope-reference-schema
    :organization/scope-context scope-context-schema
    :organization/authorization-versions authorization-versions-schema}

   (state-schema :organization-group)
   {:organization-group/parent-scope parent-scope-reference-schema
    :organization-group/scope scope-reference-schema
    :organization-group/ancestor-docs [:vector organization-group-document-schema]
    :organization-group/applicable-scopes scope-reference-vector-schema
    :organization-group/scope-context scope-context-schema
    :organization-group/authorization-versions authorization-versions-schema}

   (state-schema :location)
   {:location/parent-scope parent-scope-reference-schema
    :location/scope scope-reference-schema
    :location/ancestor-group-docs [:vector organization-group-document-schema]
    :location/applicable-scopes scope-reference-vector-schema
    :location/scope-context scope-context-schema
    :location/authorization-versions authorization-versions-schema}))

(def schema
  "Complete Organization Malli/Graph registry."
  (merge-schema generated-schema custom-schema))
