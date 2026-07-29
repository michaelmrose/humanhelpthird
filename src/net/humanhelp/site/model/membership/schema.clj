(ns net.humanhelp.site.model.membership.schema
  "Canonical persisted Membership and RoleAssignment schemas.

   Membership domain owns semantic validity. This namespace owns persisted
   structure and the gesso.model descriptors from which ordinary persistence
   normalization, by-ID reads, and Graph projection are derived.

   Membership relationships are intentionally not declared as descriptor
   equality lookups. A User may belong to multiple Organizations, an
   Organization has many Memberships, and a Membership may have many
   RoleAssignments. Those cardinalities belong in membership.graph rather than
   singular generated lookup plumbing."
  (:require
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.membership.domain :as membership]
   [net.humanhelp.site.model.organization.core :as organization]))

;; =============================================================================
;; Scalar and value schemas
;; =============================================================================

(def ^:private skill-schema
  [:fn
   {:error/message
    "must be a canonical non-blank organization-local skill"}
   membership/skill?])

(def ^:private skills-schema
  [:set
   skill-schema])

(def ^:private membership-status-schema
  [:fn
   {:error/message
    "must be active, suspended, or revoked"}
   membership/membership-status?])

(def ^:private role-schema
  [:fn
   {:error/message
    "must be helper, supervisor, or admin"}
   membership/role?])

(def ^:private role-assignment-status-schema
  [:fn
   {:error/message
    "must be active or revoked"}
   membership/role-assignment-status?])

(def ^:private reason-schema
  [:fn
   {:error/message
    "must be a qualified keyword"}
   qualified-keyword?])

(def ^:private scope-type-schema
  (into
   [:enum]
   (sort-by
    str
    organization/scope-types)))

;; =============================================================================
;; Persisted-field builders
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

(defn- document-schema
  [fields predicate message]
  [:and

   (into
    [:map
     {:closed true}]
    fields)

   [:fn
    {:error/message message}
    predicate]])

;; =============================================================================
;; Membership persisted document
;; =============================================================================

(def membership-document-schema
  (document-schema
   (into
    [[:xt/id
      :uuid]

     (graph-field
      :membership/user
      :membership/user-id
      :uuid)

     (graph-field
      :membership/organization
      :membership/organization-id
      :uuid)

     (graph-field
      :membership/skills
      skills-schema)

     (graph-field
      :membership/status
      membership-status-schema)]

    (concat
     (version-fields
      :membership)

     (audit-fields
      :membership
      :suspended
      :suspension-reason)

     (audit-fields
      :membership
      :revoked
      :revocation-reason)))

   membership/membership-document-consistent?

   "The Membership ownership, skills, version, or lifecycle fields are inconsistent."))

;; =============================================================================
;; RoleAssignment persisted document
;; =============================================================================

(def role-assignment-document-schema
  (document-schema
   (into
    [[:xt/id
      :uuid]

     (graph-field
      :role-assignment/membership
      :role-assignment/membership-id
      :uuid)

     (graph-field
      :role-assignment/role
      role-schema)

     (graph-field
      :role-assignment/scope-type
      scope-type-schema)

     (graph-field
      :role-assignment/scope-id
      :uuid)

     (graph-field
      :role-assignment/status
      role-assignment-status-schema)]

    (concat
     (version-fields
      :role-assignment)

     [(optional-graph-field
       :role-assignment/assigned-by
       :uuid)

      (optional-graph-field
       :role-assignment/assignment-reason
       reason-schema)]

     (audit-fields
      :role-assignment
      :revoked
      :revocation-reason)))

   membership/role-assignment-document-consistent?

   "The RoleAssignment membership, role, scope, version, or lifecycle fields are inconsistent."))

;; =============================================================================
;; gesso.model descriptors
;; =============================================================================

(def membership-descriptor
  {:entity-type
   membership/membership-entity-type

   :document-schema
   membership-document-schema

   :identity
   {:graph-key
    :membership/id}

   :version
   membership/membership-version})

(def role-assignment-descriptor
  {:entity-type
   membership/role-assignment-entity-type

   :document-schema
   role-assignment-document-schema

   :identity
   {:graph-key
    :role-assignment/id}

   :version
   membership/role-assignment-version})

(def descriptors
  [membership-descriptor
   role-assignment-descriptor])

;; =============================================================================
;; Custom Graph schema
;; =============================================================================

(def custom-schema
  "Membership currently needs no custom scalar Graph values.

   Relationship and authorization resolvers may consume the generated
   Membership and RoleAssignment attributes, but ordinary persisted values
   remain entirely descriptor-generated."
  {})
