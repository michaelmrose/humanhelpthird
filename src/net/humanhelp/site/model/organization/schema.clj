(ns net.humanhelp.site.model.organization.schema
  "Malli schemas for persisted Organization-model documents and Graph values.

   Shared structural authorization-scope schemas use
   model.authorization-scope. Complete Organization lifecycle, ownership,
   hierarchy, ancestry, and cross-field invariants remain owned by
   organization.domain and are applied as predicates to persisted documents.

   This namespace does not query XTDB, authorize users, or execute workflows."
  (:require
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.organization.domain :as organization]))

;; =============================================================================
;; Shared scalar schemas
;; =============================================================================

(def instant-schema
  [:fn
   {:error/message "must be a java.time.Instant"}
   model.common/timestamp-value?])

(def revision-schema
  [:int {:min 0}])

(def reason-schema
  [:fn
   {:error/message "must be a qualified keyword"}
   qualified-keyword?])

(def name-schema
  [:fn
   {:error/message
    "must be a canonical non-blank name of at most 160 characters"}
   organization/name?])

(def status-schema
  [:fn
   {:error/message "must be active, suspended, or closed"}
   organization/status?])

(def scope-type-schema
  "Compatibility alias for the shared authorization-scope schema."
  authorization-scope/scope-type-schema)

(def parent-scope-type-schema
  "Compatibility alias for the shared parent-scope schema."
  authorization-scope/parent-scope-type-schema)

(def scope-reference-schema
  "Compatibility alias for the shared authorization-scope reference schema."
  authorization-scope/scope-reference-schema)

(def parent-scope-reference-schema
  "Compatibility alias for the shared parent-scope reference schema."
  authorization-scope/parent-scope-reference-schema)

(def scope-reference-vector-schema
  [:vector
   scope-reference-schema])

;; =============================================================================
;; Organization authorization-version guards
;; =============================================================================

(def expected-version-schema
  [:map {:closed true}
   [:model/id :uuid]
   [:model/revision-key qualified-keyword?]
   [:model/revision revision-schema]
   [:model/updated-at-key qualified-keyword?]
   [:model/updated-at instant-schema]])

(def authorization-version-schema
  [:map {:closed true}
   [:model/entity-type
    [:enum
     :organization
     :organization-group
     :location]]

   [:model/expected
    expected-version-schema]])

(def authorization-versions-schema
  [:vector authorization-version-schema])

;; =============================================================================
;; Public authorization-scope context
;; =============================================================================

(def scope-context-schema
  "Compatibility alias for the shared authorization-scope context schema."
  authorization-scope/scope-context-schema)

;; =============================================================================
;; Organization document
;; =============================================================================

(def organization-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:organization/name name-schema]
    [:organization/status status-schema]
    [:organization/revision revision-schema]
    [:organization/created-at instant-schema]
    [:organization/updated-at instant-schema]

    [:organization/suspended-at
     {:optional true}
     instant-schema]

    [:organization/suspended-by
     {:optional true}
     :uuid]

    [:organization/suspension-reason
     {:optional true}
     reason-schema]

    [:organization/closed-at
     {:optional true}
     instant-schema]

    [:organization/closed-by
     {:optional true}
     :uuid]

    [:organization/closure-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The organization lifecycle fields are inconsistent."}
    organization/organization-document-consistent?]])

;; =============================================================================
;; Organization-group document
;; =============================================================================

(def organization-group-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:organization-group/organization :uuid]
    [:organization-group/parent-type parent-scope-type-schema]
    [:organization-group/parent-id :uuid]
    [:organization-group/name name-schema]
    [:organization-group/status status-schema]
    [:organization-group/revision revision-schema]
    [:organization-group/created-at instant-schema]
    [:organization-group/updated-at instant-schema]

    [:organization-group/moved-at
     {:optional true}
     instant-schema]

    [:organization-group/moved-by
     {:optional true}
     :uuid]

    [:organization-group/move-reason
     {:optional true}
     reason-schema]

    [:organization-group/suspended-at
     {:optional true}
     instant-schema]

    [:organization-group/suspended-by
     {:optional true}
     :uuid]

    [:organization-group/suspension-reason
     {:optional true}
     reason-schema]

    [:organization-group/closed-at
     {:optional true}
     instant-schema]

    [:organization-group/closed-by
     {:optional true}
     :uuid]

    [:organization-group/closure-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The organization-group ownership, parent, or lifecycle fields are inconsistent."}
    organization/organization-group-document-consistent?]])

;; =============================================================================
;; Location document
;; =============================================================================

(def location-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:location/organization :uuid]
    [:location/parent-type parent-scope-type-schema]
    [:location/parent-id :uuid]
    [:location/name name-schema]
    [:location/status status-schema]
    [:location/revision revision-schema]
    [:location/created-at instant-schema]
    [:location/updated-at instant-schema]

    [:location/moved-at
     {:optional true}
     instant-schema]

    [:location/moved-by
     {:optional true}
     :uuid]

    [:location/move-reason
     {:optional true}
     reason-schema]

    [:location/suspended-at
     {:optional true}
     instant-schema]

    [:location/suspended-by
     {:optional true}
     :uuid]

    [:location/suspension-reason
     {:optional true}
     reason-schema]

    [:location/closed-at
     {:optional true}
     instant-schema]

    [:location/closed-by
     {:optional true}
     :uuid]

    [:location/closure-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The location ownership, parent, or lifecycle fields are inconsistent."}
    organization/location-document-consistent?]])

;; =============================================================================
;; Biff/Malli registry contribution
;; =============================================================================

(def schema
  "Malli schemas contributed by the Organization model.

   Table keywords validate complete persisted documents. Attribute keywords are
   registered independently for Gesso Graph input/output validation.

   Shared :scope/* registry attributes belong to model.authorization-scope.
   Generic :model/* values may appear inside Organization-owned value schemas,
   but are not registered here as Organization attributes."
  {::instant
   instant-schema

   ::revision
   revision-schema

   ::reason
   reason-schema

   ::name
   name-schema

   ::status
   status-schema

   ::scope-type
   scope-type-schema

   ::parent-scope-type
   parent-scope-type-schema

   ::scope-reference
   scope-reference-schema

   ::parent-scope-reference
   parent-scope-reference-schema

   ::scope-context
   scope-context-schema

   ::expected-version
   expected-version-schema

   ::authorization-version
   authorization-version-schema

   ::authorization-versions
   authorization-versions-schema

   ;; Organization attributes
   :organization/id
   :uuid

   :organization/found?
   :boolean

   :organization/name
   name-schema

   :organization/status
   status-schema

   :organization/active?
   :boolean

   :organization/suspended?
   :boolean

   :organization/closed?
   :boolean

   :organization/operational?
   :boolean

   :organization/revision
   revision-schema

   :organization/created-at
   instant-schema

   :organization/updated-at
   instant-schema

   :organization/suspended-at
   instant-schema

   :organization/suspended-by
   :uuid

   :organization/suspension-reason
   reason-schema

   :organization/closed-at
   instant-schema

   :organization/closed-by
   :uuid

   :organization/closure-reason
   reason-schema

   :organization/scope
   scope-reference-schema

   :organization/scope-context
   scope-context-schema

   :organization/authorization-versions
   authorization-versions-schema

   :organization/doc
   organization-document-schema

   :organization
   organization-document-schema

   ;; Organization-group attributes
   :organization-group/id
   :uuid

   :organization-group/found?
   :boolean

   :organization-group/organization-id
   :uuid

   :organization-group/parent-type
   parent-scope-type-schema

   :organization-group/parent-id
   :uuid

   :organization-group/parent-scope
   parent-scope-reference-schema

   :organization-group/name
   name-schema

   :organization-group/status
   status-schema

   :organization-group/active?
   :boolean

   :organization-group/suspended?
   :boolean

   :organization-group/closed?
   :boolean

   :organization-group/operational?
   :boolean

   :organization-group/revision
   revision-schema

   :organization-group/created-at
   instant-schema

   :organization-group/updated-at
   instant-schema

   :organization-group/moved-at
   instant-schema

   :organization-group/moved-by
   :uuid

   :organization-group/move-reason
   reason-schema

   :organization-group/suspended-at
   instant-schema

   :organization-group/suspended-by
   :uuid

   :organization-group/suspension-reason
   reason-schema

   :organization-group/closed-at
   instant-schema

   :organization-group/closed-by
   :uuid

   :organization-group/closure-reason
   reason-schema

   :organization-group/scope
   scope-reference-schema

   :organization-group/ancestor-docs
   [:vector organization-group-document-schema]

   :organization-group/applicable-scopes
   scope-reference-vector-schema

   :organization-group/scope-context
   scope-context-schema

   :organization-group/authorization-versions
   authorization-versions-schema

   :organization-group/doc
   organization-group-document-schema

   :organization-group
   organization-group-document-schema

   ;; Location attributes
   :location/id
   :uuid

   :location/found?
   :boolean

   :location/organization-id
   :uuid

   :location/parent-type
   parent-scope-type-schema

   :location/parent-id
   :uuid

   :location/parent-scope
   parent-scope-reference-schema

   :location/name
   name-schema

   :location/status
   status-schema

   :location/active?
   :boolean

   :location/suspended?
   :boolean

   :location/closed?
   :boolean

   :location/operational?
   :boolean

   :location/revision
   revision-schema

   :location/created-at
   instant-schema

   :location/updated-at
   instant-schema

   :location/moved-at
   instant-schema

   :location/moved-by
   :uuid

   :location/move-reason
   reason-schema

   :location/suspended-at
   instant-schema

   :location/suspended-by
   :uuid

   :location/suspension-reason
   reason-schema

   :location/closed-at
   instant-schema

   :location/closed-by
   :uuid

   :location/closure-reason
   reason-schema

   :location/scope
   scope-reference-schema

   :location/ancestor-group-docs
   [:vector organization-group-document-schema]

   :location/applicable-scopes
   scope-reference-vector-schema

   :location/authorization-versions
   authorization-versions-schema

   :location/scope-context
   scope-context-schema

   :location/doc
   location-document-schema

   :location
   location-document-schema})
