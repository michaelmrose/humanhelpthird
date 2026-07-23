(ns net.humanhelp.site.model.request.schema
  "Malli schemas for persisted Request-model documents and Request-owned Graph
   values.

   The Request table schema describes the persisted Request lifecycle document.
   Helper participation is persisted separately as Request Assignment documents.

   Complete Request lifecycle invariants remain owned by request.domain.
   Complete assignment invariants remain owned by request.assignment.

   Cross-document rules such as exactly one active primary assignment for a
   claimed Request are intentionally not expressed here; Request Graph and FX
   establish those facts from current persistence state."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.assignment :as assignment]
   [net.humanhelp.site.model.request.domain :as request]))

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

(def qualified-keyword-schema
  [:fn
   {:error/message "must be a qualified keyword"}
   qualified-keyword?])

;; =============================================================================
;; Requestor values
;; =============================================================================

(def requestor-type-schema
  [:fn
   {:error/message "must be user or capability"}
   request/requestor-type?])

(def requestor-reference-schema
  [:and
   [:map {:closed true}
    [:requestor/type
     requestor-type-schema]

    [:requestor/id
     :uuid]]

   [:fn
    {:error/message
     "must be a valid Request requestor reference"}
    request/requestor-reference?]])

;; =============================================================================
;; Customer-editable Request content
;; =============================================================================

(def title-schema
  [:fn
   {:error/message
    (str
     "must be canonical non-blank text of at most "
     request/title-max
     " characters")}
   request/title?])

(def details-schema
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

(def location-detail-schema
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
   [:map {:closed true}
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
     "must be a canonical Request content value"}
    request/content?]])

;; =============================================================================
;; Request lifecycle values
;; =============================================================================

(def status-schema
  [:fn
   {:error/message
    "must be open, claimed, on-the-way, done, or cancelled"}
   request/status?])

(def operation-schema
  [:fn
   {:error/message
    "must be a supported Request-model operation"}
   request/operation?])

(def expected-version-schema
  [:map {:closed true}
   [:model/id
    :uuid]

   [:model/revision-key
    [:= :request/revision]]

   [:model/revision
    revision-schema]

   [:model/updated-at-key
    [:= :request/updated-at]]

   [:model/updated-at
    instant-schema]])

;; =============================================================================
;; Request Assignment values
;; =============================================================================

(def assignment-role-schema
  [:fn
   {:error/message
    "must be primary or collaborator"}
   assignment/role?])

(def assignment-status-schema
  [:fn
   {:error/message
    "must be active or ended"}
   assignment/status?])

(def assignment-source-schema
  qualified-keyword-schema)

(def assignment-expected-version-schema
  [:map {:closed true}
   [:model/id
    :uuid]

   [:model/revision-key
    [:= :request-assignment/revision]]

   [:model/revision
    revision-schema]

   [:model/updated-at-key
    [:= :request-assignment/updated-at]]

   [:model/updated-at
    instant-schema]])

;; =============================================================================
;; Persisted Request document
;; =============================================================================

(def request-document-schema
  [:and
   [:map {:closed true}
    [:xt/id
     :uuid]

    [:request/organization
     :uuid]

    [:request/location
     :uuid]

    [:request/requestor-type
     requestor-type-schema]

    [:request/requestor-id
     :uuid]

    [:request/title
     title-schema]

    [:request/details
     {:optional true}
     details-schema]

    [:request/location-detail
     {:optional true}
     location-detail-schema]

    [:request/status
     status-schema]

    [:request/revision
     revision-schema]

    [:request/created-at
     instant-schema]

    [:request/updated-at
     instant-schema]

    [:request/claimed-at
     {:optional true}
     instant-schema]

    [:request/on-the-way-at
     {:optional true}
     instant-schema]

    [:request/completed-at
     {:optional true}
     instant-schema]

    [:request/cancelled-at
     {:optional true}
     instant-schema]

    [:request/cancellation-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The Request ownership, content, lifecycle, or version fields are inconsistent."}
    request/request-document-consistent?]])

;; =============================================================================
;; Persisted Request Assignment document
;; =============================================================================

(def request-assignment-document-schema
  [:and
   [:map {:closed true}
    [:xt/id
     :uuid]

    [:request-assignment/request
     :uuid]

    [:request-assignment/helper
     :uuid]

    [:request-assignment/role
     assignment-role-schema]

    [:request-assignment/status
     assignment-status-schema]

    [:request-assignment/source
     assignment-source-schema]

    [:request-assignment/assigned-by
     {:optional true}
     :uuid]

    [:request-assignment/assigned-at
     instant-schema]

    [:request-assignment/revision
     revision-schema]

    [:request-assignment/created-at
     instant-schema]

    [:request-assignment/updated-at
     instant-schema]

    [:request-assignment/ended-at
     {:optional true}
     instant-schema]

    [:request-assignment/ended-by
     {:optional true}
     :uuid]

    [:request-assignment/end-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The Request Assignment identity, role, lifecycle, or version fields are inconsistent."}
    assignment/document-consistent?]])

;; =============================================================================
;; Biff/Malli registry contribution
;; =============================================================================

(def schema
  "Malli schemas contributed by the Request model.

   :request validates complete persisted Request documents.
   :request-assignment validates complete persisted helper-assignment documents.
   Attribute keys validate Request-owned Graph inputs and outputs."
  {::instant
   instant-schema

   ::revision
   revision-schema

   ::reason
   reason-schema

   ::requestor-type
   requestor-type-schema

   ::requestor-reference
   requestor-reference-schema

   ::title
   title-schema

   ::details
   details-schema

   ::location-detail
   location-detail-schema

   ::content
   content-schema

   ::status
   status-schema

   ::operation
   operation-schema

   ::expected-version
   expected-version-schema

   ::assignment-role
   assignment-role-schema

   ::assignment-status
   assignment-status-schema

   ::assignment-source
   assignment-source-schema

   ::assignment-expected-version
   assignment-expected-version-schema

   ;; --------------------------------------------------------------------------
   ;; Requestor value attributes
   ;; --------------------------------------------------------------------------

   :requestor/type
   requestor-type-schema

   :requestor/id
   :uuid

   ;; --------------------------------------------------------------------------
   ;; Request lookup and collection inputs
   ;; --------------------------------------------------------------------------

   :request/id
   :uuid

   :request/found?
   :boolean

   :request/organization
   :uuid

   :request/organization-id
   :uuid

   :request/location
   :uuid

   :request/location-id
   :uuid

   :request/include-terminal?
   :boolean

   ;; --------------------------------------------------------------------------
   ;; Request ownership
   ;; --------------------------------------------------------------------------

   :request/requestor-type
   requestor-type-schema

   :request/requestor-id
   :uuid

   :request/requestor
   requestor-reference-schema

   ;; --------------------------------------------------------------------------
   ;; Request content
   ;; --------------------------------------------------------------------------

   :request/title
   title-schema

   :request/details
   details-schema

   :request/location-detail
   location-detail-schema

   :request/content
   content-schema

   ;; --------------------------------------------------------------------------
   ;; Request lifecycle and version
   ;; --------------------------------------------------------------------------

   :request/status
   status-schema

   :request/operation
   operation-schema

   :request/revision
   revision-schema

   :request/created-at
   instant-schema

   :request/updated-at
   instant-schema

   :request/claimed-at
   instant-schema

   :request/on-the-way-at
   instant-schema

   :request/completed-at
   instant-schema

   :request/cancelled-at
   instant-schema

   :request/cancellation-reason
   reason-schema

   :request/expected-version
   expected-version-schema

   ;; --------------------------------------------------------------------------
   ;; Pure Request lifecycle facts
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
   ;; Request Assignment lookup and collection inputs
   ;; --------------------------------------------------------------------------

   :request-assignment/id
   :uuid

   :request-assignment/found?
   :boolean

   :request-assignment/request
   :uuid

   :request-assignment/request-id
   :uuid

   :request-assignment/helper
   :uuid

   :request-assignment/helper-id
   :uuid

   :request-assignment/include-ended?
   :boolean

   ;; --------------------------------------------------------------------------
   ;; Request Assignment persisted/projected values
   ;; --------------------------------------------------------------------------

   :request-assignment/role
   assignment-role-schema

   :request-assignment/status
   assignment-status-schema

   :request-assignment/source
   assignment-source-schema

   :request-assignment/assigned-by
   :uuid

   :request-assignment/assigned-at
   instant-schema

   :request-assignment/revision
   revision-schema

   :request-assignment/created-at
   instant-schema

   :request-assignment/updated-at
   instant-schema

   :request-assignment/ended-at
   instant-schema

   :request-assignment/ended-by
   :uuid

   :request-assignment/end-reason
   reason-schema

   :request-assignment/expected-version
   assignment-expected-version-schema

   ;; --------------------------------------------------------------------------
   ;; Request Assignment derived facts
   ;; --------------------------------------------------------------------------

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
   ;; Aggregate assignment facts attached to Request Graph results
   ;; --------------------------------------------------------------------------

   :request/has-primary-assignment?
   :boolean

   :request/active-helper-ids
   [:set :uuid]

   :request/active-collaborator-helper-ids
   [:set :uuid]

   ;; --------------------------------------------------------------------------
   ;; Complete persisted documents
   ;; --------------------------------------------------------------------------

   :request/doc
   request-document-schema

   :request
   request-document-schema

   :request-assignment/doc
   request-assignment-document-schema

   :request-assignment
   request-assignment-document-schema})
