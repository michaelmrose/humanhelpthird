(ns net.humanhelp.site.model.request.schema
  "Malli schemas for persisted Request documents and Request-owned Graph values.

   The closed Request table schema describes the exact persisted document
   shape. Complete ownership, content, assignment, lifecycle, timestamp, and
   cross-field invariants remain owned by request.domain and are applied as the
   final document predicate.

   This registry also defines the scalar attributes and Request-owned value
   objects consumed and produced by Gesso Graph resolvers. It does not describe
   User access contexts, Organization scope contexts, FX working state,
   transaction plans, or Gesso Live changes."
  (:require
   [net.humanhelp.site.model.common :as model.common]
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
;; Customer-editable content
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
      (string? value)
      (request/details? value)))])

(def location-detail-schema
  [:fn
   {:error/message
    (str
     "must be canonical non-blank text of at most "
     request/location-detail-max
     " characters")}
   (fn [value]
     (and
      (string? value)
      (request/location-detail? value)))])

(def content-schema
  [:and
   [:map {:closed true}
    [:title
     title-schema]

    [:details
     [:maybe details-schema]]

    [:location-detail
     [:maybe location-detail-schema]]]

   [:fn
    {:error/message
     "must be a canonical Request content value"}
    request/content?]])

;; =============================================================================
;; Lifecycle values
;; =============================================================================

(def status-schema
  [:fn
   {:error/message
    "must be open, claimed, on-the-way, done, or cancelled"}
   request/status?])

(def operation-schema
  [:fn
   {:error/message
    "must be a supported Request operation"}
   request/operation?])

;; =============================================================================
;; Request expected-version value
;; =============================================================================

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

    [:request/helper
     {:optional true}
     :uuid]

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
     "The Request ownership, content, assignment, lifecycle, or version fields are inconsistent."}
    request/request-document-consistent?]])

;; =============================================================================
;; Biff/Malli registry contribution
;; =============================================================================

(def schema
  "Malli schemas contributed by the Request model.

   :request validates complete persisted Request documents. Attribute keys
   validate Request-owned Graph inputs and outputs."
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

   ;; Requestor value attributes
   :requestor/type
   requestor-type-schema

   :requestor/id
   :uuid

   ;; Request lookup and collection inputs
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

   ;; Request ownership
   :request/requestor-type
   requestor-type-schema

   :request/requestor-id
   :uuid

   :request/requestor
   requestor-reference-schema

   ;; Request content
   :request/title
   title-schema

   :request/details
   details-schema

   :request/location-detail
   location-detail-schema

   :request/content
   content-schema

   ;; Request lifecycle and version
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

   :request/helper
   :uuid

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

   ;; Pure lifecycle facts
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

   :request/has-helper?
   :boolean

   :request/actively-assigned?
   :boolean

   ;; Complete persisted Request document
   :request/doc
   request-document-schema

   :request
   request-document-schema})
