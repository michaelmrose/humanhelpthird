(ns net.humanhelp.site.model.request.schema
  "Malli schemas for the request model.

   This registry describes persisted request documents, Graph attributes,
   request commands, and FX-machine working values.

   Request document invariants remain owned by request.domain and are included
   in the document schema so documents loaded from storage or produced by
   transitions are checked against the same rules."
  (:require
   [net.humanhelp.schema.common :as common]
   [net.humanhelp.site.model.request.domain :as request]))

(def ?
  common/?)

;; =============================================================================
;; Reusable request schemas
;; =============================================================================

(def request-status-schema
  [:enum
   :open
   :claimed
   :on-the-way
   :done
   :cancelled])

(def request-operation-schema
  [:enum
   :create
   :edit
   :claim
   :unclaim
   :mark-on-the-way
   :cancel
   :done])

(def request-title-schema
  [:and
   [:string
    {:min 1
     :max request/title-max}]
   [:fn common/non-blank-string?]])

(def request-details-schema
  [:and
   [:string
    {:min 1
     :max request/details-max}]
   [:fn common/non-blank-string?]])

(def request-store-area-text-schema
  [:and
   [:string
    {:min 1
     :max request/store-area-text-max}]
   [:fn common/non-blank-string?]])

(def request-document-schema
  [:and

   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:request/store
     ::common/id]

    [:request/user
     ?
     ::common/id]

    [:request/capability
     ?
     ::common/id]

    [:request/store-area
     ?
     ::common/id]

    [:request/store-area-text
     ?
     request-store-area-text-schema]

    [:request/title
     request-title-schema]

    [:request/details
     ?
     request-details-schema]

    [:request/status
     request-status-schema]

    [:request/revision
     ::common/revision]

    [:request/claimed-by
     ?
     ::common/id]

    [:request/created-at
     ::common/instant]

    [:request/updated-at
     ::common/instant]

    [:request/claimed-at
     ?
     ::common/instant]

    [:request/on-the-way-at
     ?
     ::common/instant]

    [:request/edited-at
     ?
     ::common/instant]

    [:request/completed-at
     ?
     ::common/instant]

    [:request/cancelled-at
     ?
     ::common/instant]]

   [:fn request/requestor-consistent?]
   [:fn request/location-consistent?]
   [:fn request/lifecycle-consistent?]])

(def request-expected-version-schema
  [:map
   {:closed true}

   [:request/id
    ::common/id]

   [:request/revision
    ::common/revision]

   [:request/status
    request-status-schema]

   [:request/updated-at
    ::common/instant]])

(def request-create-command-schema
  [:map
   {:closed true}

   [:request/operation
    [:= :create]]

   [:request/id
    ::common/id]

   [:request/after
    request-document-schema]])

(def request-update-command-schema
  [:map
   {:closed true}

   [:request/operation
    [:enum
     :edit
     :claim
     :unclaim
     :mark-on-the-way
     :cancel
     :done]]

   [:request/id
    ::common/id]

   [:request/expected
    request-expected-version-schema]

   [:request/before
    request-document-schema]

   [:request/after
    request-document-schema]])

(def request-command-schema
  [:or
   request-create-command-schema
   request-update-command-schema])

;; =============================================================================
;; Schema registry
;; =============================================================================

(def schema
  {;; ==========================================================================
   ;; Persisted request document
   ;; ==========================================================================

   :request/status
   request-status-schema

   :request/operation
   request-operation-schema

   :request/title
   request-title-schema

   :request/details
   request-details-schema

   :request/store-area-text
   request-store-area-text-schema

   :request/revision
   ::common/revision

   :request/created-at
   ::common/instant

   :request/updated-at
   ::common/instant

   :request/claimed-at
   ::common/instant

   :request/on-the-way-at
   ::common/instant

   :request/edited-at
   ::common/instant

   :request/completed-at
   ::common/instant

   :request/cancelled-at
   ::common/instant

   :request
   request-document-schema

   :request/document
   :request

   :request/doc
   :request

   ;; ==========================================================================
   ;; Request references and projected stored fields
   ;; ==========================================================================

   :request/id
   ::common/id

   :request/store-id
   ::common/id

   :request/user-id
   ::common/id

   :request/capability-id
   ::common/id

   :request/store-area-id
   ::common/id

   :request/claimed-by-id
   ::common/id

   ;; ==========================================================================
   ;; Request lookup facts
   ;; ==========================================================================

   :request/found?
   :boolean

   ;; ==========================================================================
   ;; Lifecycle facts
   ;; ==========================================================================

   :request/active?
   :boolean

   :request/terminal?
   :boolean

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

   :request/editable?
   :boolean

   :request/cancellable?
   :boolean

   :request/markable-done?
   :boolean

   :request/claimable?
   :boolean

   :request/unclaimable?
   :boolean

   :request/markable-on-the-way?
   :boolean

   :request/progress-stage
   [:enum
    :created
    :claimed
    :on-the-way
    :done
    :cancelled]

   :request/progress-index
   [:int
    {:min 0
     :max 3}]

   ;; ==========================================================================
   ;; Current-actor and assignment facts
   ;; ==========================================================================

   ;; This remains here until employee identity and authorization are moved
   ;; fully into the user model.
   :current-employee/id
   ::common/id

   :request/owned-by-current-actor?
   :boolean

   :request/assigned?
   :boolean

   :request/assigned-to-current-employee?
   :boolean

   ;; ==========================================================================
   ;; Derived request permissions
   ;; ==========================================================================

   :request/can-edit?
   :boolean

   :request/can-cancel?
   :boolean

   :request/can-mark-done?
   :boolean

   ;; ==========================================================================
   ;; Command descriptions
   ;; ==========================================================================

   :request/expected-version
   request-expected-version-schema

   :request/expected
   :request/expected-version

   :request/create-command
   request-create-command-schema

   :request/update-command
   request-update-command-schema

   :request/command
   request-command-schema

   :request/before
   :request

   :request/after
   :request

   ;; ==========================================================================
   ;; FX inputs and working values
   ;; ==========================================================================

   ;; These values intentionally remain permissive. Invalid user input must
   ;; reach request.domain so it can produce field-level validation errors
   ;; instead of being rejected by generic FX validation first.
   :request/create-input
   'map?

   :request/edit-input
   'map?

   :request/input
   'map?

   ;; Customer and employee Graph queries return different fact maps, and
   ;; missing requests legitimately return only :request/found?.
   :request/facts
   'map?

   :request/next-seed
   :int

   ;; Commit handlers may return an XTDB transaction result, a custom CAS
   ;; result, or another backend-specific value.
   :request/commit-result
   :any})
