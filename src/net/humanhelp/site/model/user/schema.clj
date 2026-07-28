(ns net.humanhelp.site.model.user.schema
  "Canonical persisted User schema and gesso.model descriptor.

   The persisted document schema is the single declaration of User storage
   shape, scalar validation, persistence codecs, ordinary Graph projections,
   and equality-lookup fields.

   gesso.model derives the ordinary by-id, phone, and email Graph plumbing from
   the descriptor. User-specific semantics remain in user.domain."
  (:require
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.user.domain :as user]))

;; =============================================================================
;; User scalar schemas
;; =============================================================================

(def phone-schema
  [:fn
   {:error/message
    "must be a canonical E.164 phone number"}
   user/phone?])

(def email-schema
  [:fn
   {:error/message
    "must be a canonical HumanHelp email address"}
   user/email?])

(def display-name-schema
  [:fn
   {:error/message
    "must be a canonical non-blank display name of at most 120 characters"}
   user/display-name?])

(def status-schema
  [:fn
   {:error/message
    "must be active, suspended, or deleted"}
   user/status?])

(def reason-schema
  [:fn
   {:error/message
    "must be a qualified keyword"}
   qualified-keyword?])

;; =============================================================================
;; Persisted-field declarations
;; =============================================================================

(defn- graph-field
  [key schema]
  [key
   {:gesso.model/graph true}
   schema])

(defn- optional-graph-field
  [key schema]
  [key
   {:optional true
    :gesso.model/graph true}
   schema])

(def user-document-schema
  [:and

   [:map
    {:closed true}

    [:xt/id
     :uuid]

    (optional-graph-field
     :user/phone
     phone-schema)

    (optional-graph-field
     :user/email
     email-schema)

    (optional-graph-field
     :user/display-name
     display-name-schema)

    (graph-field
     :user/status
     status-schema)

    (graph-field
     :user/revision
     model.schema/revision-schema)

    (graph-field
     :user/created-at
     model.schema/instant-schema)

    (graph-field
     :user/updated-at
     model.schema/instant-schema)

    (optional-graph-field
     :user/phone-verified-at
     model.schema/instant-schema)

    (optional-graph-field
     :user/email-verified-at
     model.schema/instant-schema)

    (optional-graph-field
     :user/suspended-at
     model.schema/instant-schema)

    (optional-graph-field
     :user/suspended-by
     :uuid)

    (optional-graph-field
     :user/suspension-reason
     reason-schema)

    (optional-graph-field
     :user/deleted-at
     model.schema/instant-schema)

    (optional-graph-field
     :user/deleted-by
     :uuid)

    (optional-graph-field
     :user/deletion-reason
     reason-schema)]

   [:fn
    {:error/message
     "The User contact, verification, version, or lifecycle fields are inconsistent."}
    user/document-consistent?]])

;; =============================================================================
;; gesso.model descriptor
;; =============================================================================

(def user-descriptor
  {:entity-type
   user/entity-type

   :document-schema
   user-document-schema

   :identity
   {:graph-key
    :user/id}

   :version
   user/version

   ;; These are persisted equality lookups, not additional domain concepts.
   ;; gesso.model generates their Graph resolvers and registry entries.
   :lookups
   [:user/phone
    :user/email]})

(def descriptors
  [user-descriptor])

;; =============================================================================
;; User-specific Graph schema extensions
;; =============================================================================

(def custom-schema
  "User currently has no derived Graph values beyond those generated directly
   from its persisted schema."
  {})
