(ns net.humanhelp.site.model.user.schema
  "Canonical persisted User schema and gesso.model descriptor.

   This namespace is the single declaration of User persistence shape and the
   ordinary Graph exposure derived from that shape.

   User domain owns semantic validity. gesso.model owns mechanical schema
   introspection, persistence normalization, by-ID reads, declared equality
   lookups, Graph projection, and registry generation.

   Nothing here knows about Membership, Organization, Invitation,
   authentication policy, authorization, or transaction planning."
  (:require
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.user.domain :as user]))

;; =============================================================================
;; User value schemas
;; =============================================================================

(def ^:private phone-schema
  [:fn
   {:error/message "must be a canonical E.164 phone number"}
   user/phone?])

(def ^:private email-schema
  [:fn
   {:error/message "must be a canonical HumanHelp email address"}
   user/email?])

(def ^:private display-name-schema
  [:fn
   {:error/message
    "must be a canonical non-blank display name of at most 120 characters"}
   user/display-name?])

(def ^:private status-schema
  [:fn
   {:error/message "must be active, suspended, or deleted"}
   user/status?])

(def ^:private reason-schema
  [:fn
   {:error/message "must be a qualified keyword"}
   qualified-keyword?])

;; =============================================================================
;; Persisted field declarations
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
  "Complete persisted User document.

   The structural map is intentionally closed. The final domain predicate owns
   relationships between fields such as verification/contact consistency and
   lifecycle audit consistency."
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
  "Declarative description from which gesso.model derives ordinary User model
   plumbing.

   Phone and email are equality lookups because they are globally unique User
   contact facts. They are not identity requirements: a valid User may have
   neither."
  {:entity-type
   user/entity-type

   :document-schema
   user-document-schema

   :identity
   {:graph-key :user/id}

   :version
   user/version

   :lookups
   [:user/phone
    :user/email]})

(def descriptors
  [user-descriptor])

;; =============================================================================
;; User-specific Graph schema extensions
;; =============================================================================

(def custom-schema
  "Schema for genuinely derived User Graph values.

   User currently has none; every exposed User value is generated from the
   persisted document declaration above."
  {})
