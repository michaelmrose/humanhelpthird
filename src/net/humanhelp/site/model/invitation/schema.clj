(ns net.humanhelp.site.model.invitation.schema
  "Canonical persisted Invitation schema and gesso.model descriptor.

   Invitation domain owns semantic validity and lifecycle consistency.

   This namespace owns:

   - the persisted Invitation document shape;
   - scalar validation;
   - ordinary Graph exposure;
   - the gesso.model descriptor.

   The opaque Invitation token hash is deliberately persisted without Graph
   exposure. Token lookup is an internal Invitation read concern and must not
   become a public generated Graph lookup."
  (:require
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.invitation.domain :as invitation]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Scalar schemas
;; =============================================================================

(def ^:private phone-schema
  [:fn
   {:error/message
    "must be a canonical E.164 phone number"}
   user/phone?])

(def ^:private email-schema
  [:fn
   {:error/message
    "must be a canonical HumanHelp email address"}
   user/email?])

(def ^:private role-schema
  [:fn
   {:error/message
    "must be helper, supervisor, or admin"}
   membership/role?])

(def ^:private status-schema
  [:fn
   {:error/message
    "must be pending, accepted, declined, revoked, or expired"}
   invitation/status?])

(def ^:private token-hash-schema
  [:fn
   {:error/message
    "must be a valid opaque Invitation token hash"}
   invitation/token-hash?])

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
;; Persisted-field declarations
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
  ([key schema]
   [key
    {:optional          true
     :gesso.model/graph true}
    schema])

  ([key graph-key schema]
   [key
    {:optional          true
     :gesso.model/graph graph-key}
    schema]))

(defn- optional-field
  "Persisted optional field deliberately omitted from ordinary Graph exposure."
  [key schema]
  [key
   {:optional true}
   schema])

;; =============================================================================
;; Persisted Invitation document
;; =============================================================================

(def invitation-document-schema
  "Complete persisted Invitation document.

   The map is closed. invitation/document-consistent? owns relationships among
   recipient fields, lifecycle status, terminal audit fields, expiration, and
   revision state.

   token-hash is intentionally persisted but not Graph-visible."
  [:and

   [:map
    {:closed true}

    [:xt/id
     :uuid]

    ;; -------------------------------------------------------------------------
    ;; Proposed Organization admission
    ;; -------------------------------------------------------------------------

    (graph-field
     :invitation/organization
     :invitation/organization-id
     :uuid)

    (graph-field
     :invitation/invited-by
     :invitation/invited-by-id
     :uuid)

    ;; -------------------------------------------------------------------------
    ;; Recipient
    ;; -------------------------------------------------------------------------

    (optional-graph-field
     :invitation/phone
     phone-schema)

    (optional-graph-field
     :invitation/email
     email-schema)

    ;; -------------------------------------------------------------------------
    ;; Proposed authorization
    ;; -------------------------------------------------------------------------

    (graph-field
     :invitation/role
     role-schema)

    (graph-field
     :invitation/scope-type
     scope-type-schema)

    (graph-field
     :invitation/scope-id
     :uuid)

    ;; -------------------------------------------------------------------------
    ;; Bearer-token material
    ;; -------------------------------------------------------------------------

    ;; Never mark this :gesso.model/graph.
    [:invitation/token-hash
     token-hash-schema]

    ;; -------------------------------------------------------------------------
    ;; Lifecycle
    ;; -------------------------------------------------------------------------

    (graph-field
     :invitation/status
     status-schema)

    (graph-field
     :invitation/revision
     model.schema/revision-schema)

    (graph-field
     :invitation/created-at
     model.schema/instant-schema)

    (graph-field
     :invitation/updated-at
     model.schema/instant-schema)

    (graph-field
     :invitation/expires-at
     model.schema/instant-schema)

    ;; -------------------------------------------------------------------------
    ;; Acceptance
    ;; -------------------------------------------------------------------------

    (optional-graph-field
     :invitation/accepted-at
     model.schema/instant-schema)

    (optional-graph-field
     :invitation/accepted-by
     :invitation/accepted-by-id
     :uuid)

    (optional-graph-field
     :invitation/membership
     :invitation/membership-id
     :uuid)

    (optional-graph-field
     :invitation/role-assignment
     :invitation/role-assignment-id
     :uuid)

    ;; -------------------------------------------------------------------------
    ;; Decline
    ;; -------------------------------------------------------------------------

    (optional-graph-field
     :invitation/declined-at
     model.schema/instant-schema)

    (optional-graph-field
     :invitation/declined-by
     :invitation/declined-by-id
     :uuid)

    ;; -------------------------------------------------------------------------
    ;; Revocation
    ;; -------------------------------------------------------------------------

    (optional-graph-field
     :invitation/revoked-at
     model.schema/instant-schema)

    (optional-graph-field
     :invitation/revoked-by
     :invitation/revoked-by-id
     :uuid)

    (optional-graph-field
     :invitation/revocation-reason
     reason-schema)

    ;; -------------------------------------------------------------------------
    ;; Expiration
    ;; -------------------------------------------------------------------------

    (optional-graph-field
     :invitation/expired-at
     model.schema/instant-schema)]

   [:fn
    {:error/message
     "The Invitation recipient, offer, expiration, version, or lifecycle fields are inconsistent."}
    invitation/document-consistent?]])

;; =============================================================================
;; gesso.model descriptor
;; =============================================================================

(def invitation-descriptor
  "Declarative description of persisted Invitation mechanics.

   No equality lookups are generated:

   - phone and email are not unique Invitation values;
   - Organization, inviter, Membership, and RoleAssignment relationships are
     plural or historical relationships;
   - token-hash is security-sensitive and remains an internal Invitation
     lookup implemented in invitation.graph."
  {:entity-type
   invitation/entity-type

   :document-schema
   invitation-document-schema

   :identity
   {:graph-key
    :invitation/id}

   :version
   invitation/version})

(def descriptors
  [invitation-descriptor])

;; =============================================================================
;; Invitation-specific Graph schema
;; =============================================================================

(def custom-schema
  "Invitation currently needs no custom derived Graph scalar values.

   Structural scope values and relationship-oriented reads belong in
   invitation.graph. Ordinary persisted Invitation fields are generated from
   the descriptor."
  {})
