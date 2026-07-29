(ns net.humanhelp.site.model.invitation.core
  "Stable public boundary for the HumanHelp Invitation model.

   Code outside net.humanhelp.site.model.invitation should depend on this
   namespace rather than Invitation domain, schema, Graph, or FX internals.

   Invitation owns:

   - proposed admission to an Organization;
   - the invited recipient;
   - the proposed role and Organization scope;
   - bearer-token-backed discovery;
   - invitation lifecycle;
   - retryable acceptance progression.

   Membership owns the resulting Membership and RoleAssignment.
   User owns global identity and verified contact facts.
   Organization owns hierarchy and scope structure.

   Invitation acceptance is intentionally convergent:

     create Membership if needed
       -> create exact RoleAssignment if needed
       -> accept Invitation

   Each committed intermediate state is valid independently, so callers may
   commit one returned acceptance step and retry until :complete."
  (:require
   [gesso.model.core :as model]
   [net.humanhelp.site.model.invitation.domain :as domain]
   [net.humanhelp.site.model.invitation.fx :as invitation.fx]
   [net.humanhelp.site.model.invitation.graph :as invitation.graph]
   [net.humanhelp.site.model.invitation.schema :as invitation.schema]))

;; =============================================================================
;; Model registration
;; =============================================================================

(def module
  "Invitation's Biff module contribution.

   gesso.model supplies ordinary persisted schema and by-ID Graph plumbing.
   Invitation contributes no custom public Graph resolvers."
  (model/build-module
   invitation.schema/descriptors
   {:schema
    invitation.schema/custom-schema

    :resolvers
    invitation.graph/custom-resolvers}))

(def schema
  (:schema
   module))

(def resolvers
  (:biff.graph/resolvers
   module))

;; =============================================================================
;; Errors
;; =============================================================================

(defn- fail!
  ([type message]
   (fail!
    type
    message
    nil))

  ([type message details]
   (throw
    (ex-info
     message
     (cond->
      {:error/type
       type}

       (some?
        details)
       (assoc
        :error/details
        details))))))

(defn- require-invitation-id!
  [invitation-id]
  (when-not
   (uuid?
    invitation-id)
    (fail!
     :invitation.core/invalid-invitation-id
     "Invitation ID must be a UUID."
     {:invitation/id
      invitation-id}))

  invitation-id)

;; =============================================================================
;; Invitation reads
;; =============================================================================

(defn invitation
  "Returns Invitation by UUID, or nil when absent."
  [ctx invitation-id]
  (model/load-by-id
   invitation.schema/invitation-descriptor
   ctx
   (require-invitation-id!
    invitation-id)))

(defn require-invitation
  "Returns Invitation by UUID or throws when absent."
  [ctx invitation-id]
  (or
   (invitation
    ctx
    invitation-id)

   (fail!
    :invitation/not-found
    "The Invitation does not exist."
    {:invitation/id
     invitation-id})))

(defn invitation-by-token
  "Returns the Invitation identified by raw bearer token, or nil.

   The raw token is hashed before persistence lookup and is never returned as
   part of the Invitation document."
  [ctx token]
  (invitation.graph/invitation-by-token-hash
   ctx
   (invitation.fx/hash-token
    token)))

(defn require-invitation-by-token
  "Returns the Invitation identified by raw bearer token or throws when no
   Invitation matches."
  [ctx token]
  (or
   (invitation-by-token
    ctx
    token)

   (fail!
    :invitation/not-found
    "The Invitation does not exist.")))

;; =============================================================================
;; Organization-oriented reads
;; =============================================================================

(defn invitations-for-organization
  "Returns every historical Invitation belonging to Organization."
  [ctx organization-id]
  (invitation.graph/invitations-for-organization
   ctx
   organization-id))

(defn pending-invitations-for-organization
  "Returns Organization's persisted pending Invitations."
  [ctx organization-id]
  (invitation.graph/pending-invitations-for-organization
   ctx
   organization-id))

;; =============================================================================
;; Inviter-oriented reads
;; =============================================================================

(defn invitations-by-inviter
  "Returns every historical Invitation created by User."
  [ctx user-id]
  (invitation.graph/invitations-by-inviter
   ctx
   user-id))

(defn pending-invitations-by-inviter
  "Returns pending Invitations created by User."
  [ctx user-id]
  (invitation.graph/pending-invitations-by-inviter
   ctx
   user-id))

;; =============================================================================
;; Recipient-oriented reads
;; =============================================================================

(defn invitations-for-phone
  "Returns every historical Invitation addressed to canonical phone."
  [ctx phone]
  (invitation.graph/invitations-for-phone
   ctx
   phone))

(defn invitations-for-email
  "Returns every historical Invitation addressed to canonical email."
  [ctx email]
  (invitation.graph/invitations-for-email
   ctx
   email))

(defn pending-invitations-for-phone
  "Returns pending Invitations addressed to canonical phone."
  [ctx phone]
  (invitation.graph/pending-invitations-for-phone
   ctx
   phone))

(defn pending-invitations-for-email
  "Returns pending Invitations addressed to canonical email."
  [ctx email]
  (invitation.graph/pending-invitations-for-email
   ctx
   email))

(defn pending-invitations-for-recipient
  "Returns pending Invitations for exactly one canonical recipient.

   recipient:

     {:phone canonical-phone}

   or:

     {:email canonical-email}"
  [ctx recipient]
  (invitation.graph/pending-invitations-for-recipient
   ctx
   recipient))

;; =============================================================================
;; Scope-oriented reads
;; =============================================================================

(defn invitations-at-scope
  "Returns every historical Invitation at one exact Organization scope."
  [ctx scope]
  (invitation.graph/invitations-at-scope
   ctx
   scope))

(defn pending-invitations-at-scope
  "Returns persisted pending Invitations at one exact Organization scope."
  [ctx scope]
  (invitation.graph/pending-invitations-at-scope
   ctx
   scope))

;; =============================================================================
;; Exact-offer reads
;; =============================================================================

(defn pending-invitations-for-offer
  "Returns persisted pending Invitations for one exact proposed grant.

   input:

     {:organization-id uuid
      :phone           canonical-phone | nil
      :email           canonical-email | nil
      :role            role
      :scope           Organization scope}"
  [ctx input]
  (invitation.graph/pending-invitations-for-offer
   ctx
   input))

(defn pending-invitation-for-offer
  "Returns the one pending Invitation for an exact proposed grant, or nil.

   Multiple matching pending Invitations are treated as persisted corruption."
  [ctx input]
  (invitation.graph/pending-invitation-for-offer
   ctx
   input))

;; =============================================================================
;; Guarded Invitation dependencies
;; =============================================================================

(defn invitation-dependency
  "Returns Invitation plus a guard-only transaction fragment.

   Returns nil when Invitation does not exist."
  [ctx invitation-id]
  (invitation.fx/invitation-dependency
   ctx
   invitation-id))

(defn require-invitation-dependency
  "Returns invitation-dependency or throws when Invitation does not exist."
  [ctx invitation-id]
  (invitation.fx/require-invitation-dependency
   ctx
   invitation-id))

(defn invitation-by-token-dependency
  "Returns the Invitation identified by raw bearer token plus a guard-only
   transaction fragment.

   Returns nil when token identifies no Invitation."
  [ctx token]
  (invitation.fx/invitation-by-token-dependency
   ctx
   token))

(defn require-invitation-by-token-dependency
  "Returns invitation-by-token-dependency or throws when token identifies no
   Invitation."
  [ctx token]
  (invitation.fx/require-invitation-by-token-dependency
   ctx
   token))

;; =============================================================================
;; Acceptance workflow
;; =============================================================================

(defn acceptance-state
  "Returns a read-only summary of progress toward accepting Invitation.

   This is useful for rendering and workflow decisions. It is not an atomic
   authorization proof."
  [ctx input]
  (invitation.fx/acceptance-state
   ctx
   input))

(defn next-acceptance-step
  "Returns the next independently valid step in Invitation acceptance.

   input:

     {:token   raw-bearer-token
      :user-id accepting-user-id}

   Possible :step values:

     :create-membership
     :create-role-assignment
     :accept-invitation
     :complete

   For the first three states, :plan contains a composable transaction plan.
   Commit that plan and call this function again against fresh persisted
   state."
  [ctx input]
  (invitation.fx/next-acceptance-step
   ctx
   input))

;; =============================================================================
;; Invitation lifecycle facts
;; =============================================================================

(defn invitation-id
  [invitation-document]
  (domain/invitation-id
   invitation-document))

(defn organization-id
  [invitation-document]
  (domain/organization-id
   invitation-document))

(defn invited-by-id
  [invitation-document]
  (domain/invited-by-id
   invitation-document))

(defn offered-role
  [invitation-document]
  (domain/offered-role
   invitation-document))

(defn scope
  [invitation-document]
  (domain/scope
   invitation-document))

(defn status
  [invitation-document]
  (domain/invitation-status
   invitation-document))

(defn pending?
  [invitation-document]
  (domain/pending?
   invitation-document))

(defn accepted?
  [invitation-document]
  (domain/accepted?
   invitation-document))

(defn declined?
  [invitation-document]
  (domain/declined?
   invitation-document))

(defn revoked?
  [invitation-document]
  (domain/revoked?
   invitation-document))

(defn expired?
  [invitation-document]
  (domain/expired?
   invitation-document))

(defn terminal?
  [invitation-document]
  (domain/terminal?
   invitation-document))

(defn usable-at?
  [invitation-document now]
  (domain/usable-at?
   invitation-document
   now))

(defn past-expiration?
  [invitation-document now]
  (domain/past-expiration?
   invitation-document
   now))

;; =============================================================================
;; Recipient facts
;; =============================================================================

(defn recipient-type
  [invitation-document]
  (domain/recipient-type
   invitation-document))

(defn recipient-value
  [invitation-document]
  (domain/recipient-value
   invitation-document))

(defn addressed-to?
  "Returns whether supplied phone/email values match Invitation's canonical
   recipient.

   This does not prove ownership of the contact."
  [invitation-document recipient]
  (domain/addressed-to?
   invitation-document
   recipient))

(defn addressed-to-user?
  "Returns whether Invitation addresses a verified contact owned by User."
  [invitation-document user-document]
  (domain/addressed-to-user?
   invitation-document
   user-document))

;; =============================================================================
;; Offer facts
;; =============================================================================

(defn for-organization?
  [invitation-document expected-organization-id]
  (domain/for-organization?
   invitation-document
   expected-organization-id))

(defn invited-by?
  [invitation-document expected-user-id]
  (domain/invited-by?
   invitation-document
   expected-user-id))

(defn offers-role?
  [invitation-document expected-role]
  (domain/offers-role?
   invitation-document
   expected-role))

(defn at-scope?
  [invitation-document expected-scope]
  (domain/at-scope?
   invitation-document
   expected-scope))

;; =============================================================================
;; Acceptance result facts
;; =============================================================================

(defn accepted-by-id
  [invitation-document]
  (domain/accepted-by-id
   invitation-document))

(defn accepted-membership-id
  [invitation-document]
  (domain/accepted-membership-id
   invitation-document))

(defn accepted-role-assignment-id
  [invitation-document]
  (domain/accepted-role-assignment-id
   invitation-document))

;; =============================================================================
;; Invitation creation
;; =============================================================================

(defn plan-create-invitation
  "Plans creation of a new authorized Invitation.

   The returned result contains the raw bearer token exactly once. The
   persisted Invitation contains only its token hash."
  [ctx input]
  (invitation.fx/plan-create-invitation
   ctx
   input))

;; =============================================================================
;; Recipient lifecycle planning
;; =============================================================================

(defn plan-decline-invitation
  "Plans decline by the Invitation recipient."
  [ctx input]
  (invitation.fx/plan-decline-invitation
   ctx
   input))

;; =============================================================================
;; Administrative lifecycle planning
;; =============================================================================

(defn plan-revoke-invitation
  "Plans revocation by an authorized Organization admin."
  [ctx input]
  (invitation.fx/plan-revoke-invitation
   ctx
   input))

(defn plan-expire-invitation
  "Plans explicit materialization of an Invitation that has reached its
   expiration time."
  [ctx input]
  (invitation.fx/plan-expire-invitation
   ctx
   input))

;; =============================================================================
;; Final acceptance planning
;; =============================================================================

(defn plan-accept-invitation
  "Plans only the final pending -> accepted transition.

   The required active Membership and exact active RoleAssignment must already
   exist.

   Most callers should use next-acceptance-step instead."
  [ctx input]
  (invitation.fx/plan-accept-invitation
   ctx
   input))
