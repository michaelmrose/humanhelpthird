(ns net.humanhelp.site.model.user.schema
  "Malli schemas for persisted User-model documents and Graph attributes.

   Structural schemas describe the shape and scalar types of user identities,
   memberships, role assignments, and invitations. Complete lifecycle and
   cross-field invariants remain owned by the corresponding domain namespace
   and are applied as the final predicate of each document schema.

   This namespace does not query XTDB, authorize actors, or define workflows."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.domain.common :as user.common]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.model.user.domain.invitation :as invitation]
   [net.humanhelp.site.model.user.domain.membership :as membership]
   [net.humanhelp.site.model.user.domain.role :as role]))

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

(def phone-schema
  [:fn
   {:error/message "must be a canonical E.164 phone number"}
   user.common/phone?])

(def email-schema
  [:fn
   {:error/message "must be a canonical HumanHelp email address"}
   user.common/email?])

(def role-schema
  [:fn
   {:error/message "must be helper, supervisor, or admin"}
   user.common/role?])

(def scope-type-schema
  [:fn
   {:error/message
    "must be organization, organization-group, or location"}
   user.common/scope-type?])

(def scope-reference-schema
  [:and
   [:map {:closed true}
    [:scope/type scope-type-schema]
    [:scope/id :uuid]]

   [:fn
    {:error/message "must be a valid User-model scope reference"}
    user.common/scope-reference?]])

;; =============================================================================
;; User identity document
;; =============================================================================

(def user-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]

    [:user/phone
     {:optional true}
     phone-schema]

    [:user/email
     {:optional true}
     email-schema]

    [:user/display-name
     {:optional true}
     [:string {:min 1
               :max identity/display-name-max}]]

    [:user/status
     [:fn
      {:error/message "must be active, suspended, or deleted"}
      identity/status?]]

    [:user/revision revision-schema]
    [:user/created-at instant-schema]
    [:user/updated-at instant-schema]

    [:user/phone-verified-at
     {:optional true}
     instant-schema]

    [:user/email-verified-at
     {:optional true}
     instant-schema]

    [:user/suspended-at
     {:optional true}
     instant-schema]

    [:user/suspended-by
     {:optional true}
     :uuid]

    [:user/suspension-reason
     {:optional true}
     reason-schema]

    [:user/deleted-at
     {:optional true}
     instant-schema]

    [:user/deleted-by
     {:optional true}
     :uuid]

    [:user/deletion-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The user identity lifecycle and contact fields are inconsistent."}
    identity/document-consistent?]])

;; =============================================================================
;; Membership document
;; =============================================================================

(def membership-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:membership/user :uuid]
    [:membership/organization :uuid]

    [:membership/status
     [:fn
      {:error/message "must be active, suspended, or revoked"}
      membership/status?]]

    [:membership/revision revision-schema]
    [:membership/created-at instant-schema]
    [:membership/updated-at instant-schema]

    [:membership/suspended-at
     {:optional true}
     instant-schema]

    [:membership/suspended-by
     {:optional true}
     :uuid]

    [:membership/suspension-reason
     {:optional true}
     reason-schema]

    [:membership/revoked-at
     {:optional true}
     instant-schema]

    [:membership/revoked-by
     {:optional true}
     :uuid]

    [:membership/revocation-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The membership lifecycle fields are inconsistent."}
    membership/document-consistent?]])

;; =============================================================================
;; Role-assignment document
;; =============================================================================

(def role-assignment-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:role-assignment/membership :uuid]
    [:role-assignment/organization :uuid]
    [:role-assignment/role role-schema]
    [:role-assignment/scope-type scope-type-schema]
    [:role-assignment/scope-id :uuid]

    [:role-assignment/status
     [:fn
      {:error/message "must be active or revoked"}
      role/status?]]

    [:role-assignment/revision revision-schema]
    [:role-assignment/created-at instant-schema]
    [:role-assignment/updated-at instant-schema]

    [:role-assignment/assigned-by
     {:optional true}
     :uuid]

    [:role-assignment/assignment-reason
     {:optional true}
     reason-schema]

    [:role-assignment/revoked-at
     {:optional true}
     instant-schema]

    [:role-assignment/revoked-by
     {:optional true}
     :uuid]

    [:role-assignment/revocation-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The role-assignment scope or lifecycle fields are inconsistent."}
    role/document-consistent?]])

;; =============================================================================
;; Invitation document
;; =============================================================================

(def invitation-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:invitation/organization :uuid]
    [:invitation/invited-by :uuid]

    [:invitation/phone
     {:optional true}
     phone-schema]

    [:invitation/email
     {:optional true}
     email-schema]

    [:invitation/role role-schema]
    [:invitation/scope-type scope-type-schema]
    [:invitation/scope-id :uuid]

    [:invitation/token-hash
     [:fn
      {:error/message "must be a valid opaque invitation token hash"}
      invitation/token-hash?]]

    [:invitation/status
     [:fn
      {:error/message
       "must be pending, accepted, declined, revoked, or expired"}
      invitation/status?]]

    [:invitation/revision revision-schema]
    [:invitation/created-at instant-schema]
    [:invitation/updated-at instant-schema]
    [:invitation/expires-at instant-schema]

    [:invitation/accepted-at
     {:optional true}
     instant-schema]

    [:invitation/accepted-by
     {:optional true}
     :uuid]

    [:invitation/membership
     {:optional true}
     :uuid]

    [:invitation/role-assignment
     {:optional true}
     :uuid]

    [:invitation/declined-at
     {:optional true}
     instant-schema]

    [:invitation/declined-by
     {:optional true}
     :uuid]

    [:invitation/revoked-at
     {:optional true}
     instant-schema]

    [:invitation/revoked-by
     {:optional true}
     :uuid]

    [:invitation/revocation-reason
     {:optional true}
     reason-schema]]

   [:fn
    {:error/message
     "The invitation recipient, scope, expiration, or lifecycle fields are inconsistent."}
    invitation/document-consistent?]])

;; =============================================================================
;; Biff/Malli registry contribution
;; =============================================================================

(def schema
  "Malli schemas contributed by the User model.

   Table keywords validate complete persisted documents. Attribute keywords are
   also registered independently for Gesso Graph input and output validation."
  {::instant instant-schema
   ::revision revision-schema
   ::reason reason-schema
   ::phone phone-schema
   ::email email-schema
   ::role role-schema
   ::scope-type scope-type-schema
   ::scope-reference scope-reference-schema

   ;; Shared User-model Graph values
   :user/role role-schema
   :user/scope-type scope-type-schema
   :user/scope-reference scope-reference-schema

   ;; User identity attributes
   :user/id :uuid
   :user/phone phone-schema
   :user/email email-schema
   :user/display-name
   [:string {:min 1
             :max identity/display-name-max}]
   :user/status
   [:fn identity/status?]
   :user/revision revision-schema
   :user/created-at instant-schema
   :user/updated-at instant-schema
   :user/phone-verified-at instant-schema
   :user/email-verified-at instant-schema
   :user/suspended-at instant-schema
   :user/suspended-by :uuid
   :user/suspension-reason reason-schema
   :user/deleted-at instant-schema
   :user/deleted-by :uuid
   :user/deletion-reason reason-schema
   :user/doc user-document-schema
   :user user-document-schema

   ;; Membership attributes
   :membership/id :uuid
   :membership/user-id :uuid
   :membership/organization-id :uuid
   :membership/status
   [:fn membership/status?]
   :membership/revision revision-schema
   :membership/created-at instant-schema
   :membership/updated-at instant-schema
   :membership/suspended-at instant-schema
   :membership/suspended-by :uuid
   :membership/suspension-reason reason-schema
   :membership/revoked-at instant-schema
   :membership/revoked-by :uuid
   :membership/revocation-reason reason-schema
   :membership/doc membership-document-schema
   :membership membership-document-schema

   ;; Role-assignment attributes
   :role-assignment/id :uuid
   :role-assignment/membership-id :uuid
   :role-assignment/organization-id :uuid
   :role-assignment/role role-schema
   :role-assignment/scope-type scope-type-schema
   :role-assignment/scope-id :uuid
   :role-assignment/scope scope-reference-schema
   :role-assignment/status
   [:fn role/status?]
   :role-assignment/revision revision-schema
   :role-assignment/created-at instant-schema
   :role-assignment/updated-at instant-schema
   :role-assignment/assigned-by :uuid
   :role-assignment/assignment-reason reason-schema
   :role-assignment/revoked-at instant-schema
   :role-assignment/revoked-by :uuid
   :role-assignment/revocation-reason reason-schema
   :role-assignment/doc role-assignment-document-schema
   :role-assignment role-assignment-document-schema

   ;; Invitation attributes
   :invitation/id :uuid
   :invitation/organization-id :uuid
   :invitation/invited-by-id :uuid
   :invitation/phone phone-schema
   :invitation/email email-schema
   :invitation/recipient-type
   [:enum :phone :email]
   :invitation/recipient-value
   [:or phone-schema email-schema]
   :invitation/role role-schema
   :invitation/scope-type scope-type-schema
   :invitation/scope-id :uuid
   :invitation/scope scope-reference-schema
   :invitation/token-hash
   [:fn invitation/token-hash?]
   :invitation/status
   [:fn invitation/status?]
   :invitation/revision revision-schema
   :invitation/created-at instant-schema
   :invitation/updated-at instant-schema
   :invitation/expires-at instant-schema
   :invitation/accepted-at instant-schema
   :invitation/accepted-by-id :uuid
   :invitation/membership-id :uuid
   :invitation/role-assignment-id :uuid
   :invitation/declined-at instant-schema
   :invitation/declined-by-id :uuid
   :invitation/revoked-at instant-schema
   :invitation/revoked-by-id :uuid
   :invitation/revocation-reason reason-schema
   :invitation/doc invitation-document-schema
   :invitation invitation-document-schema})
