(ns net.humanhelp.site.model.user.core
  "Stable public boundary for the HumanHelp User model.

   Code outside net.humanhelp.site.model.user should require this namespace,
   not User domain, schema, Graph, or FX implementation namespaces.

   User owns global identity, optional contact/profile facts, verification
   state, and account lifecycle. It does not own Organization membership,
   roles, authorization, invitations, or authentication-flow policy.

   gesso.model supplies ordinary persisted-schema, lookup, Graph, concurrency,
   and transaction-fragment mechanics underneath this facade. Public callers
   depend on HumanHelp concepts rather than those implementation details."
  (:require
   [gesso.model.core :as model]
   [net.humanhelp.site.model.user.domain :as user.domain]
   [net.humanhelp.site.model.user.fx :as user.fx]
   [net.humanhelp.site.model.user.graph :as user.graph]
   [net.humanhelp.site.model.user.schema :as user.schema]))

;; =============================================================================
;; Model registration
;; =============================================================================

(def module
  "User's Biff module contribution.

   gesso.model derives ordinary by-ID, phone, email, and persisted-field Graph
   behavior from the User descriptor. gesso.model.tx/module is installed once
   separately at the application level."
  (model/build-module
   user.schema/descriptors
   {:schema user.schema/custom-schema
    :resolvers user.graph/custom-resolvers}))

(def schema
  (:schema module))

(def resolvers
  (:biff.graph/resolvers module))

;; =============================================================================
;; Public read errors and input normalization
;; =============================================================================

(defn- fail!
  ([type message]
   (fail! type message nil))
  ([type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type type}
       (some? details)
       (assoc :error/details details))))))

(defn- require-user-id!
  [user-id]
  (when-not (uuid? user-id)
    (fail!
     :user.core/invalid-user-id
     "User ID must be a UUID."
     {:user/id user-id}))
  user-id)

(defn- require-phone!
  [value]
  (let [phone
        (user.domain/normalize-phone value)]
    (when-not (user.domain/phone? phone)
      (fail!
       :user.core/invalid-phone
       "User phone lookup requires a canonical E.164 phone number."
       {:phone value}))
    phone))

(defn- require-email!
  [value]
  (let [email
        (user.domain/normalize-email value)]
    (when-not (user.domain/email? email)
      (fail!
       :user.core/invalid-email
       "User email lookup requires a valid email address."
       {:email value}))
    email))

(defn- require-present!
  [document lookup]
  (or
   document
   (fail!
    :user/not-found
    "The User does not exist."
    {:lookup lookup})))

;; =============================================================================
;; Stable User reads
;; =============================================================================

(defn user
  "Returns the current User document by UUID, or nil when absent."
  [ctx user-id]
  (model/load-by-id
   user.schema/user-descriptor
   ctx
   (require-user-id! user-id)))

(defn require-user
  "Returns the current User document by UUID or throws when absent."
  [ctx user-id]
  (require-present!
   (user ctx user-id)
   {:user/id user-id}))

(defn user-by-phone
  "Returns the User with phone, or nil when none exists.

   User accepts canonical E.164 here. Friendly/local phone parsing belongs at
   the authentication or input boundary, not in the global User model."
  [ctx phone]
  (let [phone
        (require-phone! phone)]
    (model/load-by-lookup
     user.schema/user-descriptor
     ctx
     :user/phone
     phone)))

(defn require-user-by-phone
  "Returns the User with phone or throws when none exists."
  [ctx phone]
  (let [phone
        (require-phone! phone)]
    (require-present!
     (model/load-by-lookup
      user.schema/user-descriptor
      ctx
      :user/phone
      phone)
     {:user/phone phone})))

(defn user-by-email
  "Returns the User with email, or nil when none exists.

   Email lookup uses User's canonical case-insensitive representation."
  [ctx email]
  (let [email
        (require-email! email)]
    (model/load-by-lookup
     user.schema/user-descriptor
     ctx
     :user/email
     email)))

(defn require-user-by-email
  "Returns the User with email or throws when none exists."
  [ctx email]
  (let [email
        (require-email! email)]
    (require-present!
     (model/load-by-lookup
      user.schema/user-descriptor
      ctx
      :user/email
      email)
     {:user/email email})))

;; =============================================================================
;; Cross-model User dependencies
;; =============================================================================

(defn user-dependency
  "Returns the current User plus a guard-only transaction fragment.

   Use this from another top-level model when an atomic decision depends on
   current User state. The returned guard keeps the exact User version used by
   that decision current until the composed transaction commits.

   Returns nil when the User does not exist."
  [ctx user-id]
  (user.fx/user-dependency
   ctx
   user-id))

(defn require-user-dependency
  "Returns user-dependency or throws when the User does not exist."
  [ctx user-id]
  (user.fx/require-user-dependency
   ctx
   user-id))

;; =============================================================================
;; Stable User value API
;; =============================================================================

(defn normalize-phone
  [value]
  (user.domain/normalize-phone value))

(defn phone?
  [value]
  (user.domain/phone? value))

(defn normalize-email
  [value]
  (user.domain/normalize-email value))

(defn email?
  [value]
  (user.domain/email? value))

(defn normalize-display-name
  [value]
  (user.domain/normalize-display-name value))

(defn display-name?
  [value]
  (user.domain/display-name? value))

;; =============================================================================
;; Stable User document facts
;; =============================================================================

(defn user-id
  [user-document]
  (user.domain/user-id user-document))

(defn user-phone
  [user-document]
  (user.domain/user-phone user-document))

(defn user-email
  [user-document]
  (user.domain/user-email user-document))

(defn user-display-name
  [user-document]
  (user.domain/user-display-name user-document))

(defn user-status
  [user-document]
  (user.domain/user-status user-document))

(defn active?
  [user-document]
  (user.domain/active? user-document))

(defn suspended?
  [user-document]
  (user.domain/suspended? user-document))

(defn deleted?
  [user-document]
  (user.domain/deleted? user-document))

(defn has-phone?
  [user-document]
  (user.domain/has-phone? user-document))

(defn has-email?
  [user-document]
  (user.domain/has-email? user-document))

(defn has-contact?
  [user-document]
  (user.domain/has-contact? user-document))

(defn phone-verified?
  [user-document]
  (user.domain/phone-verified? user-document))

(defn email-verified?
  [user-document]
  (user.domain/email-verified? user-document))

(defn has-verified-contact?
  [user-document]
  (user.domain/has-verified-contact? user-document))

;; =============================================================================
;; Stable User mutation planning API
;; =============================================================================

(defn plan-create-user
  [ctx input]
  (user.fx/plan-create-user
   ctx
   input))

(defn plan-edit-profile
  [ctx input]
  (user.fx/plan-edit-profile
   ctx
   input))

(defn plan-replace-phone
  [ctx input]
  (user.fx/plan-replace-phone
   ctx
   input))

(defn plan-replace-email
  [ctx input]
  (user.fx/plan-replace-email
   ctx
   input))

(defn plan-remove-phone
  [ctx input]
  (user.fx/plan-remove-phone
   ctx
   input))

(defn plan-remove-email
  [ctx input]
  (user.fx/plan-remove-email
   ctx
   input))

(defn plan-verify-phone
  [ctx input]
  (user.fx/plan-verify-phone
   ctx
   input))

(defn plan-verify-email
  [ctx input]
  (user.fx/plan-verify-email
   ctx
   input))

(defn plan-suspend-user
  [ctx input]
  (user.fx/plan-suspend-user
   ctx
   input))

(defn plan-reactivate-user
  [ctx input]
  (user.fx/plan-reactivate-user
   ctx
   input))

(defn plan-delete-user
  [ctx input]
  (user.fx/plan-delete-user
   ctx
   input))
