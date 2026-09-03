(ns net.humanhelp.site.model.organization.core
  "Public boundary for the HumanHelp Organization model.

   Code outside net.humanhelp.site.model.organization should depend on this
   namespace rather than domain, schema, Graph, or FX internals.

   Organization owns hierarchy structure and scope semantics. Mutation planners
   return composable transaction fragments; cross-model authorization is added
   by the caller before the final atomic transaction is committed."
  (:require
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.organization.domain :as domain]
   [net.humanhelp.site.model.organization.fx :as org.fx]
   [net.humanhelp.site.model.organization.graph :as org.graph]
   [net.humanhelp.site.model.organization.schema :as org.schema]))

;; =============================================================================
;; Model registration
;; =============================================================================

(def module
  "Organization's Biff module contribution.

   gesso.model supplies conventional persisted schema and resolvers. Organization
   contributes only its hierarchy-specific Graph schema and resolvers.

   Install gesso.model.tx/module once separately at the application level."
  (model/build-module
   org.schema/descriptors
   {:schema
    org.schema/custom-schema

    :resolvers
    org.graph/custom-resolvers}))

(def schema
  (:schema module))

(def resolvers
  (:biff.graph/resolvers module))

;; =============================================================================
;; Scope values
;; =============================================================================

(def scope-types
  domain/scope-types)

(def parent-scope-types
  domain/parent-scope-types)

(defn scope?
  [value]
  (domain/scope? value))

(defn parent-scope?
  [value]
  (domain/parent-scope? value))

(defn scope-type
  [scope]
  (domain/scope-type scope))

(defn scope-id
  [scope]
  (domain/scope-id scope))

(defn organization-scope
  [organization-id]
  (domain/organization-scope
   organization-id))

(defn organization-group-scope
  [organization-group-id]
  (domain/organization-group-scope
   organization-group-id))

(defn location-scope
  [location-id]
  (domain/location-scope
   location-id))

(defn organization-scope?
  [scope]
  (domain/organization-scope?
   scope))

(defn organization-group-scope?
  [scope]
  (domain/organization-group-scope?
   scope))

(defn location-scope?
  [scope]
  (domain/location-scope?
   scope))

(defn same-scope?
  [left right]
  (domain/same-scope?
   left
   right))

;; =============================================================================
;; Scope-context semantics
;; =============================================================================

(defn scope-context?
  [value]
  (domain/scope-context? value))

(defn scope-context-organization-id
  [scope-context]
  (domain/scope-context-organization-id
   scope-context))

(defn scope-context-target
  [scope-context]
  (domain/scope-context-target
   scope-context))

(defn scope-context-operational?
  [scope-context]
  (domain/scope-context-operational?
   scope-context))

(defn scope-applies?
  [scope-context scope]
  (domain/scope-applies?
   scope-context
   scope))

(defn scope-context
  "Returns the authoritative current context for scope, or nil when the target
   entity does not exist.

   A present target with corrupt or inconsistent hierarchy state is an error."
  [ctx scope]
  (some->
   (org.graph/scope-snapshot
    ctx
    scope)
   :scope-context))

(defn require-scope-context
  "Returns the authoritative current context for scope or throws when the
   target entity does not exist."
  [ctx scope]
  (:scope-context
   (org.graph/require-scope-snapshot
    ctx
    scope)))

(defn scope-dependency
  "Returns the current scope context plus the guard-only transaction fragment
   that keeps every Organization document used to derive it current until
   commit.

   This is the normal cross-model API when another model makes an atomic
   decision from Organization hierarchy state.

   Returns nil when the target scope does not exist."
  [ctx scope]
  (org.fx/scope-dependency
   ctx
   scope))

(defn require-scope-dependency
  "Returns scope-dependency or throws when the target scope does not exist."
  [ctx scope]
  (org.fx/require-scope-dependency
   ctx
   scope))

;; =============================================================================
;; Entity reads
;; =============================================================================

(defn organization
  "Returns the current Organization document, or nil when absent."
  [ctx organization-id]
  (when-not
   (uuid? organization-id)
    (throw
     (ex-info
      "Organization ID must be a UUID."
      {:error/type
       :organization.core/invalid-organization-id

       :error/details
       {:organization/id organization-id}})))

  (model/load-by-id
   org.schema/organization-descriptor
   ctx
   organization-id))

(defn require-organization
  "Returns the current Organization document or throws when absent."
  [ctx organization-id]
  (or
   (organization
    ctx
    organization-id)

   (throw
    (ex-info
     "The Organization does not exist."
     {:error/type
      :organization/not-found

      :error/details
      {:organization/id organization-id}}))))

(defn organization-group
  "Returns the current Organization Group document, or nil when absent."
  [ctx organization-group-id]
  (when-not
   (uuid? organization-group-id)
    (throw
     (ex-info
      "Organization Group ID must be a UUID."
      {:error/type
       :organization.core/invalid-organization-group-id

       :error/details
       {:organization-group/id organization-group-id}})))

  (model/load-by-id
   org.schema/organization-group-descriptor
   ctx
   organization-group-id))

(defn require-organization-group
  "Returns the current Organization Group document or throws when absent."
  [ctx organization-group-id]
  (or
   (organization-group
    ctx
    organization-group-id)

   (throw
    (ex-info
     "The Organization Group does not exist."
     {:error/type
      :organization-group/not-found

      :error/details
      {:organization-group/id
       organization-group-id}}))))

(defn location
  "Returns the current Location document, or nil when absent."
  [ctx location-id]
  (when-not
   (uuid? location-id)
    (throw
     (ex-info
      "Location ID must be a UUID."
      {:error/type
       :organization.core/invalid-location-id

       :error/details
       {:location/id location-id}})))

  (model/load-by-id
   org.schema/location-descriptor
   ctx
   location-id))

(defn require-location
  "Returns the current Location document or throws when absent."
  [ctx location-id]
  (or
   (location
    ctx
    location-id)

   (throw
    (ex-info
     "The Location does not exist."
     {:error/type
      :location/not-found

      :error/details
      {:location/id location-id}}))))

;; =============================================================================
;; Stable document facts
;; =============================================================================

(defn organization-id
  [document]
  (domain/organization-id
   document))

(defn organization-name
  [document]
  (domain/organization-name
   document))

(defn organization-status
  [document]
  (domain/organization-status
   document))

(defn organization-active?
  [document]
  (domain/organization-active?
   document))

(defn organization-suspended?
  [document]
  (domain/organization-suspended?
   document))

(defn organization-closed?
  [document]
  (domain/organization-closed?
   document))

(defn organization-group-id
  [document]
  (domain/organization-group-id
   document))

(defn organization-group-organization-id
  [document]
  (domain/organization-group-organization-id
   document))

(defn organization-group-name
  [document]
  (domain/organization-group-name
   document))

(defn organization-group-status
  [document]
  (domain/organization-group-status
   document))

(defn organization-group-active?
  [document]
  (domain/organization-group-active?
   document))

(defn organization-group-suspended?
  [document]
  (domain/organization-group-suspended?
   document))

(defn organization-group-closed?
  [document]
  (domain/organization-group-closed?
   document))

(defn organization-group-parent-scope
  [document]
  (domain/organization-group-parent-scope
   document))

(defn location-id
  [document]
  (domain/location-id
   document))

(defn location-organization-id
  [document]
  (domain/location-organization-id
   document))

(defn location-name
  [document]
  (domain/location-name
   document))

(defn location-status
  [document]
  (domain/location-status
   document))

(defn location-active?
  [document]
  (domain/location-active?
   document))

(defn location-suspended?
  [document]
  (domain/location-suspended?
   document))

(defn location-closed?
  [document]
  (domain/location-closed?
   document))

(defn location-parent-scope
  [document]
  (domain/location-parent-scope
   document))

;; =============================================================================
;; Organization mutation planning / execution
;; =============================================================================

(defn- transaction-plan
  [{:keys [transaction-fragment transaction-options]}]
  (merge
   transaction-fragment
   transaction-options))

(defn- commit-planned!
  "Commit one already-planned Organization operation through the shared Gesso
   model transaction boundary.

   Organization planners remain the semantic source of truth. This helper adds
   only stable commit status and transaction-established progression to the
   planner result; raw transaction fragments and XTDB internals remain private."
  [ctx {:keys [result] :as planned}]
  (let [transaction
        (model.tx/transact!
         ctx
         (transaction-plan planned))]
    (cond->
     (assoc
      result
      :commit/status
      (:commit/status transaction))

      (contains? transaction :progression)
      (assoc
       :progression
       (:progression transaction)))))

(defn plan-create-organization
  [ctx input]
  (org.fx/plan-create-organization
   ctx
   input))

(defn create-organization
  "Authoritatively creates one Organization in a single Gesso model
   transaction.

   input is the same semantic input accepted by plan-create-organization."
  [ctx input]
  (commit-planned!
   ctx
   (plan-create-organization
    ctx
    input)))

(defn plan-create-organization-group
  [ctx input]
  (org.fx/plan-create-organization-group
   ctx
   input))

(defn plan-create-location
  [ctx input]
  (org.fx/plan-create-location
   ctx
   input))

(defn create-location
  "Authoritatively creates one Location beneath an existing operational
   Organization/Organization Group in a single Gesso model transaction.

   input is the same semantic input accepted by plan-create-location."
  [ctx input]
  (commit-planned!
   ctx
   (plan-create-location
    ctx
    input)))

(defn plan-rename-organization
  [ctx input]
  (org.fx/plan-rename-organization
   ctx
   input))

(defn plan-suspend-organization
  [ctx input]
  (org.fx/plan-suspend-organization
   ctx
   input))

(defn plan-reactivate-organization
  [ctx input]
  (org.fx/plan-reactivate-organization
   ctx
   input))

(defn plan-close-organization
  [ctx input]
  (org.fx/plan-close-organization
   ctx
   input))

(defn plan-rename-organization-group
  [ctx input]
  (org.fx/plan-rename-organization-group
   ctx
   input))

(defn plan-move-organization-group
  [ctx input]
  (org.fx/plan-move-organization-group
   ctx
   input))

(defn plan-suspend-organization-group
  [ctx input]
  (org.fx/plan-suspend-organization-group
   ctx
   input))

(defn plan-reactivate-organization-group
  [ctx input]
  (org.fx/plan-reactivate-organization-group
   ctx
   input))

(defn plan-close-organization-group
  [ctx input]
  (org.fx/plan-close-organization-group
   ctx
   input))

(defn plan-rename-location
  [ctx input]
  (org.fx/plan-rename-location
   ctx
   input))

(defn plan-move-location
  [ctx input]
  (org.fx/plan-move-location
   ctx
   input))

(defn plan-suspend-location
  [ctx input]
  (org.fx/plan-suspend-location
   ctx
   input))

(defn plan-reactivate-location
  [ctx input]
  (org.fx/plan-reactivate-location
   ctx
   input))

(defn plan-close-location
  [ctx input]
  (org.fx/plan-close-location
   ctx
   input))
