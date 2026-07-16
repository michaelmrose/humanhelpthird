(ns net.humanhelp.site.model.organization.core
  "The supported public facade for the HumanHelp Organization model.

   Code outside net.humanhelp.site.model.organization should require this
   namespace instead of depending directly on Organization domain, Graph,
   schema, or FX implementation namespaces.

   This facade exposes:

   - the Organization model's Biff module contribution;
   - stable Graph query contracts and named hierarchy reads;
   - authoritative Organization-owned scope contexts;
   - the currently supported effectful hierarchy operations;
   - selected pure Organization values and predicates.

   It deliberately does not expose domain command constructors, lifecycle
   transition implementations, Graph resolver functions, hierarchy loaders,
   authorization-document construction, or FX transaction planners.

   Organization creation and terminal close operations are not public here
   because the current Organization FX slice does not implement their complete
   authorization and cross-model consequences."
  (:require
   [gesso.graph :as graph]
   [net.humanhelp.site.model.organization.domain :as organization]
   [net.humanhelp.site.model.organization.fx :as organization.fx]
   [net.humanhelp.site.model.organization.graph :as organization.graph]
   [net.humanhelp.site.model.organization.schema :as organization.schema]))

;; =============================================================================
;; Model registration
;; =============================================================================

(def schema
  "Malli schemas contributed by the Organization model."
  organization.schema/schema)

(def resolvers
  "Gesso Graph resolvers contributed by the Organization model."
  organization.graph/resolvers)

;; net.humanhelp.site.model.fx/module must be installed separately, exactly once
;; for the application. Organization FX uses that shared transaction handler
;; and does not contribute another transaction implementation.
(def module
  "Biff module contribution for the Organization model."
  {:schema schema
   :biff.graph/resolvers resolvers})

;; =============================================================================
;; Public Graph query contracts
;; =============================================================================

(def organization-document-query
  organization.graph/organization-document-query)

(def organization-group-document-query
  organization.graph/organization-group-document-query)

(def location-document-query
  organization.graph/location-document-query)

(def organization-query
  "Loads one Organization lookup result with :organization/found? and
   optional :organization/doc."
  organization.graph/organization-command-query)

(def organization-group-query
  "Loads one Organization Group lookup result with
   :organization-group/found? and optional :organization-group/doc."
  organization.graph/organization-group-command-query)

(def location-query
  "Loads one Location lookup result with :location/found? and optional
   :location/doc."
  organization.graph/location-command-query)

(def organization-context-query
  "Loads an Organization document and its authoritative root scope context."
  organization.graph/organization-scope-context-query)

(def organization-group-context-query
  "Loads an Organization Group, its ancestry, effective operational state,
   and authoritative scope context."
  organization.graph/organization-group-scope-context-query)

(def location-context-query
  "Loads a Location, its ancestry, effective operational state, and
   authoritative scope context."
  organization.graph/location-context-query)

;; =============================================================================
;; Named Organization reads
;; =============================================================================

(defn organization-facts
  "Loads one Organization by UUID.

   The result follows organization-query and contains
   :organization/found? plus optional :organization/doc."
  [ctx organization-id]
  (graph/query
   ctx
   (organization.graph/organization-query-input
    {:organization-id organization-id})
   organization-query))

(defn organization-group-facts
  "Loads one Organization Group by UUID.

   This lookup does not establish ancestry or operational status. Use
   organization-group-context when hierarchy facts are required."
  [ctx organization-group-id]
  (graph/query
   ctx
   (organization.graph/organization-group-query-input
    {:organization-group-id organization-group-id})
   organization-group-query))

(defn location-facts
  "Loads one Location by UUID.

   This lookup does not establish ancestry or effective operational status.
   Use location-context when hierarchy facts are required."
  [ctx location-id]
  (graph/query
   ctx
   (organization.graph/location-query-input
    {:location-id location-id})
   location-query))

;; =============================================================================
;; Authoritative hierarchy-context reads
;; =============================================================================

(defn organization-context
  "Loads one Organization and its authoritative root scope context.

   Input is the Organization UUID. The result includes:

     :organization/found?
     :organization/doc
     :organization/active?
     :organization/operational?
     :organization/scope
     :organization/scope-context
     :organization/authorization-versions"
  [ctx organization-id]
  (graph/query
   ctx
   (organization.graph/organization-scope-context-query-input
    {:organization-id organization-id})
   organization-context-query))

(defn organization-group-context
  "Loads one Organization Group in the named Organization and establishes its
   authoritative hierarchy context.

   Input:

     {:organization-id organization-id
      :organization-group-id organization-group-id}

   The result includes the target document, target-first ancestor documents,
   applicable scopes, effective operational state, scope context, and the
   Organization document versions that established that context."
  [ctx {:keys [organization-id organization-group-id]}]
  (graph/query
   ctx
   (organization.graph/organization-group-scope-context-query-input
    {:organization-id organization-id
     :organization-group-id organization-group-id})
   organization-group-context-query))

(defn location-context
  "Loads one Location in the named Organization and establishes its
   authoritative hierarchy context.

   Input:

     {:organization-id organization-id
      :location-id location-id}

   The result includes the Location document, target-first ancestor group
   documents, applicable scopes, effective operational state, scope context,
   and the Organization document versions that established that context.

   :location/active? in this result means effectively active across the entire
   hierarchy. The Location document's local lifecycle status remains available
   in :location/doc."
  [ctx {:keys [organization-id location-id]}]
  (graph/query
   ctx
   (organization.graph/location-context-query-input
    {:organization-id organization-id
     :location-id location-id})
   location-context-query))

;; =============================================================================
;; Supported Organization operations
;; =============================================================================

(defn create-organization-group
  "Creates one active Organization Group beneath an operational Organization
   or Organization Group.

   input:

     {:organization-id organization-id
      :parent-scope parent-scope
      :name name}

   The authenticated user must have effective administrator authority at the
   parent scope."
  [ctx input]
  (organization.fx/create-organization-group ctx input))

(defn create-location
  "Creates one active Location beneath an operational Organization or
   Organization Group.

   input:

     {:organization-id organization-id
      :parent-scope parent-scope
      :name name}

   The authenticated user must have effective administrator authority at the
   parent scope."
  [ctx input]
  (organization.fx/create-location ctx input))

(defn rename-organization
  "Renames one Organization after reloading and authorizing current facts."
  [ctx input]
  (organization.fx/rename-organization ctx input))

(defn suspend-organization
  "Suspends one active Organization after reloading and authorizing current
   facts. Descendant documents are not rewritten; their effective operational
   state follows the hierarchy."
  [ctx input]
  (organization.fx/suspend-organization ctx input))

(defn reactivate-organization
  "Reactivates one suspended Organization after reloading and authorizing
   current facts."
  [ctx input]
  (organization.fx/reactivate-organization ctx input))

(defn rename-organization-group
  "Renames one Organization Group after reloading and authorizing current
   facts."
  [ctx input]
  (organization.fx/rename-organization-group ctx input))

(defn move-organization-group
  "Moves one Organization Group beneath another valid parent scope.

   Current and destination hierarchies are both reloaded, authorized, and
   guarded in the transaction. Moving beneath itself or a descendant is
   rejected."
  [ctx input]
  (organization.fx/move-organization-group ctx input))

(defn suspend-organization-group
  "Suspends one active Organization Group after reloading and authorizing
   current facts. Descendant documents are not rewritten."
  [ctx input]
  (organization.fx/suspend-organization-group ctx input))

(defn reactivate-organization-group
  "Reactivates one suspended Organization Group after reloading and
   authorizing current facts."
  [ctx input]
  (organization.fx/reactivate-organization-group ctx input))

(defn rename-location
  "Renames one Location after reloading and authorizing current facts."
  [ctx input]
  (organization.fx/rename-location ctx input))

(defn move-location
  "Moves one Location beneath another valid parent scope.

   Current and destination hierarchies are both reloaded, authorized, and
   guarded in the transaction."
  [ctx input]
  (organization.fx/move-location ctx input))

(defn suspend-location
  "Suspends one active Location after reloading and authorizing current facts."
  [ctx input]
  (organization.fx/suspend-location ctx input))

(defn reactivate-location
  "Reactivates one suspended Location after reloading and authorizing current
   facts."
  [ctx input]
  (organization.fx/reactivate-location ctx input))

(def operations
  "Public Organization operation registry. Entries point at this facade rather
   than the internal FX namespace."
  {:organization/create-group #'create-organization-group
   :organization/create-location #'create-location
   :organization/rename #'rename-organization
   :organization/suspend #'suspend-organization
   :organization/reactivate #'reactivate-organization
   :organization-group/rename #'rename-organization-group
   :organization-group/move #'move-organization-group
   :organization-group/suspend #'suspend-organization-group
   :organization-group/reactivate #'reactivate-organization-group
   :location/rename #'rename-location
   :location/move #'move-location
   :location/suspend #'suspend-location
   :location/reactivate #'reactivate-location})

;; =============================================================================
;; Shared Organization values
;; =============================================================================

(def organization-entity-type
  organization/organization-entity-type)

(def organization-group-entity-type
  organization/organization-group-entity-type)

(def location-entity-type
  organization/location-entity-type)

(def statuses
  organization/statuses)

(def scope-types
  organization/scope-types)

(def parent-scope-types
  organization/parent-scope-types)

(defn normalize-name
  [value]
  (organization/normalize-name value))

(defn name?
  [value]
  (organization/name? value))

(defn status?
  [value]
  (organization/status? value))

(defn active-status?
  [value]
  (organization/active-status? value))

(defn suspended-status?
  [value]
  (organization/suspended-status? value))

(defn closed-status?
  [value]
  (organization/closed-status? value))

(defn can-transition-status?
  [status operation]
  (organization/can-transition-status? status operation))

;; =============================================================================
;; Scope values
;; =============================================================================

(defn scope-type?
  [value]
  (organization/scope-type? value))

(defn parent-scope-type?
  [value]
  (organization/parent-scope-type? value))

(defn scope-reference?
  [value]
  (organization/scope-reference? value))

(defn parent-scope-reference?
  [value]
  (organization/parent-scope-reference? value))

(defn organization-scope
  [organization-id]
  (organization/organization-scope organization-id))

(defn organization-group-scope
  [organization-group-id]
  (organization/organization-group-scope organization-group-id))

(defn location-scope
  [location-id]
  (organization/location-scope location-id))

(defn organization-scope?
  [scope]
  (organization/organization-scope? scope))

(defn organization-group-scope?
  [scope]
  (organization/organization-group-scope? scope))

(defn location-scope?
  [scope]
  (organization/location-scope? scope))

(defn same-scope?
  [a b]
  (organization/same-scope? a b))

(defn scope-context?
  "Returns true for the compact authoritative scope context produced by this
   model's context reads."
  [value]
  (organization/scope-context? value))

;; =============================================================================
;; Organization document facts
;; =============================================================================

(defn organization-document?
  [document]
  (organization/organization-document-consistent? document))

(defn organization-id
  [document]
  (organization/organization-id document))

(defn organization-name
  [document]
  (organization/organization-name document))

(defn organization-status
  [document]
  (organization/organization-status document))

(defn organization-active?
  [document]
  (organization/organization-active? document))

(defn organization-suspended?
  [document]
  (organization/organization-suspended? document))

(defn organization-closed?
  [document]
  (organization/organization-closed? document))

(defn organization-can-transition?
  [document operation]
  (organization/organization-can-transition? document operation))

(defn organization-scope-of
  [document]
  (organization/organization-scope-of document))

;; =============================================================================
;; Organization Group document facts
;; =============================================================================

(defn organization-group-document?
  [document]
  (organization/organization-group-document-consistent? document))

(defn organization-group-id
  [document]
  (organization/organization-group-id document))

(defn organization-group-organization-id
  [document]
  (organization/organization-group-organization-id document))

(defn organization-group-name
  [document]
  (organization/organization-group-name document))

(defn organization-group-status
  [document]
  (organization/organization-group-status document))

(defn organization-group-active?
  "Returns the local lifecycle status of the Organization Group document.
   Effective activity across its ancestry is returned by
   organization-group-context."
  [document]
  (organization/organization-group-active? document))

(defn organization-group-suspended?
  [document]
  (organization/organization-group-suspended? document))

(defn organization-group-closed?
  [document]
  (organization/organization-group-closed? document))

(defn organization-group-can-transition?
  [document operation]
  (organization/organization-group-can-transition? document operation))

(defn organization-group-scope-of
  [document]
  (organization/organization-group-scope-of document))

(defn organization-group-parent-scope
  [document]
  (organization/organization-group-parent-scope document))

(defn organization-group-for-organization?
  [document organization-id]
  (organization/organization-group-for-organization?
   document
   organization-id))

(defn organization-group-direct-child-of?
  [document parent-scope]
  (organization/organization-group-direct-child-of?
   document
   parent-scope))

;; =============================================================================
;; Location document facts
;; =============================================================================

(defn location-document?
  [document]
  (organization/location-document-consistent? document))

(defn location-id
  [document]
  (organization/location-id document))

(defn location-organization-id
  [document]
  (organization/location-organization-id document))

(defn location-name
  [document]
  (organization/location-name document))

(defn location-status
  [document]
  (organization/location-status document))

(defn location-active?
  "Returns the local lifecycle status of the Location document.

   This is distinct from :location/active? returned by location-context, which
   means effectively active across the entire Organization hierarchy."
  [document]
  (organization/location-active? document))

(defn location-suspended?
  [document]
  (organization/location-suspended? document))

(defn location-closed?
  [document]
  (organization/location-closed? document))

(defn location-can-transition?
  [document operation]
  (organization/location-can-transition? document operation))

(defn location-scope-of
  [document]
  (organization/location-scope-of document))

(defn location-parent-scope
  [document]
  (organization/location-parent-scope document))

(defn location-for-organization?
  [document organization-id]
  (organization/location-for-organization?
   document
   organization-id))

(defn location-direct-child-of?
  [document parent-scope]
  (organization/location-direct-child-of?
   document
   parent-scope))

;; =============================================================================
;; Pure hierarchy facts
;; =============================================================================

(defn organization-group-ancestry-consistent?
  "Returns true when group plus target-first ancestors form one valid chain to
   organization. ancestors exclude the target group."
  [organization-document group-document ancestors]
  (organization/organization-group-ancestry-consistent?
   organization-document
   group-document
   ancestors))

(defn location-ancestry-consistent?
  "Returns true when location plus target-first ancestor groups form one valid
   chain to organization."
  [organization-document location-document ancestor-groups]
  (organization/location-ancestry-consistent?
   organization-document
   location-document
   ancestor-groups))

(defn organization-group-operational?
  "Returns true when the Organization, target group, and every ancestor group
   are locally active and form a valid hierarchy."
  [organization-document group-document ancestors]
  (organization/organization-group-operational?
   organization-document
   group-document
   ancestors))

(defn location-operational?
  "Returns true when the Organization, Location, and every ancestor group are
   locally active and form a valid hierarchy."
  [organization-document location-document ancestor-groups]
  (organization/location-operational?
   organization-document
   location-document
   ancestor-groups))
