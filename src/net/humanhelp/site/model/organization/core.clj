(ns net.humanhelp.site.model.organization.core
  "The supported public facade for the HumanHelp Organization model.

   Code outside net.humanhelp.site.model.organization should require this
   namespace instead of depending directly on Organization domain, Graph,
   schema, or FX implementation namespaces.

   gesso.model supplies the conventional persisted-schema and Graph plumbing
   underneath organization.schema and organization.graph. This namespace keeps
   the HumanHelp-facing API stable: named reads, normalized hierarchy contexts,
   authorized operations, and selected pure Organization values/predicates."
  (:require
   [gesso.graph :as graph]
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
   [net.humanhelp.site.model.organization.domain :as organization]
   [net.humanhelp.site.model.organization.fx :as organization.fx]
   [net.humanhelp.site.model.organization.graph :as organization.graph]
   [net.humanhelp.site.model.organization.schema :as organization.schema]))

;; =============================================================================
;; Model registration
;; =============================================================================

;; Schema and Graph each assemble their generated gesso.model pieces exactly
;; once. Core only exposes that completed model surface; it does not recompile
;; descriptors and create a second set of resolver values.
(def schema organization.schema/schema)
(def resolvers organization.graph/resolvers)

;; gesso.model.tx/module must be installed once at the application level.
(def module
  {:schema schema
   :biff.graph/resolvers resolvers})

;; =============================================================================
;; Public Graph query contracts
;; =============================================================================

(def organization-document-query organization.graph/organization-document-query)
(def organization-group-document-query organization.graph/organization-group-document-query)
(def location-document-query organization.graph/location-document-query)

(def organization-query organization.graph/organization-command-query)
(def organization-group-query organization.graph/organization-group-command-query)
(def location-query organization.graph/location-command-query)

(def organization-context-query organization.graph/organization-scope-context-query)
(def organization-group-context-query organization.graph/organization-group-scope-context-query)
(def location-context-query organization.graph/location-context-query)

;; =============================================================================
;; Raw named reads
;; =============================================================================

;; Keep these as ordinary Graph calls rather than hiding them behind
;; gesso.model.core/facts. The query contract is part of the existing facade,
;; and graph/query remains replaceable at the REPL and in tests.
(defn organization-facts [ctx organization-id]
  (graph/query
   ctx
   (organization.graph/organization-query-input
    {:organization-id organization-id})
   organization-query))

(defn organization-group-facts [ctx organization-group-id]
  (graph/query
   ctx
   (organization.graph/organization-group-query-input
    {:organization-group-id organization-group-id})
   organization-group-query))

(defn location-facts [ctx location-id]
  (graph/query
   ctx
   (organization.graph/location-query-input
    {:location-id location-id})
   location-query))

(defn organization-context [ctx organization-id]
  (graph/query
   ctx
   (organization.graph/organization-scope-context-query-input
    {:organization-id organization-id})
   organization-context-query))

(defn organization-group-context [ctx {:keys [organization-id organization-group-id]}]
  (graph/query
   ctx
   (organization.graph/organization-group-scope-context-query-input
    {:organization-id organization-id
     :organization-group-id organization-group-id})
   organization-group-context-query))

(defn location-context [ctx {:keys [organization-id location-id]}]
  (graph/query
   ctx
   (organization.graph/location-context-query-input
    {:organization-id organization-id
     :location-id location-id})
   location-context-query))

;; =============================================================================
;; Normalized required reads
;; =============================================================================

(defn- fail! [error-type message details]
  (throw
   (ex-info
    message
    {:error/type error-type
     :error/details details})))

(defn- require-uuid! [value error-type message details]
  (when-not (uuid? value)
    (fail! error-type message details))
  value)

(defn- require-document!
  [facts found-key document-key document-predicate error-type message details]
  (when-not (true? (get facts found-key))
    (fail! error-type message details))
  (let [document (get facts document-key)]
    (when-not (document-predicate document)
      (fail!
       :organization.core/invalid-read-result
       "Organization Graph returned a found entity without a valid document."
       (assoc details
              :found-key found-key
              :document-key document-key
              :facts facts)))
    document))

(defn- require-scope-context! [scope-context organization-id target-scope details]
  (when-not (authorization-scope/scope-context? scope-context)
    (fail!
     :organization.core/invalid-scope-context
     "Organization Graph returned an invalid authorization-scope context."
     (assoc details :scope-context scope-context)))
  (when-not (= organization-id (:organization/id scope-context))
    (fail!
     :organization.core/scope-organization-mismatch
     "Organization Graph returned an authorization-scope context for another Organization."
     (assoc details :scope-context scope-context)))
  (when-not (authorization-scope/same-scope? target-scope (:scope/target scope-context))
    (fail!
     :organization.core/scope-target-mismatch
     "Organization Graph returned an authorization-scope context for another target."
     (assoc details
            :expected-target target-scope
            :scope-context scope-context)))
  scope-context)

(defn- require-operational-value! [value details]
  (when-not (boolean? value)
    (fail!
     :organization.core/invalid-operational-value
     "Organization Graph returned a non-boolean operational value."
     (assoc details :operational? value)))
  value)

(defn- require-ancestor-groups! [value details]
  (when-not (and (vector? value)
                 (every? organization/organization-group-document-consistent? value))
    (fail!
     :organization.core/invalid-ancestor-groups
     "Organization Graph returned an invalid ancestor-group collection."
     (assoc details :ancestor-groups value)))
  value)

(defn require-organization [ctx organization-id]
  (require-uuid!
   organization-id
   :organization.core/invalid-organization-id
   "Organization ID must be a UUID."
   {:organization/id organization-id})
  (require-document!
   (organization-facts ctx organization-id)
   :organization/found?
   :organization/doc
   organization/organization-document-consistent?
   :organization/not-found
   "The Organization does not exist."
   {:organization/id organization-id}))

(defn require-organization-context [ctx organization-id]
  (require-uuid!
   organization-id
   :organization.core/invalid-organization-id
   "Organization ID must be a UUID."
   {:organization/id organization-id})
  (let [facts (organization-context ctx organization-id)
        details {:organization/id organization-id}
        document
        (require-document!
         facts
         :organization/found?
         :organization/doc
         organization/organization-document-consistent?
         :organization/not-found
         "The Organization does not exist."
         details)
        scope-context
        (require-scope-context!
         (:organization/scope-context facts)
         organization-id
         (authorization-scope/organization-scope organization-id)
         details)]
    {:organization document
     :scope-context scope-context
     :operational?
     (require-operational-value!
      (:organization/operational? facts)
      details)}))

(def ^:private child-read-specs
  {:organization-group
   {:id-key :organization-group-id
    :details-id-key :organization-group/id
    :invalid-id-type :organization.core/invalid-organization-group-id
    :id-message "Organization Group ID must be a UUID."
    :facts-fn #'organization-group-context
    :found-key :organization-group/found?
    :document-key :organization-group/doc
    :document-predicate #'organization/organization-group-document-consistent?
    :not-found-type :organization-group/not-found
    :not-found-message "The Organization Group does not exist in the named Organization."
    :organization-id-fn #'organization/organization-group-organization-id
    :mismatch-type :organization-group/organization-mismatch
    :mismatch-message "The Organization Group belongs to another Organization."
    :ancestors-key :organization-group/ancestor-docs
    :scope-context-key :organization-group/scope-context
    :operational-key :organization-group/operational?
    :scope-fn #'authorization-scope/organization-group-scope
    :result-key :organization-group}

   :location
   {:id-key :location-id
    :details-id-key :location/id
    :invalid-id-type :organization.core/invalid-location-id
    :id-message "Location ID must be a UUID."
    :facts-fn #'location-context
    :found-key :location/found?
    :document-key :location/doc
    :document-predicate #'organization/location-document-consistent?
    :not-found-type :location/not-found
    :not-found-message "The Location does not exist in the named Organization."
    :organization-id-fn #'organization/location-organization-id
    :mismatch-type :location/organization-mismatch
    :mismatch-message "The Location belongs to another Organization."
    :ancestors-key :location/ancestor-group-docs
    :scope-context-key :location/scope-context
    :operational-key :location/operational?
    :scope-fn #'authorization-scope/location-scope
    :result-key :location}})

(defn- require-child-context [entity ctx {:keys [organization-id] :as input}]
  (let [{:keys [id-key details-id-key invalid-id-type id-message facts-fn
                found-key document-key document-predicate not-found-type
                not-found-message organization-id-fn mismatch-type
                mismatch-message ancestors-key scope-context-key operational-key
                scope-fn result-key]}
        (get child-read-specs entity)
        target-id (get input id-key)]
    (require-uuid!
     organization-id
     :organization.core/invalid-organization-id
     "Organization ID must be a UUID."
     {:input input})
    (require-uuid!
     target-id
     invalid-id-type
     id-message
     {:input input})
    (let [facts (facts-fn ctx input)
          details {:organization/id organization-id
                   details-id-key target-id}
          document
          (require-document!
           facts
           found-key
           document-key
           document-predicate
           not-found-type
           not-found-message
           details)
          actual-organization-id (organization-id-fn document)]
      (when-not (= organization-id actual-organization-id)
        (fail!
         mismatch-type
         mismatch-message
         (assoc details :actual-organization-id actual-organization-id)))
      {:organization/id organization-id
       result-key document
       :ancestor-groups
       (require-ancestor-groups!
        (get facts ancestors-key)
        details)
       :scope-context
       (require-scope-context!
        (get facts scope-context-key)
        organization-id
        (scope-fn target-id)
        details)
       :operational?
       (require-operational-value!
        (get facts operational-key)
        details)})))

(defn require-organization-group-context [ctx input]
  (require-child-context :organization-group ctx input))

(defn require-location-context [ctx input]
  (require-child-context :location ctx input))

;; =============================================================================
;; Supported Organization operations
;; =============================================================================

;; These remain forwarding functions, not captured function aliases. That keeps
;; with-redefs and REPL replacement of organization.fx live through the public
;; facade.
(defn create-organization-group [ctx input]
  (organization.fx/create-organization-group ctx input))

(defn create-location [ctx input]
  (organization.fx/create-location ctx input))

(defn rename-organization [ctx input]
  (organization.fx/rename-organization ctx input))

(defn suspend-organization [ctx input]
  (organization.fx/suspend-organization ctx input))

(defn reactivate-organization [ctx input]
  (organization.fx/reactivate-organization ctx input))

(defn rename-organization-group [ctx input]
  (organization.fx/rename-organization-group ctx input))

(defn move-organization-group [ctx input]
  (organization.fx/move-organization-group ctx input))

(defn suspend-organization-group [ctx input]
  (organization.fx/suspend-organization-group ctx input))

(defn reactivate-organization-group [ctx input]
  (organization.fx/reactivate-organization-group ctx input))

(defn rename-location [ctx input]
  (organization.fx/rename-location ctx input))

(defn move-location [ctx input]
  (organization.fx/move-location ctx input))

(defn suspend-location [ctx input]
  (organization.fx/suspend-location ctx input))

(defn reactivate-location [ctx input]
  (organization.fx/reactivate-location ctx input))

(def operations
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

;; Constants are safe aliases. Function-valued facade entries remain wrappers so
;; replacing their implementation Vars remains visible without reloading core.
(def organization-entity-type organization/organization-entity-type)
(def organization-group-entity-type organization/organization-group-entity-type)
(def location-entity-type organization/location-entity-type)
(def statuses organization/statuses)
(def scope-types authorization-scope/scope-types)
(def parent-scope-types authorization-scope/parent-scope-types)

(defn normalize-name [value] (organization/normalize-name value))
(defn name? [value] (organization/name? value))
(defn status? [value] (organization/status? value))
(defn active-status? [value] (organization/active-status? value))
(defn suspended-status? [value] (organization/suspended-status? value))
(defn closed-status? [value] (organization/closed-status? value))
(defn can-transition-status? [status operation]
  (organization/can-transition-status? status operation))

;; =============================================================================
;; Scope values
;; =============================================================================

(defn scope-type? [value] (authorization-scope/scope-type? value))
(defn parent-scope-type? [value] (authorization-scope/parent-scope-type? value))
(defn scope-reference? [value] (authorization-scope/scope-reference? value))
(defn parent-scope-reference? [value] (authorization-scope/parent-scope-reference? value))
(defn organization-scope [organization-id]
  (authorization-scope/organization-scope organization-id))
(defn organization-group-scope [organization-group-id]
  (authorization-scope/organization-group-scope organization-group-id))
(defn location-scope [location-id]
  (authorization-scope/location-scope location-id))
(defn organization-scope? [scope] (authorization-scope/organization-scope? scope))
(defn organization-group-scope? [scope]
  (authorization-scope/organization-group-scope? scope))
(defn location-scope? [scope] (authorization-scope/location-scope? scope))
(defn same-scope? [left right] (authorization-scope/same-scope? left right))
(defn scope-context? [value] (authorization-scope/scope-context? value))

;; =============================================================================
;; Organization document facts
;; =============================================================================

(defn organization-document? [document]
  (organization/organization-document-consistent? document))
(defn organization-id [document] (organization/organization-id document))
(defn organization-name [document] (organization/organization-name document))
(defn organization-status [document] (organization/organization-status document))
(defn organization-active? [document] (organization/organization-active? document))
(defn organization-suspended? [document]
  (organization/organization-suspended? document))
(defn organization-closed? [document] (organization/organization-closed? document))
(defn organization-can-transition? [document operation]
  (organization/organization-can-transition? document operation))
(defn organization-scope-of [document] (organization/organization-scope-of document))

;; =============================================================================
;; Organization Group document facts
;; =============================================================================

(defn organization-group-document? [document]
  (organization/organization-group-document-consistent? document))
(defn organization-group-id [document] (organization/organization-group-id document))
(defn organization-group-organization-id [document]
  (organization/organization-group-organization-id document))
(defn organization-group-name [document] (organization/organization-group-name document))
(defn organization-group-status [document] (organization/organization-group-status document))
(defn organization-group-active? [document]
  (organization/organization-group-active? document))
(defn organization-group-suspended? [document]
  (organization/organization-group-suspended? document))
(defn organization-group-closed? [document]
  (organization/organization-group-closed? document))
(defn organization-group-can-transition? [document operation]
  (organization/organization-group-can-transition? document operation))
(defn organization-group-scope-of [document]
  (organization/organization-group-scope-of document))
(defn organization-group-parent-scope [document]
  (organization/organization-group-parent-scope document))
(defn organization-group-for-organization? [document organization-id]
  (organization/organization-group-for-organization? document organization-id))
(defn organization-group-direct-child-of? [document parent-scope]
  (organization/organization-group-direct-child-of? document parent-scope))

;; =============================================================================
;; Location document facts
;; =============================================================================

(defn location-document? [document]
  (organization/location-document-consistent? document))
(defn location-id [document] (organization/location-id document))
(defn location-organization-id [document]
  (organization/location-organization-id document))
(defn location-name [document] (organization/location-name document))
(defn location-status [document] (organization/location-status document))
(defn location-active? [document] (organization/location-active? document))
(defn location-suspended? [document] (organization/location-suspended? document))
(defn location-closed? [document] (organization/location-closed? document))
(defn location-can-transition? [document operation]
  (organization/location-can-transition? document operation))
(defn location-scope-of [document] (organization/location-scope-of document))
(defn location-parent-scope [document] (organization/location-parent-scope document))
(defn location-for-organization? [document organization-id]
  (organization/location-for-organization? document organization-id))
(defn location-direct-child-of? [document parent-scope]
  (organization/location-direct-child-of? document parent-scope))

;; =============================================================================
;; Pure hierarchy facts
;; =============================================================================

(defn organization-group-ancestry-consistent? [organization-document group-document ancestors]
  (organization/organization-group-ancestry-consistent?
   organization-document
   group-document
   ancestors))

(defn location-ancestry-consistent? [organization-document location-document ancestor-groups]
  (organization/location-ancestry-consistent?
   organization-document
   location-document
   ancestor-groups))

(defn organization-group-operational? [organization-document group-document ancestors]
  (organization/organization-group-operational?
   organization-document
   group-document
   ancestors))

(defn location-operational? [organization-document location-document ancestor-groups]
  (organization/location-operational?
   organization-document
   location-document
   ancestor-groups))
