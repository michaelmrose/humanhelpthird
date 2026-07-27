(ns net.humanhelp.site.model.organization.graph
  "Read-only Graph extensions for the HumanHelp Organization model.

   gesso.model owns ordinary persisted-document loading, normalization, and
   projection. This namespace owns derived scopes, hierarchy traversal,
   authoritative hierarchy contexts, and the temporary legacy authorization
   proof values still consumed by User and Request."
  (:require
   [gesso.graph :as graph]
   [gesso.model.core :as model]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.organization.domain :as organization]
   [net.humanhelp.site.model.organization.schema :as organization.schema]))

;; =============================================================================
;; Conventional model surfaces
;; =============================================================================

(def organization-descriptor organization.schema/organization-descriptor)
(def organization-group-descriptor organization.schema/organization-group-descriptor)
(def location-descriptor organization.schema/location-descriptor)

(def organization-document-columns
  (model/document-columns organization-descriptor))
(def organization-group-document-columns
  (model/document-columns organization-group-descriptor))
(def location-document-columns
  (model/document-columns location-descriptor))

(def organization-document-query (model/document-query organization-descriptor))
(def organization-group-document-query (model/document-query organization-group-descriptor))
(def location-document-query (model/document-query location-descriptor))

(def scope-query [:scope/type :scope/id])
(def scope-context-value-query [:*])
(def authorization-versions-value-query [:*])

(def organization-field-query
  (conj (model/field-query organization-descriptor)
        {:organization/scope scope-query}))
(def organization-group-field-query
  (into (model/field-query organization-group-descriptor)
        [{:organization-group/parent-scope scope-query}
         {:organization-group/scope scope-query}]))
(def location-field-query
  (into (model/field-query location-descriptor)
        [{:location/parent-scope scope-query}
         {:location/scope scope-query}]))

;; =============================================================================
;; Query inputs
;; =============================================================================

(defn- without-nils [value]
  (into {} (remove (comp nil? val)) value))

(defn organization-query-input [{:keys [organization-id]}]
  (without-nils {:organization/id organization-id}))
(defn organization-group-query-input [{:keys [organization-group-id]}]
  (without-nils {:organization-group/id organization-group-id}))
(defn location-query-input [{:keys [location-id]}]
  (without-nils {:location/id location-id}))
(defn organization-scope-context-query-input [{:keys [organization-id]}]
  (without-nils {:organization/id organization-id}))
(defn organization-group-scope-context-query-input
  [{:keys [organization-id organization-group-id]}]
  (without-nils {:organization/id organization-id
                 :organization-group/id organization-group-id}))
(defn location-context-query-input [{:keys [organization-id location-id]}]
  (without-nils {:organization/id organization-id
                 :location/id location-id}))

;; =============================================================================
;; Hierarchy loading
;; =============================================================================

(defn- load-organization [ctx id]
  (model/load-by-id organization-descriptor ctx id))
(defn- load-organization-group [ctx id]
  (model/load-by-id organization-group-descriptor ctx id))

(def hierarchy-depth-limit 256)

(defn- hierarchy-error! [type message details]
  (throw (ex-info message {:error/type type :error/details details})))

(defn- require-valid-organization! [document organization-id]
  (when-not document
    (hierarchy-error! :organization/not-found
                      "The hierarchy refers to an organization that no longer exists."
                      {:organization/id organization-id}))
  ;; gesso.model already validates persisted reads. Keep this assertion so the
  ;; helper also behaves correctly with values supplied directly in tests/REPL.
  (when-not (organization/organization-document-consistent? document)
    (hierarchy-error! :organization.graph/invalid-organization
                      "The persisted organization document is internally inconsistent."
                      {:organization/id organization-id}))
  document)

(defn- require-valid-group! [group group-id organization-id]
  (when-not group
    (hierarchy-error! :organization-group/not-found
                      "The hierarchy refers to an organization group that no longer exists."
                      {:organization-group/id group-id
                       :organization/id organization-id}))
  (when-not (organization/organization-group-document-consistent? group)
    (hierarchy-error! :organization.graph/invalid-group
                      "A persisted organization-group document is internally inconsistent."
                      {:organization-group/id group-id
                       :organization/id organization-id}))
  (when-not (organization/organization-group-for-organization? group organization-id)
    (hierarchy-error! :organization.graph/cross-organization-parent
                      "An organization-group parent belongs to another organization."
                      {:organization-group/id group-id
                       :organization/id organization-id
                       :actual-organization-id
                       (organization/organization-group-organization-id group)}))
  group)

(defn- load-group-ancestors
  "Loads ancestor groups immediate-parent first, excluding the Organization."
  [ctx organization-id initial-parent-scope]
  (loop [parent-scope initial-parent-scope
         ancestors []
         visited #{}
         depth 0]
    (when (>= depth hierarchy-depth-limit)
      (hierarchy-error! :organization.graph/hierarchy-too-deep
                        "The organization hierarchy exceeds the supported defensive depth."
                        {:organization/id organization-id
                         :parent-scope parent-scope
                         :depth depth}))
    (case (:scope/type parent-scope)
      :organization
      (if (= organization-id (:scope/id parent-scope))
        ancestors
        (hierarchy-error! :organization.graph/cross-organization-root
                          "The hierarchy terminates at a different organization."
                          {:organization/id organization-id
                           :actual-root-id (:scope/id parent-scope)}))

      :organization-group
      (let [group-id (:scope/id parent-scope)]
        (when (contains? visited group-id)
          (hierarchy-error! :organization.graph/hierarchy-cycle
                            "The organization hierarchy contains a cycle."
                            {:organization/id organization-id
                             :organization-group/id group-id
                             :visited-group-ids visited}))
        (let [group (-> (load-organization-group ctx group-id)
                        (require-valid-group! group-id organization-id))]
          (recur (organization/organization-group-parent-scope group)
                 (conj ancestors group)
                 (conj visited group-id)
                 (inc depth))))

      (hierarchy-error! :organization.graph/invalid-parent-scope
                        "The hierarchy contains an invalid parent scope."
                        {:organization/id organization-id
                         :parent-scope parent-scope}))))

;; =============================================================================
;; Temporary legacy authorization proofs
;; =============================================================================

;; Remove this adapter when User and Request migrate to gesso.model guards.
(defn- authorization-version [document]
  (cond
    (contains? document :location/organization)
    (model.common/authorization-version organization/location-entity-type
                                        document organization/location-version)
    (contains? document :organization-group/organization)
    (model.common/authorization-version organization/organization-group-entity-type
                                        document organization/organization-group-version)
    (contains? document :organization/name)
    (model.common/authorization-version organization/organization-entity-type
                                        document organization/organization-version)
    :else
    (hierarchy-error! :organization.graph/unknown-authorization-document
                      "A hierarchy authorization document has no known entity type."
                      {:document-id (:xt/id document)})))

(defn- authorization-versions [documents]
  (mapv authorization-version documents))

;; =============================================================================
;; Hierarchy context composition
;; =============================================================================

(defn- organization-context-facts [document]
  (let [context (organization/organization-scope-context document)
        operational? (:scope/operational? context)]
    {:organization/found? true
     :organization/doc document
     :organization/active? operational?
     :organization/operational? operational?
     :organization/scope (:scope/target context)
     :organization/scope-context context
     :organization/authorization-versions [(authorization-version document)]}))

(def ^:private child-context-specs
  {:organization-group
   {:organization-id-fn organization/organization-group-organization-id
    :parent-fn organization/organization-group-parent-scope
    :context-fn organization/organization-group-scope-context
    :documents-fn organization/organization-group-authorization-documents
    :keys [:organization-group/found? :organization-group/doc
           :organization-group/organization-id :organization-group/parent-scope
           :organization-group/active? :organization-group/operational?
           :organization-group/ancestor-docs :organization-group/applicable-scopes
           :organization-group/scope-context :organization-group/authorization-versions]}
   :location
   {:organization-id-fn organization/location-organization-id
    :parent-fn organization/location-parent-scope
    :context-fn organization/location-scope-context
    :documents-fn organization/location-authorization-documents
    :keys [:location/found? :location/doc :location/organization-id
           :location/parent-scope :location/active? :location/operational?
           :location/ancestor-group-docs :location/applicable-scopes
           :location/scope-context :location/authorization-versions]}})

(defn- child-context-facts [entity ctx document]
  (let [{:keys [organization-id-fn parent-fn context-fn documents-fn keys]}
        (get child-context-specs entity)
        [found-key doc-key organization-id-key parent-key active-key operational-key
         ancestors-key applicable-key context-key versions-key] keys
        organization-id (organization-id-fn document)
        organization-document (-> (load-organization ctx organization-id)
                                  (require-valid-organization! organization-id))
        parent-scope (parent-fn document)
        ancestors (load-group-ancestors ctx organization-id parent-scope)
        context (context-fn organization-document document ancestors)
        documents (documents-fn organization-document document ancestors)
        operational? (:scope/operational? context)]
    {found-key true
     doc-key document
     organization-id-key organization-id
     parent-key parent-scope
     active-key operational?
     operational-key operational?
     ancestors-key ancestors
     applicable-key (:scope/applicable context)
     context-key context
     versions-key (authorization-versions documents)}))

(defn- organization-group-context-facts [ctx group]
  (child-context-facts :organization-group ctx group))
(defn- location-context-facts [ctx location]
  (child-context-facts :location ctx location))

;; =============================================================================
;; Resolvers
;; =============================================================================

;; Conventional resolver values are generated exactly once and reused in the
;; complete resolver collection below.
(def organization-by-id (model/build-by-id-resolver organization-descriptor))
(def organization-fields (model/build-field-resolver organization-descriptor))
(def organization-group-by-id (model/build-by-id-resolver organization-group-descriptor))
(def organization-group-fields (model/build-field-resolver organization-group-descriptor))
(def location-by-id (model/build-by-id-resolver location-descriptor))
(def location-fields (model/build-field-resolver location-descriptor))

(defn- derived-resolver [id doc-key document-query output derive-fn]
  (graph/resolver
   {:id id
    :input [{doc-key document-query}]
    :output output
    :resolve-fn (fn [_ctx input] (derive-fn (get input doc-key)))}))

(def organization-derived-fields
  (derived-resolver
   ::organization-derived-fields :organization/doc organization-document-query
   [{:organization/scope scope-query}]
   #(hash-map :organization/scope (organization/organization-scope-of %))))

(def organization-group-derived-fields
  (derived-resolver
   ::organization-group-derived-fields :organization-group/doc organization-group-document-query
   [{:organization-group/parent-scope scope-query}
    {:organization-group/scope scope-query}]
   #(hash-map :organization-group/parent-scope
              (organization/organization-group-parent-scope %)
              :organization-group/scope
              (organization/organization-group-scope-of %))))

(def location-derived-fields
  (derived-resolver
   ::location-derived-fields :location/doc location-document-query
   [{:location/parent-scope scope-query}
    {:location/scope scope-query}]
   #(hash-map :location/parent-scope (organization/location-parent-scope %)
              :location/scope (organization/location-scope-of %))))

(defn- context-resolver [id doc-key document-query output context-fn envelope-keys]
  (graph/resolver
   {:id id
    :input [{doc-key document-query}]
    :output output
    :resolve-fn
    (fn [ctx input]
      (apply dissoc (context-fn ctx (get input doc-key)) envelope-keys))}))

(def organization-scope-context-resolver
  (graph/resolver
   {:id ::organization-scope-context
    :input [{:organization/doc organization-document-query}]
    :output [[:? :organization/active?]
             [:? :organization/operational?]
             {[:? :organization/scope-context] scope-context-value-query}
             {[:? :organization/authorization-versions]
              authorization-versions-value-query}]
    :resolve-fn
    (fn [_ctx {:organization/keys [doc]}]
      (dissoc (organization-context-facts doc)
              :organization/found? :organization/doc :organization/scope))}))

(def organization-group-scope-context-resolver
  (context-resolver
   ::organization-group-scope-context :organization-group/doc
   organization-group-document-query
   [[:? :organization-group/active?]
    [:? :organization-group/operational?]
    {[:? :organization-group/ancestor-docs] organization-group-document-query}
    {[:? :organization-group/applicable-scopes] scope-query}
    {[:? :organization-group/scope-context] scope-context-value-query}
    {[:? :organization-group/authorization-versions]
     authorization-versions-value-query}]
   organization-group-context-facts
   [:organization-group/found? :organization-group/doc
    :organization-group/organization-id :organization-group/parent-scope]))

(def location-context-resolver
  (context-resolver
   ::location-context :location/doc location-document-query
   [[:? :location/active?]
    [:? :location/operational?]
    {[:? :location/ancestor-group-docs] organization-group-document-query}
    {[:? :location/applicable-scopes] scope-query}
    {[:? :location/scope-context] scope-context-value-query}
    {[:? :location/authorization-versions] authorization-versions-value-query}]
   location-context-facts
   [:location/found? :location/doc :location/organization-id :location/parent-scope]))

;; =============================================================================
;; Public query contracts
;; =============================================================================

(def organization-command-query (model/lookup-query organization-descriptor))
(def organization-group-command-query (model/lookup-query organization-group-descriptor))
(def location-command-query (model/lookup-query location-descriptor))

(def organization-scope-context-query
  [:organization/found?
   {[:? :organization/doc] organization-document-query}
   [:? :organization/active?]
   [:? :organization/operational?]
   {[:? :organization/scope] scope-query}
   {[:? :organization/scope-context] scope-context-value-query}
   {[:? :organization/authorization-versions] authorization-versions-value-query}])

(def organization-group-scope-context-query
  [:organization-group/found?
   {[:? :organization-group/doc] organization-group-document-query}
   [:? :organization-group/organization-id]
   {[:? :organization-group/parent-scope] scope-query}
   [:? :organization-group/active?]
   [:? :organization-group/operational?]
   {[:? :organization-group/ancestor-docs] organization-group-document-query}
   {[:? :organization-group/applicable-scopes] scope-query}
   {[:? :organization-group/scope-context] scope-context-value-query}
   {[:? :organization-group/authorization-versions] authorization-versions-value-query}])

(def location-context-query
  "Stable Organization Graph contract consumed by User and Request.
   :location/active? means effective activity across the complete hierarchy."
  [:location/found?
   {[:? :location/doc] location-document-query}
   [:? :location/organization-id]
   {[:? :location/parent-scope] scope-query}
   [:? :location/active?]
   [:? :location/operational?]
   {[:? :location/ancestor-group-docs] organization-group-document-query}
   {[:? :location/applicable-scopes] scope-query}
   {[:? :location/scope-context] scope-context-value-query}
   {[:? :location/authorization-versions] authorization-versions-value-query}])

;; =============================================================================
;; Resolver collection
;; =============================================================================

(def custom-resolvers
  [organization-derived-fields organization-scope-context-resolver
   organization-group-derived-fields organization-group-scope-context-resolver
   location-derived-fields location-context-resolver])

(def resolvers
  "Complete Organization resolver collection. Generated resolvers are compiled
   once; only derived scope and hierarchy resolvers are hand-written."
  [organization-by-id organization-fields
   organization-derived-fields organization-scope-context-resolver
   organization-group-by-id organization-group-fields
   organization-group-derived-fields organization-group-scope-context-resolver
   location-by-id location-fields location-derived-fields location-context-resolver])
