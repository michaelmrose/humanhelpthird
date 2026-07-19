(ns net.humanhelp.site.model.organization.graph
  "Read-only Gesso Graph resolvers for the HumanHelp Organization model.

   This namespace loads organizations, organization groups, and locations from
   XTDB. It also walks already-persisted parent relationships to construct the
   authoritative scope contexts consumed by User and Request.

   Hierarchy reads are target-first:

     location
       -> immediate organization group
       -> broader organization groups
       -> organization

   A context read fails when the persisted hierarchy is broken, cyclic, or
   crosses organizations. This namespace performs no mutations, User
   authorization, role revocation, or Request behavior."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.organization.domain :as organization]))

;; =============================================================================
;; Stored documents
;; =============================================================================

(def organization-document-columns
  [:xt/id
   :organization/name
   :organization/status
   :organization/revision
   :organization/created-at
   :organization/updated-at
   :organization/suspended-at
   :organization/suspended-by
   :organization/suspension-reason
   :organization/closed-at
   :organization/closed-by
   :organization/closure-reason])

(def organization-group-document-columns
  [:xt/id
   :organization-group/organization
   :organization-group/parent-type
   :organization-group/parent-id
   :organization-group/name
   :organization-group/status
   :organization-group/revision
   :organization-group/created-at
   :organization-group/updated-at
   :organization-group/moved-at
   :organization-group/moved-by
   :organization-group/move-reason
   :organization-group/suspended-at
   :organization-group/suspended-by
   :organization-group/suspension-reason
   :organization-group/closed-at
   :organization-group/closed-by
   :organization-group/closure-reason])

(def location-document-columns
  [:xt/id
   :location/organization
   :location/parent-type
   :location/parent-id
   :location/name
   :location/status
   :location/revision
   :location/created-at
   :location/updated-at
   :location/moved-at
   :location/moved-by
   :location/move-reason
   :location/suspended-at
   :location/suspended-by
   :location/suspension-reason
   :location/closed-at
   :location/closed-by
   :location/closure-reason])

(def organization-document-query
  [:*])

(def organization-group-document-query
  [:*])

(def location-document-query
  [:*])

;; =============================================================================
;; Graph field projections
;; =============================================================================

(def organization-field-pairs
  [[:xt/id :organization/id]
   [:organization/name :organization/name]
   [:organization/status :organization/status]
   [:organization/revision :organization/revision]
   [:organization/created-at :organization/created-at]
   [:organization/updated-at :organization/updated-at]
   [:organization/suspended-at :organization/suspended-at]
   [:organization/suspended-by :organization/suspended-by]
   [:organization/suspension-reason :organization/suspension-reason]
   [:organization/closed-at :organization/closed-at]
   [:organization/closed-by :organization/closed-by]
   [:organization/closure-reason :organization/closure-reason]])

(def organization-group-field-pairs
  [[:xt/id :organization-group/id]
   [:organization-group/organization
    :organization-group/organization-id]
   [:organization-group/parent-type
    :organization-group/parent-type]
   [:organization-group/parent-id
    :organization-group/parent-id]
   [:organization-group/name
    :organization-group/name]
   [:organization-group/status
    :organization-group/status]
   [:organization-group/revision
    :organization-group/revision]
   [:organization-group/created-at
    :organization-group/created-at]
   [:organization-group/updated-at
    :organization-group/updated-at]
   [:organization-group/moved-at
    :organization-group/moved-at]
   [:organization-group/moved-by
    :organization-group/moved-by]
   [:organization-group/move-reason
    :organization-group/move-reason]
   [:organization-group/suspended-at
    :organization-group/suspended-at]
   [:organization-group/suspended-by
    :organization-group/suspended-by]
   [:organization-group/suspension-reason
    :organization-group/suspension-reason]
   [:organization-group/closed-at
    :organization-group/closed-at]
   [:organization-group/closed-by
    :organization-group/closed-by]
   [:organization-group/closure-reason
    :organization-group/closure-reason]])

(def location-field-pairs
  [[:xt/id :location/id]
   [:location/organization :location/organization-id]
   [:location/parent-type :location/parent-type]
   [:location/parent-id :location/parent-id]
   [:location/name :location/name]
   [:location/status :location/status]
   [:location/revision :location/revision]
   [:location/created-at :location/created-at]
   [:location/updated-at :location/updated-at]
   [:location/moved-at :location/moved-at]
   [:location/moved-by :location/moved-by]
   [:location/move-reason :location/move-reason]
   [:location/suspended-at :location/suspended-at]
   [:location/suspended-by :location/suspended-by]
   [:location/suspension-reason :location/suspension-reason]
   [:location/closed-at :location/closed-at]
   [:location/closed-by :location/closed-by]
   [:location/closure-reason :location/closure-reason]])

(def organization-field-query
  (conj
   (mapv second organization-field-pairs)
   {:organization/scope
    [:scope/type :scope/id]}))

(def organization-group-field-query
  (into
   (mapv second organization-group-field-pairs)
   [{:organization-group/parent-scope
     [:scope/type :scope/id]}
    {:organization-group/scope
     [:scope/type :scope/id]}]))

(def location-field-query
  (into
   (mapv second location-field-pairs)
   [{:location/parent-scope
     [:scope/type :scope/id]}
    {:location/scope
     [:scope/type :scope/id]}]))

(defn- project-document
  [document field-pairs]
  (reduce
   (fn [result [document-key graph-key]]
     (if-some [value
               (get document document-key)]
       (assoc result graph-key value)
       result))
   {}
   field-pairs))

;; =============================================================================
;; Query inputs
;; =============================================================================

(defn- without-nils
  [m]
  (into
   {}
   (remove
    (comp nil? val))
   m))

(defn organization-query-input
  [{:keys [organization-id]}]
  (without-nils
   {:organization/id organization-id}))

(defn organization-group-query-input
  [{:keys [organization-group-id]}]
  (without-nils
   {:organization-group/id organization-group-id}))

(defn location-query-input
  [{:keys [location-id]}]
  (without-nils
   {:location/id location-id}))

(defn organization-scope-context-query-input
  [{:keys [organization-id]}]
  (without-nils
   {:organization/id organization-id}))

(defn organization-group-scope-context-query-input
  [{:keys [organization-id organization-group-id]}]
  (without-nils
   {:organization/id organization-id
    :organization-group/id organization-group-id}))

(defn location-context-query-input
  [{:keys [organization-id location-id]}]
  (without-nils
   {:organization/id organization-id
    :location/id location-id}))

;; =============================================================================
;; XTDB reads
;; =============================================================================

(defn- q
  [ctx query]
  (biffx/q
   (:biff/conn ctx)
   query))

(defn- load-by-id
  [ctx table columns id]
  (when
   (uuid? id)
    (first
     (q
      ctx
      {:select columns
       :from table
       :where [:= :xt/id id]}))))

(defn- load-organization
  [ctx organization-id]
  (load-by-id
   ctx
   organization/organization-entity-type
   organization-document-columns
   organization-id))

(defn- load-organization-group
  [ctx organization-group-id]
  (load-by-id
   ctx
   organization/organization-group-entity-type
   organization-group-document-columns
   organization-group-id))

(defn- load-location
  [ctx location-id]
  (load-by-id
   ctx
   organization/location-entity-type
   location-document-columns
   location-id))

(defn- lookup-result
  [found-key document-key document]
  (if document
    {found-key true
     document-key document}
    {found-key false}))

;; =============================================================================
;; Hierarchy loading
;; =============================================================================

(def hierarchy-depth-limit
  "Defensive limit for corrupted parent chains.

   Cycle detection normally terminates traversal first. The limit prevents an
   unexpectedly enormous corrupt hierarchy from monopolizing one Graph read."
  256)

(defn- hierarchy-error!
  [error-type message details]
  (throw
   (ex-info
    message
    {:error/type error-type
     :error/details details})))

(defn- require-valid-organization!
  [organization-document organization-id]
  (when-not organization-document
    (hierarchy-error!
     :organization/not-found
     "The hierarchy refers to an organization that no longer exists."
     {:organization/id organization-id}))

  (when-not
   (organization/organization-document-consistent?
    organization-document)
    (hierarchy-error!
     :organization.graph/invalid-organization
     "The persisted organization document is internally inconsistent."
     {:organization/id organization-id}))

  organization-document)

(defn- require-valid-group!
  [group group-id organization-id]
  (when-not group
    (hierarchy-error!
     :organization-group/not-found
     "The hierarchy refers to an organization group that no longer exists."
     {:organization-group/id group-id
      :organization/id organization-id}))

  (when-not
   (organization/organization-group-document-consistent?
    group)
    (hierarchy-error!
     :organization.graph/invalid-group
     "A persisted organization-group document is internally inconsistent."
     {:organization-group/id group-id
      :organization/id organization-id}))

  (when-not
   (organization/organization-group-for-organization?
    group
    organization-id)
    (hierarchy-error!
     :organization.graph/cross-organization-parent
     "An organization-group parent belongs to another organization."
     {:organization-group/id group-id
      :organization/id organization-id
      :actual-organization-id
      (organization/organization-group-organization-id
       group)}))

  group)

(defn- load-group-ancestors
  "Loads groups beginning at initial-parent-scope.

   Returns immediate parent first and the broadest group last. The organization
   scope itself is not returned."
  [ctx organization-id initial-parent-scope]
  (loop [parent-scope initial-parent-scope
         ancestors []
         visited #{}
         depth 0]
    (when
     (>= depth hierarchy-depth-limit)
      (hierarchy-error!
       :organization.graph/hierarchy-too-deep
       "The organization hierarchy exceeds the supported defensive depth."
       {:organization/id organization-id
        :parent-scope parent-scope
        :depth depth}))

    (case
     (:scope/type parent-scope)

      :organization
      (if
       (= organization-id
          (:scope/id parent-scope))
        ancestors
        (hierarchy-error!
         :organization.graph/cross-organization-root
         "The hierarchy terminates at a different organization."
         {:organization/id organization-id
          :actual-root-id (:scope/id parent-scope)}))

      :organization-group
      (let [group-id
            (:scope/id parent-scope)]
        (when
         (contains? visited group-id)
          (hierarchy-error!
           :organization.graph/hierarchy-cycle
           "The organization hierarchy contains a cycle."
           {:organization/id organization-id
            :organization-group/id group-id
            :visited-group-ids visited}))

        (let [group
              (-> (load-organization-group
                   ctx
                   group-id)
                  (require-valid-group!
                   group-id
                   organization-id))]
          (recur
           (organization/organization-group-parent-scope
            group)
           (conj ancestors group)
           (conj visited group-id)
           (inc depth))))

      (hierarchy-error!
       :organization.graph/invalid-parent-scope
       "The hierarchy contains an invalid parent scope."
       {:organization/id organization-id
        :parent-scope parent-scope}))))

(defn- authorization-version
  [document]
  (cond
    (contains?
     document
     :location/organization)
    (model.common/authorization-version
     organization/location-entity-type
     document
     organization/location-version)

    (contains?
     document
     :organization-group/organization)
    (model.common/authorization-version
     organization/organization-group-entity-type
     document
     organization/organization-group-version)

    (contains?
     document
     :organization/name)
    (model.common/authorization-version
     organization/organization-entity-type
     document
     organization/organization-version)

    :else
    (hierarchy-error!
     :organization.graph/unknown-authorization-document
     "A hierarchy authorization document has no known entity type."
     {:document-id
      (:xt/id document)})))

(defn- authorization-versions
  [documents]
  (mapv
   authorization-version
   documents))

(defn- organization-context-facts
  [organization-document]
  (let [scope-context
        (organization/organization-scope-context
         organization-document)]
    {:organization/found? true
     :organization/doc organization-document
     :organization/active?
     (:scope/operational? scope-context)
     :organization/operational?
     (:scope/operational? scope-context)
     :organization/scope
     (:scope/target scope-context)
     :organization/scope-context
     scope-context
     :organization/authorization-versions
     [(authorization-version
       organization-document)]}))

(defn- organization-group-context-facts
  [ctx group]
  (let [organization-id
        (organization/organization-group-organization-id
         group)

        organization-document
        (-> (load-organization
             ctx
             organization-id)
            (require-valid-organization!
             organization-id))

        ancestors
        (load-group-ancestors
         ctx
         organization-id
         (organization/organization-group-parent-scope
          group))

        scope-context
        (organization/organization-group-scope-context
         organization-document
         group
         ancestors)

        authorization-documents
        (organization/organization-group-authorization-documents
         organization-document
         group
         ancestors)]
    {:organization-group/found? true
     :organization-group/doc group
     :organization-group/organization-id organization-id
     :organization-group/parent-scope
     (organization/organization-group-parent-scope group)
     :organization-group/active?
     (:scope/operational? scope-context)
     :organization-group/operational?
     (:scope/operational? scope-context)
     :organization-group/ancestor-docs
     ancestors
     :organization-group/applicable-scopes
     (:scope/applicable scope-context)
     :organization-group/scope-context
     scope-context
     :organization-group/authorization-versions
     (authorization-versions
      authorization-documents)}))

(defn- location-context-facts
  [ctx location]
  (let [organization-id
        (organization/location-organization-id
         location)

        organization-document
        (-> (load-organization
             ctx
             organization-id)
            (require-valid-organization!
             organization-id))

        groups
        (load-group-ancestors
         ctx
         organization-id
         (organization/location-parent-scope
          location))

        scope-context
        (organization/location-scope-context
         organization-document
         location
         groups)

        authorization-documents
        (organization/location-authorization-documents
         organization-document
         location
         groups)]
    {:location/found? true
     :location/doc location
     :location/organization-id organization-id
     :location/parent-scope
     (organization/location-parent-scope location)
     :location/active?
     (:scope/operational? scope-context)
     :location/operational?
     (:scope/operational? scope-context)
     :location/ancestor-group-docs
     groups
     :location/applicable-scopes
     (:scope/applicable scope-context)
     :location/scope-context
     scope-context
     :location/authorization-versions
     (authorization-versions
      authorization-documents)}))

;; =============================================================================
;; Organizations
;; =============================================================================

(graph/defresolver organization-by-id
  {:input
   [:organization/id]

   :output
   [:organization/found?
    {[:? :organization/doc]
     organization-document-query}]}
  [ctx {:organization/keys [id]}]
  (lookup-result
   :organization/found?
   :organization/doc
   (load-organization ctx id)))

(graph/defresolver organization-fields
  {:input
   [{:organization/doc
     organization-document-query}]

   :output
   organization-field-query}
  [_ctx {:organization/keys [doc]}]
  (assoc
   (project-document
    doc
    organization-field-pairs)
   :organization/scope
   (organization/organization-scope-of
    doc)))

(graph/defresolver organization-scope-context-resolver
  {:input
   [{:organization/doc
     organization-document-query}]

   :output
   [[:? :organization/active?]
    [:? :organization/operational?]
    [:? :organization/scope-context]
    [:? :organization/authorization-versions]]}
  [_ctx {:organization/keys [doc]}]
  (dissoc
   (organization-context-facts doc)
   :organization/found?
   :organization/doc
   :organization/scope))

;; =============================================================================
;; Organization groups
;; =============================================================================

(graph/defresolver organization-group-by-id
  {:input
   [:organization-group/id]

   :output
   [:organization-group/found?
    {[:? :organization-group/doc]
     organization-group-document-query}]}
  [ctx {:organization-group/keys [id]}]
  (lookup-result
   :organization-group/found?
   :organization-group/doc
   (load-organization-group ctx id)))

(graph/defresolver organization-group-fields
  {:input
   [{:organization-group/doc
     organization-group-document-query}]

   :output
   organization-group-field-query}
  [_ctx {:organization-group/keys [doc]}]
  (merge
   (project-document
    doc
    organization-group-field-pairs)
   {:organization-group/parent-scope
    (organization/organization-group-parent-scope
     doc)

    :organization-group/scope
    (organization/organization-group-scope-of
     doc)}))

(graph/defresolver organization-group-scope-context-resolver
  {:input
   [{:organization-group/doc
     organization-group-document-query}]

   :output
   [[:? :organization-group/active?]
    [:? :organization-group/operational?]
    {[:? :organization-group/ancestor-docs]
     organization-group-document-query}
    {[:? :organization-group/applicable-scopes]
     [:scope/type :scope/id]}
    [:? :organization-group/scope-context]
    [:? :organization-group/authorization-versions]]}
  [ctx {:organization-group/keys [doc]}]
  (dissoc
   (organization-group-context-facts ctx doc)
   :organization-group/found?
   :organization-group/doc
   :organization-group/organization-id
   :organization-group/parent-scope))

;; =============================================================================
;; Locations
;; =============================================================================

(graph/defresolver location-by-id
  {:input
   [:location/id]

   :output
   [:location/found?
    {[:? :location/doc]
     location-document-query}]}
  [ctx {:location/keys [id]}]
  (lookup-result
   :location/found?
   :location/doc
   (load-location ctx id)))

(graph/defresolver location-fields
  {:input
   [{:location/doc
     location-document-query}]

   :output
   location-field-query}
  [_ctx {:location/keys [doc]}]
  (merge
   (project-document
    doc
    location-field-pairs)
   {:location/parent-scope
    (organization/location-parent-scope
     doc)

    :location/scope
    (organization/location-scope-of
     doc)}))

(graph/defresolver location-context-resolver
  {:input
   [{:location/doc
     location-document-query}]

   :output
   [[:? :location/active?]
    [:? :location/operational?]
    {[:? :location/ancestor-group-docs]
     organization-group-document-query}
    {[:? :location/applicable-scopes]
     [:scope/type :scope/id]}
    [:? :location/scope-context]
    [:? :location/authorization-versions]]}
  [ctx {:location/keys [doc]}]
  (dissoc
   (location-context-facts ctx doc)
   :location/found?
   :location/doc
   :location/organization-id
   :location/parent-scope))

;; =============================================================================
;; Public query contracts
;; =============================================================================

(def organization-command-query
  [:organization/found?
   {[:? :organization/doc]
    organization-document-query}])

(def organization-group-command-query
  [:organization-group/found?
   {[:? :organization-group/doc]
    organization-group-document-query}])

(def location-command-query
  [:location/found?
   {[:? :location/doc]
    location-document-query}])

(def organization-scope-context-query
  [:organization/found?
   {[:? :organization/doc]
    organization-document-query}
   [:? :organization/active?]
   [:? :organization/operational?]
   {[:? :organization/scope]
    [:scope/type :scope/id]}
   [:? :organization/scope-context]
   [:? :organization/authorization-versions]])

(def organization-group-scope-context-query
  [:organization-group/found?
   {[:? :organization-group/doc]
    organization-group-document-query}
   [:? :organization-group/organization-id]
   {[:? :organization-group/parent-scope]
    [:scope/type :scope/id]}
   [:? :organization-group/active?]
   [:? :organization-group/operational?]
   {[:? :organization-group/ancestor-docs]
    organization-group-document-query}
   {[:? :organization-group/applicable-scopes]
    [:scope/type :scope/id]}
   [:? :organization-group/scope-context]
   [:? :organization-group/authorization-versions]])

(def location-context-query
  "Stable Organization Graph contract consumed by User and Request.

   :location/active? is effective operational activity across the entire
   Organization hierarchy, not merely the local Location status. The local
   lifecycle remains available in :location/doc and :location/status."
  [:location/found?
   {[:? :location/doc]
    location-document-query}
   [:? :location/organization-id]
   {[:? :location/parent-scope]
    [:scope/type :scope/id]}
   [:? :location/active?]
   [:? :location/operational?]
   {[:? :location/ancestor-group-docs]
    organization-group-document-query}
   {[:? :location/applicable-scopes]
    [:scope/type :scope/id]}
   [:? :location/scope-context]
   [:? :location/authorization-versions]])

;; =============================================================================
;; Resolver collection
;; =============================================================================

(def resolvers
  [organization-by-id
   organization-fields
   organization-scope-context-resolver

   organization-group-by-id
   organization-group-fields
   organization-group-scope-context-resolver

   location-by-id
   location-fields
   location-context-resolver])
