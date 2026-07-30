(ns net.humanhelp.site.model.organization.graph
  "Organization-specific read composition.

   gesso.model owns ordinary persisted-document loading and generated Graph
   resolvers. This namespace adds only hierarchy-derived values: scopes,
   ancestry, effective operational state, and authoritative scope contexts."
  (:require
   [com.biffweb.graph :as graph]
   [gesso.model.core :as model]
   [net.humanhelp.site.model.organization.domain :as organization]
   [net.humanhelp.site.model.organization.schema :as organization.schema]))

;; =============================================================================
;; Graph shapes used by custom resolvers
;; =============================================================================

(def ^:private organization-document-query
  (model/document-query
   organization.schema/organization-descriptor))

(def ^:private organization-group-document-query
  (model/document-query
   organization.schema/organization-group-descriptor))

(def ^:private location-document-query
  (model/document-query
   organization.schema/location-descriptor))

(def ^:private scope-query
  [:scope/type
   :scope/id])

(def ^:private scope-context-query
  [:*])

;; =============================================================================
;; Persisted hierarchy loading
;; =============================================================================

(def ^:private hierarchy-depth-limit
  256)

(defn- hierarchy-error!
  [type message details]
  (throw
   (ex-info
    message
    {:error/type type
     :error/details details})))

(defn- load-organization
  [ctx organization-id]
  (model/load-by-id
   organization.schema/organization-descriptor
   ctx
   organization-id))

(defn- load-organization-group
  [ctx organization-group-id]
  (model/load-by-id
   organization.schema/organization-group-descriptor
   ctx
   organization-group-id))

(defn- load-location
  [ctx location-id]
  (model/load-by-id
   organization.schema/location-descriptor
   ctx
   location-id))

(defn- require-valid-organization!
  [document organization-id]
  (when-not
   (organization/organization-document-consistent? document)
    (hierarchy-error!
     :organization.graph/invalid-organization
     "The persisted organization document is internally inconsistent."
     {:organization/id organization-id}))
  document)

(defn- require-valid-group!
  [group organization-group-id]
  (when-not
   (organization/organization-group-document-consistent? group)
    (hierarchy-error!
     :organization.graph/invalid-group
     "The persisted organization-group document is internally inconsistent."
     {:organization-group/id organization-group-id}))
  group)

(defn- require-valid-location!
  [location location-id]
  (when-not
   (organization/location-document-consistent? location)
    (hierarchy-error!
     :organization.graph/invalid-location
     "The persisted location document is internally inconsistent."
     {:location/id location-id}))
  location)

(defn- require-group-for-organization!
  [group organization-id]
  (when-not
   (organization/organization-group-for-organization?
    group
    organization-id)
    (hierarchy-error!
     :organization.graph/cross-organization-parent
     "An organization-group parent belongs to another organization."
     {:organization-group/id
      (organization/organization-group-id group)
      :organization/id
      organization-id
      :actual-organization-id
      (organization/organization-group-organization-id group)}))
  group)

(defn- require-root!
  [ctx organization-id]
  (if-let [organization-document
           (load-organization
            ctx
            organization-id)]
    (require-valid-organization!
     organization-document
     organization-id)

    (hierarchy-error!
     :organization/not-found
     "The hierarchy refers to an organization that no longer exists."
     {:organization/id organization-id})))

(defn- require-ancestor-group!
  [ctx organization-id organization-group-id]
  (if-let [group
           (load-organization-group
            ctx
            organization-group-id)]
    (-> group
        (require-valid-group!
         organization-group-id)
        (require-group-for-organization!
         organization-id))

    (hierarchy-error!
     :organization-group/not-found
     "The hierarchy refers to an organization group that no longer exists."
     {:organization-group/id organization-group-id
      :organization/id organization-id})))

(defn- load-group-ancestors
  "Loads ancestor groups immediate-parent first, excluding the Organization."
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
     (organization/scope-type parent-scope)

      :organization
      (if
       (= organization-id
          (organization/scope-id parent-scope))
        ancestors

        (hierarchy-error!
         :organization.graph/cross-organization-root
         "The hierarchy terminates at a different organization."
         {:organization/id organization-id
          :actual-root-id
          (organization/scope-id parent-scope)}))

      :organization-group
      (let [group-id
            (organization/scope-id parent-scope)]
        (when
         (contains? visited group-id)
          (hierarchy-error!
           :organization.graph/hierarchy-cycle
           "The organization hierarchy contains a cycle."
           {:organization/id organization-id
            :organization-group/id group-id
            :visited-group-ids visited}))

        (let [group
              (require-ancestor-group!
               ctx
               organization-id
               group-id)]
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

;; =============================================================================
;; Authoritative hierarchy snapshots
;; =============================================================================

(defn- organization-snapshot-from-document
  [document]
  (let [organization-id
        (organization/organization-id document)

        document
        (require-valid-organization!
         document
         organization-id)

        scope-context
        (organization/organization-scope-context
         document)]
    {:organization document
     :target document
     :ancestors []
     :scope-context scope-context
     :active?
     (organization/organization-active? document)
     :operational?
     (organization/scope-context-operational?
      scope-context)}))

(defn- organization-group-snapshot-from-document
  [ctx group]
  (let [group-id
        (organization/organization-group-id group)

        group
        (require-valid-group!
         group
         group-id)

        organization-id
        (organization/organization-group-organization-id
         group)

        organization-document
        (require-root!
         ctx
         organization-id)

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
         ancestors)]
    {:organization organization-document
     :target group
     :ancestors ancestors
     :scope-context scope-context
     :active?
     (organization/organization-group-active? group)
     :operational?
     (organization/scope-context-operational?
      scope-context)}))

(defn- location-snapshot-from-document
  [ctx location]
  (let [location-id
        (organization/location-id location)

        location
        (require-valid-location!
         location
         location-id)

        organization-id
        (organization/location-organization-id
         location)

        organization-document
        (require-root!
         ctx
         organization-id)

        ancestors
        (load-group-ancestors
         ctx
         organization-id
         (organization/location-parent-scope
          location))

        scope-context
        (organization/location-scope-context
         organization-document
         location
         ancestors)]
    {:organization organization-document
     :target location
     :ancestors ancestors
     :scope-context scope-context
     :active?
     (organization/location-active? location)
     :operational?
     (organization/scope-context-operational?
      scope-context)}))

(defn scope-snapshot
  "Returns the authoritative hierarchy snapshot for scope, or nil when the
   target entity does not exist.

   Missing ancestors/root documents and structurally corrupt hierarchy state are
   errors: a present target never yields a partial snapshot."
  [ctx scope]
  (when-not
   (organization/scope? scope)
    (hierarchy-error!
     :organization.graph/invalid-scope
     "A valid Organization scope is required."
     {:scope scope}))

  (let [id
        (organization/scope-id scope)]
    (case
     (organization/scope-type scope)

      :organization
      (some->
       (load-organization ctx id)
       organization-snapshot-from-document)

      :organization-group
      (when-let [group
                 (load-organization-group ctx id)]
        (organization-group-snapshot-from-document
         ctx
         group))

      :location
      (when-let [location
                 (load-location ctx id)]
        (location-snapshot-from-document
         ctx
         location)))))

(defn require-scope-snapshot
  "Returns the authoritative hierarchy snapshot for scope or throws when the
   target entity does not exist."
  [ctx scope]
  (or
   (scope-snapshot
    ctx
    scope)

   (hierarchy-error!
    :organization/scope-not-found
    "The Organization scope does not exist."
    {:scope scope})))

(defn snapshot-documents
  "Returns every persisted Organization document used to derive snapshot,
   target-first and Organization-last."
  [{:keys [organization target ancestors]}]
  (into
   [target]
   (concat
    ancestors
    (when
     (not=
      (:xt/id target)
      (:xt/id organization))
      [organization]))))

;; =============================================================================
;; Custom derived resolvers
;; =============================================================================

(defn- derived-resolver
  [id doc-key document-query output derive]
  (graph/resolver
   {:id id
    :input
    [{doc-key document-query}]
    :output
    output
    :resolve-fn
    (fn [_ctx input]
      (derive
       (get input doc-key)))}))

(def organization-derived-fields
  (derived-resolver
   ::organization-derived-fields
   :organization/doc
   organization-document-query
   [{:organization/scope
     scope-query}]
   (fn [document]
     {:organization/scope
      (organization/organization-scope-of
       document)})))

(def organization-group-derived-fields
  (derived-resolver
   ::organization-group-derived-fields
   :organization-group/doc
   organization-group-document-query
   [{:organization-group/parent-scope
     scope-query}
    {:organization-group/scope
     scope-query}]
   (fn [group]
     {:organization-group/parent-scope
      (organization/organization-group-parent-scope
       group)
      :organization-group/scope
      (organization/organization-group-scope-of
       group)})))

(def location-derived-fields
  (derived-resolver
   ::location-derived-fields
   :location/doc
   location-document-query
   [{:location/parent-scope
     scope-query}
    {:location/scope
     scope-query}]
   (fn [location]
     {:location/parent-scope
      (organization/location-parent-scope
       location)
      :location/scope
      (organization/location-scope-of
       location)})))

(def organization-scope-context-resolver
  (graph/resolver
   {:id
    ::organization-scope-context

    :input
    [{:organization/doc
      organization-document-query}]

    :output
    [[:? :organization/active?]
     [:? :organization/operational?]
     {[:? :organization/scope-context]
      scope-context-query}]

    :resolve-fn
    (fn [_ctx {:organization/keys [doc]}]
      (let [{:keys
             [scope-context
              active?
              operational?]}
            (organization-snapshot-from-document
             doc)]
        {:organization/active?
         active?
         :organization/operational?
         operational?
         :organization/scope-context
         scope-context}))}))

(def organization-group-scope-context-resolver
  (graph/resolver
   {:id
    ::organization-group-scope-context

    :input
    [{:organization-group/doc
      organization-group-document-query}]

    :output
    [[:? :organization-group/organization-id]
     [:? :organization-group/active?]
     [:? :organization-group/operational?]
     {[:? :organization-group/ancestor-docs]
      organization-group-document-query}
     {[:? :organization-group/scope-context]
      scope-context-query}]

    :resolve-fn
    (fn [ctx {:organization-group/keys [doc]}]
      (let [{:keys
             [ancestors
              scope-context
              active?
              operational?]}
            (organization-group-snapshot-from-document
             ctx
             doc)]
        {:organization-group/organization-id
         (organization/organization-group-organization-id
          doc)
         :organization-group/active?
         active?
         :organization-group/operational?
         operational?
         :organization-group/ancestor-docs
         ancestors
         :organization-group/scope-context
         scope-context}))}))

(def location-scope-context-resolver
  (graph/resolver
   {:id
    ::location-scope-context

    :input
    [{:location/doc
      location-document-query}]

    :output
    [[:? :location/organization-id]
     [:? :location/active?]
     [:? :location/operational?]
     {[:? :location/ancestor-group-docs]
      organization-group-document-query}
     {[:? :location/scope-context]
      scope-context-query}]

    :resolve-fn
    (fn [ctx {:location/keys [doc]}]
      (let [{:keys
             [ancestors
              scope-context
              active?
              operational?]}
            (location-snapshot-from-document
             ctx
             doc)]
        {:location/organization-id
         (organization/location-organization-id
          doc)
         :location/active?
         active?
         :location/operational?
         operational?
         :location/ancestor-group-docs
         ancestors
         :location/scope-context
         scope-context}))}))

(def custom-resolvers
  "Organization's hand-written Graph contribution.

   gesso.model/build-module supplies the conventional by-id and field resolvers
   for the three descriptors."
  [organization-derived-fields
   organization-scope-context-resolver
   organization-group-derived-fields
   organization-group-scope-context-resolver
   location-derived-fields
   location-scope-context-resolver])
