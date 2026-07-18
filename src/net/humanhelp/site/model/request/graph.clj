(ns net.humanhelp.site.model.request.graph
  "Read-only Gesso Graph resolvers for the HumanHelp Request model.

   This namespace loads Request documents from XTDB and derives only
   Request-owned facts:

   - persisted Request fields;
   - structural requestor projection;
   - lifecycle and helper-assignment facts;
   - optimistic-concurrency metadata;
   - canonical Request collections scoped to one Organization Location.

   Location collections query canonical Request documents directly. They do not
   require mailbox projection documents or a cache. Collection rows seed the
   complete :request/doc so downstream Request field and lifecycle resolvers do
   not need to reload each Request by ID.

   It does not validate Organization hierarchy, derive User access, authenticate
   Request capabilities, decide current-actor permissions, execute mutations,
   or publish Gesso Live invalidations. Request FX composes those external facts
   through the public Organization and User facades."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.domain.core :as request]))

;; =============================================================================
;; Stored Request documents
;; =============================================================================

(def request-document-columns
  [:xt/id
   :request/organization
   :request/location
   :request/requestor-type
   :request/requestor-id
   :request/title
   :request/details
   :request/location-detail
   :request/status
   :request/revision
   :request/created-at
   :request/updated-at
   :request/helper
   :request/claimed-at
   :request/on-the-way-at
   :request/completed-at
   :request/cancelled-at
   :request/cancellation-reason])

(def request-document-query
  [:*])

;; =============================================================================
;; Graph field projections
;; =============================================================================

(def request-field-pairs
  [[:xt/id
    :request/id]

   [:request/organization
    :request/organization-id]

   [:request/location
    :request/location-id]

   [:request/requestor-type
    :request/requestor-type]

   [:request/requestor-id
    :request/requestor-id]

   [:request/title
    :request/title]

   [:request/details
    :request/details]

   [:request/location-detail
    :request/location-detail]

   [:request/status
    :request/status]

   [:request/revision
    :request/revision]

   [:request/created-at
    :request/created-at]

   [:request/updated-at
    :request/updated-at]

   [:request/helper
    :request/helper]

   [:request/claimed-at
    :request/claimed-at]

   [:request/on-the-way-at
    :request/on-the-way-at]

   [:request/completed-at
    :request/completed-at]

   [:request/cancelled-at
    :request/cancelled-at]

   [:request/cancellation-reason
    :request/cancellation-reason]])

(def request-field-query
  (into
   (mapv second request-field-pairs)
   [{:request/requestor
     [:requestor/type
      :requestor/id]}
    :request/expected-version]))

(def request-lifecycle-query
  [:request/open?
   :request/claimed?
   :request/on-the-way?
   :request/done?
   :request/cancelled?
   :request/active?
   :request/terminal?
   :request/editable?
   :request/claimable?
   :request/unclaimable?
   :request/markable-on-the-way?
   :request/completable?
   :request/cancellable?
   :request/has-helper?
   :request/actively-assigned?])

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

(defn request-query-input
  [{:keys [request-id]}]
  (without-nils
   {:request/id
    request-id}))

(defn location-requests-query-input
  "Builds the Graph input for one canonical Location Request collection.

   Terminal Requests are excluded unless :include-terminal? is exactly true."
  [{:keys
    [organization-id
     location-id
     include-terminal?]}]
  (without-nils
   {:request/organization-id
    organization-id

    :request/location-id
    location-id

    :request/include-terminal?
    (true? include-terminal?)}))

;; =============================================================================
;; XTDB reads
;; =============================================================================

(defn- q
  [ctx query]
  (biffx/q
   (:biff/conn ctx)
   query))

(defn- load-request
  [ctx request-id]
  (when
   (uuid? request-id)
    (first
     (q
      ctx
      {:select
       request-document-columns

       :from
       request/entity-type

       :where
       [:= :xt/id request-id]}))))

(def active-status-predicate
  [:or
   [:= :request/status :open]
   [:= :request/status :claimed]
   [:= :request/status :on-the-way]])

(defn- location-requests-where
  [organization-id location-id include-terminal?]
  (cond->
   [:and
    [:= :request/organization organization-id]
    [:= :request/location location-id]]

    (not include-terminal?)
    (conj
     active-status-predicate)))

(defn- load-location-requests
  [ctx organization-id location-id include-terminal?]
  (if
   (and
    (uuid? organization-id)
    (uuid? location-id))
    (vec
     (q
      ctx
      {:select
       request-document-columns

       :from
       request/entity-type

       :where
       (location-requests-where
        organization-id
        location-id
        include-terminal?)

       :order-by
       [[:request/created-at :desc]
        [:xt/id :desc]]}))
    []))

(defn- request-document-seeds
  [documents]
  (mapv
   (fn [document]
     {:request/doc
      document})
   documents))

(defn- lookup-result
  [document]
  (if
   document
    {:request/found?
     true

     :request/doc
     document}
    {:request/found?
     false}))

;; =============================================================================
;; Request lookup
;; =============================================================================

(graph/defresolver request-by-id
  {:input
   [:request/id]

   :output
   [:request/found?
    {[:? :request/doc]
     request-document-query}]}
  [ctx {:request/keys [id]}]
  (lookup-result
   (load-request
    ctx
    id)))

;; =============================================================================
;; Location Request collections
;; =============================================================================

(graph/defresolver requests-at-location
  {:input
   [:request/organization-id
    :request/location-id
    :request/include-terminal?]

   :output
   [{:request/location-requests
     [{:request/doc
       request-document-query}]}]}
  [ctx
   {:request/keys
    [organization-id
     location-id
     include-terminal?]}]
  {:request/location-requests
   (request-document-seeds
    (load-location-requests
     ctx
     organization-id
     location-id
     include-terminal?))})

;; =============================================================================
;; Stored Request fields
;; =============================================================================

(graph/defresolver request-fields
  {:input
   [{:request/doc
     request-document-query}]

   :output
   request-field-query}
  [_ctx {:request/keys [doc]}]
  (merge
   (project-document
    doc
    request-field-pairs)

   {:request/requestor
    (request/requestor
     doc)

    :request/expected-version
    (model.common/expected-version
     doc
     request/version)}))

;; =============================================================================
;; Lifecycle facts
;; =============================================================================

(graph/defresolver request-lifecycle-facts
  {:input
   [{:request/doc
     request-document-query}]

   :output
   request-lifecycle-query}
  [_ctx {:request/keys [doc]}]
  {:request/open?
   (request/open?
    doc)

   :request/claimed?
   (request/claimed?
    doc)

   :request/on-the-way?
   (request/on-the-way?
    doc)

   :request/done?
   (request/done?
    doc)

   :request/cancelled?
   (request/cancelled?
    doc)

   :request/active?
   (request/active?
    doc)

   :request/terminal?
   (request/terminal?
    doc)

   :request/editable?
   (request/editable?
    doc)

   :request/claimable?
   (request/claimable?
    doc)

   :request/unclaimable?
   (request/unclaimable?
    doc)

   :request/markable-on-the-way?
   (request/markable-on-the-way?
    doc)

   :request/completable?
   (request/completable?
    doc)

   :request/cancellable?
   (request/cancellable?
    doc)

   :request/has-helper?
   (request/has-helper?
    doc)

   :request/actively-assigned?
   (request/actively-assigned?
    doc)})

;; =============================================================================
;; Public query contracts
;; =============================================================================

(defn- optional-query-item
  [query-item]
  (if
   (map? query-item)
    {(vector
      :?
      (ffirst query-item))
     (second
      (first query-item))}
    [:?
     query-item]))

(def request-derived-facts-query
  (into
   []
   (concat
    (map
     optional-query-item
     request-field-query)

    (map
     (fn [attribute]
       [:? attribute])
     request-lifecycle-query))))

(def request-location-item-query
  "Request-owned facts returned for each member of a Location collection."
  (into
   [{:request/doc
     request-document-query}]
   request-derived-facts-query))

(def request-command-query
  "Loads the current Request document and its expected-version metadata.

   FX uses this query before constructing an update command."
  [:request/found?

   {[:? :request/doc]
    request-document-query}

   [:? :request/expected-version]])

(def request-facts-query
  "Loads one Request with all Request-owned projected and lifecycle facts.

   Organization and User facts are intentionally absent."
  (into
   [:request/found?

    {[:? :request/doc]
     request-document-query}]

   request-derived-facts-query))

(def location-requests-query
  "Loads the canonical Request collection for one Organization Location.

   Results are ordered newest first with Request ID as a deterministic
   tiebreaker. The query input controls whether terminal Requests are included.

   Organization validity, Location hierarchy, User access, actor-specific
   capabilities, and identity display enrichment are intentionally absent."
  [{:request/location-requests
    request-location-item-query}])

;; =============================================================================
;; Resolver collection
;; =============================================================================

(def resolvers
  [request-by-id
   requests-at-location
   request-fields
   request-lifecycle-facts])
