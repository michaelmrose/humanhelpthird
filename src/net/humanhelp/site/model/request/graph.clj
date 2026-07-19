(ns net.humanhelp.site.model.request.graph
  "Gesso Graph resolvers and XTDB2 read contracts for the Request model.

   This namespace owns Request document lookup, canonical Location Request
   collections, Request-owned projections, lifecycle facts, and public Graph
   query vectors.

   Ordinary persistence reads use Biff's XTDB2 query helper. This namespace
   does not use Gesso Live as a database abstraction, derive Organization
   hierarchy or operational state, calculate User access, authorize actors,
   execute Request operations, or publish invalidations. FX composes Request
   facts with the public Organization and User model interfaces."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.domain :as request]))

;; =============================================================================
;; Persisted document query shape
;; =============================================================================

(def request-required-document-columns
  [:xt/id
   :request/organization
   :request/location
   :request/requestor-type
   :request/requestor-id
   :request/title
   :request/status
   :request/revision
   :request/created-at
   :request/updated-at])

(def request-optional-document-columns
  [:request/details
   :request/location-detail
   :request/helper
   :request/claimed-at
   :request/on-the-way-at
   :request/completed-at
   :request/cancelled-at
   :request/cancellation-reason])

(def request-document-columns
  "Exact XTDB2 columns needed to reconstruct a persisted Request document."
  (into
   request-required-document-columns
   request-optional-document-columns))

(def request-document-query
  "Gesso Graph shape for a complete persisted Request document."
  (into
   request-required-document-columns
   (map
    (fn [attribute]
      [:? attribute]))
   request-optional-document-columns))

;; =============================================================================
;; Request-owned projected facts
;; =============================================================================

(def request-required-field-pairs
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

   [:request/status
    :request/status]

   [:request/revision
    :request/revision]

   [:request/created-at
    :request/created-at]

   [:request/updated-at
    :request/updated-at]])

(def request-optional-field-pairs
  [[:request/details
    :request/details]

   [:request/location-detail
    :request/location-detail]

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

(def request-field-pairs
  (into
   request-required-field-pairs
   request-optional-field-pairs))

(def request-field-query
  "Request-owned scalar projections and stable value objects derived from
   :request/doc."
  (into
   []
   (concat
    (map second
         request-required-field-pairs)

    (map
     (fn [[_document-key graph-key]]
       [:? graph-key])
     request-optional-field-pairs)

    [{:request/requestor
      [:requestor/type
       :requestor/id]}

     :request/expected-version])))

(def request-lifecycle-query
  "Pure lifecycle facts derived from :request/doc."
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
     (if-some [value (get document document-key)]
       (assoc result graph-key value)
       result))
   {}
   field-pairs))

;; =============================================================================
;; Query input builders
;; =============================================================================

(defn- without-nils
  [value]
  (into
   {}
   (remove
    (comp nil? val))
   value))

(defn request-query-input
  "Builds the Graph input for one Request lookup."
  [{:keys [request-id]}]
  (without-nils
   {:request/id request-id}))

(defn location-requests-query-input
  "Builds the Graph input for one canonical Location Request collection.

   Terminal Requests are excluded unless :include-terminal? is exactly true."
  [{:keys [organization-id
           location-id
           include-terminal?]}]
  (without-nils
   {:request/organization-id organization-id
    :request/location-id location-id
    :request/include-terminal? (true? include-terminal?)}))

;; =============================================================================
;; XTDB2 reads
;; =============================================================================

(defn- q
  "Runs an ordinary Request model read through Biff's XTDB2 helper.

   :biff/conn is the canonical Biff XTDB2 connectable. Read-after-write
   consistency for rendered live fragments belongs to the Gesso Live fragment
   query boundary, not to Request Graph."
  [ctx query]
  (biffx/q
   (:biff/conn ctx)
   query))

(defn- valid-loaded-request
  [document]
  (when document
    (request/require-request-document document)))

(defn- load-request
  [ctx request-id]
  (when
   (uuid? request-id)
    (some->
     (first
      (q
       ctx
       {:select request-document-columns
        :from request/request-entity-type
        :where [:= :xt/id request-id]
        :limit 1}))
     valid-loaded-request)))

(def active-status-predicate
  "XTDB2 predicate selecting Requests that still participate in the active
   Location board."
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
    (conj active-status-predicate)))

(defn- load-location-requests
  [ctx organization-id location-id include-terminal?]
  (if
   (and
    (uuid? organization-id)
    (uuid? location-id))
    (mapv
     valid-loaded-request
     (q
      ctx
      {:select request-document-columns
       :from request/request-entity-type
       :where
       (location-requests-where
        organization-id
        location-id
        include-terminal?)
       :order-by
       [[:request/created-at :desc]
        [:xt/id :desc]]}))
    []))

(defn- lookup-result
  [document]
  (if document
    {:request/found? true
     :request/doc document}
    {:request/found? false}))

(defn- request-document-seeds
  [documents]
  (mapv
   (fn [document]
     {:request/doc document})
   documents))

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
   (load-request ctx id)))

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
;; Stored Request projections
;; =============================================================================

(graph/defresolver request-fields
  {:input
   [{:request/doc
     request-document-query}]

   :output
   request-field-query}
  [_ctx {:request/keys [doc]}]
  (let [doc
        (request/require-request-document doc)]
    (merge
     (project-document
      doc
      request-field-pairs)

     {:request/requestor
      (request/requestor doc)

      :request/expected-version
      (model.common/expected-version
       doc
       request/request-version)})))

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
  (let [doc
        (request/require-request-document doc)]
    {:request/open?
     (request/open? doc)

     :request/claimed?
     (request/claimed? doc)

     :request/on-the-way?
     (request/on-the-way? doc)

     :request/done?
     (request/done? doc)

     :request/cancelled?
     (request/cancelled? doc)

     :request/active?
     (request/active? doc)

     :request/terminal?
     (request/terminal? doc)

     :request/editable?
     (request/editable? doc)

     :request/claimable?
     (request/claimable? doc)

     :request/unclaimable?
     (request/unclaimable? doc)

     :request/markable-on-the-way?
     (request/markable-on-the-way? doc)

     :request/completable?
     (request/completable? doc)

     :request/cancellable?
     (request/cancellable? doc)

     :request/has-helper?
     (request/has-helper? doc)

     :request/actively-assigned?
     (request/actively-assigned? doc)}))

;; =============================================================================
;; Public Graph query contracts
;; =============================================================================

(defn- optional-query-item
  [query-item]
  (cond
    (and
     (vector? query-item)
     (= :? (first query-item)))
    query-item

    (map? query-item)
    (let [[join-key subquery]
          (first query-item)

          optional-key
          (if
           (and
            (vector? join-key)
            (= :? (first join-key)))
            join-key
            [:? join-key])]
      {optional-key subquery})

    :else
    [:? query-item]))

(def request-derived-facts-query
  (into
   []
   (concat
    (map optional-query-item
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
  "Loads the current Request document and optimistic-concurrency metadata.

   Request FX uses this query before constructing an update command."
  [:request/found?

   {[:? :request/doc]
    request-document-query}

   [:? :request/expected-version]])

(def request-facts-query
  "Loads one Request with all Request-owned projections and lifecycle facts.

   Organization hierarchy, operational state, User identity, and actor access
   are intentionally absent."
  (into
   [:request/found?

    {[:? :request/doc]
     request-document-query}]

   request-derived-facts-query))

(def location-requests-query
  "Loads the canonical Request collection for one Organization Location.

   Results are ordered newest first with Request ID as a deterministic
   tiebreaker. Input controls whether terminal Requests are included.

   Authorization and display enrichment are intentionally composed by callers
   through the public Organization and User model interfaces."
  [{:request/location-requests
    request-location-item-query}])

;; =============================================================================
;; Resolver registry
;; =============================================================================

(def resolvers
  [request-by-id
   requests-at-location
   request-fields
   request-lifecycle-facts])
