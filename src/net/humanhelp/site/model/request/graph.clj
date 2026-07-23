(ns net.humanhelp.site.model.request.graph
  "Gesso Graph resolvers and XTDB2 read contracts for the Request model.

   This namespace owns:

   - Request document lookup;
   - canonical Location Request collections;
   - Request-owned scalar and lifecycle projections;
   - Request Assignment document lookup;
   - active Request Assignment collections for a Request;
   - aggregate primary/collaborator facts derived from those assignments;
   - public Graph query vectors consumed by Request Core and FX.

   Ordinary persistence reads use Biff's XTDB2 query helper. Gesso Live is not
   used as a database abstraction.

   Helper eligibility, Organization hierarchy, Location operational state, User
   profiles, User roles, and User skills remain outside this namespace."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.assignment :as assignment]
   [net.humanhelp.site.model.request.domain :as request]))

;; =============================================================================
;; Persisted Request document query shape
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
;; Persisted Request Assignment document query shape
;; =============================================================================

(def assignment-required-document-columns
  [:xt/id
   :request-assignment/request
   :request-assignment/helper
   :request-assignment/role
   :request-assignment/status
   :request-assignment/source
   :request-assignment/assigned-at
   :request-assignment/revision
   :request-assignment/created-at
   :request-assignment/updated-at])

(def assignment-optional-document-columns
  [:request-assignment/assigned-by
   :request-assignment/ended-at
   :request-assignment/ended-by
   :request-assignment/end-reason])

(def assignment-document-columns
  "Exact XTDB2 columns needed to reconstruct a persisted Request Assignment."
  (into
   assignment-required-document-columns
   assignment-optional-document-columns))

(def assignment-document-query
  "Gesso Graph shape for one complete persisted Request Assignment document."
  (into
   assignment-required-document-columns
   (map
    (fn [attribute]
      [:? attribute]))
   assignment-optional-document-columns))

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
    (map
     second
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
  "Pure lifecycle facts derived only from :request/doc.

   Assignment possession is deliberately absent because helper participation is
   stored in Request Assignment documents."
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
   :request/expects-primary-assignment?])

;; =============================================================================
;; Request Assignment projected facts
;; =============================================================================

(def assignment-required-field-pairs
  [[:xt/id
    :request-assignment/id]

   [:request-assignment/request
    :request-assignment/request-id]

   [:request-assignment/helper
    :request-assignment/helper-id]

   [:request-assignment/role
    :request-assignment/role]

   [:request-assignment/status
    :request-assignment/status]

   [:request-assignment/source
    :request-assignment/source]

   [:request-assignment/assigned-at
    :request-assignment/assigned-at]

   [:request-assignment/revision
    :request-assignment/revision]

   [:request-assignment/created-at
    :request-assignment/created-at]

   [:request-assignment/updated-at
    :request-assignment/updated-at]])

(def assignment-optional-field-pairs
  [[:request-assignment/assigned-by
    :request-assignment/assigned-by]

   [:request-assignment/ended-at
    :request-assignment/ended-at]

   [:request-assignment/ended-by
    :request-assignment/ended-by]

   [:request-assignment/end-reason
    :request-assignment/end-reason]])

(def assignment-field-pairs
  (into
   assignment-required-field-pairs
   assignment-optional-field-pairs))

(def assignment-field-query
  (into
   []
   (concat
    (map
     second
     assignment-required-field-pairs)

    (map
     (fn [[_document-key graph-key]]
       [:? graph-key])
     assignment-optional-field-pairs)

    [:request-assignment/expected-version])))

(def assignment-lifecycle-query
  [:request-assignment/active?
   :request-assignment/ended?
   :request-assignment/primary?
   :request-assignment/collaborator?
   :request-assignment/active-primary?
   :request-assignment/active-collaborator?])

(def request-assignment-summary-query
  "Aggregate active-assignment facts attached to a Request result."
  [:request/has-primary-assignment?
   :request/active-helper-ids
   :request/active-collaborator-helper-ids])

(defn- project-document
  [document field-pairs]
  (reduce
   (fn [result [document-key graph-key]]
     (if-some [value
               (get
                document
                document-key)]
       (assoc
        result
        graph-key
        value)
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
    (comp
     nil?
     val))
   value))

(defn request-query-input
  "Builds the Graph input for one Request lookup."
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
    (true?
     include-terminal?)}))

(defn assignment-query-input
  "Builds the Graph input for one Request Assignment lookup."
  [{:keys [assignment-id]}]
  (without-nils
   {:request-assignment/id
    assignment-id}))

(defn request-assignments-query-input
  "Builds input for Request Assignment reads.

   :include-ended? defaults to false."
  [{:keys
    [request-id
     include-ended?]}]
  (without-nils
   {:request/id
    request-id

    :request-assignment/include-ended?
    (true?
     include-ended?)}))

;; =============================================================================
;; XTDB2 reads
;; =============================================================================

(defn- q
  "Runs an ordinary Request-model read through Biff's XTDB2 helper."
  [ctx query]
  (biffx/q
   (:biff/conn ctx)
   query))

(defn- valid-loaded-request
  [document]
  (when
   document
    (request/require-request-document
     document)))

(defn- valid-loaded-assignment
  [document]
  (when
   document
    (assignment/require-document
     document)))

(defn- load-request
  [ctx request-id]
  (when
   (uuid?
    request-id)
    (some->
     (first
      (q
       ctx
       {:select
        request-document-columns

        :from
        request/request-entity-type

        :where
        [:= :xt/id request-id]

        :limit
        1}))
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

    (not
     include-terminal?)
    (conj
     active-status-predicate)))

(defn- load-location-requests
  [ctx organization-id location-id include-terminal?]
  (if
   (and
    (uuid?
     organization-id)

    (uuid?
     location-id))
    (mapv
     valid-loaded-request
     (q
      ctx
      {:select
       request-document-columns

       :from
       request/request-entity-type

       :where
       (location-requests-where
        organization-id
        location-id
        include-terminal?)

       :order-by
       [[:request/created-at :desc]
        [:xt/id :desc]]}))
    []))

(defn- load-assignment
  [ctx assignment-id]
  (when
   (uuid?
    assignment-id)
    (some->
     (first
      (q
       ctx
       {:select
        assignment-document-columns

        :from
        assignment/entity-type

        :where
        [:= :xt/id assignment-id]

        :limit
        1}))
     valid-loaded-assignment)))

(defn- request-assignments-where
  [request-id include-ended?]
  (cond->
   [:and
    [:= :request-assignment/request request-id]]

    (not
     include-ended?)
    (conj
     [:= :request-assignment/status :active])))

(defn- load-request-assignments
  [ctx request-id include-ended?]
  (if
   (uuid?
    request-id)
    (mapv
     valid-loaded-assignment
     (q
      ctx
      {:select
       assignment-document-columns

       :from
       assignment/entity-type

       :where
       (request-assignments-where
        request-id
        include-ended?)

       :order-by
       [[:request-assignment/assigned-at :asc]
        [:xt/id :asc]]}))
    []))

(defn- lookup-result
  [found-key document-key document]
  (if
   document
    {found-key
     true

     document-key
     document}
    {found-key
     false}))

(defn- request-document-seeds
  [documents]
  (mapv
   (fn [document]
     {:request/doc
      document})
   documents))

(defn- assignment-document-seeds
  [documents]
  (mapv
   (fn [document]
     {:request-assignment/doc
      document})
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
   :request/found?
   :request/doc
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
        (request/require-request-document
         doc)]
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
       request/request-version)})))

;; =============================================================================
;; Pure Request lifecycle facts
;; =============================================================================

(graph/defresolver request-lifecycle-facts
  {:input
   [{:request/doc
     request-document-query}]

   :output
   request-lifecycle-query}
  [_ctx {:request/keys [doc]}]
  (let [doc
        (request/require-request-document
         doc)]
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

     :request/expects-primary-assignment?
     (request/lifecycle-expects-primary-assignment?
      doc)}))

;; =============================================================================
;; Request Assignment lookup
;; =============================================================================

(graph/defresolver assignment-by-id
  {:input
   [:request-assignment/id]

   :output
   [:request-assignment/found?

    {[:? :request-assignment/doc]
     assignment-document-query}]}
  [ctx {:request-assignment/keys [id]}]
  (lookup-result
   :request-assignment/found?
   :request-assignment/doc
   (load-assignment
    ctx
    id)))

;; =============================================================================
;; Request Assignment collections
;; =============================================================================

(graph/defresolver assignments-for-request
  {:input
   [:request/id
    :request-assignment/include-ended?]

   :output
   [{:request/assignments
     [{:request-assignment/doc
       assignment-document-query}]}]}
  [ctx
   {:request/keys [id]
    :request-assignment/keys [include-ended?]}]
  {:request/assignments
   (assignment-document-seeds
    (load-request-assignments
     ctx
     id
     include-ended?))})

;; =============================================================================
;; Request Assignment projections
;; =============================================================================

(graph/defresolver assignment-fields
  {:input
   [{:request-assignment/doc
     assignment-document-query}]

   :output
   assignment-field-query}
  [_ctx {:request-assignment/keys [doc]}]
  (let [doc
        (assignment/require-document
         doc)]
    (assoc
     (project-document
      doc
      assignment-field-pairs)

     :request-assignment/expected-version
     (model.common/expected-version
      doc
      assignment/version))))

(graph/defresolver assignment-lifecycle-facts
  {:input
   [{:request-assignment/doc
     assignment-document-query}]

   :output
   assignment-lifecycle-query}
  [_ctx {:request-assignment/keys [doc]}]
  (let [doc
        (assignment/require-document
         doc)]
    {:request-assignment/active?
     (assignment/active?
      doc)

     :request-assignment/ended?
     (assignment/ended?
      doc)

     :request-assignment/primary?
     (assignment/primary?
      doc)

     :request-assignment/collaborator?
     (assignment/collaborator?
      doc)

     :request-assignment/active-primary?
     (assignment/active-primary?
      doc)

     :request-assignment/active-collaborator?
     (assignment/active-collaborator?
      doc)}))

;; =============================================================================
;; Request Assignment aggregate facts
;; =============================================================================

(defn- assignment-documents
  [nodes]
  (mapv
   :request-assignment/doc
   (or
    nodes
    [])))

(defn- require-assignment-belongs-to-request!
  [request-document assignment-document]
  (assignment/require-document
   assignment-document)

  (when-not
   (assignment/for-request?
    assignment-document
    (request/request-id
     request-document))
    (throw
     (ex-info
      "Request Graph received an assignment for another Request."
      {:error/type
       :request.graph/assignment-request-mismatch

       :request/id
       (request/request-id
        request-document)

       :request-assignment/id
       (assignment/assignment-id
        assignment-document)

       :request-assignment/request
       (assignment/request-id
        assignment-document)})))

  assignment-document)

(graph/defresolver request-assignment-summary
  {:input
   [{:request/doc
     request-document-query}

    {:request/assignments
     [{:request-assignment/doc
       assignment-document-query}]}]

   :output
   request-assignment-summary-query}
  [_ctx input]
  (let [request-document
        (request/require-request-document
         (:request/doc input))

        assignments
        (mapv
         #(require-assignment-belongs-to-request!
           request-document
           %)
         (assignment-documents
          (:request/assignments input)))

        primary
        (assignment/active-primary-assignment
         assignments)]
    {:request/has-primary-assignment?
     (boolean
      primary)

     :request/active-helper-ids
     (assignment/active-helper-ids
      assignments)

     :request/active-collaborator-helper-ids
     (assignment/active-collaborator-helper-ids
      assignments)}))

;; =============================================================================
;; Public Graph query contracts
;; =============================================================================

(defn- optional-query-item
  [query-item]
  (cond
    (and
     (vector?
      query-item)

     (=
      :?
      (first query-item)))
    query-item

    (map?
     query-item)
    (let [[join-key subquery]
          (first query-item)

          optional-key
          (if
           (and
            (vector?
             join-key)

            (=
             :?
             (first join-key)))
            join-key
            [:? join-key])]
      {optional-key
       subquery})

    :else
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
       [:?
        attribute])
     request-lifecycle-query))))

(def assignment-derived-facts-query
  (into
   []
   (concat
    (map
     optional-query-item
     assignment-field-query)

    (map
     (fn [attribute]
       [:?
        attribute])
     assignment-lifecycle-query))))

(def assignment-facts-query
  "Loads one Request Assignment with all assignment-owned projections and
   lifecycle facts."
  (into
   [:request-assignment/found?

    {[:? :request-assignment/doc]
     assignment-document-query}]
   assignment-derived-facts-query))

(def assignment-command-query
  "Loads the current Request Assignment document and optimistic-concurrency
   metadata for Request FX."
  [:request-assignment/found?

   {[:? :request-assignment/doc]
    assignment-document-query}

   [:?
    :request-assignment/expected-version]])

(def active-assignments-query
  "Loads active Request Assignments for one Request.

   Callers should supply :request-assignment/include-ended? false."
  [{:request/assignments
    (into
     [{:request-assignment/doc
       assignment-document-query}]
     assignment-derived-facts-query)}])

(def assignment-history-query
  "Loads all current Request Assignment records for one Request, including
   ended records.

   Callers should supply :request-assignment/include-ended? true."
  active-assignments-query)

(def request-assignment-query
  "Active assignment nodes and aggregate helper/collaborator facts for one
   Request."
  (into
   [{:request/assignments
     (into
      [{:request-assignment/doc
        assignment-document-query}]
      assignment-derived-facts-query)}]
   request-assignment-summary-query))

(def request-location-item-query
  "Request-owned facts returned for each member of a Location collection.

   Assignment nodes are active assignments only."
  (into
   [{:request/doc
     request-document-query}]
   (concat
    request-derived-facts-query
    request-assignment-query)))

(def request-command-query
  "Loads the current Request document, optimistic-concurrency metadata, and
   active Request Assignments.

   Request FX uses this query before constructing lifecycle or assignment
   transaction plans."
  (into
   [:request/found?

    {[:? :request/doc]
     request-document-query}

    [:?
     :request/expected-version]]
   request-assignment-query))

(def request-facts-query
  "Loads one Request with all Request-owned projections, lifecycle facts, active
   assignments, and aggregate helper/collaborator facts."
  (into
   [:request/found?

    {[:? :request/doc]
     request-document-query}]
   (concat
    request-derived-facts-query
    request-assignment-query)))

(def location-requests-query
  "Loads the canonical Request collection for one Organization Location.

   Results are ordered newest first with Request ID as a deterministic
   tiebreaker. Each item includes active Request Assignments and aggregate
   primary/collaborator facts.

   Authorization and display enrichment remain the caller's responsibility via
   the public Organization and User model interfaces."
  [{:request/location-requests
    request-location-item-query}])

;; =============================================================================
;; Resolver registry
;; =============================================================================

(def resolvers
  [request-by-id
   requests-at-location
   request-fields
   request-lifecycle-facts

   assignment-by-id
   assignments-for-request
   assignment-fields
   assignment-lifecycle-facts
   request-assignment-summary])
