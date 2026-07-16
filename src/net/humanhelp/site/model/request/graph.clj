(ns net.humanhelp.site.model.request.graph
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [net.humanhelp.site.model.request.domain :as request]))

;; =============================================================================
;; Stored request documents
;; =============================================================================

(def request-document-columns
  [:xt/id
   :request/store
   :request/user
   :request/capability
   :request/store-area
   :request/store-area-text
   :request/title
   :request/details
   :request/status
   :request/revision
   :request/claimed-by
   :request/created-at
   :request/updated-at
   :request/claimed-at
   :request/on-the-way-at
   :request/edited-at
   :request/completed-at
   :request/cancelled-at])

(def request-document-query
  [:*])

;; =============================================================================
;; Query inputs
;; =============================================================================

(defn query-input
  [{:keys [request-id user-id capability-id]}]
  (request/without-nils
   {:request/id request-id
    :current-user/id user-id
    :current-request-capability/id capability-id}))

;; =============================================================================
;; XTDB reads
;; =============================================================================

(defn- queryable-from-ctx
  [ctx]
  (or (:biff/conn ctx)
      (:biff/db ctx)
      (:biff/node ctx)
      (:xtdb/node ctx)
      (throw
       (ex-info
        "Request Graph requires :biff/conn, :biff/db, :biff/node, or :xtdb/node."
        {:error/type :request.graph/missing-queryable
         :ctx-keys (when (map? ctx)
                     (set (keys ctx)))}))))

(defn- q
  [ctx query]
  (biffx/q (queryable-from-ctx ctx) query))

(defn- load-request
  [ctx request-id]
  (when (request/uuid-value? request-id)
    (first
     (q ctx
        {:select request-document-columns
         :from request/entity-type
         :where [:= :xt/id request-id]}))))

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
  (if-some [doc (load-request ctx id)]
    {:request/found? true
     :request/doc doc}
    {:request/found? false}))

;; =============================================================================
;; Stored request fields
;; =============================================================================

(graph/defresolver request-fields
  {:input
   [{:request/doc
     request-document-query}]

   :output
   [:request/id
    :request/store-id
    :request/user-id
    :request/capability-id
    :request/store-area-id
    :request/store-area-text
    :request/title
    :request/details
    :request/status
    :request/revision
    :request/claimed-by-id
    :request/created-at
    :request/updated-at
    :request/claimed-at
    :request/on-the-way-at
    :request/edited-at
    :request/completed-at
    :request/cancelled-at]}
  [_ctx {:request/keys [doc]}]
  (request/without-nils
   {:request/id (:xt/id doc)
    :request/store-id (:request/store doc)
    :request/user-id (:request/user doc)
    :request/capability-id (:request/capability doc)
    :request/store-area-id (:request/store-area doc)
    :request/store-area-text (:request/store-area-text doc)
    :request/title (:request/title doc)
    :request/details (:request/details doc)
    :request/status (:request/status doc)
    :request/revision (:request/revision doc)
    :request/claimed-by-id (:request/claimed-by doc)
    :request/created-at (:request/created-at doc)
    :request/updated-at (:request/updated-at doc)
    :request/claimed-at (:request/claimed-at doc)
    :request/on-the-way-at (:request/on-the-way-at doc)
    :request/edited-at (:request/edited-at doc)
    :request/completed-at (:request/completed-at doc)
    :request/cancelled-at (:request/cancelled-at doc)}))

;; =============================================================================
;; Lifecycle facts
;; =============================================================================

(graph/defresolver request-lifecycle-facts
  {:input
   [{:request/doc
     request-document-query}]

   :output
   [:request/active?
    :request/terminal?
    :request/open?
    :request/claimed?
    :request/on-the-way?
    :request/done?
    :request/cancelled?
    :request/editable?
    :request/cancellable?
    :request/markable-done?
    :request/claimable?
    :request/unclaimable?
    :request/markable-on-the-way?
    :request/progress-stage
    :request/progress-index]}
  [_ctx {:request/keys [doc]}]
  (request/without-nils
   {:request/active? (request/active? doc)
    :request/terminal? (request/terminal? doc)
    :request/open? (request/open? doc)
    :request/claimed? (request/claimed? doc)
    :request/on-the-way? (request/on-the-way? doc)
    :request/done? (request/done? doc)
    :request/cancelled? (request/cancelled? doc)
    :request/editable? (request/editable? doc)
    :request/cancellable? (request/cancellable? doc)
    :request/markable-done? (request/markable-done? doc)
    :request/claimable? (request/claimable? doc)
    :request/unclaimable? (request/unclaimable? doc)
    :request/markable-on-the-way?
    (request/markable-on-the-way? doc)
    :request/progress-stage (request/progress-stage doc)
    :request/progress-index (request/progress-index doc)}))

;; =============================================================================
;; Current-actor facts
;; =============================================================================

(graph/defresolver request-owned-by-current-actor
  {:input
   [{:request/doc
     request-document-query}
    [:? :current-user/id]
    [:? :current-request-capability/id]]

   :output
   [:request/owned-by-current-actor?]}
  [_ctx input]
  {:request/owned-by-current-actor?
   (request/owned-by?
    (:request/doc input)
    {:user-id (:current-user/id input)
     :capability-id (:current-request-capability/id input)})})

(graph/defresolver request-assigned-to-current-employee
  {:input
   [{:request/doc
     request-document-query}
    [:? :current-employee/id]]

   :output
   [:request/assigned?
    :request/assigned-to-current-employee?]}
  [_ctx input]
  (let [doc (:request/doc input)
        employee-id (:current-employee/id input)]
    {:request/assigned?
     (some? (:request/claimed-by doc))

     :request/assigned-to-current-employee?
     (request/assigned-to? doc employee-id)}))

;; =============================================================================
;; Requestor permissions
;; =============================================================================

(graph/defresolver request-customer-permissions
  {:input
   [:request/owned-by-current-actor?
    :request/editable?
    :request/cancellable?
    :request/markable-done?]

   :output
   [:request/can-edit?
    :request/can-cancel?
    :request/can-mark-done?]}
  [_ctx input]
  (let [owner? (:request/owned-by-current-actor? input)]
    {:request/can-edit?
     (and owner?
          (:request/editable? input))

     :request/can-cancel?
     (and owner?
          (:request/cancellable? input))

     :request/can-mark-done?
     (and owner?
          (:request/markable-done? input))}))

;; =============================================================================
;; Shared request-model queries
;; =============================================================================

(def customer-command-query
  [:request/found?

   {[:? :request/doc]
    request-document-query}

   [:? :request/owned-by-current-actor?]
   [:? :request/editable?]
   [:? :request/cancellable?]
   [:? :request/markable-done?]
   [:? :request/can-edit?]
   [:? :request/can-cancel?]
   [:? :request/can-mark-done?]])

(def employee-command-query
  [:request/found?

   {[:? :request/doc]
    request-document-query}

   [:? :request/claimable?]
   [:? :request/unclaimable?]
   [:? :request/markable-on-the-way?]
   [:? :request/assigned?]
   [:? :request/assigned-to-current-employee?]])

;; =============================================================================
;; Resolver collection
;; =============================================================================

(def resolvers
  [request-by-id
   request-fields
   request-lifecycle-facts
   request-owned-by-current-actor
   request-assigned-to-current-employee
   request-customer-permissions])
