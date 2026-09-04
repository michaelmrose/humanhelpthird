(ns net.humanhelp.example.routes
  "Route constants, route specs, route binding, and URL builders for the Human
   Help analogue.

   This namespace is intentionally dependency-light.

   It exists so:
   - views can generate hx-get/hx-post URLs
   - live can generate fragment and stream URLs
   - app.clj can bind route ids to concrete handler functions

   routes.clj owns route facts.
   app.clj owns handler functions."
  (:require
   [clojure.string :as str])
  (:import
   [java.net URLEncoder]
   [java.nio.charset StandardCharsets]))

;; -----------------------------------------------------------------------------
;; Base
;; -----------------------------------------------------------------------------

(def base-path
  "/app")

;; Compatibility only. Prefer net.humanhelp.example.model/store-id for domain code.
(def store-id
  "demo-store")

;; -----------------------------------------------------------------------------
;; Route param names
;; -----------------------------------------------------------------------------

(def request-id-param
  "request-id")

;; -----------------------------------------------------------------------------
;; Query/view-state parameter names
;; -----------------------------------------------------------------------------

(def search-param
  "q")

(def visible-revision-param
  "visible-revision")

(def created-order-param
  "created-order")

(def mine-first-param
  "mine-first")

(def unclaimed-first-param
  "unclaimed-first")

(def show-terminal-param
  "show-terminal")

;; -----------------------------------------------------------------------------
;; Route ids
;; -----------------------------------------------------------------------------

(def page-id
  :humanhelp/page)

(def request-toolbar-fragment-id
  :humanhelp/request-toolbar-fragment)

(def request-list-fragment-id
  :humanhelp/request-list-fragment)

(def create-request-dialog-fragment-id
  :humanhelp/create-request-dialog-fragment)

(def request-toolbar-stream-id
  :humanhelp/request-toolbar-stream)

(def request-list-stream-id
  :humanhelp/request-list-stream)

(def create-request-id
  :humanhelp/create-request)

(def refresh-requests-id
  :humanhelp/refresh-requests)

(def search-requests-id
  :humanhelp/search-requests)

(def apply-board-options-id
  :humanhelp/apply-board-options)

(def claim-request-id
  :humanhelp/claim-request)

(def unclaim-request-id
  :humanhelp/unclaim-request)

(def mark-on-the-way-request-id
  :humanhelp/mark-on-the-way-request)

(def complete-request-id
  :humanhelp/complete-request)

(def reassign-request-id
  :humanhelp/reassign-request)

(def cancel-request-id
  :humanhelp/cancel-request)


;; -----------------------------------------------------------------------------
;; Relative route fragments for Reitit nesting under base-path
;; -----------------------------------------------------------------------------

(def page-route
  "")

(def request-toolbar-fragment-route
  "/fragments/request-toolbar")

(def request-list-fragment-route
  "/fragments/requests")

(def create-request-dialog-fragment-route
  "/fragments/create-request-dialog")

(def request-toolbar-stream-route
  "/streams/request-toolbar")

(def request-list-stream-route
  "/streams/requests")

(def create-request-route
  "/requests")

(def refresh-requests-route
  "/requests/refresh")

(def search-requests-route
  "/requests/search")

(def apply-board-options-route
  "/humanhelp/board-options")

(def claim-request-route
  "/requests/:request-id/claim")

(def unclaim-request-route
  "/requests/:request-id/unclaim")

(def mark-on-the-way-request-route
  "/requests/:request-id/mark-on-the-way")

(def complete-request-route
  "/requests/:request-id/complete")

(def reassign-request-route
  "/requests/:request-id/reassign")

(def cancel-request-route
  "/requests/:request-id/cancel")


;; -----------------------------------------------------------------------------
;; Route specs
;; -----------------------------------------------------------------------------

(def route-specs
  [{:id     page-id
    :method :get
    :route  page-route}

   {:id     request-toolbar-fragment-id
    :method :get
    :route  request-toolbar-fragment-route}

   {:id     request-list-fragment-id
    :method :get
    :route  request-list-fragment-route}

   {:id     create-request-dialog-fragment-id
    :method :get
    :route  create-request-dialog-fragment-route}

   {:id     request-toolbar-stream-id
    :method :get
    :route  request-toolbar-stream-route}

   {:id     request-list-stream-id
    :method :get
    :route  request-list-stream-route}

   {:id     create-request-id
    :method :post
    :route  create-request-route}

   {:id     refresh-requests-id
    :method :post
    :route  refresh-requests-route}

   {:id     search-requests-id
    :method :get
    :route  search-requests-route}

   {:id     apply-board-options-id
    :method :post
    :route  apply-board-options-route}

   {:id     claim-request-id
    :method :post
    :route  claim-request-route}

   {:id     unclaim-request-id
    :method :post
    :route  unclaim-request-route}

   {:id     mark-on-the-way-request-id
    :method :post
    :route  mark-on-the-way-request-route}

   {:id     complete-request-id
    :method :post
    :route  complete-request-route}

   {:id     cancel-request-id
    :method :post
    :route  cancel-request-route}

   {:id     reassign-request-id
    :method :post
    :route  reassign-request-route}
])

(def route-spec-by-id
  (into {}
        (map (juxt :id identity))
        route-specs))

(defn route-spec
  [route-id]
  (or (get route-spec-by-id route-id)
      (throw
       (ex-info "Unknown Human Help route id."
                {:route-id        route-id
                 :known-route-ids (set (keys route-spec-by-id))}))))

;; -----------------------------------------------------------------------------
;; Handler binding
;; -----------------------------------------------------------------------------

(defn- handler-for!
  [handlers {:keys [id method route] :as spec}]
  (or (get handlers id)
      (throw
       (ex-info "Missing Human Help route handler."
                {:route-id    id
                 :method      method
                 :route       route
                 :spec        spec
                 :handler-ids (set (keys handlers))}))))

(defn- route-entry
  [handlers {:keys [method route] :as spec}]
  [route {method (handler-for! handlers spec)}])

(def required-route-ids
  "Every route id owned by the current Human Help example application.

   Route assembly is fail-fast: app.clj must provide a handler for every route
   declared here so partially migrated route tables cannot silently omit an
   application capability."
  [page-id
   request-toolbar-fragment-id
   request-list-fragment-id
   create-request-dialog-fragment-id
   request-toolbar-stream-id
   request-list-stream-id
   create-request-id
   refresh-requests-id
   search-requests-id
   apply-board-options-id
   claim-request-id
   unclaim-request-id
   mark-on-the-way-request-id
   complete-request-id
   cancel-request-id])

(def optional-route-ids
  "Production Request routes whose UI is not yet a required part of the example.

   Reassign needs a selected target helper, so it remains optional until the
   manager affordance is added. Optional means only route assembly is optional;
   when present, the route still binds normally."
  [reassign-request-id])

(defn route-table
  "Return a Reitit route table for Human Help.

   handlers is a map of route id -> handler function.

   Required route ids remain fail-fast. Optional route ids are included only
   when their handlers are present, so this namespace can add new route facts
   without breaking the currently deployed app.clj.

   Example:

     (routes/route-table
       {routes/page-id app-page
        routes/request-list-fragment-id request-list-fragment}
       {:middleware [mid/wrap-signed-in]})

   routes.clj owns route facts. app.clj owns the handler map and middleware."
  ([handlers]
   (route-table handlers nil))
  ([handlers {:keys [middleware]}]
   (let [base-options
         (cond-> {}
           (seq middleware)
           (assoc :middleware middleware))]
     [(into
       [base-path base-options]
       (concat
        (map
         #(route-entry handlers (route-spec %))
         required-route-ids)
        (for [route-id optional-route-ids
              :when (contains? handlers route-id)]
          (route-entry handlers (route-spec route-id)))))])))

;; -----------------------------------------------------------------------------
;; URL helpers
;; -----------------------------------------------------------------------------

(defn path
  "Return an absolute app path from a relative route fragment.

   Example:
     (path request-list-fragment-route)
     => \"/app/fragments/requests\""
  [relative-route]
  (str base-path relative-route))

(defn- encode
  [x]
  (URLEncoder/encode
   (str x)
   (.name StandardCharsets/UTF_8)))

(defn- present?
  [x]
  (and (some? x)
       (not (str/blank? (str x)))))

(defn- truthy-value?
  [x]
  (contains? #{"true" "on" "1" "yes"}
             (some-> x str str/trim str/lower-case)))

(defn- created-order-query-value
  [created-order]
  (let [created-order' (cond
                         (keyword? created-order)
                         (name created-order)

                         (some? created-order)
                         (-> created-order str str/trim)

                         :else
                         nil)]
    (when (and (present? created-order')
               (not= "newest" created-order'))
      created-order')))

(defn query-string
  "Build a URL query string from a map.

   Nil and blank values are omitted. Sequential values produce repeated keys."
  [params]
  (let [pairs
        (mapcat
         (fn [[k v]]
           (cond
             (nil? v)
             []

             (and (string? v) (str/blank? v))
             []

             (sequential? v)
             (for [item  v
                   :when (present? item)]
               [(name k) item])

             :else
             [[(name k) v]]))
         params)]
    (when (seq pairs)
      (str "?"
           (str/join
            "&"
            (for [[k v] pairs]
              (str (encode k) "=" (encode v))))))))

(defn with-query
  [url params]
  (str url (or (query-string params) "")))

(defn view-state-query
  "Return query params shared by request-board fragment URLs.

   Expected view-state keys:
     :search
     :visible-revision
     :created-order
     :mine-first?
     :unclaimed-first?
     :show-terminal?

   q/visible-revision are always present in the returned map, while board-option
   keys are included only when they carry non-default state."
  [{:keys [search
           visible-revision
           created-order
           mine-first?
           unclaimed-first?
           show-terminal?]}]
  (let [created-order' (created-order-query-value created-order)]
    (cond-> {search-param           search
             visible-revision-param visible-revision}
      (some? created-order')
      (assoc created-order-param created-order')

      (truthy-value? mine-first?)
      (assoc mine-first-param "true")

      (truthy-value? unclaimed-first?)
      (assoc unclaimed-first-param "true")

      (truthy-value? show-terminal?)
      (assoc show-terminal-param "true"))))

(defn request-route
  "Substitute request-id into a relative request route."
  [relative-route request-id]
  (str/replace relative-route
               ":request-id"
               (encode request-id)))

;; -----------------------------------------------------------------------------
;; Page
;; -----------------------------------------------------------------------------

(defn page-url
  ([]
   base-path)
  ([view-state]
   (with-query base-path (view-state-query view-state))))

;; -----------------------------------------------------------------------------
;; Fragment URLs
;; -----------------------------------------------------------------------------

(defn request-toolbar-fragment-url
  ([]
   (path request-toolbar-fragment-route))
  ([view-state]
   (with-query
     (request-toolbar-fragment-url)
     (view-state-query view-state))))

(defn request-list-fragment-url
  ([]
   (path request-list-fragment-route))
  ([view-state]
   (with-query
     (request-list-fragment-url)
     (view-state-query view-state))))

(defn create-request-dialog-fragment-url
  []
  (path create-request-dialog-fragment-route))

;; -----------------------------------------------------------------------------
;; Stream URLs
;; -----------------------------------------------------------------------------

(defn request-toolbar-stream-url
  ([]
   (path request-toolbar-stream-route))
  ([view-state]
   (with-query
     (request-toolbar-stream-url)
     (view-state-query view-state))))

(defn request-list-stream-url
  ([]
   (path request-list-stream-route))
  ([view-state]
   (with-query
     (request-list-stream-url)
     (view-state-query view-state))))

;; -----------------------------------------------------------------------------
;; Request creation / list controls
;; -----------------------------------------------------------------------------

(defn create-request-url
  []
  (path create-request-route))

(defn refresh-requests-url
  ([]
   (path refresh-requests-route))
  ([view-state]
   (with-query
     (refresh-requests-url)
     (view-state-query view-state))))

(defn search-requests-url
  ([]
   (path search-requests-route))
  ([view-state]
   (with-query
     (search-requests-url)
     (view-state-query view-state))))

(defn apply-board-options-url
  []
  (path apply-board-options-route))

;; -----------------------------------------------------------------------------
;; Request lifecycle action URLs
;; -----------------------------------------------------------------------------

(defn claim-request-url
  [request-id]
  (path (request-route claim-request-route request-id)))

(defn unclaim-request-url
  [request-id]
  (path (request-route unclaim-request-route request-id)))

(defn mark-on-the-way-request-url
  [request-id]
  (path (request-route mark-on-the-way-request-route request-id)))

(defn complete-request-url
  [request-id]
  (path (request-route complete-request-route request-id)))

(defn reassign-request-url
  [request-id]
  (path (request-route reassign-request-route request-id)))

(defn cancel-request-url
  [request-id]
  (path (request-route cancel-request-route request-id)))

(defn operation-url
  "Return the HTTP endpoint for one production Request semantic operation."
  [request-id operation]
  (case operation
    :request/claim
    (claim-request-url request-id)

    :request/unclaim
    (unclaim-request-url request-id)

    :request/mark-on-the-way
    (mark-on-the-way-request-url request-id)

    :request/complete
    (complete-request-url request-id)

    :request/cancel
    (cancel-request-url request-id)

    :request/reassign
    (reassign-request-url request-id)

    (throw
     (ex-info "Unknown production HumanHelp Request operation."
              {:request-id request-id
               :operation operation}))))

