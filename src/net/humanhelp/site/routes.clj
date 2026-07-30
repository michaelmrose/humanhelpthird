(ns net.humanhelp.site.routes
  (:require
   [clojure.string :as str])
  (:import
   [java.net URLEncoder]
   [java.nio.charset StandardCharsets]))

(def base-path
  "/app")

(def location-id-param
  "location-id")

(def request-id-param
  "request-id")

(def page-id
  :site/get-help-page)

(def validate-request-id
  :site/validate-request)

(def create-request-id
  :site/create-request)

(def request-page-id
  :site/request-page)

(def page-route
  "")

(def validate-request-route
  "/request-validation")

(def create-request-route
  "/requests")

(def request-page-route
  "/requests/:request-id")

(def route-specs
  [{:id page-id
    :method :get
    :route page-route}

   {:id validate-request-id
    :method :post
    :route validate-request-route}

   {:id create-request-id
    :method :post
    :route create-request-route}

   {:id request-page-id
    :method :get
    :route request-page-route}])

(def route-spec-by-id
  (into
   {}
   (map
    (juxt :id identity))
   route-specs))

(defn route-spec
  [route-id]
  (or
   (get
    route-spec-by-id
    route-id)
   (throw
    (ex-info
     "Unknown HumanHelp site route id."
     {:route-id route-id
      :known-route-ids
      (set
       (keys
        route-spec-by-id))}))))

(defn- handler-for!
  [handlers {:keys [id method route] :as spec}]
  (or
   (get
    handlers
    id)
   (throw
    (ex-info
     "Missing HumanHelp site route handler."
     {:route-id id
      :method method
      :route route
      :spec spec
      :handler-ids
      (set
       (keys
        handlers))}))))

(defn- route-entry
  [handlers {:keys [method route] :as spec}]
  [route
   {method
    (handler-for!
     handlers
     spec)}])

(defn route-table
  ([handlers]
   (route-table
    handlers
    nil))
  ([handlers {:keys [middleware]}]
   [(into
     [base-path
      (cond-> {}
        (seq middleware)
        (assoc
         :middleware
         middleware))]
     (map
      (partial route-entry handlers)
      route-specs))]))

(defn- encode
  [value]
  (URLEncoder/encode
   (str value)
   (.name
    StandardCharsets/UTF_8)))

(defn- present?
  [value]
  (and
   (some?
    value)
   (not
    (str/blank?
     (str value)))))

(defn query-string
  [params]
  (let [pairs
        (for
         [[key value]
          params
          :when
          (present?
           value)]
          [(name key)
           value])]
    (when
     (seq pairs)
      (str
       "?"
       (str/join
        "&"
        (for
         [[key value]
          pairs]
          (str
           (encode key)
           "="
           (encode value))))))))

(defn with-query
  [url params]
  (str
   url
   (or
    (query-string
     params)
    "")))

(defn page-url
  ([]
   base-path)
  ([{:keys [location-id]}]
   (with-query
     base-path
     {location-id-param
      location-id})))

(defn location-selected-url
  [location-id]
  (page-url
   {:location-id
    location-id}))

(defn create-request-validation-url
  []
  (str
   base-path
   validate-request-route))

(defn create-request-url
  []
  (str
   base-path
   create-request-route))

(defn request-url
  [request-id]
  (str
   base-path
   "/requests/"
   (encode
    request-id)))
