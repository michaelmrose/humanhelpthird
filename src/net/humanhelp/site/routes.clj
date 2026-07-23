(ns net.humanhelp.site.routes
  "Route facts and URL builders for the real HumanHelp site app.

   The ordinary customer flow lives at one stable /app entrypoint:

     authenticate -> choose location -> describe request -> wait for help

   Location selection is flow state carried by the same /app URL rather than a
   separate route. QR codes therefore enter the exact same flow by supplying
   location-id, and later other preselected values, as query parameters.

   routes.clj owns route facts and URL construction.
   site.app owns handlers and middleware."
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

;; -----------------------------------------------------------------------------
;; Flow parameter names
;; -----------------------------------------------------------------------------

(def location-id-param
  "location-id")

;; -----------------------------------------------------------------------------
;; Route ids
;; -----------------------------------------------------------------------------

(def page-id
  :site/new-request-page)

;; -----------------------------------------------------------------------------
;; Relative routes
;; -----------------------------------------------------------------------------

(def page-route
  "")

;; -----------------------------------------------------------------------------
;; Route specs
;; -----------------------------------------------------------------------------

(def route-specs
  [{:id page-id
    :method :get
    :route page-route}])

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
     {:route-id
      route-id

      :known-route-ids
      (set
       (keys
        route-spec-by-id))}))))

;; -----------------------------------------------------------------------------
;; Handler binding
;; -----------------------------------------------------------------------------

(defn- handler-for!
  [handlers {:keys [id method route] :as spec}]
  (or
   (get
    handlers
    id)
   (throw
    (ex-info
     "Missing HumanHelp site route handler."
     {:route-id
      id

      :method
      method

      :route
      route

      :spec
      spec

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
  "Return the Reitit route table for the real HumanHelp site app.

   handlers maps route id to handler function. Middleware belongs to site.app
   and is attached to the /app route group here."
  ([handlers]
   (route-table
    handlers
    nil))
  ([handlers {:keys [middleware]}]
   [[base-path
     (cond-> {}
       (seq middleware)
       (assoc
        :middleware
        middleware))

     (route-entry
      handlers
      (route-spec
       page-id))]]))

;; -----------------------------------------------------------------------------
;; URL encoding
;; -----------------------------------------------------------------------------

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
  "Build a query string from non-nil, non-blank values."
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

;; -----------------------------------------------------------------------------
;; New-request flow URLs
;; -----------------------------------------------------------------------------

(defn page-url
  "Return the ordinary HumanHelp app entrypoint.

   Supplying location-id represents the same state produced by the interactive
   Location-selection step. QR URLs can therefore call this same function."
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
