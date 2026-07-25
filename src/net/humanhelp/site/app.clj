(ns net.humanhelp.site.app
  "HTTP boundary for the signed-in HumanHelp customer Get Help flow.

   This namespace owns request parameter interpretation, current-user loading,
   temporary development fixture setup, route handlers, and composition of the
   public model modules required by the flow.

   Views receive already-resolved values and perform no persistence work."
  (:require
   [gesso.core :as g]
   [net.humanhelp.middleware :as mid]
   [net.humanhelp.site.mock-data :as mock-data]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.routes :as routes]
   [net.humanhelp.site.views.get-help.await-help :as await-help]
   [net.humanhelp.site.views.get-help.new-request :as new-request]
   [net.humanhelp.site.views.get-help.request-finished :as request-finished]
   [net.humanhelp.site.views.get-help.select-location :as select-location])
  (:import
   [java.util UUID]))

;; =============================================================================
;; Request parameters
;; =============================================================================

(defn- scalar-param-value
  [value]
  (cond
    (nil? value)
    nil

    (and
     (sequential? value)
     (not
      (map? value)))
    (last value)

    :else
    value))

(defn- param
  [ctx key]
  (let [string-key
        (name key)

        keyword-key
        (keyword string-key)

        value-at
        (fn [source]
          (or
           (get source key)
           (get source string-key)
           (get source keyword-key)))]

    (scalar-param-value
     (or
      (value-at (:params ctx))
      (value-at (:query-params ctx))
      (value-at (:form-params ctx))
      (value-at (:path-params ctx))
      (value-at
       (get-in
        ctx
        [:reitit.core/match
         :path-params]))))))

(defn- ->uuid
  [value]
  (cond
    (uuid? value)
    value

    (string? value)
    (try
      (UUID/fromString value)
      (catch IllegalArgumentException _
        nil))

    :else
    nil))

(defn- selected-location-id
  [ctx]
  (->uuid
   (param
    ctx
    routes/location-id-param)))

(defn- selected-request-id
  [ctx]
  (->uuid
   (param
    ctx
    routes/request-id-param)))

(defn- request-values
  [ctx]
  {:title
   (or
    (param ctx :title)
    "")

   :details
   (or
    (param ctx :details)
    "")

   :location-detail
   (or
    (param ctx :location-detail)
    "")})

;; =============================================================================
;; HTTP responses
;; =============================================================================

(defn- redirect
  [url]
  {:status 303
   :headers
   {"location" url}
   :body ""})

;; =============================================================================
;; Current User
;; =============================================================================

(defn- session-user-id
  [ctx]
  (->uuid
   (or
    (:current-user/id ctx)
    (:user/id ctx)
    (get-in ctx [:user :xt/id])
    (get-in ctx [:session :uid])
    (get-in ctx [:session :user]))))

(defn- current-user
  [ctx]
  (let [user-id
        (or
         (session-user-id ctx)
         (throw
          (ex-info
           "Signed-in HumanHelp request did not contain a UUID User id."
           {:error/type
            :site.app/missing-user-id})))]
    (user/require-user
     ctx
     {:user-id user-id})))

;; =============================================================================
;; Temporary development Location source
;; =============================================================================

(defn- mock-location
  [location-id]
  (mock-data/location-by-id
   location-id))

(defn- request-location-name
  [request-document]
  (some->
   (request/location-id
    request-document)
   mock-location
   :location/name))

;; =============================================================================
;; Get Help entrypoint
;; =============================================================================

(defn app-page
  [ctx]
  (let [user-document
        (current-user ctx)]

    ;; The development fixture is persisted, not merely represented by UI
    ;; values. ensure! creates only missing documents and verifies existing
    ;; fixed fixture IDs rather than rewriting them.
    (mock-data/ensure!
     ctx)

    (let [location-id
          (selected-location-id
           ctx)

          location
          (mock-location
           location-id)]

      (if
       location
        (new-request/page
         ctx
         {:user
          user-document

          :location
          location

          :values
          {}

          :errors
          {}})

        (select-location/page
         ctx
         {:user
          user-document

          :locations
          mock-data/locations

          :selected-location-id
          mock-data/default-location-id})))))

;; =============================================================================
;; Request validation
;; =============================================================================

(defn validate-request
  [ctx]
  (g/html-response
   (g/render-oob-error-map
    (request/content-errors
     (request-values
      ctx)))))

;; =============================================================================
;; Request creation
;; =============================================================================

(defn create-request
  [ctx]
  (let [user-document
        (current-user ctx)]

    ;; Keep direct POSTs correct even when the user did not first render /app.
    (mock-data/ensure!
     ctx)

    (let [user-id
          (:xt/id user-document)

          location-id
          (selected-location-id
           ctx)

          location
          (mock-location
           location-id)

          values
          (request-values
           ctx)

          errors
          (request/content-errors
           values)]

      (cond
        ;; A Location id submitted by the browser must be one of the locations
        ;; exposed by this temporary development source.
        (nil?
         location)
        (redirect
         routes/base-path)

        ;; Authoritative Request-domain validation remains the final server
        ;; check. The Gesso field validation endpoint is only the UX layer.
        (seq
         errors)
        (new-request/page
         ctx
         {:user
          user-document

          :location
          location

          :values
          values

          :errors
          errors})

        :else
        (let [result
              (request/create-request
               (assoc
                ctx
                :current-user/id
                user-id)
               {:organization-id
                mock-data/organization-id

                :location-id
                location-id

                :content
                values})

              request-document
              (:request result)]

          (redirect
           (routes/request-url
            (request/request-id
             request-document))))))))

;; =============================================================================
;; Existing Request page
;; =============================================================================

(defn request-page
  [ctx]
  (let [user-document
        (current-user ctx)

        user-id
        (:xt/id user-document)

        request-id
        (selected-request-id
         ctx)

        request-document
        (when
         request-id
          (request/request-document
           ctx
           request-id))]

    (if-not
     (and
      request-document
      (request/requested-by-user?
       request-document
       user-id))

      (redirect
       routes/base-path)

      (let [page-props
            {:user
             user-document

             :request-document
             request-document

             :location-name
             (request-location-name
              request-document)}]

        (if
         (request/terminal?
          request-document)

          (request-finished/page
           ctx
           page-props)

          (await-help/page
           ctx
           page-props))))))

;; =============================================================================
;; Routes
;; =============================================================================

(def handlers
  {routes/page-id
   app-page

   routes/validate-request-id
   validate-request

   routes/create-request-id
   create-request

   routes/request-page-id
   request-page})

;; =============================================================================
;; Site module
;; =============================================================================

(def resolvers
  "Graph resolvers required by the current customer Get Help vertical slice."
  (vec
   (concat
    (:biff.graph/resolvers
     user/module)

    (:biff.graph/resolvers
     organization/module)

    (:biff.graph/resolvers
     request/module))))

(def module
  {:routes
   (routes/route-table
    handlers
    {:middleware
     [mid/wrap-signed-in]})

   ;; net.humanhelp.schema already contributes User and Request schemas.
   ;; Organization was not part of that existing application registry, so this
   ;; flow contributes only the Organization schema it newly requires. This
   ;; avoids changing the application-wide schema namespace and avoids
   ;; duplicate User/Request schema registrations.
   :schema
   (:schema
    organization/module)

   :biff.graph/resolvers
   resolvers

   :biff.fx/handlers
   (:biff.fx/handlers
    model.fx/module)})
