(ns net.humanhelp.site.app
  (:require
   [net.humanhelp.middleware :as mid]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.routes :as routes]
   [net.humanhelp.site.views.location-selection :as location-selection]
   [net.humanhelp.site.views.new-request :as new-request])
  (:import
   [java.util UUID]))

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
  (scalar-param-value
   (or
    (get-in ctx [:params key])
    (get-in ctx [:params (name key)])
    (get-in ctx [:query-params key])
    (get-in ctx [:query-params (name key)])
    (get-in ctx [:form-params key])
    (get-in ctx [:form-params (name key)])
    (get-in ctx [:path-params key])
    (get-in ctx [:path-params (name key)])
    (get-in ctx [:reitit.core/match :path-params key])
    (get-in ctx [:reitit.core/match :path-params (name key)]))))

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

(defn- location-by-id
  [location-id]
  (when location-id
    (some
     (fn [location]
       (when
        (=
         location-id
         (:location/id location))
         location))
     location-selection/mock-locations)))

(defn app-page
  [ctx]
  (let [user
        (current-user ctx)

        location-id
        (selected-location-id ctx)

        location
        (location-by-id location-id)]
    (if location
      (new-request/page
       ctx
       {:user user
        :location location
        :values {}
        :errors {}})

      (location-selection/page
       ctx
       {:user user
        :selected-location-id
        location-selection/default-location-id}))))

(defn create-request!
  [_ctx]
  {:status 501
   :headers
   {"content-type"
    "text/plain; charset=utf-8"}
   :body
   "Request creation is wired, but persisted Location selection is not connected yet."})

(def handlers
  {routes/page-id
   app-page

   routes/create-request-id
   create-request!})

(def resolvers
  (vec
   (concat
    user/resolvers
    request/resolvers)))

(def module
  {:routes
   (routes/route-table
    handlers
    {:middleware
     [mid/wrap-signed-in]})

   :biff.graph/resolvers
   resolvers

   :biff.fx/handlers
   model.fx/handlers})
