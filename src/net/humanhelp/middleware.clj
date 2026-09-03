(ns net.humanhelp.middleware
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [com.biffweb.ring :as biff.ring]
   [muuntaja.middleware :as muuntaja]
   [ring.middleware.anti-forgery :as csrf]
   [ring.middleware.defaults :as rd]
   [rum.core :as rum]))

(defn valid-session-user-id?
  "Return true only for session user ids that can represent HumanHelp identity.

   Authentication middleware owns only the signed-in/session boundary; it does
   not choose among application identity sources. UUIDs and nonblank strings
   are accepted because the production site persists UUID User ids while the
   removable example and focused tests may use string ids. Malformed present
   values must not count as authentication."
  [value]
  (or
   (uuid? value)
   (and
    (string? value)
    (not
     (str/blank? value)))))

(defn signed-in?
  [ctx]
  (valid-session-user-id?
   (get-in ctx [:session :uid])))

(defn wrap-redirect-signed-in [handler]
  (fn [ctx]
    (if (signed-in? ctx)
      {:status  303
       :headers {"location" "/app"}}
      (handler ctx))))

(defn wrap-signed-in [handler]
  (fn [ctx]
    (if (signed-in? ctx)
      (handler ctx)
      {:status  303
       :headers {"location" "/signin?error=not-signed-in"}})))

;; Stick this function somewhere in your middleware stack below if you want to
;; inspect what things look like before/after certain middleware fns run.
(defn wrap-debug [handler]
  (fn [ctx]
    (let [response (handler ctx)]
      (println "REQUEST")
      (pprint/pprint ctx)
      (def ctx* ctx)
      (println "RESPONSE")
      (pprint/pprint response)
      (def response* response)
      response)))

;; -----------------------------------------------------------------------------
;; Rum response compatibility
;; -----------------------------------------------------------------------------

(defn wrap-render-rum
  "Preserve the Biff 1 route contract for handlers that return Rum/Hiccup vectors.

   Biff 2 renders Hiccup automatically for biff.ring/defroute handlers, but
   HumanHelp still uses ordinary Reitit handlers. Keep this boundary local until
   those routes are intentionally migrated."
  [handler]
  (fn [ctx]
    (let [response
          (handler ctx)]
      (if
       (vector? response)
        {:status
         200

         :headers
         {"content-type"
          "text/html"}

         :body
         (str
          "<!DOCTYPE html>\n"
          (rum/render-static-markup
           response))}
        response))))

(defn wrap-site-defaults [handler]
  (-> handler
      wrap-render-rum
      biff.ring/wrap-anti-forgery-websockets
      csrf/wrap-anti-forgery
      biff.ring/wrap-session
      muuntaja/wrap-params
      muuntaja/wrap-format
      (rd/wrap-defaults (-> rd/site-defaults
                            (assoc-in [:security :anti-forgery] false)
                            (assoc-in [:responses :absolute-redirects] true)
                            (assoc :session false)
                            (assoc :static false)))))

(defn wrap-base-defaults [handler]
  (-> handler
      biff.ring/wrap-https-scheme
      biff.ring/wrap-resource
      biff.ring/wrap-internal-error
      biff.ring/wrap-ssl
      biff.ring/wrap-log-requests))
