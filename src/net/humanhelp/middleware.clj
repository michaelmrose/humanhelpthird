(ns net.humanhelp.middleware
  (:require
   [clojure.string :as str]
   [com.biffweb :as biff]
   [muuntaja.middleware :as muuntaja]
   [ring.middleware.anti-forgery :as csrf]
   [ring.middleware.defaults :as rd]))

(defn wrap-redirect-signed-in [handler]
  (fn [{:keys [session] :as ctx}]
    (if (some? (:uid session))
      {:status 303
       :headers {"location" "/app"}}
      (handler ctx))))

(defn wrap-signed-in [handler]
  (fn [{:keys [session] :as ctx}]
    (if (some? (:uid session))
      (handler ctx)
      {:status 303
       :headers {"location" "/signin?error=not-signed-in"}})))

;; -----------------------------------------------------------------------------
;; Dev/load-test token gate
;; -----------------------------------------------------------------------------

(def dev-load-prefix
  "/api/microblog/dev/load")

(defn dev-load-request?
  [ctx]
  (str/starts-with? (or (:uri ctx) "") dev-load-prefix))

(defn configured-dev-load-token
  [ctx]
  (or (:net.humanhelp.load/dev-token ctx)
      (:dev/load-token ctx)
      (System/getenv "GESSOTEST_LOAD_TOKEN")))

(defn bearer-token
  [ctx]
  (some-> (get-in ctx [:headers "authorization"])
          (str/replace #"(?i)^Bearer\s+" "")))

(defn forbidden
  [message]
  {:status 403
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body message})

(defn wrap-dev-load-token
  "Protect only /api/microblog/dev/load... routes.

   The token authenticates the load tool, not the simulated user. Dev/load
   handlers should still read user identity from params or headers such as:

     user-id
     X-Load-User
     mode
     X-Load-Mode"
  [handler]
  (fn [ctx]
    (if-not (dev-load-request? ctx)
      (handler ctx)
      (let [expected (configured-dev-load-token ctx)
            actual   (bearer-token ctx)]
        (cond
          (str/blank? expected)
          (forbidden "GESSOTEST_LOAD_TOKEN is not configured.")

          (= expected actual)
          (handler (assoc ctx :net.humanhelp.load/authorized? true))

          :else
          (forbidden "Invalid load-test token."))))))

;; Stick this function somewhere in your middleware stack below if you want to
;; inspect what things look like before/after certain middleware fns run.
(defn wrap-debug [handler]
  (fn [ctx]
    (let [response (handler ctx)]
      (println "REQUEST")
      (biff/pprint ctx)
      (def ctx* ctx)
      (println "RESPONSE")
      (biff/pprint response)
      (def response* response)
      response)))

(defn wrap-site-defaults [handler]
  (-> handler
      biff/wrap-render-rum
      biff/wrap-anti-forgery-websockets
      csrf/wrap-anti-forgery
      biff/wrap-session
      muuntaja/wrap-params
      muuntaja/wrap-format
      (rd/wrap-defaults (-> rd/site-defaults
                            (assoc-in [:security :anti-forgery] false)
                            (assoc-in [:responses :absolute-redirects] true)
                            (assoc :session false)
                            (assoc :static false)))))

(defn wrap-api-defaults [handler]
  (-> handler
      muuntaja/wrap-params
      muuntaja/wrap-format
      (rd/wrap-defaults rd/api-defaults)))

(defn wrap-base-defaults [handler]
  (-> handler
      biff/wrap-https-scheme
      biff/wrap-resource
      biff/wrap-internal-error
      biff/wrap-ssl
      biff/wrap-log-requests))
