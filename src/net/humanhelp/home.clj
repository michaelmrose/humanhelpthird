(ns net.humanhelp.home
  (:require
   [gesso.core :as g]
   [net.humanhelp.auth.phone :as auth.phone]
   [net.humanhelp.components.phone-auth.core :as phone-auth]
   [net.humanhelp.components.phone-auth.sms :as phone-auth.sms]
   [net.humanhelp.middleware :as mid]
   [net.humanhelp.ui :as ui]))

(def phone-auth-id "phone-auth")
(def phone-auth-send-action "/auth/phone/send-code")
(def phone-auth-verify-action "/auth/phone/verify-code")
(def phone-auth-change-href "/")
(def app-path "/app")

(defn- request-param
  [params k]
  (or (get params k)
      (get params (name k))
      (get params (keyword k))))

(defn- request-header
  [ctx k]
  (let [headers (:headers ctx)]
    (or (get headers k)
        (get headers (name k))
        (get headers (clojure.string/lower-case (name k))))))

(defn- htmx-request?
  [ctx]
  (= "true" (request-header ctx :hx-request)))

(defn- submitted-phone
  [params]
  (request-param params :phone))

(defn- auth-shell
  [ctx title & body]
  (ui/page-shell
   ctx
   {:user (:user ctx)
    :brand (ui/brand {:href "/"})
    :main-class "flex-grow py-10"}
   [:section {:class "mx-auto w-full max-w-lg px-4"}
    (g/card
     {:class "shadow-lg"
      :title title
      :content body})]))

(defn- phone-auth-phone-panel
  ([ctx]
   (phone-auth-phone-panel ctx {}))
  ([ctx opts]
   (phone-auth/phone-panel
    (merge
     {:ctx ctx
      :id phone-auth-id
      :title nil
      :body "Enter your phone number to continue."
      :phone-label "Phone number"
      :phone-help "Enter a 10-digit US mobile number."
      :submit-text "Continue"
      :send-action phone-auth-send-action}
     opts))))

(defn- phone-auth-code-panel
  [ctx opts]
  (phone-auth/code-panel
   (merge
    {:ctx ctx
     :id phone-auth-id
     :title "Enter your code"
     :submit-text "Continue"
     :verify-action phone-auth-verify-action
     :resend-action phone-auth-send-action
     :change-href phone-auth-change-href
     :change-text "Use a different phone number"}
    opts)))

(defn- code-sent-toast
  [{:keys [phone phone-display]}]
  (g/render-toast-oob
   {:variant :info
    :duration 4000
    :title "Code sent"
    :description (str "We sent a verification code to "
                      (or phone-display phone "your phone")
                      ".")}))

(defn- code-sent-response
  [ctx result]
  (g/html-response
   [:<>
    (phone-auth-code-panel
     ctx
     {:phone (:phone result)
      :length (:length result 6)})
    (code-sent-toast result)]))

(defn- signed-in-response
  [ctx {:keys [user-id]}]
  (let [session' (assoc (or (:session ctx) {}) :uid user-id)]
    (if (htmx-request? ctx)
      {:status 200
       :headers {"HX-Redirect" app-path}
       :session session'
       :body ""}

      {:status 303
       :headers {"location" app-path}
       :session session'})))

(defn phone-entry-page
  [ctx]
  (auth-shell
   ctx
   "Welcome to HumanHelp"
   (phone-auth-phone-panel ctx)))

(defn home-page
  [ctx]
  (phone-entry-page ctx))

(defn signin-page
  [ctx]
  (phone-entry-page ctx))

(defn send-phone-code
  [{:keys [params] :as ctx}]
  (let [phone  (submitted-phone params)
        result (phone-auth.sms/start-verification!
                {:phone phone
                 :length 6})]
    (if (:ok? result)
      (code-sent-response ctx result)

      (g/html-response
       (phone-auth-phone-panel
        ctx
        {:phone phone
         :phone-error (:error result "Could not send a code.")})))))

(defn verify-phone-code
  [{:keys [params] :as ctx}]
  (let [phone  (submitted-phone params)
        code   (request-param params :code)
        result (phone-auth.sms/check-verification!
                {:phone phone
                 :code code})]
    (if (:ok? result)
      (let [signin-result (auth.phone/complete-phone-signin!
                           ctx
                           {:phone (:phone result phone)})]
        (if (:ok? signin-result)
          (signed-in-response ctx signin-result)

          (g/html-response
           (phone-auth-code-panel
            ctx
            {:phone (:phone result phone)
             :length 6
             :code-error (:error signin-result
                                 "Could not finish signing in. Try again.")}))))

      (g/html-response
       (phone-auth-code-panel
        ctx
        {:phone (:phone result phone)
         :length 6
         :code-error (:error result "That code didn’t match.")})))))

(def module
  {:routes [["" {:middleware [mid/wrap-redirect-signed-in]}
             ["/" {:get home-page}]
             ["/signin" {:get signin-page}]
             ["/auth/phone/send-code" {:post send-phone-code}]
             ["/auth/phone/verify-code" {:post verify-phone-code}]]]})
