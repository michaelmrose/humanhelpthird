(ns net.humanhelp.home
  (:require
   [gesso.core :as g]
   [net.humanhelp.components.phone-auth.core :as phone-auth]
   [net.humanhelp.components.phone-auth.sms :as phone-auth.sms]
   [net.humanhelp.middleware :as mid]
   [net.humanhelp.ui :as ui]))

(def phone-auth-id "phone-auth")
(def phone-auth-send-action "/auth/phone/send-code")
(def phone-auth-verify-action "/auth/phone/verify-code")
(def phone-auth-change-href "/")

(defn- request-param
  [params k]
  (or (get params k)
      (get params (name k))
      (get params (keyword k))))

(defn- submitted-phone
  [params]
  (request-param params :phone))

(defn- brand-mark
  []
  [:span {:aria-hidden "true"
          :class "font-heading weight-bold-theme leading-none"
          :style {:display "inline-grid"
                  :place-items "center"
                  :inline-size "2.25rem"
                  :block-size "2.25rem"
                  :border-radius "var(--radius-md)"
                  :background "var(--primary)"
                  :color "var(--primary-foreground)"
                  :font-size "var(--text-sm)"}}
   "HH"])

(defn- palette-icon
  []
  [:svg {:aria-hidden "true"
         :viewBox "0 0 24 24"
         :fill "none"
         :stroke "currentColor"
         :stroke-width "2"
         :stroke-linecap "round"
         :stroke-linejoin "round"
         :style {:inline-size "1.35rem"
                 :block-size "1.35rem"}}
   [:circle {:cx "13.5" :cy "6.5" :r ".5" :fill "currentColor"}]
   [:circle {:cx "17.5" :cy "10.5" :r ".5" :fill "currentColor"}]
   [:circle {:cx "8.5" :cy "7.5" :r ".5" :fill "currentColor"}]
   [:circle {:cx "6.5" :cy "12.5" :r ".5" :fill "currentColor"}]
   [:path {:d "M12 22C6.477 22 2 17.971 2 13c0-4.971 4.477-9 10-9s10 3.582 10 8c0 2.21-1.79 4-4 4h-1.5a2.5 2.5 0 0 0-2.5 2.5c0 .83-.67 1.5-1.5 1.5H12Z"}]])

(defn- user-icon
  []
  [:svg {:aria-hidden "true"
         :viewBox "0 0 24 24"
         :fill "none"
         :stroke "currentColor"
         :stroke-width "2"
         :stroke-linecap "round"
         :stroke-linejoin "round"
         :style {:inline-size "1.55rem"
                 :block-size "1.55rem"}}
   [:circle {:cx "12" :cy "8" :r "4"}]
   [:path {:d "M4 21a8 8 0 0 1 16 0"}]])

(defn- icon-control-attrs
  [label]
  {:aria-label label
   :class "radius-md border-theme"
   :style {:display "inline-grid"
           :place-items "center"
           :inline-size "3rem"
           :block-size "3rem"
           :background "var(--card)"
           :color "var(--foreground)"
           :text-decoration "none"}})

(defn- app-bar
  []
  [:header {:class "border-b bg-card text-card-foreground"}
   [:div {:class "mx-auto flex w-full max-w-5xl items-center justify-between px-4 py-4"}
    [:a {:href "/"
         :class "cluster-theme gap-inline"
         :style {:align-items "center"
                 :color "inherit"
                 :text-decoration "none"}}
     (brand-mark)
     [:span {:class "font-heading text-2xl-theme weight-semibold-theme leading-tight"}
      "HumanHelp"]]

    [:div {:class "cluster-theme gap-inline"
           :style {:align-items "center"}}
     [:button (assoc (icon-control-attrs "Theme")
                     :type "button")
      (palette-icon)]

     [:a (assoc (icon-control-attrs "Sign in")
                :href "/signin")
      (user-icon)]]]])

(defn- auth-shell
  [ctx title & body]
  (ui/page
   ctx
   [:div {:class "min-h-screen flex flex-col bg-background text-foreground"}
    (app-bar)

    [:main {:class "flex-grow"}
     [:section {:class "mx-auto w-full max-w-lg px-4 py-12"}
      (g/card
       {:class "shadow-lg"
        :title title
        :content body})]]]))

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
      (g/html-response
       (phone-auth-code-panel
        ctx
        {:phone (:phone result)
         :length (:length result 6)}))

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
      (g/html-response
       (phone-auth/success-panel
        {:id phone-auth-id
         :title "Phone verified"
         :body (str "Mock phone auth succeeded for "
                    (:phone-display result (:phone result))
                    ". Next we can wire this to real sign-in/session creation.")}))

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
