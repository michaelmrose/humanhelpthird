(ns net.humanhelp.home
  (:require
   [com.biffweb :as biff]
   [gesso.core :as g]
   [net.humanhelp.middleware :as mid]
   [net.humanhelp.settings :as settings]
   [net.humanhelp.ui :as ui]))

(defn- email-disabled-notice
  []
  (g/alert
   {:content
    (str "Until you add API keys for MailerSend and reCAPTCHA, we'll print your "
         "sign-up link to the console. See config.edn.")}))

(defn- auth-shell
  [ctx title & body]
  (ui/page
   ctx
   [:section {:class "mx-auto w-full max-w-lg"}
    (g/card
     {:class "shadow-lg"
      :title title
      :content body})]))

(defn- auth-copy
  [& children]
  (into
   [:p {:class "font-body text-sm-theme leading-body"}]
   children))

(defn- auth-link
  [{:keys [href]} & children]
  (into
   [:a {:href href
        :class "link font-body weight-medium-theme"}]
   children))

(defn- email-error
  [error]
  (case error
    "recaptcha"
    (str "You failed the recaptcha test. Try again, "
         "and make sure you aren't blocking scripts from Google.")

    "invalid-email"
    "Invalid email. Try again with a different address."

    "send-failed"
    (str "We weren't able to send an email to that address. "
         "If the problem persists, try another address.")

    "invalid-link"
    "Invalid or expired link. Sign in to get a new link."

    "not-signed-in"
    "You must be signed in to view that page."

    "There was an error."))

(defn- verify-error
  [error]
  (case error
    "incorrect-email" "Incorrect email address. Try again."
    "There was an error."))

(defn- code-error
  [error]
  (case error
    "invalid-code" "Invalid code."
    "There was an error."))

(defn- email-input
  ([]
   (email-input {}))
  ([attrs]
   (g/input
    (merge
     {:id "email"
      :name "email"
      :type "email"
      :autocomplete "email"
      :placeholder "Enter your email address"
      :class "w-full"}
     attrs))))

(defn- code-input
  []
  (g/input
   {:id "code"
    :name "code"
    :type "text"
    :inputmode "numeric"
    :autocomplete "one-time-code"
    :placeholder "Enter code"
    :class "w-full"}))

(defn- submit-button
  [{:keys [text site-key callback]}]
  (g/button
   {:text text
    :class (cond-> "w-full"
             site-key (str " g-recaptcha"))
    :attrs (merge
            {:type "submit"}
            (when site-key
              {:data-sitekey site-key
               :data-callback callback}))}))

(defn- recaptcha-footer
  []
  [:div {:class "content-stack-theme gap-field"}
   biff/recaptcha-disclosure
   (email-disabled-notice)])

(defn home-page
  [{:keys [recaptcha/site-key params] :as ctx}]
  (auth-shell
   (assoc ctx ::ui/recaptcha true)
   (str "Sign up for " settings/app-name)

   (biff/form
    {:action "/auth/send-link"
     :id "signup"
     :hidden {:on-error "/"}}

    (biff/recaptcha-callback "submitSignup" "signup")

    [:div {:class "form-theme"}
     (g/field
      {:for "email"
       :label-text "Email address"
       :description "We'll send you a sign-up link."
       :error (some-> (:error params) email-error)
       :control (email-input)})

     (submit-button
      {:text "Sign up"
       :site-key site-key
       :callback "submitSignup"})

     (auth-copy
      "Already have an account? "
      (auth-link {:href "/signin"} "Sign in")
      ".")

     (recaptcha-footer)])))

(defn link-sent
  [{:keys [params] :as ctx}]
  (auth-shell
   ctx
   "Check your inbox"

   (auth-copy
    "We've sent a sign-in link to "
    [:span {:class "weight-semibold-theme"} (:email params)]
    ".")))

(defn verify-email-page
  [{:keys [params] :as ctx}]
  (auth-shell
   ctx
   (str "Sign up for " settings/app-name)

   (biff/form
    {:action "/auth/verify-link"
     :hidden {:token (:token params)}}

    [:div {:class "form-theme"}
     (g/field
      {:for "email"
       :label-text "Confirm your email"
       :description (str "It looks like you opened this link on a different device "
                         "or browser than the one you signed up on.")
       :error (some-> (:error params) verify-error)
       :control (email-input {:autocomplete "email"})})

     (g/button
      {:text "Sign in"
       :class "w-full"
       :attrs {:type "submit"}})])))

(defn signin-page
  [{:keys [recaptcha/site-key params] :as ctx}]
  (auth-shell
   (assoc ctx ::ui/recaptcha true)
   (str "Sign in to " settings/app-name)

   (biff/form
    {:action "/auth/send-code"
     :id "signin"
     :hidden {:on-error "/signin"}}

    (biff/recaptcha-callback "submitSignin" "signin")

    [:div {:class "form-theme"}
     (g/field
      {:for "email"
       :label-text "Email address"
       :description "We'll send you a sign-in code."
       :error (some-> (:error params) email-error)
       :control (email-input)})

     (submit-button
      {:text "Sign in"
       :site-key site-key
       :callback "submitSignin"})

     (auth-copy
      "Don't have an account yet? "
      (auth-link {:href "/"} "Sign up")
      ".")

     (recaptcha-footer)])))

(defn enter-code-page
  [{:keys [recaptcha/site-key params] :as ctx}]
  (auth-shell
   (assoc ctx ::ui/recaptcha true)
   "Enter your sign-in code"

   (biff/form
    {:action "/auth/verify-code"
     :id "code-form"
     :hidden {:email (:email params)}}

    (biff/recaptcha-callback "submitCode" "code-form")

    [:div {:class "form-theme"}
     (g/field
      {:for "code"
       :label-text "Verification code"
       :description [:<>
                     "Enter the 6-digit code sent to "
                     [:span {:class "weight-semibold-theme"} (:email params)]
                     "."]
       :error (some-> (:error params) code-error)
       :control (code-input)})

     (submit-button
      {:text "Sign in"
       :site-key site-key
       :callback "submitCode"})])

   (biff/form
    {:action "/auth/send-code"
     :id "signin"
     :hidden {:email (:email params)
              :on-error "/signin"}}

    (biff/recaptcha-callback "submitSignin" "signin")

    (g/button
     {:variant :link
      :text "Send another code"
      :class (when site-key "g-recaptcha")
      :attrs (merge
              {:type "submit"}
              (when site-key
                {:data-sitekey site-key
                 :data-callback "submitSignin"}))}))))

(def module
  {:routes [["" {:middleware [mid/wrap-redirect-signed-in]}
             ["/"                  {:get home-page}]]
            ["/link-sent"          {:get link-sent}]
            ["/verify-link"        {:get verify-email-page}]
            ["/signin"             {:get signin-page}]
            ["/verify-code"        {:get enter-code-page}]]})
