(ns net.humanhelp.components.phone-auth.core
  (:require
   [clojure.string :as str]
   [gesso.core :as g]
   [net.humanhelp.components.one-time-code.core :as one-time-code]
   [net.humanhelp.components.phone-auth.attr :as attr]
   [net.humanhelp.components.phone-auth.scripts :as scripts]
   [net.humanhelp.components.phone-auth.sms :as sms]))

(defn- param-name
  [k]
  (cond
    (keyword? k) (name k)
    (string? k)  k
    :else        (str k)))

(defn- hidden-input
  [[k v]]
  (when (some? v)
    [:input {:type  "hidden"
             :name  (param-name k)
             :value (str v)}]))

(defn- hidden-inputs
  [m]
  (->> m
       (keep hidden-input)
       vec))

(defn- anti-forgery-node
  [{:keys [ctx anti-forgery]}]
  (or anti-forgery
      (when ctx
        (g/anti-forgery-input ctx))))

(defn- form-node
  [attrs anti-forgery hidden children]
  (into [:form attrs]
        (remove nil?
                (concat [anti-forgery]
                        (hidden-inputs hidden)
                        children))))

(defn- target-selector
  [id]
  (str "#" id))

(defn- blankish?
  [x]
  (str/blank? (str (or x ""))))

(defn- phone-hidden-value
  [phone]
  (or (sms/normalize-phone phone)
      ""))

(defn- phone-display-value
  [phone]
  (or (sms/phone-display phone)
      (when-not (blankish? phone)
        (str/trim (str phone)))
      ""))

(defn- phone-display-text
  [phone]
  (or (sms/phone-display phone)
      (when-not (blankish? phone)
        (str/trim (str phone)))
      "your phone"))

(defn- default-code-message
  [phone length]
  (str "We sent a " length "-digit code to " (phone-display-text phone) "."))

(defn phone-panel
  "Render the phone entry step.

  The visible phone input is display-only and has no name. The hidden phone
  input is the submitted value and contains only 10 numeric digits.

  This component owns reusable UI only. The app still owns routes, user lookup,
  sessions, redirects, and persistence."
  [{:keys [id
           class
           attrs
           ctx
           anti-forgery

           title
           body

           phone
           phone-display-id
           phone-hidden-id
           phone-name
           phone-label
           phone-placeholder
           phone-help
           phone-error
           phone-required-message
           phone-invalid-message
           phone-display-attrs
           phone-hidden-attrs

           send-action
           send-method
           send-attrs
           hidden

           submit-text]
    :or   {id                     "phone-auth"
           title                  "Sign in with your phone"
           body                   "Enter your phone number and we’ll send you a verification code."
           phone-display-id       "phone-display"
           phone-hidden-id        "phone"
           phone-name             "phone"
           phone-label            "Phone number"
           phone-required-message "Please enter a 10-digit US mobile number."
           phone-invalid-message  "Please enter a 10-digit US mobile number."
           send-action            "/auth/phone/send-code"
           send-method            "post"
           submit-text            "Continue"}}]
  (let [phone-help-id  (str phone-display-id "-help")
        phone-error-id (str phone-display-id "-error")
        anti-forgery   (anti-forgery-node {:ctx          ctx
                                           :anti-forgery anti-forgery})]
    [:div (attr/panel-attrs {:id    id
                             :class class
                             :attrs attrs})
     (scripts/phone-auth-script)

     [:div (attr/copy-attrs {})
      (when title
        [:h1 (attr/title-attrs {}) title])
      (when body
        [:p (attr/body-attrs {}) body])]

     (form-node
      (attr/form-attrs
       {:id     (str id "-send-form")
        :method send-method
        :action send-action
        :attrs  (merge
                 {:hx-post   send-action
                  :hx-target (target-selector id)
                  :hx-swap   "outerHTML show:none focus-scroll:false"}
                 send-attrs)})
      anti-forgery
      hidden
      [[:div {:class "form-theme"}
        (g/field
         {:for         phone-display-id
          :label-text  phone-label
          :description phone-help
          :error       phone-error
          :control
          [:div (attr/field-control-attrs {})
           [:input (attr/phone-display-input-attrs
                    {:id               phone-display-id
                     :value            (phone-display-value phone)
                     :placeholder      phone-placeholder
                     :hidden-id        phone-hidden-id
                     :error-id         phone-error-id
                     :help-id          (when phone-help phone-help-id)
                     :required-message phone-required-message
                     :invalid-message  phone-invalid-message
                     :autofocus?       true
                     :attrs            phone-display-attrs})]

           [:input (attr/phone-hidden-input-attrs
                    {:id    phone-hidden-id
                     :name  phone-name
                     :value (phone-hidden-value phone)
                     :attrs phone-hidden-attrs})]

           (when phone-help
             [:p (attr/help-attrs {:id phone-help-id})
              phone-help])

           [:p (attr/error-attrs {:id      phone-error-id
                                  :hidden? (nil? phone-error)})
            (or phone-error "")]]})

        (g/button
         {:text    submit-text
          :variant :primary
          :attrs   {:type  "submit"
                    :style (attr/submit-style)}})]])]))

(defn code-panel
  "Render the code entry step.

  The phone value passed here should be the canonical 10-digit value whenever
  possible. It is submitted as a hidden field with the verification code."
  [{:keys [id
           class
           attrs
           ctx
           anti-forgery

           title
           phone
           phone-name

           length
           code-id
           code-name
           code-label
           code-help
           code-error

           verify-action
           verify-method
           verify-attrs

           resend-action
           resend-method
           resend-attrs
           resend-text
           resend?

           change-href
           change-text
           change?

           hidden
           submit-text]
    :or   {id            "phone-auth"
           title         "Enter your code"
           phone-name    "phone"
           length        6
           code-id       "code"
           code-name     "code"
           code-label    "Code"
           verify-action "/auth/phone/verify-code"
           verify-method "post"
           resend-action "/auth/phone/send-code"
           resend-method "post"
           resend-text   "Send another code"
           resend?       true
           change-href   "/signin"
           change-text   "Use a different phone number"
           change?       true
           submit-text   "Continue"}}]
  (let [phone'         (phone-hidden-value phone)
        verify-form-id (str id "-verify-form")
        resend-form-id (str id "-resend-form")
        hidden-fields  (cond-> (or hidden {})
                         (not (blankish? phone'))
                         (assoc phone-name phone'))
        anti-forgery   (anti-forgery-node {:ctx          ctx
                                           :anti-forgery anti-forgery})]
    [:div (attr/panel-attrs {:id    id
                             :class class
                             :attrs attrs})
     [:div (attr/copy-attrs {})
      (when title
        [:h1 (attr/title-attrs {}) title])
      [:p (attr/body-attrs {})
       (default-code-message phone' length)]]

     (form-node
      (attr/form-attrs
       {:id     verify-form-id
        :method verify-method
        :action verify-action
        :attrs  (merge
                 {:hx-post   verify-action
                  :hx-target (target-selector id)
                  :hx-swap   "outerHTML show:none focus-scroll:false"}
                 verify-attrs)})
      anti-forgery
      hidden-fields
      [[:div {:class "form-theme"}
        (one-time-code/input
         {:id          code-id
          :name        code-name
          :label       code-label
          :help        code-help
          :error       code-error
          :length      length
          :required?   true
          :autofocus?  true
          :input-class "w-full"})

        (g/button
         {:text    submit-text
          :variant :primary
          :attrs   {:type  "submit"
                    :style (attr/submit-style)}})]])

     [:div (attr/action-row-attrs {})
      (when resend?
        (form-node
         (attr/inline-form-attrs
          {:id     resend-form-id
           :method resend-method
           :action resend-action
           :attrs  (merge
                    {:hx-post   resend-action
                     :hx-target (target-selector id)
                     :hx-swap   "outerHTML show:none focus-scroll:false"}
                    resend-attrs)})
         anti-forgery
         hidden-fields
         [[:button (attr/link-button-attrs {})
           resend-text]]))

      (when (and resend? change?)
        [:span (attr/separator-attrs) "·"])

      (when change?
        [:a (attr/link-attrs {:href change-href})
         change-text])]]))

(defn success-panel
  [{:keys [id class attrs title body]
    :or   {id    "phone-auth"
           title "Phone verified"
           body  "Your phone number has been verified."}}]
  [:div (attr/panel-attrs {:id    id
                           :class class
                           :attrs attrs})
   [:div (attr/copy-attrs {})
    (when title
      [:h1 (attr/title-attrs {}) title])
    (when body
      [:p (attr/body-attrs {}) body])]])
