(ns net.humanhelp.components.phone-auth.core
  (:require
   [clojure.string :as str]
   [gesso.core :as g]
   [net.humanhelp.components.one-time-code.core :as one-time-code]
   [net.humanhelp.components.phone-auth.attr :as attr]
   [net.humanhelp.components.phone-auth.scripts :as scripts]
   [net.humanhelp.components.phone-auth.sms :as sms]))

(def default-phone-schema
  [:map
   [:phone
    [:string
     {:min 12
      :max 12
      :gesso.html/pattern "[0-9]{3}-[0-9]{3}-[0-9]{4}"
      :gesso.error/required "Enter your phone number."
      :gesso.error/min "Enter a 10-digit US mobile number."
      :gesso.error/max "Enter a 10-digit US mobile number."
      :gesso.error/pattern "Enter a 10-digit US mobile number."}]]])

(defn- param-name
  [k]
  (cond
    (keyword? k) (name k)
    (string? k)  k
    :else        (str k)))

(defn- hidden-input
  [[k v]]
  (when (some? v)
    [:input {:type "hidden"
             :name (param-name k)
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

(defn- phone-display-value
  [phone]
  (or (sms/normalize-phone phone)
      (when-not (blankish? phone)
        (str/trim (str phone)))
      ""))

(defn- phone-display
  [phone]
  (let [value (phone-display-value phone)]
    (if (blankish? value)
      "your phone"
      value)))

(defn- default-code-message
  [phone length]
  (str "We sent a " length "-digit code to " (phone-display phone) "."))

(defn phone-panel
  [{:keys [id
           class
           attrs
           ctx
           anti-forgery

           title
           body

           phone
           phone-id
           phone-name
           phone-label
           phone-placeholder
           phone-help
           phone-error
           phone-schema
           phone-field-key
           phone-input-attrs

           send-action
           send-method
           send-attrs
           hidden

           submit-text]
    :or {id "phone-auth"
         title "Sign in with your phone"
         body "Enter your phone number and we’ll send you a verification code."
         phone-id "phone"
         phone-name "phone"
         phone-label "Phone number"
         phone-schema default-phone-schema
         phone-field-key :phone
         send-action "/auth/phone/send-code"
         send-method "post"
         submit-text "Continue"}}]
  (let [form-id         (str id "-send-form")
        phone-error-id  (str phone-id "-error")
        validation-plan (g/field-plan phone-schema phone-field-key phone-error-id)
        anti-forgery    (anti-forgery-node {:ctx ctx
                                            :anti-forgery anti-forgery})]
    [:div (attr/panel-attrs {:id id
                             :class class
                             :attrs attrs})
     [:div (attr/copy-attrs {})
      (when title
        [:h1 (attr/title-attrs {}) title])
      (when body
        [:p (attr/body-attrs {}) body])]

     (form-node
      (attr/form-attrs
       {:id form-id
        :method send-method
        :action send-action
        :attrs (merge
                {:hx-post send-action
                 :hx-target (target-selector id)
                 :hx-swap "outerHTML show:none focus-scroll:false"}
                send-attrs)})
      anti-forgery
      hidden
      [[:div (attr/field-attrs {})
        [:label (attr/label-attrs {:id phone-id})
         phone-label]

        [:input (attr/phone-input-attrs
                 {:id phone-id
                  :name phone-name
                  :value (phone-display-value phone)
                  :placeholder phone-placeholder
                  :autofocus? true
                  :help phone-help
                  :error phone-error
                  :validation-plan validation-plan
                  :format-script (scripts/us-phone-format-script)
                  :attrs phone-input-attrs})]

        (when phone-help
          [:p (attr/help-attrs {:id phone-id}) phone-help])

        [:p (attr/error-attrs {:id phone-id
                               :hidden? (nil? phone-error)})
         (or phone-error "")]]

       (g/button
        {:text submit-text
         :variant :primary
         :attrs {:type "submit"
                 :style (attr/submit-style)}})])]))

(defn code-panel
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
    :or {id "phone-auth"
         title "Enter your code"
         phone-name "phone"
         length 6
         code-id "code"
         code-name "code"
         code-label "Code"
         verify-action "/auth/phone/verify-code"
         verify-method "post"
         resend-action "/auth/phone/send-code"
         resend-method "post"
         resend-text "Send another code"
         resend? true
         change-href "/signin"
         change-text "Use a different phone number"
         change? true
         submit-text "Continue"}}]
  (let [verify-form-id (str id "-verify-form")
        resend-form-id (str id "-resend-form")
        normalized     (sms/normalize-phone phone)
        hidden-fields  (cond-> (or hidden {})
                         normalized
                         (assoc phone-name normalized))
        anti-forgery   (anti-forgery-node {:ctx ctx
                                           :anti-forgery anti-forgery})]
    [:div (attr/panel-attrs {:id id
                             :class class
                             :attrs attrs})
     [:div (attr/copy-attrs {})
      (when title
        [:h1 (attr/title-attrs {}) title])
      [:p (attr/body-attrs {})
       (default-code-message normalized length)]]

     (form-node
      (attr/form-attrs
       {:id verify-form-id
        :method verify-method
        :action verify-action
        :attrs (merge
                {:hx-post verify-action
                 :hx-target (target-selector id)
                 :hx-swap "outerHTML show:none focus-scroll:false"}
                verify-attrs)})
      anti-forgery
      hidden-fields
      [(one-time-code/input
        {:id code-id
         :name code-name
         :label code-label
         :help code-help
         :error code-error
         :length length
         :required? true
         :autofocus? true
         :input-class "w-full"})])

     [:div (attr/action-row-attrs {})
      (when resend?
        (form-node
         (attr/inline-form-attrs
          {:id resend-form-id
           :method resend-method
           :action resend-action
           :attrs (merge
                   {:hx-post resend-action
                    :hx-target (target-selector id)
                    :hx-swap "outerHTML show:none focus-scroll:false"}
                   resend-attrs)})
         anti-forgery
         hidden-fields
         [[:button (attr/link-button-attrs {})
           resend-text]]))

      (when (and resend? change?)
        [:span (attr/separator-attrs) "·"])

      (when change?
        [:a (attr/link-attrs {:href change-href})
         change-text])]

     (g/button
      {:text submit-text
       :variant :primary
       :attrs {:type "submit"
               :form verify-form-id
               :style (attr/submit-style)}})]))

(defn success-panel
  [{:keys [id class attrs title body]
    :or {id "phone-auth"
         title "Phone verified"
         body "Your phone number has been verified."}}]
  [:div (attr/panel-attrs {:id id
                           :class class
                           :attrs attrs})
   [:div (attr/copy-attrs {})
    (when title
      [:h1 (attr/title-attrs {}) title])
    (when body
      [:p (attr/body-attrs {}) body])]])
