(ns net.humanhelp.site.views.new-request
  (:require
   [com.biffweb :as biff]
   [gesso.core :as g]
   [net.humanhelp.site.routes :as routes]
   [net.humanhelp.ui :as ui]))

(defn- field-value
  [values field]
  (or
   (get values field)
   (get values (name field))
   ""))

(defn- field-error
  [errors field]
  (or
   (get errors field)
   (get errors (name field))))

(defn- error-text
  [errors field]
  (when-let [message
             (field-error errors field)]
    [:p
     {:class "mt-1 font-body text-xs-theme leading-body"
      :style {:color "var(--destructive)"}}
     message]))

(defn- input-field
  [{:keys
    [field
     label
     value
     placeholder
     maxlength
     required?
     errors]}]
  [:label
   {:class "block"}

   [:span
    {:class
     "mb-2 block font-heading text-sm-theme leading-heading tracking-heading weight-semibold-theme"}
    label]

   [:input
    (cond->
     {:type "text"
      :name (name field)
      :value value
      :placeholder placeholder
      :maxlength maxlength
      :class
      "control-theme w-full rounded-md border border-border bg-background px-3 py-2 font-body text-sm-theme"}

      required?
      (assoc
       :required true)

      (field-error errors field)
      (assoc
       :aria-invalid "true"))]

   (error-text
    errors
    field)])

(defn- details-field
  [values errors]
  [:label
   {:class "block"}

   [:span
    {:class
     "mb-1 block font-heading text-sm-theme leading-heading tracking-heading weight-semibold-theme"}
    "Anything else we should know?"]

   [:span
    {:class
     "mb-2 block font-body text-xs-theme leading-body"
     :style
     {:color "var(--muted-foreground)"}}
    "Optional"]

   [:textarea
    (cond->
     {:name "details"
      :maxlength 500
      :rows 5
      :placeholder
      "Add any details that would help someone understand what you need."
      :class
      "control-theme w-full resize-y rounded-md border border-border bg-background px-3 py-2 font-body text-sm-theme"}

      (field-error errors :details)
      (assoc
       :aria-invalid "true"))
    (field-value
     values
     :details)]

   (error-text
    errors
    :details)])

(defn page
  [ctx
   {:keys
    [user
     location
     values
     errors]
    :or
    {values {}
     errors {}}}]
  (ui/page-shell
   ctx
   {:user user
    :main-class "flex-grow"}

   [:section
    {:class
     "mx-auto w-full max-w-xl px-4 py-10 sm:px-6 sm:py-14"}

    [:header
     {:class "mb-8 text-center"}

     [:h1
      {:class
       "font-heading text-2xl-theme leading-heading tracking-heading weight-semibold-theme"}
      "How can we help?"]

     [:p
      {:class
       "mx-auto mt-2 max-w-md font-body text-sm-theme leading-body"
       :style
       {:color "var(--muted-foreground)"}}
      "Tell us what you need and someone nearby can help."]]

    [:div
     {:class
      "mb-8 flex items-center justify-between gap-4 rounded-xl border border-border bg-card px-5 py-4"}

     [:div
      {:class "min-w-0"}

      [:div
       {:class
        "font-body text-xs-theme leading-body weight-semibold-theme"
        :style
        {:color "var(--muted-foreground)"}}
       "Your location"]

      [:div
       {:class
        "mt-1 font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme"}
       (:location/name location)]]

     [:a
      {:href routes/base-path
       :class
       "font-body text-sm-theme leading-body weight-semibold-theme"
       :style
       {:color "var(--primary)"
        :text-decoration "none"}}
      "Change"]]

    (biff/form
     {:action routes/base-path
      :method "post"}

     [:input
      {:type "hidden"
       :name routes/location-id-param
       :value
       (str
        (:location/id location))}]

     [:div
      {:class "space-y-6"}

      (input-field
       {:field :title
        :label "What do you need?"
        :value
        (field-value
         values
         :title)
        :placeholder
        "For example: Help finding an item"
        :maxlength 60
        :required? true
        :errors errors})

      (details-field
       values
       errors)

      (input-field
       {:field :location-detail
        :label "Where should we find you?"
        :value
        (field-value
         values
         :location-detail)
        :placeholder
        "For example: Aisle 8 near the freezer case"
        :maxlength 120
        :errors errors})]

     [:div
      {:class "mt-8 flex justify-end"}

      (g/button
       {:variant :primary
        :text "Get help"
        :attrs
        {:type "submit"}})])]))
