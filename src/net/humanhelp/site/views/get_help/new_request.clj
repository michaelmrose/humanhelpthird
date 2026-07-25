(ns net.humanhelp.site.views.get-help.new-request
  (:require
   [gesso.core :as g]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.routes :as routes]
   [net.humanhelp.ui :as ui]))

(def request-form-schema
  [:map
   [:title
    [:string
     {:min 1
      :max request/title-max
      :gesso.error/required "Tell us what you need."
      :gesso.error/maxlength
      (str
       "Your request must be "
       request/title-max
       " characters or fewer.")}]]

   [:details
    {:optional true}
    [:string
     {:max request/details-max
      :gesso.error/maxlength
      (str
       "Details must be "
       request/details-max
       " characters or fewer.")}]]

   [:location-detail
    {:optional true}
    [:string
     {:max request/location-detail-max
      :gesso.error/maxlength
      (str
       "Location details must be "
       request/location-detail-max
       " characters or fewer.")}]]])

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

(defn page
  [ctx
   {:keys
    [user
     location
     values
     errors
     form-error]
    :or
    {values {}
     errors {}}}]
  (ui/page-shell
   ctx
   {:user user
    :main-class "flex-grow"}

   [:section
    {:class
     "section-theme mx-auto w-full max-w-xl px-container py-10 sm:py-14"}

    [:header
     {:class
      "title-stack-theme text-center"}

     (g/page-title
      {:text "How can we help?"})

     (g/muted-text
      {:text
       "Tell us what you need and someone nearby can help."
       :class
       "mx-auto max-w-md"})]

    [:div
     {:class
      "flex flex-col gap-content"}

     (g/label-text
      {:text "Your location"})

     (g/card
      {:class
       "radius-md border-theme pad-card"}
      (g/group
       {:align :between
        :wrap? false}

       (g/card-title
        {:text
         (:location/name location)})

       [:a
        {:href routes/base-path
         :class "btn-link"}
        "Change"]))]

    (g/form
     ctx
     {:validate-url
      (routes/create-request-validation-url)

      :csrf?
      true

      :attrs
      {:action
       (routes/create-request-url)

       :method
       "post"}}

     [:input
      {:type "hidden"
       :name routes/location-id-param
       :value
       (str
        (:location/id location))}]

     (when
      form-error
       (g/alert
        {:variant :destructive
         :title "We couldn't create this request"
         :content form-error}))

     (when
      (seq errors)
       (g/alert
        {:variant :destructive
         :title "Please check your request"
         :content
         "Correct the highlighted fields and try again."}))

     [:div
      {:class
       "flex flex-col gap-section"}

      (g/field
       {:for "title"
        :field-key :title
        :schema request-form-schema
        :class "gap-content"
        :label-text "What do you need?"
        :error
        (field-error
         errors
         :title)

        :control
        (g/input
         {:id "title"
          :name "title"
          :value
          (field-value
           values
           :title)
          :placeholder
          "For example: Help finding an item"})})

      (g/field
       {:for "details"
        :field-key :details
        :schema request-form-schema
        :class "gap-content"
        :label-text
        "Anything else we should know?"
        :description "Optional"
        :error
        (field-error
         errors
         :details)

        :control
        (g/textarea
         {:id "details"
          :name "details"
          :value
          (field-value
           values
           :details)
          :rows 5
          :placeholder
          "Add any details that would help someone understand what you need."
          :class "resize-y"})})

      (g/field
       {:for "location-detail"
        :field-key :location-detail
        :schema request-form-schema
        :class "gap-content"
        :label-text
        "Where should we find you?"
        :error
        (field-error
         errors
         :location-detail)

        :control
        (g/input
         {:id "location-detail"
          :name "location-detail"
          :value
          (field-value
           values
           :location-detail)
          :placeholder
          "For example: Aisle 8 near the freezer case"})})]

     (g/group
      {:align :end}
      (g/button
       {:variant :primary
        :text "Get help"
        :attrs
        {:type "submit"}})))

    (g/scroll-buffer
     {:size :sm})]))
