(ns net.humanhelp.site.views.new-request
  (:require
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
     "section-theme mx-auto w-full max-w-xl px-container py-10 sm:py-14"}

    [:header
     {:class
      "title-stack-theme text-center"}

     (g/page-title
      {:text "How can we help?"})

     (g/muted-text
      {:text
       "Tell us what you need and get help."
       :class
       "mx-auto max-w-md"})]

    [:div
     {:class
      "flex flex-col gap-list"}

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
     {:post
      (routes/create-request-url)

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
        :class "gap-list"
        :label-text "What do you need?"
        :required? true
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
          "For example: Help finding an item"
          :maxlength 60
          :required? true})})

      (g/field
       {:for "details"
        :class "gap-list"
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
          :maxlength 500
          :rows 5
          :placeholder
          "Add any details that would help someone understand what you need."
          :class "resize-y"})})

      (g/field
       {:for "location-detail"
        :class "gap-list"
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
          "For example: Aisle 8 near the freezer case"
          :maxlength 120})})]

     (g/group
      {:align :end}
      (g/button
       {:variant :primary
        :text "Get help"
        :attrs
        {:type "submit"}})))

    (g/scroll-buffer
     {:size :sm})]))
