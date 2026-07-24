(ns net.humanhelp.site.views.get-help.select-location
  (:require
   [gesso.core :as g]
   [net.humanhelp.site.components.glow.core :as glow]
   [net.humanhelp.site.routes :as routes]
   [net.humanhelp.ui :as ui]))

(defn- location-option
  [location selected-location-id]
  (let [location-id
        (:location/id location)

        likely?
        (true?
         (:location/likely? location))

        selected?
        (=
         location-id
         selected-location-id)]
    (glow/glow
     {:active?
      likely?

      :class
      "block"}

     [:label
      {:class
       "interactive-row-theme radius-xl border-theme flex cursor-pointer items-center gap-inline bg-card text-card-foreground"}

      (g/radio
       {:name routes/location-id-param
        :value (str location-id)
        :checked selected?})

      [:span
       {:class
        "min-w-0 flex-1"}

       [:span
        {:class
         "cluster-theme items-center"}

        [:span
         {:class
          "font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme"}
         (:location/name location)]

        (when
         likely?
          (g/badge
           {:text "Most likely"
            :variant :secondary}))]

       (g/muted-text
        {:as :span
         :text (:location/distance location)
         :class "block text-sm-theme"})]])))

(defn page
  [ctx
   {:keys
    [user
     locations
     selected-location-id]}]
  (let [selected-location-id
        (or
         selected-location-id
         (:location/id
          (first locations)))

        location-options
        (map
         #(location-option
           %
           selected-location-id)
         locations)]
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
        {:text "Where are you?"})

       (g/muted-text
        {:text
         "Choose the location where you need help."
         :class
         "mx-auto max-w-md"})]

      [:form
       {:method "get"
        :action routes/base-path
        :class "form-theme"}

       (apply
        g/radio-group
        {:class "gap-list"
         :attrs
         {:aria-label
          "Choose your location"}}
        location-options)

       [:div
        {:class
         "cluster-theme justify-end"}

        (g/button
         {:variant :primary
          :text "Continue"
          :attrs
          {:type "submit"}})]]])))

