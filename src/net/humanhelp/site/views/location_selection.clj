(ns net.humanhelp.site.views.location-selection
  (:require
   [gesso.core :as g]
   [net.humanhelp.site.components.glow.core :as glow]
   [net.humanhelp.site.routes :as routes]
   [net.humanhelp.ui :as ui])
  (:import
   [java.util UUID]))

(def mock-locations
  [{:location/id
    (UUID/fromString "30000000-0000-0000-0000-000000000001")
    :location/name "Northgate"
    :location/distance "0.3 mi away"
    :location/likely? true}

   {:location/id
    (UUID/fromString "30000000-0000-0000-0000-000000000002")
    :location/name "Lake City"
    :location/distance "2.1 mi away"}

   {:location/id
    (UUID/fromString "30000000-0000-0000-0000-000000000003")
    :location/name "University Village"
    :location/distance "3.8 mi away"}])

(def default-location-id
  (:location/id
   (first mock-locations)))

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
      (and
       likely?
       selected?)

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
     selected-location-id]}]
  (let [selected-location-id
        (or
         selected-location-id
         default-location-id)

        location-options
        (map
         #(location-option
           %
           selected-location-id)
         mock-locations)]
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
        {:text "Choose the location where you need help."
         :class "mx-auto max-w-md"})]

      [:form
       {:method "get"
        :action routes/base-path
        :class "form-theme"}

       (apply
        g/radio-group
        {:class "gap-list"
         :attrs
         {:aria-label "Choose your location"}}
        location-options)

       [:div
        {:class
         "cluster-theme justify-end"}

        (g/button
         {:variant :primary
          :text "Continue"
          :attrs
          {:type "submit"}})]]])))

