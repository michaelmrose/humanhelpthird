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
       "flex cursor-pointer items-center gap-4 rounded-xl border border-border bg-card px-5 py-4 text-card-foreground"}

      [:input
       (cond->
        {:type "radio"
         :name routes/location-id-param
         :value (str location-id)}

         selected?
         (assoc
          :checked true))]

      [:span
       {:class "min-w-0 flex-1"}

       [:span
        {:class "flex flex-wrap items-center gap-x-3 gap-y-1"}

        [:span
         {:class
          "font-heading text-lg-theme leading-heading tracking-heading weight-semibold-theme"}
         (:location/name location)]

        (when
         likely?
          [:span
           {:class
            "font-body text-xs-theme leading-body weight-semibold-theme"}
           "Most likely"])]

       [:span
        {:class
         "mt-1 block font-body text-sm-theme leading-body"
         :style
         {:color "var(--muted-foreground)"}}
        (:location/distance location)]]])))

(defn page
  [ctx
   {:keys
    [user
     selected-location-id]}]
  (let [selected-location-id
        (or
         selected-location-id
         default-location-id)]
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
        "Where are you?"]

       [:p
        {:class
         "mx-auto mt-2 max-w-md font-body text-sm-theme leading-body"
         :style
         {:color "var(--muted-foreground)"}}
        "Choose the location where you need help."]]

      [:form
       {:method "get"
        :action routes/base-path}

       [:fieldset
        {:class "space-y-4"}

        [:legend
         {:class "sr-only"}
         "Choose your location"]

        (for
         [location mock-locations]
          (location-option
           location
           selected-location-id))]

       [:div
        {:class "mt-8 flex justify-end"}

        (g/button
         {:variant :primary
          :text "Continue"
          :attrs
          {:type "submit"}})]]])))
