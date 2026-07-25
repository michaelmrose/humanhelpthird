(ns net.humanhelp.site.views.get-help.await-help
  (:require
   [gesso.core :as g]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.ui :as ui]))

(defn- status-presentation
  [status]
  (case status
    :open
    {:pill-status :waiting
     :pill-text "Waiting"
     :title "We're finding someone nearby"
     :description
     "Keep this page open. Your request is waiting for someone at this location."}

    :claimed
    {:pill-status :claimed
     :pill-text "Claimed"
     :title "Someone is helping you"
     :description
     "A helper has claimed your request."}

    :on-the-way
    {:pill-status :active
     :pill-text "On the way"
     :title "Help is on the way"
     :description
     "Your helper is on the way to you."}

    {:pill-status :active
     :pill-text "Active"
     :title "Your request is active"
     :description
     "Keep this page open for updates."}))

(defn- request-detail
  [label value]
  (when
   value
    [:div
     {:class
      "content-stack-theme gap-title"}

     (g/label-text
      {:text label})

     (g/text
      {:as :p
       :text value})]))

(defn page
  [ctx
   {:keys
    [user
     request-document
     location-name]}]
  (let [{:keys
         [pill-status
          pill-text
          title
          description]}
        (status-presentation
         (request/status
          request-document))

        content
        (request/content
         request-document)]
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

       [:div
        {:class
         "cluster-theme justify-center"}

        (g/status-pill
         {:status pill-status
          :text pill-text
          :dot? true})]

       (g/page-title
        {:text title})

       (g/muted-text
        {:text description
         :class "mx-auto max-w-md"})]

      (g/card
       {:class
        "radius-md border-theme pad-card"}
       [:div
        {:class
         "content-stack-theme"}

        (when
         location-name
          (request-detail
           "Location"
           location-name))

        (request-detail
         "What you need"
         (:title content))

        (request-detail
         "Details"
         (:details content))

        (request-detail
         "Where to find you"
         (:location-detail content))])

      (g/scroll-buffer
       {:size :sm})])))
