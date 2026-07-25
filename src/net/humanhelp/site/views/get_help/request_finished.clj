(ns net.humanhelp.site.views.get-help.request-finished
  (:require
   [gesso.core :as g]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.routes :as routes]
   [net.humanhelp.ui :as ui]))

(defn- status-presentation
  [status]
  (case status
    :done
    {:pill-status :complete
     :pill-text "Complete"
     :title "Request complete"
     :description "Your help request has been completed."}

    :cancelled
    {:pill-status :cancelled
     :pill-text "Cancelled"
     :title "Request cancelled"
     :description "This help request is no longer active."}

    {:pill-status :muted
     :pill-text "Finished"
     :title "Request finished"
     :description "This help request is no longer active."}))

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
          :text pill-text})]

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
          (g/muted-text
           {:as :p
            :text location-name}))

        (g/card-title
         {:text
          (:title content)})

        (when-let [details
                   (:details content)]
          (g/text
           {:as :p
            :text details}))])

      (g/group
       {:align :end}

       [:a
        {:href routes/base-path
         :class "btn"}
        "Get help again"])

      (g/scroll-buffer
       {:size :sm})])))
