(ns net.humanhelp.example.components.refresh-button.core
  (:require
   [gesso.core :as g]
   [net.humanhelp.example.components.refresh-button.attr :as attr]))

(defn refresh-button
  [{:keys [stale?]}]
  [:span {:data-humanhelp-refresh-button-frame true
          :data-humanhelp-refresh-stale (when stale? "true")}
   (g/button
    {:variant (if stale? :primary :outline)
     :text "↻"
     :class "humanhelp-refresh-button"
     :attrs (attr/button-attrs stale?)})])
