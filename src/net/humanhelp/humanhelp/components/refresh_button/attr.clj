(ns net.humanhelp.humanhelp.components.refresh-button.attr)

(defn button-attrs
  [stale?]
  {:type "submit"
   :data-humanhelp-refresh-button true
   :data-humanhelp-refresh-stale (when stale? "true")
   :aria-label (if stale?
                 "Refresh requests. New request data is available."
                 "Refresh requests")
   :title (if stale?
            "New requests received"
            "Refresh requests")})
