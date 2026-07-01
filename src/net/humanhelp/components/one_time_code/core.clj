(ns net.humanhelp.components.one-time-code.core
  (:require
   [net.humanhelp.components.one-time-code.attr :as attr]))

(defn input
  "Render a single real one-time-code input.

  This component is intentionally dumb:
  - it renders one input
  - it defaults to 5 numeric digits
  - it asks mobile browsers for a numeric keypad
  - it uses autocomplete=\"one-time-code\"
  - it does not know about auth, SMS, challenges, or redirects"
  [{:keys [id label help error] :as opts}]
  (let [id'   (or id "one-time-code")
        opts' (assoc opts :id id')]
    [:div (attr/root-attrs opts')
     (when label
       [:label (attr/label-attrs opts') label])

     [:input (attr/input-attrs opts')]

     (when help
       [:p (attr/help-attrs opts') help])

     (when error
       [:p (attr/error-attrs opts') error])]))
