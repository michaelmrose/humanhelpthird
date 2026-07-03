(ns net.humanhelp.components.phone-auth.scripts
  (:require
   [gesso.hyperscript :refer [hs]]))

(defn- phone-format-statements
  []
  [[:if "me.dataset.phoneAuthFormatting == 'true'"
    ["exit"]]

   [:set 'raw "my.value.replace(/\\D/g, '')"]

   [:if "raw.length > 10 and raw.charAt(0) == '1'"
    [[:set 'raw "raw.slice(1)"]]]

   [:set 'digits "raw.slice(0, 10)"]

   [:if "digits.length <= 3"
    [[:set 'formatted 'digits]]
    [[:if "digits.length <= 6"
      [[:set 'formatted "digits.slice(0, 3) + '-' + digits.slice(3)"]]
      [[:set 'formatted "digits.slice(0, 3) + '-' + digits.slice(3, 6) + '-' + digits.slice(6)"]]]]]

   [:if "my.value is not formatted"
    [[:set 'me.dataset.phoneAuthFormatting "'true'"]
     [:set 'my.value 'formatted]
     [:set 'me.dataset.phoneAuthFormatting "'false'"]
     "trigger input on me"]]])

(defn us-phone-format-script
  []
  (hs
   [:on :input
    (phone-format-statements)]

   [:on :paste
    "wait 0ms"
    "trigger input on me"]

   [:on :blur
    (phone-format-statements)]))
