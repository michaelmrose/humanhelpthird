(ns net.humanhelp.schema
  (:require [tick.core :as tick]))

(def ? {:optional true})

(def schema
  {::string [:string {:max 1000}]
   ::phone-digits [:string {:min 10 :max 10}]
   ::phone-display [:string {:max 20}]
   ::zdt    [:fn tick/zoned-date-time?]

   :user [:map {:closed true}
          [:xt/id                  :uuid]
          [:user/email             ? ::string]
          [:user/phone             ? ::phone-digits]
          [:user/phone-display     ? ::phone-display]
          [:user/phone-verified-at ? ::zdt]
          [:user/joined-at         ::zdt]
          [:user/foo               ? ::string]
          [:user/bar               ? ::string]]

   :msg [:map {:closed true}
         [:xt/id       :uuid]
         [:msg/user    :uuid]
         [:msg/content [:string {:max 10000}]]
         [:msg/sent-at ::zdt]]})

(def module
  {:schema schema})
