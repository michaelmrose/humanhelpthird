(ns net.humanhelp.worker
  (:require
   [clojure.tools.logging :as log]
   [com.biffweb.xtdb :as biff.xtdb]
   [tick.core :as tick]))

(defn every-n-minutes
  [n]
  (iterate
   #(tick/>> % (tick/of-minutes n))
   (tick/now)))

(defn print-usage
  [ctx]
  ;; For a real app, you can have this run once per day and send you the output
  ;; in an email.
  (let [[{n-users :cnt}]
        (biff.xtdb/q
         ctx
         {:select
          [[[:count '*]
            :cnt]]

          :from
          [:user]})]
    (log/info
     "There are"
     n-users
     "users.")))

(defn echo-consumer
  [{:biff.background/keys [job]}]
  (prn
   :echo
   job)

  (when-some [callback
              (:biff/callback
               job)]
    (callback
     job)))

(def module
  {:biff.background/tasks
   [{:task
     #'print-usage

     :schedule
     #(every-n-minutes
       5)}]

   :biff.background/queues
   {:echo
    {:consumer
     #'echo-consumer}}})
