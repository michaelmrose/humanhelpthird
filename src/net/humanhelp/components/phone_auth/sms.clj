(ns net.humanhelp.components.phone-auth.sms
  (:require
   [clojure.string :as str]))

(def default-code-length 6)
(def default-ttl-seconds 600)
(def default-max-attempts 5)

(defonce ^:private challenges
  (atom {}))

(def ^:private random
  (java.security.SecureRandom.))

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn digits-only
  [x]
  (->> (str (or x ""))
       (filter #(Character/isDigit ^char %))
       (apply str)))

(defn us-phone-digits
  [phone]
  (let [digits (digits-only phone)]
    (cond
      (= 10 (count digits))
      digits

      (and (= 11 (count digits))
           (str/starts-with? digits "1"))
      (subs digits 1)

      :else
      nil)))

(defn format-us-phone
  [digits]
  (when (= 10 (count digits))
    (str (subs digits 0 3)
         "-"
         (subs digits 3 6)
         "-"
         (subs digits 6 10))))

(defn normalize-phone
  [phone]
  (some-> phone
          us-phone-digits
          format-us-phone))

(defn canonical-phone
  [phone]
  (us-phone-digits phone))

(defn- generate-code
  [length]
  (let [limit (long (Math/pow 10 length))
        n     (.nextInt random (int limit))]
    (format (str "%0" length "d") n)))

(defn start-verification!
  [{:keys [phone length ttl-seconds]
    :or {length default-code-length
         ttl-seconds default-ttl-seconds}}]
  (let [phone-key     (canonical-phone phone)
        phone-display (some-> phone-key format-us-phone)]
    (if-not phone-key
      {:ok? false
       :error "Enter a 10-digit US mobile number."}

      (let [code       (generate-code length)
            expires-at (+ (now-ms) (* ttl-seconds 1000))]
        (swap! challenges assoc phone-key {:code code
                                           :length length
                                           :attempts 0
                                           :expires-at expires-at
                                           :phone phone-display})

        (println)
        (println "========================================")
        (println "PHONE AUTH CODE")
        (println "phone:" phone-display)
        (println "code: " code)
        (println "expires in seconds:" ttl-seconds)
        (println "========================================")
        (println)

        {:ok? true
         :phone phone-display
         :phone-digits phone-key
         :length length
         :expires-at expires-at}))))

(defn check-verification!
  [{:keys [phone code max-attempts]
    :or {max-attempts default-max-attempts}}]
  (let [phone-key     (canonical-phone phone)
        phone-display (some-> phone-key format-us-phone)
        code'         (some-> code str str/trim)
        challenge     (get @challenges phone-key)]
    (cond
      (nil? phone-key)
      {:ok? false
       :error "Missing or invalid phone number."}

      (str/blank? (or code' ""))
      {:ok? false
       :phone phone-display
       :error "Enter the code we sent you."}

      (nil? challenge)
      {:ok? false
       :phone phone-display
       :error "Send a new code and try again."}

      (< (:expires-at challenge) (now-ms))
      (do
        (swap! challenges dissoc phone-key)
        {:ok? false
         :phone phone-display
         :error "That code expired. Send another code and try again."})

      (>= (:attempts challenge) max-attempts)
      (do
        (swap! challenges dissoc phone-key)
        {:ok? false
         :phone phone-display
         :error "Too many attempts. Send another code and try again."})

      (= code' (:code challenge))
      (do
        (swap! challenges dissoc phone-key)
        {:ok? true
         :phone phone-display
         :phone-digits phone-key})

      :else
      (do
        (swap! challenges update-in [phone-key :attempts] (fnil inc 0))
        {:ok? false
         :phone phone-display
         :error "That code didn’t match. Try again."}))))

(def provider
  {:start-verification! start-verification!
   :check-verification! check-verification!})
