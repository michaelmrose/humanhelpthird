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
  "Return exactly 10 US phone digits, or nil.

  Accepts:
  - 1234567890
  - 123-456-7890
  - (123) 456-7890
  - +1 123 456 7890

  The returned value is always only the 10 submitted digits."
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
  "Format exactly 10 digits for display only."
  [digits]
  (when (= 10 (count digits))
    (str (subs digits 0 3)
         "-"
         (subs digits 3 6)
         "-"
         (subs digits 6 10))))

(defn normalize-phone
  "Normalize a submitted phone value into the canonical server value.

  The canonical value is exactly 10 numeric digits with no formatting."
  [phone]
  (us-phone-digits phone))

(defn phone-display
  "Return a display version of a submitted/canonical phone value."
  [phone]
  (some-> phone
          us-phone-digits
          format-us-phone))

(defn valid-phone?
  [phone]
  (boolean (normalize-phone phone)))

(defn- generate-code
  [length]
  (let [limit (long (Math/pow 10 length))
        n     (.nextInt random (int limit))]
    (format (str "%0" length "d") n)))

(defn start-verification!
  "Start an SMS verification.

  Current implementation is intentionally local/dev-only:
  - validate/normalize a US phone number
  - generate a code
  - store it in memory
  - print it to the console

  Later this function can be replaced with a Twilio Verify implementation while
  preserving the same input and return shape."
  [{:keys [phone length ttl-seconds]
    :or   {length      default-code-length
           ttl-seconds default-ttl-seconds}}]
  (let [phone'        (normalize-phone phone)
        phone-display (phone-display phone')]
    (if-not phone'
      {:ok?   false
       :error "Please enter a 10-digit US mobile number."}

      (let [code       (generate-code length)
            expires-at (+ (now-ms) (* ttl-seconds 1000))]
        (swap! challenges assoc phone' {:code       code
                                        :length     length
                                        :attempts   0
                                        :expires-at expires-at})

        (println)
        (println "========================================")
        (println "PHONE AUTH CODE")
        (println "phone:" phone-display)
        (println "submitted phone:" phone')
        (println "code:" code)
        (println "expires in seconds:" ttl-seconds)
        (println "========================================")
        (println)

        {:ok?           true
         :phone         phone'
         :phone-display phone-display
         :length        length
         :expires-at    expires-at}))))

(defn- verification-outcome
  [phone phone-display code max-attempts now challenge]
  (cond
    (nil? challenge)
    {:ok?           false
     :phone         phone
     :phone-display phone-display
     :error         "Send a new code and try again."}

    (< (:expires-at challenge) now)
    {:ok?           false
     :phone         phone
     :phone-display phone-display
     :error         "That code expired. Send another code and try again."}

    (>= (:attempts challenge) max-attempts)
    {:ok?           false
     :phone         phone
     :phone-display phone-display
     :error         "Too many attempts. Send another code and try again."}

    (= code (:code challenge))
    {:ok?           true
     :phone         phone
     :phone-display phone-display}

    :else
    {:ok?           false
     :phone         phone
     :phone-display phone-display
     :error         "That code didn’t match. Try again."}))

(defn- advance-challenges
  "Atomically advance one verification challenge for an attempted code.

  A successful, expired, or exhausted challenge is consumed. A wrong code
  increments the attempt counter. Missing challenges leave the map unchanged.
  This function is pure so Atom CAS retries cannot duplicate side effects."
  [challenge-map phone code max-attempts now]
  (let [challenge
        (get challenge-map phone)]
    (cond
      (nil? challenge)
      challenge-map

      (< (:expires-at challenge) now)
      (dissoc challenge-map phone)

      (>= (:attempts challenge) max-attempts)
      (dissoc challenge-map phone)

      (= code (:code challenge))
      (dissoc challenge-map phone)

      :else
      (update-in challenge-map [phone :attempts] (fnil inc 0)))))

(defn check-verification!
  "Check an SMS verification code.

  Current implementation checks the in-memory dev code. Later this function can
  be replaced with a Twilio Verify implementation while preserving the same
  input and return shape.

  Challenge inspection and consumption happen in one Atom state transition.
  Consequently one stored code can produce at most one successful verification,
  even when duplicate verification requests arrive concurrently."
  [{:keys [phone code max-attempts]
    :or   {max-attempts default-max-attempts}}]
  (let [phone'        (normalize-phone phone)
        phone-display (phone-display phone')
        code'         (some-> code str str/trim)]
    (cond
      (nil? phone')
      {:ok?   false
       :error "Missing or invalid phone number."}

      (str/blank? (or code' ""))
      {:ok?           false
       :phone         phone'
       :phone-display phone-display
       :error         "Enter the code we sent you."}

      :else
      (let [now
            (now-ms)

            [before _after]
            (swap-vals!
             challenges
             advance-challenges
             phone'
             code'
             max-attempts
             now)

            challenge
            (get before phone')]
        (verification-outcome
         phone'
         phone-display
         code'
         max-attempts
         now
         challenge)))))

(def provider
  {:start-verification! start-verification!
   :check-verification! check-verification!})
