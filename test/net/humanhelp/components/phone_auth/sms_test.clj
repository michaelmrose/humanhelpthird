(ns net.humanhelp.components.phone-auth.sms-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [net.humanhelp.components.phone-auth.sms :as sms]))

(def ^:private challenges-var
  (ns-resolve 'net.humanhelp.components.phone-auth.sms 'challenges))

(def ^:private generate-code-var
  (ns-resolve 'net.humanhelp.components.phone-auth.sms 'generate-code))

(def ^:private now-ms-var
  (ns-resolve 'net.humanhelp.components.phone-auth.sms 'now-ms))

(defn- challenges-atom
  []
  (var-get challenges-var))

(defn- challenge-state
  []
  @(challenges-atom))

(defn- reset-challenges!
  []
  (reset! (challenges-atom) {}))

(defn- with-provider-stubs
  [{:keys [code now]
    :or {code "123456"
         now 1000}}
   f]
  (with-redefs-fn
    {generate-code-var (fn [_length] code)
     now-ms-var        (fn [] now)}
    f))

(use-fixtures
  :each
  (fn [f]
    (reset-challenges!)
    (try
      (f)
      (finally
        (reset-challenges!)))))

(deftest phone-normalization-test
  (testing "common US phone forms normalize to ten canonical digits"
    (doseq [value ["2065550123"
                   "206-555-0123"
                   "(206) 555-0123"
                   "+1 206 555 0123"]]
      (is (= "2065550123"
             (sms/normalize-phone value)))))

  (testing "invalid digit counts do not normalize"
    (doseq [value [nil
                   ""
                   "5550123"
                   "22065550123"
                   "+44 20 7946 0958"]]
      (is (nil? (sms/normalize-phone value))
          (str "Expected invalid phone: " (pr-str value)))))

  (is (= "206-555-0123"
         (sms/phone-display "+1 (206) 555-0123")))
  (is (true? (sms/valid-phone? "2065550123")))
  (is (false? (sms/valid-phone? "5550123"))))

(deftest start-verification-test
  (with-provider-stubs
    {:code "314159"
     :now 2000}
    (fn []
      (binding [*out* (java.io.StringWriter.)]
        (is (= {:ok? true
                :phone "2065550123"
                :phone-display "206-555-0123"
                :length 6
                :expires-at 12000}
               (sms/start-verification!
                {:phone "+1 (206) 555-0123"
                 :ttl-seconds 10}))))
      (is (= {"2065550123"
              {:code "314159"
               :length 6
               :attempts 0
               :expires-at 12000}}
             (challenge-state)))))

  (testing "invalid phones create no challenge"
    (reset-challenges!)
    (is (= {:ok? false
            :error "Please enter a 10-digit US mobile number."}
           (sms/start-verification! {:phone "5550123"})))
    (is (empty? (challenge-state)))))

(deftest successful-verification-consumes-challenge-test
  (reset! (challenges-atom)
          {"2065550123" {:code "123456"
                         :length 6
                         :attempts 0
                         :expires-at 5000}})
  (with-provider-stubs
    {:now 1000}
    (fn []
      (is (= {:ok? true
              :phone "2065550123"
              :phone-display "206-555-0123"}
             (sms/check-verification!
              {:phone "206-555-0123"
               :code " 123456 "})))
      (is (empty? (challenge-state)))
      (is (= {:ok? false
              :phone "2065550123"
              :phone-display "206-555-0123"
              :error "Send a new code and try again."}
             (sms/check-verification!
              {:phone "2065550123"
               :code "123456"}))))))

(deftest expired-verification-consumes-challenge-test
  (reset! (challenges-atom)
          {"2065550123" {:code "123456"
                         :length 6
                         :attempts 0
                         :expires-at 999}})
  (with-provider-stubs
    {:now 1000}
    (fn []
      (is (= {:ok? false
              :phone "2065550123"
              :phone-display "206-555-0123"
              :error "That code expired. Send another code and try again."}
             (sms/check-verification!
              {:phone "2065550123"
               :code "123456"})))
      (is (empty? (challenge-state))))))

(deftest wrong-codes-advance-attempt-count-test
  (reset! (challenges-atom)
          {"2065550123" {:code "123456"
                         :length 6
                         :attempts 0
                         :expires-at 5000}})
  (with-provider-stubs
    {:now 1000}
    (fn []
      (dotimes [attempt 2]
        (is (= {:ok? false
                :phone "2065550123"
                :phone-display "206-555-0123"
                :error "That code didn’t match. Try again."}
               (sms/check-verification!
                {:phone "2065550123"
                 :code "000000"
                 :max-attempts 2})))
        (is (= (inc attempt)
               (get-in (challenge-state)
                       ["2065550123" :attempts]))))

      (is (= {:ok? false
              :phone "2065550123"
              :phone-display "206-555-0123"
              :error "Too many attempts. Send another code and try again."}
             (sms/check-verification!
              {:phone "2065550123"
               :code "123456"
               :max-attempts 2})))
      (is (empty? (challenge-state))))))

(deftest missing-or-incomplete-verification-input-test
  (is (= {:ok? false
          :error "Missing or invalid phone number."}
         (sms/check-verification!
          {:phone "5550123"
           :code "123456"})))
  (is (= {:ok? false
          :phone "2065550123"
          :phone-display "206-555-0123"
          :error "Enter the code we sent you."}
         (sms/check-verification!
          {:phone "2065550123"
           :code "   "})))
  (is (empty? (challenge-state))))

(deftest concurrent-success-consumes-code-once-test
  (reset! (challenges-atom)
          {"2065550123" {:code "123456"
                         :length 6
                         :attempts 0
                         :expires-at 5000}})
  (with-provider-stubs
    {:now 1000}
    (fn []
      (let [worker-count 32
            ready        (java.util.concurrent.CountDownLatch. worker-count)
            start        (promise)
            workers      (doall
                          (repeatedly
                           worker-count
                           #(future
                              (.countDown ready)
                              @start
                              (sms/check-verification!
                               {:phone "2065550123"
                                :code "123456"}))))]
        (is (.await ready
                    5
                    java.util.concurrent.TimeUnit/SECONDS))
        (deliver start true)
        (let [results   (mapv deref workers)
              successes (filterv :ok? results)]
          (is (= 1 (count successes)))
          (is (= (dec worker-count)
                 (count (remove :ok? results))))
          (is (every?
               #(= "Send a new code and try again."
                   (:error %))
               (remove :ok? results)))
          (is (empty? (challenge-state))))))))

(deftest provider-map-exposes-public-provider-contract-test
  (is (= sms/start-verification!
         (:start-verification! sms/provider)))
  (is (= sms/check-verification!
         (:check-verification! sms/provider))))
