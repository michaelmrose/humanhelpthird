(ns net.humanhelp.middleware-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [net.humanhelp.middleware :as mid]))

(defn recording-handler
  [calls response]
  (fn [ctx]
    (swap! calls conj ctx)
    response))

(deftest valid-session-user-id-test
  (testing "UUID and nonblank string identities are valid"
    (is (true? (mid/valid-session-user-id?
                #uuid "00000000-0000-0000-0000-000000000001")))
    (is (true? (mid/valid-session-user-id? "user-1")))
    (is (true? (mid/valid-session-user-id? " user-1 "))))

  (testing "missing or malformed identities are invalid"
    (doseq [value [nil
                   ""
                   "   "
                   42
                   :user-1
                   {:id "user-1"}
                   ["user-1"]]]
      (is (false? (mid/valid-session-user-id? value))
          (str "Expected invalid session user id: " (pr-str value))))))

(deftest signed-in-test
  (is (true? (mid/signed-in? {:session {:uid "user-1"}})))
  (is (true? (mid/signed-in?
              {:session
               {:uid #uuid "00000000-0000-0000-0000-000000000001"}})))

  (doseq [ctx [{}
               {:session {}}
               {:session {:uid nil}}
               {:session {:uid ""}}
               {:session {:uid "   "}}
               {:session {:uid 42}}
               {:session {:uid {:id "user-1"}}}]]
    (is (false? (mid/signed-in? ctx))
        (str "Expected unsigned context: " (pr-str ctx)))))

(deftest wrap-signed-in-allows-valid-session-test
  (let [calls    (atom [])
        response {:status 200 :body "ok"}
        handler  (mid/wrap-signed-in
                  (recording-handler calls response))
        ctx      {:session    {:uid "user-1"}
                  :request/id "request-1"}]
    (is (= response (handler ctx)))
    (is (= [ctx] @calls))))

(deftest wrap-signed-in-rejects-missing-or-malformed-session-test
  (doseq [ctx [{}
               {:session {:uid nil}}
               {:session {:uid ""}}
               {:session {:uid "   "}}
               {:session {:uid 42}}
               {:session {:uid {:id "user-1"}}}]]
    (let [calls   (atom [])
          handler (mid/wrap-signed-in
                   (recording-handler calls {:status 200}))]
      (is (= {:status  303
              :headers {"location" "/signin?error=not-signed-in"}}
             (handler ctx))
          (str "Expected sign-in redirect for: " (pr-str ctx)))
      (is (empty? @calls)))))

(deftest wrap-redirect-signed-in-redirects-valid-session-test
  (let [calls   (atom [])
        handler (mid/wrap-redirect-signed-in
                 (recording-handler calls {:status 200}))]
    (is (= {:status  303
            :headers {"location" "/app"}}
           (handler {:session {:uid "user-1"}})))
    (is (empty? @calls))))

(deftest wrap-redirect-signed-in-allows-missing-or-malformed-session-test
  (doseq [ctx [{}
               {:session {:uid nil}}
               {:session {:uid ""}}
               {:session {:uid "   "}}
               {:session {:uid 42}}
               {:session {:uid {:id "user-1"}}}]]
    (let [calls    (atom [])
          response {:status 200 :body "signin"}
          handler  (mid/wrap-redirect-signed-in
                    (recording-handler calls response))]
      (is (= response (handler ctx))
          (str "Expected sign-in page for: " (pr-str ctx)))
      (is (= [ctx] @calls)))))
