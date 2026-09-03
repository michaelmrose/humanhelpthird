(ns net.humanhelp.auth.phone-test
  "Tests for the HumanHelp phone-authentication boundary.

   These tests keep provider-facing US phone normalization separate from the
   canonical User model and pin the account-status rule at sign-in completion:
   only active existing Users may authenticate. Suspended and deleted Users
   fail closed without exposing lifecycle details to the caller."
  (:require
   [clojure.test :refer [deftest is testing]]
   [net.humanhelp.auth.phone :as phone]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.util UUID]))

(def existing-user-id
  (UUID/fromString "00000000-0000-0000-0000-000000000001"))

(def created-user-id
  (UUID/fromString "00000000-0000-0000-0000-000000000002"))

(defn- user-document
  [id status]
  {:xt/id       id
   :user/status status
   :user/phone  "+12065550123"})

(def active-user
  (user-document existing-user-id :active))

(def suspended-user
  (user-document existing-user-id :suspended))

(def deleted-user
  (user-document existing-user-id :deleted))

(def unavailable-result
  {:ok?          false
   :phone        "2065550123"
   :phone-display "206-555-0123"
   :error        "This account is not available for sign-in."})

(deftest phone-boundary-normalization-test
  (testing "provider-facing phone values normalize to exactly 10 US digits"
    (is (= "2065550123"
           (phone/normalize-phone "206-555-0123")))
    (is (= "2065550123"
           (phone/normalize-phone "+1 (206) 555-0123")))
    (is (nil? (phone/normalize-phone "555-0123"))))

  (testing "display formatting remains provider/UI-facing"
    (is (= "206-555-0123"
           (phone/phone-display "2065550123")))
    (is (= "206-555-0123"
           (phone/phone-display "+1 206 555 0123")))
    (is (nil? (phone/phone-display "555-0123")))))

(deftest existing-active-user-signin-test
  (let [lookup
        (atom [])]
    (with-redefs
     [user/user-by-phone
      (fn [ctx canonical-phone]
        (swap! lookup conj [ctx canonical-phone])
        active-user)]
      (let [ctx    {:request/id :request-1}
            result (phone/complete-phone-signin!
                    ctx
                    {:phone "+1 (206) 555-0123"})]
        (is (= [[ctx "+12065550123"]]
               @lookup))
        (is (= {:ok?          true
                :user-id      existing-user-id
                :phone        "2065550123"
                :phone-display "206-555-0123"
                :new-user?    false}
               result))))))

(deftest unavailable-existing-user-signin-test
  (testing "suspended Users cannot authenticate through a verified phone"
    (with-redefs
     [user/user-by-phone
      (fn [_ctx _canonical-phone]
        suspended-user)]
      (is (= unavailable-result
             (phone/complete-phone-signin!
              {}
              {:phone "2065550123"})))))

  (testing "deleted Users cannot authenticate through a verified phone"
    (with-redefs
     [user/user-by-phone
      (fn [_ctx _canonical-phone]
        deleted-user)]
      (is (= unavailable-result
             (phone/complete-phone-signin!
              {}
              {:phone "2065550123"})))))

  (testing "lifecycle state is not disclosed through distinct error messages"
    (let [result-for
          (fn [existing]
            (with-redefs
             [user/user-by-phone
              (fn [_ctx _canonical-phone]
                existing)]
              (phone/complete-phone-signin!
               {}
               {:phone "2065550123"})))]
      (is (= (:error (result-for suspended-user))
             (:error (result-for deleted-user)))))))

(deftest missing-existing-user-creates-verified-user-test
  (let [lookup
        (atom [])

        machine-input
        (atom nil)]
    (with-redefs
     [user/user-by-phone
      (fn [ctx canonical-phone]
        (swap! lookup conj [ctx canonical-phone])
        nil)

      phone/create-user-machine
      (fn [ctx]
        (reset! machine-input ctx)
        {:user
         {:xt/id       created-user-id
          :user/status :active
          :user/phone  "+12065550123"}})]
      (let [ctx    {:request/id :request-2}
            result (phone/complete-phone-signin!
                    ctx
                    {:phone "206 555 0123"})]
        (is (= [[ctx "+12065550123"]]
               @lookup))
        (is (= ctx
               (dissoc @machine-input
                       :net.humanhelp.auth.phone/create-user-input)))
        (is (= {:phone           "+12065550123"
                :phone-verified? true}
               (:net.humanhelp.auth.phone/create-user-input
                @machine-input)))
        (is (= {:ok?          true
                :user-id      created-user-id
                :phone        "2065550123"
                :phone-display "206-555-0123"
                :new-user?    true}
               result))))))

(deftest invalid-phone-fails-before-user-access-test
  (let [lookups
        (atom 0)]
    (with-redefs
     [user/user-by-phone
      (fn [& _]
        (swap! lookups inc)
        (throw (ex-info "unexpected lookup" {})))]
      (is (= {:ok?  false
              :error "Missing or invalid phone number."}
             (phone/complete-phone-signin!
              {}
              {:phone "555-0123"})))
      (is (zero? @lookups)))))
