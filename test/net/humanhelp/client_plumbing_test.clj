(ns net.humanhelp.client-plumbing-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [gesso.live.client :as live-client]
   [net.humanhelp.client-plumbing :as plumbing]
   [net.humanhelp.middleware :as mid]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def base-ctx
  {:anti-forgery-token "test-token"
   :user/id "user-1"
   :user/email "user1@example.com"
   :session {:uid "session-user"
             :email "session@example.com"}})

(def session-only-ctx
  {:session {:uid "session-user"
             :email "session@example.com"}})

(def email-only-session-ctx
  {:session {:email "session@example.com"}})

(def anonymous-ctx
  {})

(def fragment-a
  [:div {:id "a"} "A"])

(def fragment-b
  [:div {:id "b"} "B"])

;; -----------------------------------------------------------------------------
;; Generic helpers
;; -----------------------------------------------------------------------------

(defn html-response?
  [response]
  (and (= 200 (:status response))
       (str/starts-with?
        (or (get-in response [:headers "content-type"]) "")
        "text/html")
       (string? (:body response))))

(defn no-content-response?
  [response]
  (and (= 204 (:status response))
       (or (nil? (:body response))
           (= "" (:body response)))))

(defn route-strings
  [route-tree]
  (set
   (filter string?
           (tree-seq
            (fn [x]
              (and (sequential? x)
                   (not (string? x))))
            seq
            route-tree))))

(defn route-pairs
  [route-tree]
  (filter
   (fn [x]
     (and (vector? x)
          (string? (first x))
          (map? (second x))))
   (tree-seq
    (fn [x]
      (and (sequential? x)
           (not (string? x))))
    seq
    route-tree)))

(defn route-map-for
  [route-tree route]
  (second
   (first
    (filter #(= route (first %))
            (route-pairs route-tree)))))

(defn recording-fn
  [calls return-value]
  (fn [& args]
    (swap! calls conj args)
    return-value))

(defn one-call
  [calls]
  (is (= 1 (count @calls)))
  (first @calls))

(defn non-blank-string?
  [x]
  (and (string? x)
       (not (str/blank? x))))

;; -----------------------------------------------------------------------------
;; Constants / identity / scopes
;; -----------------------------------------------------------------------------

(deftest endpoint-and-scope-shape-test
  (is (= "/app/client-plumbing" (:base-path plumbing/endpoint)))
  (is (= "/app/client-plumbing/stream" (:stream-path plumbing/endpoint)))
  (is (= "/app/client-plumbing/pending" (:pending-path plumbing/endpoint)))
  (is (= :client-id (:client-id-param plumbing/endpoint)))

  (is (= [:app/all] plumbing/app-scope))
  (is (= [:user "user-1"] (plumbing/user-scope "user-1")))
  (is (= [:user "42"] (plumbing/user-scope 42))))

(deftest new-client-id-test
  (let [a (plumbing/new-client-id)
        b (plumbing/new-client-id)]
    (is (non-blank-string? a))
    (is (non-blank-string? b))
    (is (not= a b))))

(deftest current-user-id-test
  (is (= "user-1" (plumbing/current-user-id base-ctx)))
  (is (= "session-user" (plumbing/current-user-id session-only-ctx)))
  (is (= "session@example.com" (plumbing/current-user-id email-only-session-ctx)))
  (is (= "demo-user" (plumbing/current-user-id anonymous-ctx))))

(deftest current-user-email-test
  (is (= "user1@example.com" (plumbing/current-user-email base-ctx)))
  (is (= "session@example.com" (plumbing/current-user-email session-only-ctx)))

  (is (= "param@example.com"
         (plumbing/current-user-email
          {:params {"email" "param@example.com"}})))

  ;; This is a display helper, not proof that the value is a real email.
  ;; Human Help filters displayable emails at its own boundary.
  (is (= "session-user"
         (plumbing/current-user-email
          {:session {:uid "session-user"}}))))

(deftest current-client-test
  (is (= {:client/user-id "user-1"
          :client/scopes #{[:user "user-1"] [:app/all]}}
         (plumbing/current-client base-ctx)))

  (is (= {:client/user-id "session-user"
          :client/scopes #{[:user "session-user"] [:app/all]}}
         (plumbing/current-client session-only-ctx))))

;; -----------------------------------------------------------------------------
;; Listener / stream / pending
;; -----------------------------------------------------------------------------

(deftest listener-default-test
  (let [calls (atom [])]
    (with-redefs [live-client/listener
                  (recording-fn calls [:listener])]
      (is (= [:listener] (plumbing/listener base-ctx))))

    (let [[channel ctx opts] (one-call calls)]
      (is (some? channel))
      (is (= base-ctx ctx))
      (is (non-blank-string? (:client/id opts)))
      (is (str/starts-with? (:id opts) "client-plumbing-listener-"))
      (is (true? (get-in opts [:attrs :data-client-plumbing-listener]))))))

(deftest listener-client-id-test
  (let [calls (atom [])]
    (with-redefs [live-client/listener
                  (recording-fn calls [:listener])]
      (is (= [:listener]
             (plumbing/listener base-ctx "client-123"))))

    (let [[_channel ctx opts] (one-call calls)]
      (is (= base-ctx ctx))
      (is (= "client-123" (:client/id opts)))
      (is (= "client-plumbing-listener-client-123" (:id opts)))
      (is (true? (get-in opts [:attrs :data-client-plumbing-listener]))))))

(deftest listener-options-test
  (let [calls (atom [])
        options {:client/id "client-from-options"
                 :id "custom-listener"
                 :attrs {:data-extra true}
                 :trigger-attrs {:hx-include "#board-state"}}]
    (with-redefs [live-client/listener
                  (recording-fn calls [:listener])]
      (is (= [:listener]
             (plumbing/listener base-ctx options))))

    (let [[_channel _ctx opts] (one-call calls)]
      (is (= "client-from-options" (:client/id opts)))
      (is (= "custom-listener" (:id opts)))
      (is (= true (get-in opts [:attrs :data-extra])))
      (is (= true (get-in opts [:attrs :data-client-plumbing-listener])))
      (is (= {:hx-include "#board-state"} (:trigger-attrs opts))))))

(deftest listener-client-id-plus-options-test
  (let [calls (atom [])]
    (with-redefs [live-client/listener
                  (recording-fn calls [:listener])]
      (plumbing/listener
       base-ctx
       "client-explicit"
       {:attrs {:data-extra true}}))

    (let [[_channel _ctx opts] (one-call calls)]
      (is (= "client-explicit" (:client/id opts)))
      (is (= "client-plumbing-listener-client-explicit" (:id opts)))
      (is (= true (get-in opts [:attrs :data-extra])))
      (is (= true (get-in opts [:attrs :data-client-plumbing-listener]))))))

(deftest stream-test
  (let [calls (atom [])
        response {:status 200
                  :headers {"content-type" "text/event-stream"}
                  :body ::stream}]
    (with-redefs [live-client/stream-response
                  (recording-fn calls response)]
      (is (= response (plumbing/stream base-ctx))))

    (let [[channel ctx] (one-call calls)]
      (is (some? channel))
      (is (= base-ctx ctx)))))

(deftest pending-with-fragment-test
  (let [calls (atom [])
        fragment [:div {:id "toast"} "Toast"]]
    (with-redefs [live-client/drain-fragment!
                  (recording-fn calls fragment)]
      (let [response (plumbing/pending base-ctx)]
        (is (html-response? response))
        (is (str/includes? (:body response) "Toast"))
        (is (str/includes? (:body response) "id=\"toast\""))))

    (let [[channel ctx] (one-call calls)]
      (is (some? channel))
      (is (= base-ctx ctx)))))

(deftest pending-without-fragment-test
  (let [calls (atom [])]
    (with-redefs [live-client/drain-fragment!
                  (recording-fn calls nil)]
      (is (no-content-response? (plumbing/pending base-ctx))))

    (is (= 1 (count @calls)))))

;; -----------------------------------------------------------------------------
;; Send API
;; -----------------------------------------------------------------------------

(deftest send-test
  (let [calls (atom [])
        target [:scope [:demo :scope]]
        result {:sent 2 :woke 2}]
    (with-redefs [live-client/send!
                  (recording-fn calls result)]
      (is (= result
             (plumbing/send! target fragment-a fragment-b))))

    (let [[channel request] (one-call calls)]
      (is (some? channel))
      (is (= target (:to request)))
      (is (= [fragment-a fragment-b]
             (vec (:fragments request)))))))

(deftest send-unknown-target-propagates-live-client-error-test
  (with-redefs [live-client/send!
                (fn [_channel request]
                  (throw
                   (ex-info "Unsupported gesso.live client delivery target."
                            {:request request})))]
    (try
      (plumbing/send! [:nope "x"] fragment-a)
      (is false "Expected send! to throw.")
      (catch clojure.lang.ExceptionInfo e
        (is (str/includes? (ex-message e)
                           "Unsupported gesso.live client delivery target"))
        (is (= [:nope "x"] (get-in (ex-data e) [:request :to])))))))

(deftest send-convenience-functions-test
  (let [calls (atom [])]
    (with-redefs [live-client/send-to-client!
                  (recording-fn calls {:sent 1})]
      (is (= {:sent 1}
             (plumbing/send-to-client! "client-1" fragment-a fragment-b))))

    (is (= ["client-1" fragment-a fragment-b]
           (vec (rest (one-call calls))))))

  (let [calls (atom [])]
    (with-redefs [live-client/send-to-user!
                  (recording-fn calls {:sent 2})]
      (is (= {:sent 2}
             (plumbing/send-to-user! "user-1" fragment-a))))

    (is (= ["user-1" fragment-a]
           (vec (rest (one-call calls))))))

  (let [calls (atom [])]
    (with-redefs [live-client/send-to-scope!
                  (recording-fn calls {:sent 3})]
      (is (= {:sent 3}
             (plumbing/send-to-scope! [:demo :scope] fragment-a))))

    (is (= [[:demo :scope] fragment-a]
           (vec (rest (one-call calls))))))

  (let [calls (atom [])]
    (with-redefs [live-client/broadcast!
                  (recording-fn calls {:sent 4})]
      (is (= {:sent 4}
             (plumbing/broadcast! fragment-a fragment-b))))

    (is (= [fragment-a fragment-b]
           (vec (rest (one-call calls)))))))

;; -----------------------------------------------------------------------------
;; Scope-except-user policy
;; -----------------------------------------------------------------------------

(deftest client-descriptor-helper-test
  (is (= "user-1"
         (plumbing/client-user-id
          {:client/user-id "user-1"})))

  (is (nil?
       (plumbing/client-user-id
        {:user-id "user-2"})))

  (is (nil?
       (plumbing/client-user-id
        {:client {:user-id "user-3"}})))

  (is (= #{[:app/all]}
         (plumbing/client-scopes
          {:client/scopes #{[:app/all]}})))

  (is (= #{}
         (plumbing/client-scopes
          {:scopes [[:app/all]]})))

  (is (= #{}
         (plumbing/client-scopes
          {:client {:scopes #{[:app/all]}}})))

  (is (true?
       (plumbing/client-in-scope?
        [:app/all]
        {:client/scopes #{[:app/all]}})))

  (is (false?
       (plumbing/client-in-scope?
        [:missing]
        {:client/scopes #{[:app/all]}}))))

(deftest target-client-ids-for-scope-except-user-test
  (let [clients {"client-owner" {:client/user-id "owner"
                                 :client/scopes #{[:app/all]}}
                 "client-helper" {:client/user-id "helper"
                                  :client/scopes #{[:app/all]}}
                 "client-other-scope" {:client/user-id "other"
                                       :client/scopes #{[:other/scope]}}
                 "client-alt-shape" {:user-id "helper-2"
                                     :scopes [[:app/all]]}}]
    (with-redefs [live-client/connected-clients
                  (fn [_channel]
                    clients)]
      (is (= ["client-helper"]
             (plumbing/target-client-ids-for-scope-except-user
              [:app/all]
              "owner"))))))

(deftest summarize-send-results-test
  (is (= {:sent 3
          :woke 1
          :woke? true
          :target [:scope-except-user [:app/all] "owner"]
          :scope [:app/all]
          :excluded-user-id "owner"
          :client-ids ["client-1" "client-2"]
          :fragment-count 2
          :results [{:sent 1 :woke 1}
                    {:sent 2 :woke 0}]}
         (plumbing/summarize-send-results
          {:target [:scope-except-user [:app/all] "owner"]
           :scope [:app/all]
           :excluded-user-id "owner"
           :client-ids ["client-1" "client-2"]
           :fragment-count 2
           :results [{:sent 1 :woke 1}
                     {:sent 2 :woke 0}]}))))

(deftest send-to-scope-except-user-test
  (let [calls (atom [])]
    (with-redefs [plumbing/target-client-ids-for-scope-except-user
                  (fn [scope excluded-user-id]
                    (is (= [:app/all] scope))
                    (is (= "owner" excluded-user-id))
                    ["client-2" "client-3"])

                  plumbing/send-to-client!
                  (fn [client-id & fragments]
                    (swap! calls conj {:client-id client-id
                                       :fragments (vec fragments)})
                    {:sent 1
                     :woke 1})]
      (let [result (plumbing/send-to-scope-except-user!
                    [:app/all]
                    "owner"
                    fragment-a
                    fragment-b)]
        (is (= 2 (:sent result)))
        (is (= 2 (:woke result)))
        (is (true? (:woke? result)))
        (is (= [:scope-except-user [:app/all] "owner"]
               (:target result)))
        (is (= ["client-2" "client-3"] (:client-ids result)))
        (is (= 2 (:fragment-count result)))))

    (is (= [{:client-id "client-2"
             :fragments [fragment-a fragment-b]}
            {:client-id "client-3"
             :fragments [fragment-a fragment-b]}]
           @calls))))

;; -----------------------------------------------------------------------------
;; Toast helpers
;; -----------------------------------------------------------------------------

(deftest normalize-toast-test
  (is (= {:variant :info
          :title "Hello"
          :description "The page received a live update."
          :duration 1000}
         (plumbing/normalize-toast {:title "Hello"})))

  (is (= {:variant :danger
          :title "Nope"
          :description "Bad thing"
          :duration 2500}
         (plumbing/normalize-toast
          {:variant :danger
           :title "Nope"
           :description "Bad thing"
           :duration 2500}))))

(deftest toast-oob-test
  (let [node (plumbing/toast-oob {:title "Hello"})]
    (is (vector? node))))

(deftest toast-send-helper-test
  (let [calls (atom [])
        toast {:variant :info
               :title "Hello"
               :description "World"}
        normalized (plumbing/normalize-toast toast)]
    (with-redefs [plumbing/send-to-scope!
                  (fn [scope fragment]
                    (swap! calls conj {:scope scope
                                       :fragment fragment})
                    {:sent 1})]
      (is (= {:sent 1
              :toast normalized}
             (plumbing/send-toast-to-scope! [:demo :scope] toast))))

    (let [call (first @calls)]
      (is (= [:demo :scope] (:scope call)))
      (is (vector? (:fragment call))))))

(deftest toast-send-except-user-helper-test
  (let [calls (atom [])
        toast {:title "Hello"}
        normalized (plumbing/normalize-toast toast)]
    (with-redefs [plumbing/send-to-scope-except-user!
                  (fn [scope excluded-user-id fragment]
                    (swap! calls conj {:scope scope
                                       :excluded-user-id excluded-user-id
                                       :fragment fragment})
                    {:sent 2})]
      (is (= {:sent 2
              :toast normalized}
             (plumbing/send-toast-to-scope-except-user!
              [:app/all]
              "owner"
              toast))))

    (let [call (first @calls)]
      (is (= [:app/all] (:scope call)))
      (is (= "owner" (:excluded-user-id call)))
      (is (vector? (:fragment call))))))

(deftest toast-target-helper-test
  (let [calls (atom [])
        toast {:title "Hello"}
        normalized (plumbing/normalize-toast toast)]
    (with-redefs [plumbing/send!
                  (fn [target fragment]
                    (swap! calls conj {:target target
                                       :fragment fragment})
                    {:sent 1})]
      (is (= {:sent 1
              :toast normalized}
             (plumbing/send-toast! [:client "client-1"] toast))))

    (let [call (first @calls)]
      (is (= [:client "client-1"] (:target call)))
      (is (vector? (:fragment call))))))

(deftest broadcast-toast-test
  (let [calls (atom [])
        toast {:title "Hello"}
        normalized (plumbing/normalize-toast toast)]
    (with-redefs [plumbing/broadcast!
                  (fn [fragment]
                    (swap! calls conj fragment)
                    {:sent 5})]
      (is (= {:sent 5
              :toast normalized}
             (plumbing/broadcast-toast! toast))))

    (is (vector? (first @calls)))))

;; -----------------------------------------------------------------------------
;; Introspection / reset
;; -----------------------------------------------------------------------------

(deftest introspection-helper-test
  (let [calls (atom [])]
    (with-redefs [live-client/connected-client-ids
                  (recording-fn calls ["client-1"])]
      (is (= ["client-1"] (plumbing/connected-client-ids))))

    (is (= 1 (count @calls))))

  (let [calls (atom [])]
    (with-redefs [live-client/latest-client-id
                  (recording-fn calls "client-2")]
      (is (= "client-2" (plumbing/latest-client-id))))

    (is (= 1 (count @calls))))

  (let [calls (atom [])]
    (with-redefs [live-client/pending-counts
                  (recording-fn calls {"client-1" 2})]
      (is (= {"client-1" 2} (plumbing/pending-counts))))

    (is (= 1 (count @calls))))

  (let [calls (atom [])]
    (with-redefs [live-client/state-summary
                  (recording-fn calls {:connected-client-count 1})]
      (is (= {:connected-client-count 1} (plumbing/state-summary))))

    (is (= 1 (count @calls)))))

(deftest reset-plumbing-test
  (let [calls (atom [])]
    (with-redefs [live-client/reset-channel!
                  (recording-fn calls {:reset true})]
      (is (= {:reset true} (plumbing/reset-plumbing!))))

    (let [[channel] (one-call calls)]
      (is (some? channel)))))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(deftest module-shape-test
  (is (map? plumbing/module))
  (is (vector? (:routes plumbing/module)))
  (is (seq (:routes plumbing/module)))

  (let [root-route (first (:routes plumbing/module))]
    (is (= (:base-path plumbing/endpoint) (first root-route)))
    (is (= {:middleware [mid/wrap-signed-in]}
           (second root-route)))))

(deftest module-routes-test
  (let [routes (:routes plumbing/module)
        strings (route-strings routes)]
    (is (contains? strings "/stream"))
    (is (contains? strings "/pending"))

    (is (= {:get plumbing/stream}
           (route-map-for routes "/stream")))
    (is (= {:get plumbing/pending}
           (route-map-for routes "/pending")))))
