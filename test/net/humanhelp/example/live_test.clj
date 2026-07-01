(ns net.humanhelp.example.live-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.live.core :as live]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.live :as hh-live]
   [net.humanhelp.example.model :as model]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.example.views :as views]
   [xtdb.node :as xtn]))

;; -----------------------------------------------------------------------------
;; XTDB fixture
;; -----------------------------------------------------------------------------

(defonce !ctx
  (atom nil))

(defn ctx
  []
  (or @!ctx
      (throw
       (ex-info "live-test ctx has not been initialized."
                {}))))

(defn xtdb-fixture
  [f]
  (with-open [node (xtn/start-node)]
    (reset! !ctx {:anti-forgery-token "test-token"
                  :biff/node node
                  :biff/conn node
                  :xtdb/node node})
    (try
      (f)
      (finally
        (reset! !ctx nil)))))

(defn reset-model-fixture
  [f]
  (model/reset-demo-state! (ctx))
  (try
    (f)
    (finally
      (model/reset-demo-state! (ctx)))))

(use-fixtures :once xtdb-fixture)
(use-fixtures :each reset-model-fixture)

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def user-owner
  {:user/id "user-owner"
   :user/email "owner@example.com"})

(def user-helper
  {:user/id "user-helper"
   :user/email "helper@example.com"})

(def view-state
  {:search "garden"
   :visible-revision 3})

(defn latest-view-state
  []
  {:search ""
   :visible-revision (model/latest-revision (ctx))})

(defn render-opts
  [overrides]
  (merge
   {:user user-owner
    :view-state (latest-view-state)}
   overrides))

(defn valid-input
  [overrides]
  (merge
   {:title "Need help finding a rake"
    :area "Garden"
    :details "Looking for a sturdy rake for leaves."
    :customer-name "Jon"}
   overrides))

(defn request
  [overrides]
  (merge
   {:request/id "hh-req-99"
    :request/number 99
    :request/store-id model/store-id
    :request/title "Need help finding a rake"
    :request/area "Garden"
    :request/details "Looking for a sturdy rake for leaves."
    :request/customer-user-id "user-owner"
    :request/customer-name "Jon"
    :request/status :open
    :request/claimed-by nil
    :request/claimed-by-email nil
    :request/created-at-ms 1000000
    :request/updated-at-ms 1000000
    :request/created-revision 4
    :request/updated-revision 4}
   overrides))

;; -----------------------------------------------------------------------------
;; Hiccup / response helpers
;; -----------------------------------------------------------------------------

(defn hiccup-branch?
  [x]
  (and (sequential? x)
       (not (string? x))
       (not (map? x))))

(defn hiccup-seq
  [x]
  (tree-seq hiccup-branch? seq x))

(defn node?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defn attrs
  [node]
  (when (and (vector? node)
             (map? (second node)))
    (second node)))

(defn find-by-id
  [tree id]
  (some
   (fn [node]
     (when (= id (:id (attrs node)))
       node))
   (filter node? (hiccup-seq tree))))

(defn all-text
  [tree]
  (apply str
         (filter string? (hiccup-seq tree))))

(defn contains-text?
  [tree text]
  (str/includes? (all-text tree) text))

(defn response-body
  [response]
  (or (:body response) ""))

(defn body-contains?
  [response text]
  (str/includes? (response-body response) (str text)))

(defn html-response?
  [response]
  (and (= 200 (:status response))
       (str/starts-with?
        (or (get-in response [:headers "content-type"]) "")
        "text/html")
       (string? (:body response))))

(defn split-url
  [url]
  (let [[path query] (str/split (str url) #"\?" 2)]
    {:path path
     :query query}))

(defn path-of
  [url]
  (:path (split-url url)))

(defn no-query?
  [url]
  (nil? (:query (split-url url))))

(defn expected-path
  [route-fragment]
  (str routes/base-path route-fragment))

(defn scope-key
  [scope]
  [(:topic scope) (:id scope)])

(defn scope-keys
  [scopes]
  (mapv scope-key scopes))

(defn change-actor-id
  [change]
  (or (:actor/id change)
      (get-in change [:actor :user/id])))

(defn change-actor-email
  [change]
  (or (:actor/email change)
      (get-in change [:actor :user/email])))

(defn assert-render-ctx
  [actual-ctx expected-render-opts]
  (is (= (:anti-forgery-token (ctx))
         (:anti-forgery-token actual-ctx)))
  (is (= (:biff/node (ctx))
         (:biff/node actual-ctx)))
  (is (= expected-render-opts
         (#'hh-live/render-options actual-ctx)))
  (is (= (ctx)
         (dissoc actual-ctx ::hh-live/render-options))))

;; -----------------------------------------------------------------------------
;; Constants / compiled model
;; -----------------------------------------------------------------------------

(deftest constants-test
  (is (= model/store-id hh-live/store-id))
  (is (= client-plumbing/app-scope hh-live/notification-scope)))

(deftest compiled-live-shape-test
  (is hh-live/compiled-live)
  (is (true? (:compiled? hh-live/compiled-live)))
  (is (seq hh-live/live-rules))
  (is (coll? hh-live/live-rules))

  (testing "the live graph has the Human Help topics we care about"
    (let [topics (set (map :when-topic hh-live/live-rules))]
      (doseq [topic [:request/created
                     :request/claimed
                     :request/unclaimed
                     :request/taken-over
                     :request/done
                     :request/cancelled
                     :clock/minute
                     :humanhelp-demo/reset]]
        (is (contains? topics topic)
            (str "Missing live topic: " topic))))))

(deftest compiled-scope-test
  (is (= {:topic :humanhelp/request-toolbar
          :id model/store-id
          :gesso.live/scope :request-toolbar
          :gesso.live/scope-label "Request toolbar"}
         (live/live-scope
          hh-live/compiled-live
          :request-toolbar
          model/store-id)))

  (is (= {:topic :humanhelp/request-list
          :id model/store-id
          :gesso.live/scope :request-list
          :gesso.live/scope-label "Request list"}
         (live/live-scope
          hh-live/compiled-live
          :request-list
          model/store-id))))

(deftest allow-demo-store-test
  (is (true? (hh-live/allow-demo-store? (ctx) model/store-id)))
  (is (false? (hh-live/allow-demo-store? (ctx) "other-store")))
  (is (false? (hh-live/allow-demo-store? (ctx) nil))))

;; -----------------------------------------------------------------------------
;; Fragment query contracts
;; -----------------------------------------------------------------------------

(deftest request-toolbar-query-test
  (testing "query merges model toolbar data with ctx, store id, and render user"
    (let [opts {:user user-owner
                :view-state {:search ""
                             :visible-revision (model/latest-revision (ctx))}}
          render-ctx (#'hh-live/with-render-options (ctx) opts)
          result (hh-live/request-toolbar-query render-ctx model/store-id)]
      (assert-render-ctx (:ctx result) opts)
      (is (= model/store-id (:store/id result)))
      (is (= user-owner (:user result)))
      (is (= (model/latest-revision (ctx)) (:latest-revision result)))
      (is (integer? (:open-count result)))
      (is (integer? (:pending-open-count result)))
      (is (false? (:stale? result)))
      (is (map? (:view-state result)))))

  (testing "toolbar query reports stale data when visible revision is behind"
    (let [visible-before (model/latest-revision (ctx))]
      (model/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "New pending toolbar request"})})
      (let [opts {:user user-owner
                  :view-state {:search ""
                               :visible-revision visible-before}}
            render-ctx (#'hh-live/with-render-options (ctx) opts)
            result (hh-live/request-toolbar-query render-ctx model/store-id)]
        (assert-render-ctx (:ctx result) opts)
        (is (true? (:stale? result)))
        (is (= 1 (:pending-open-count result)))))))

(deftest request-list-query-test
  (testing "query merges model board data with ctx, store id, and render user"
    (let [opts {:user user-owner
                :view-state {:search ""
                             :visible-revision (model/latest-revision (ctx))}}
          render-ctx (#'hh-live/with-render-options (ctx) opts)
          result (hh-live/request-list-query render-ctx model/store-id)]
      (assert-render-ctx (:ctx result) opts)
      (is (= model/store-id (:store/id result)))
      (is (= user-owner (:user result)))
      (is (= (model/latest-revision (ctx)) (:latest-revision result)))
      (is (vector? (:requests result)))
      (is (seq (:requests result)))
      (is (map? (:view-state result)))))

  (testing "query respects search"
    (model/create-request!
     (ctx)
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Front doors"
               :customer-name "Mina"})})
    (let [opts {:user user-owner
                :view-state {:search "purple mina seasonal"
                             :visible-revision (model/latest-revision (ctx))}}
          render-ctx (#'hh-live/with-render-options (ctx) opts)
          result (hh-live/request-list-query render-ctx model/store-id)]
      (assert-render-ctx (:ctx result) opts)
      (is (some #(= "Need a purple snow shovel" (:request/title %))
                (:requests result))))))

(deftest request-list-query-visible-revision-test
  (let [visible-before (model/latest-revision (ctx))
        {:keys [request revision]}
        (model/create-request!
         (ctx)
         {:user user-owner
          :input (valid-input {:title "Hidden from stale query"})})
        stale-opts {:user user-owner
                    :view-state {:search ""
                                 :visible-revision visible-before}}
        fresh-opts {:user user-owner
                    :view-state {:search ""
                                 :visible-revision revision}}
        stale-result (hh-live/request-list-query
                      (#'hh-live/with-render-options (ctx) stale-opts)
                      model/store-id)
        fresh-result (hh-live/request-list-query
                      (#'hh-live/with-render-options (ctx) fresh-opts)
                      model/store-id)]
    (is (not (contains? (set (map :request/id (:requests stale-result)))
                        (:request/id request))))
    (is (contains? (set (map :request/id (:requests fresh-result)))
                   (:request/id request)))))

;; -----------------------------------------------------------------------------
;; Fragment render / response / stream adapters
;; -----------------------------------------------------------------------------

(deftest render-functions-test
  (let [latest (model/latest-revision (ctx))
        toolbar (hh-live/request-toolbar-render
                 {:ctx (ctx)
                  :user user-owner
                  :view-state {:search ""
                               :visible-revision latest}
                  :open-count 2
                  :pending-open-count 0
                  :stale? false
                  :latest-revision latest})
        request-list (hh-live/request-list-render
                      {:ctx (ctx)
                       :user user-owner
                       :view-state {:search ""
                                    :visible-revision latest}
                       :requests (model/all-requests (ctx))
                       :latest-revision latest})]
    (is (= views/request-toolbar-dom-id (:id (attrs toolbar))))
    (is (= "request-toolbar" (:data-humanhelp-fragment (attrs toolbar))))
    (is (contains-text? toolbar "Requests"))

    (is (= views/request-list-dom-id (:id (attrs request-list))))
    (is (= "request-list" (:data-humanhelp-fragment (attrs request-list))))))

(deftest render-fragment-node-delegation-test
  (let [calls (atom [])
        opts {:user user-owner
              :view-state view-state}]
    (with-redefs [live/render-fragment-node
                  (fn [& args]
                    (swap! calls conj args)
                    [:rendered-fragment])]
      (is (= [:rendered-fragment]
             (hh-live/render-fragment-node
              (ctx)
              :request-toolbar
              opts)))
      (is (= 1 (count @calls)))
      (let [[compiled ctx' fragment-name id] (first @calls)]
        (is (= hh-live/compiled-live compiled))
        (is (= :request-toolbar fragment-name))
        (is (= model/store-id id))
        (assert-render-ctx ctx' opts)))))

(deftest render-fragment-response-delegation-test
  (let [calls (atom [])
        opts {:user user-owner
              :view-state view-state}]
    (with-redefs [live/render-fragment-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :headers {"content-type" "text/html; charset=utf-8"}
                     :body "ok"})]
      (is (= {:status 200
              :headers {"content-type" "text/html; charset=utf-8"}
              :body "ok"}
             (hh-live/render-fragment-response
              (ctx)
              :request-list
              opts)))
      (is (= 1 (count @calls)))
      (let [[compiled ctx' fragment-name id] (first @calls)]
        (is (= hh-live/compiled-live compiled))
        (is (= :request-list fragment-name))
        (is (= model/store-id id))
        (assert-render-ctx ctx' opts)))))

(deftest stream-response-delegation-test
  (let [calls (atom [])
        opts {:user user-owner
              :view-state view-state}]
    (with-redefs [live/start-fragment-stream!
                  (fn [& args]
                    (swap! calls conj args)
                    {:response {:status 200
                                :headers {"content-type" "text/event-stream"}
                                :body ::stream-body}
                     :ignored true})]
      (is (= {:status 200
              :headers {"content-type" "text/event-stream"}
              :body ::stream-body}
             (hh-live/stream-response
              ::live-system
              (ctx)
              :request-toolbar
              opts)))
      (is (= 1 (count @calls)))
      (let [[live-system compiled ctx' fragment-name id options] (first @calls)]
        (is (= ::live-system live-system))
        (is (= hh-live/compiled-live compiled))
        (is (= :request-toolbar fragment-name))
        (is (= model/store-id id))
        (assert-render-ctx ctx' opts)
        (is (= {:flow-options {:relieve? true}}
               options))))))

;; -----------------------------------------------------------------------------
;; Fragment URLs and live panels
;; -----------------------------------------------------------------------------

(deftest fragment-options-test
  (let [state {:search "garden rake"
               :visible-revision 3}
        toolbar (hh-live/fragment-options :request-toolbar state)
        request-list (hh-live/fragment-options :request-list state)]
    (testing "fragment URLs point at the mounted endpoints"
      (is (= (expected-path routes/request-toolbar-fragment-route)
             (path-of (:fragment-url toolbar))))
      (is (= (expected-path routes/request-toolbar-stream-route)
             (path-of (:stream-url toolbar))))
      (is (= (expected-path routes/request-list-fragment-route)
             (path-of (:fragment-url request-list))))
      (is (= (expected-path routes/request-list-stream-route)
             (path-of (:stream-url request-list)))))

    (testing "live fragment URLs do not bake board state into the URL"
      (is (no-query? (:fragment-url toolbar)))
      (is (no-query? (:stream-url toolbar)))
      (is (no-query? (:fragment-url request-list)))
      (is (no-query? (:stream-url request-list)))))

  (testing "unknown fragments throw useful ex-info"
    (try
      (hh-live/fragment-options :missing-fragment view-state)
      (is false "Expected fragment-options to throw.")
      (catch clojure.lang.ExceptionInfo e
        (is (str/includes? (ex-message e)
                           "Unknown Human Help live fragment"))
        (is (= :missing-fragment (:fragment (ex-data e))))
        (is (= [:request-toolbar :request-list]
               (:known-fragments (ex-data e))))))))

(deftest panel-helper-delegation-test
  (testing "toolbar panel delegates to model-fragment-panel with compiled model and fragment options"
    (let [calls (atom [])]
      (with-redefs [live/model-fragment-panel
                    (fn [& args]
                      (swap! calls conj args)
                      [:panel :toolbar])]
        (is (= [:panel :toolbar]
               (hh-live/request-toolbar-panel view-state)))
        (let [[compiled fragment-name id opts] (first @calls)]
          (is (= hh-live/compiled-live compiled))
          (is (= :request-toolbar fragment-name))
          (is (= model/store-id id))
          (is (= (hh-live/fragment-options :request-toolbar view-state)
                 opts))))))

  (testing "list panel delegates to model-fragment-panel with compiled model and fragment options"
    (let [calls (atom [])]
      (with-redefs [live/model-fragment-panel
                    (fn [& args]
                      (swap! calls conj args)
                      [:panel :list])]
        (is (= [:panel :list]
               (hh-live/request-list-panel view-state)))
        (let [[compiled fragment-name id opts] (first @calls)]
          (is (= hh-live/compiled-live compiled))
          (is (= :request-list fragment-name))
          (is (= model/store-id id))
          (is (= (hh-live/fragment-options :request-list view-state)
                 opts)))))))

(deftest page-panels-test
  (let [calls (atom [])]
    (with-redefs [hh-live/request-toolbar-panel
                  (fn []
                    (swap! calls conj :toolbar)
                    [:toolbar-panel])

                  hh-live/request-list-panel
                  (fn []
                    (swap! calls conj :list)
                    [:list-panel])]
      (is (= {:request-toolbar-panel [:toolbar-panel]
              :request-list-panel [:list-panel]}
             (hh-live/page-panels)))
      (is (= [:toolbar :list] @calls)))))

(deftest live-panel-browser-contract-test
  (testing "toolbar panel keeps the SSE/fetch trigger on the stable wrapper"
    (let [node (hh-live/request-toolbar-panel view-state)
          a (attrs node)]
      (is (= "sse" (:hx-ext a)))
      (is (= (expected-path routes/request-toolbar-stream-route)
             (path-of (:sse-connect a))))
      (is (= (expected-path routes/request-toolbar-fragment-route)
             (path-of (:hx-get a))))
      (is (no-query? (:sse-connect a)))
      (is (no-query? (:hx-get a)))
      (is (= (str "#" views/board-state-form-id)
             (:hx-include a)))
      (is (= (str "#" views/request-toolbar-dom-id)
             (:hx-target a)))
      (is (= "outerHTML" (:hx-swap a)))
      (is (str/includes? (:hx-trigger a) "sse:live-update"))
      (is (find-by-id node views/request-toolbar-dom-id))))

  (testing "request-list panel keeps the SSE/fetch trigger on the stable wrapper"
    (let [node (hh-live/request-list-panel view-state)
          a (attrs node)]
      (is (= "sse" (:hx-ext a)))
      (is (= (expected-path routes/request-list-stream-route)
             (path-of (:sse-connect a))))
      (is (= (expected-path routes/request-list-fragment-route)
             (path-of (:hx-get a))))
      (is (no-query? (:sse-connect a)))
      (is (no-query? (:hx-get a)))
      (is (= (str "#" views/board-state-form-id)
             (:hx-include a)))
      (is (= (str "#" views/request-list-dom-id)
             (:hx-target a)))
      (is (= "outerHTML show:none focus-scroll:false" (:hx-swap a)))
      (is (str/includes? (:hx-trigger a) "sse:live-update"))
      (is (find-by-id node views/request-list-dom-id)))))

;; -----------------------------------------------------------------------------
;; Invalidation graph
;; -----------------------------------------------------------------------------

(deftest request-created-expansion-test
  (let [r (request {:request/id "hh-req-4"
                    :request/number 4
                    :request/status :open})
        change (hh-live/request-created-change
                {:request r
                 :revision 4
                 :actor user-owner})
        scopes (live/expand-change hh-live/compiled-live (ctx) change)]
    (is (= [[:humanhelp/request-toolbar model/store-id]]
           (scope-keys scopes)))))

(deftest request-transition-expansion-test
  (let [current (request {:request/id "hh-req-4"
                          :request/number 4
                          :request/status :claimed})
        previous (request {:request/id "hh-req-4"
                           :request/number 4
                           :request/status :open})
        change (hh-live/request-transition-change
                {:action :claim
                 :request current
                 :previous previous
                 :revision 5
                 :actor user-helper})
        scopes (live/expand-change hh-live/compiled-live (ctx) change)]
    (is (= [[:humanhelp/request-toolbar model/store-id]
            [:humanhelp/request-list model/store-id]]
           (scope-keys scopes)))))

(deftest minute-tick-expansion-test
  (let [scopes (live/expand-change
                hh-live/compiled-live
                (ctx)
                (hh-live/minute-tick-change))]
    (is (= [[:humanhelp/request-list model/store-id]]
           (scope-keys scopes)))))

(deftest demo-reset-expansion-test
  (let [change (hh-live/demo-reset-change
                {:revision 3
                 :actor user-owner})
        scopes (live/expand-change hh-live/compiled-live (ctx) change)]
    (is (= [[:humanhelp/request-toolbar model/store-id]
            [:humanhelp/request-list model/store-id]]
           (scope-keys scopes)))))

;; -----------------------------------------------------------------------------
;; Change constructors
;; -----------------------------------------------------------------------------

(deftest request-created-change-test
  (let [r (request {:request/id "hh-req-4"
                    :request/number 4
                    :request/status :open})
        change (hh-live/request-created-change
                {:request r
                 :revision 4
                 :actor user-owner})]
    (is (= :request/created (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (= "hh-req-4" (:request/id change)))
    (is (= 4 (:request/number change)))
    (is (= :open (:request/status change)))
    (is (= 4 (:revision change)))
    (is (= "user-owner" (change-actor-id change)))
    (is (= "owner@example.com" (change-actor-email change)))))

(deftest request-transition-topic-test
  (is (= :request/claimed (hh-live/request-transition-topic :claim)))
  (is (= :request/unclaimed (hh-live/request-transition-topic :unclaim)))
  (is (= :request/taken-over (hh-live/request-transition-topic :take-over)))
  (is (= :request/done (hh-live/request-transition-topic :done)))
  (is (= :request/cancelled (hh-live/request-transition-topic :cancel)))

  (try
    (hh-live/request-transition-topic :explode)
    (is false "Expected request-transition-topic to throw.")
    (catch clojure.lang.ExceptionInfo e
      (is (str/includes? (ex-message e)
                         "Unknown Human Help request transition action"))
      (is (= :explode (:action (ex-data e)))))))

(deftest request-transition-change-test
  (let [r (request {:request/id "hh-req-4"
                    :request/number 4
                    :request/status :claimed
                    :request/claimed-by "user-helper"
                    :request/claimed-by-email "helper@example.com"})
        previous (request {:request/id "hh-req-4"
                           :request/number 4
                           :request/status :open})
        change (hh-live/request-transition-change
                {:action :claim
                 :request r
                 :previous previous
                 :revision 5
                 :actor user-helper})]
    (is (= :request/claimed (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (= "hh-req-4" (:request/id change)))
    (is (= 4 (:request/number change)))
    (is (= :claimed (:request/status change)))
    (is (= :open (:previous/status change)))
    (is (= :claim (:action change)))
    (is (= 5 (:revision change)))
    (is (= "user-helper" (change-actor-id change)))
    (is (= "helper@example.com" (change-actor-email change)))))

(deftest minute-tick-change-test
  (let [before (System/currentTimeMillis)
        change (hh-live/minute-tick-change)
        after (System/currentTimeMillis)]
    (is (= :clock/minute (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (integer? (:at-ms change)))
    (is (<= before (:at-ms change) after))))

(deftest demo-reset-change-test
  (let [change (hh-live/demo-reset-change
                {:revision 9
                 :actor user-owner})]
    (is (= :humanhelp-demo/reset (:topic change)))
    (is (= model/store-id (:id change)))
    (is (= model/store-id (:store/id change)))
    (is (= 9 (:revision change)))
    (is (= "user-owner" (change-actor-id change)))
    (is (= "owner@example.com" (change-actor-email change)))))

;; -----------------------------------------------------------------------------
;; Notification / toast helpers
;; -----------------------------------------------------------------------------

(deftest request-toast-description-test
  (is (= "Jon added request #4: Need a rake"
         (hh-live/request-toast-description
          {:request/customer-name "Jon"
           :request/number 4
           :request/title "Need a rake"})))

  (is (= "Someone added request #4: Need a rake"
         (hh-live/request-toast-description
          {:request/customer-name nil
           :request/number 4
           :request/title "Need a rake"})))

  (is (= "Jon added request #4"
         (hh-live/request-toast-description
          {:request/customer-name "Jon"
           :request/number 4
           :request/title ""}))))

(deftest send-new-request-toast-test
  (testing "without actor, toast is sent to the notification scope"
    (let [scope-calls (atom [])
          except-calls (atom [])]
      (with-redefs [client-plumbing/send-toast-to-scope!
                    (fn [scope toast]
                      (swap! scope-calls conj {:scope scope
                                               :toast toast})
                      {:sent 1
                       :toast toast})

                    client-plumbing/send-toast-to-scope-except-user!
                    (fn [& args]
                      (swap! except-calls conj args)
                      {:sent 0})]
        (let [result (hh-live/send-new-request-toast!
                      {:request/customer-name "Jon"
                       :request/number 4
                       :request/title "Need a rake"})]
          (is (= 1 (:sent result)))
          (is (= 1 (count @scope-calls)))
          (is (empty? @except-calls))
          (is (= hh-live/notification-scope
                 (:scope (first @scope-calls))))
          (is (= {:variant :info
                  :title "New request received"
                  :description "Jon added request #4: Need a rake"}
                 (:toast (first @scope-calls))))))))

  (testing "with actor, toast excludes that user's connected clients"
    (let [scope-calls (atom [])
          except-calls (atom [])]
      (with-redefs [client-plumbing/send-toast-to-scope!
                    (fn [& args]
                      (swap! scope-calls conj args)
                      {:sent 0})

                    client-plumbing/send-toast-to-scope-except-user!
                    (fn [scope excluded-user-id toast]
                      (swap! except-calls conj {:scope scope
                                                :excluded-user-id excluded-user-id
                                                :toast toast})
                      {:sent 1
                       :toast toast})]
        (let [result (hh-live/send-new-request-toast!
                      {:request/customer-name "Jon"
                       :request/number 4
                       :request/title "Need a rake"}
                      {:actor user-owner})]
          (is (= 1 (:sent result)))
          (is (empty? @scope-calls))
          (is (= 1 (count @except-calls)))
          (is (= hh-live/notification-scope
                 (:scope (first @except-calls))))
          (is (= "user-owner"
                 (:excluded-user-id (first @except-calls))))
          (is (= {:variant :info
                  :title "New request received"
                  :description "Jon added request #4: Need a rake"}
                 (:toast (first @except-calls)))))))))

(deftest send-reset-toast-test
  (let [calls (atom [])]
    (with-redefs [client-plumbing/send-toast-to-scope!
                  (fn [scope toast]
                    (swap! calls conj {:scope scope
                                       :toast toast})
                    {:sent 1
                     :toast toast})]
      (let [result (hh-live/send-reset-toast!)]
        (is (= 1 (:sent result)))
        (is (= 1 (count @calls)))
        (is (= hh-live/notification-scope
               (:scope (first @calls))))
        (is (= {:variant :info
                :title "Demo reset"
                :description "The Human Help request board was reset."}
               (:toast (first @calls))))))))

(deftest send-request-action-error-toast-test
  (let [calls (atom [])]
    (with-redefs [client-plumbing/send-toast-to-scope!
                  (fn [scope toast]
                    (swap! calls conj {:scope scope
                                       :toast toast})
                    {:sent 1
                     :toast toast})]
      (hh-live/send-request-action-error-toast! "Nope.")
      (hh-live/send-request-action-error-toast! nil)

      (is (= hh-live/notification-scope
             (:scope (first @calls))))
      (is (= {:variant :danger
              :title "Request not updated"
              :description "Nope."}
             (:toast (first @calls))))
      (is (= "That request action could not be completed."
             (get-in (second @calls) [:toast :description]))))))

;; -----------------------------------------------------------------------------
;; notify!
;; -----------------------------------------------------------------------------

(deftest notify-test
  (let [calls (atom [])
        live-system ::live-system
        change {:topic :request/created
                :id model/store-id
                :store/id model/store-id}]
    (with-redefs [live/submit-expanded!
                  (fn [& args]
                    (swap! calls conj args)
                    {:submitted true})]
      (is (= {:submitted true}
             (hh-live/notify! live-system (ctx) change)))
      (is (= [[live-system (ctx) change]]
             @calls)))))

;; -----------------------------------------------------------------------------
;; Integration-ish render checks without transport
;; -----------------------------------------------------------------------------

(deftest render-fragment-node-integration-test
  (let [latest (model/latest-revision (ctx))
        toolbar (hh-live/render-fragment-node
                 (ctx)
                 :request-toolbar
                 {:user user-owner
                  :view-state {:search ""
                               :visible-revision latest}})
        request-list (hh-live/render-fragment-node
                      (ctx)
                      :request-list
                      {:user user-owner
                       :view-state {:search ""
                                    :visible-revision latest}})]
    (is (= views/request-toolbar-dom-id (:id (attrs toolbar))))
    (is (contains-text? toolbar "Requests"))

    (is (= views/request-list-dom-id (:id (attrs request-list))))
    (is (or (contains-text? request-list "Need help")
            (contains-text? request-list "No requests")))))

(deftest render-fragment-response-integration-test
  (let [latest (model/latest-revision (ctx))
        toolbar-response (hh-live/render-fragment-response
                          (ctx)
                          :request-toolbar
                          {:user user-owner
                           :view-state {:search ""
                                        :visible-revision latest}})
        list-response (hh-live/render-fragment-response
                       (ctx)
                       :request-list
                       {:user user-owner
                        :view-state {:search ""
                                     :visible-revision latest}})]
    (is (html-response? toolbar-response))
    (is (body-contains? toolbar-response views/request-toolbar-dom-id))

    (is (html-response? list-response))
    (is (body-contains? list-response views/request-list-dom-id))))

(deftest stale-render-integration-test
  (let [visible-before (model/latest-revision (ctx))
        {:keys [request revision]}
        (model/create-request!
         (ctx)
         {:user user-owner
          :input (valid-input {:title "Hidden live list target"})})
        stale-toolbar (hh-live/render-fragment-response
                       (ctx)
                       :request-toolbar
                       {:user user-helper
                        :view-state {:search ""
                                     :visible-revision visible-before}})
        stale-list (hh-live/render-fragment-response
                    (ctx)
                    :request-list
                    {:user user-helper
                     :view-state {:search ""
                                  :visible-revision visible-before}})
        fresh-list (hh-live/render-fragment-response
                    (ctx)
                    :request-list
                    {:user user-helper
                     :view-state {:search ""
                                  :visible-revision revision}})]
    (is (html-response? stale-toolbar))
    (is (html-response? stale-list))
    (is (html-response? fresh-list))

    (is (body-contains? stale-toolbar "+1 new"))
    (is (body-contains? stale-toolbar "New request data is available"))

    (is (not (body-contains? stale-list (:request/title request))))
    (is (body-contains? fresh-list (:request/title request)))))
