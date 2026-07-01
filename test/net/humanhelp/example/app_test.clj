(ns net.humanhelp.example.app-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.example.app :as app]
   [net.humanhelp.example.live :as app-live]
   [net.humanhelp.example.model :as model]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.example.views :as views]
   [net.humanhelp.middleware :as mid]
   [xtdb.node :as xtn]))

;; -----------------------------------------------------------------------------
;; XTDB fixture
;; -----------------------------------------------------------------------------

(defonce !ctx-base
  (atom nil))

(defn ctx-base
  []
  (or @!ctx-base
      (throw
       (ex-info "app-test ctx has not been initialized."
                {}))))

(defn xtdb-fixture
  [f]
  (with-open [node (xtn/start-node)]
    (reset! !ctx-base {:biff/node node
                       :biff/conn node
                       :xtdb/node node})
    (try
      (f)
      (finally
        (reset! !ctx-base nil)))))

(defn base-ctx
  []
  (merge
   (ctx-base)
   {:anti-forgery-token "test-token"
    :gesso.live/system ::live-system
    :user/id "owner"
    :user/email "owner@example.com"
    :session {:uid "owner"
              :email "owner@example.com"}}))

(defn helper-ctx
  []
  (assoc (base-ctx)
         :user/id "helper"
         :user/email "helper@example.com"
         :session {:uid "helper"
                   :email "helper@example.com"}))

(defn reset-model-fixture
  [f]
  (model/reset-demo-state! (base-ctx))
  (try
    (f)
    (finally
      (model/reset-demo-state! (base-ctx)))))

(use-fixtures :once xtdb-fixture)
(use-fixtures :each reset-model-fixture)

;; -----------------------------------------------------------------------------
;; Hiccup helpers
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

(defn text-nodes
  [tree]
  (filter string? (hiccup-seq tree)))

(defn contains-text?
  [tree text]
  (boolean
   (some #(str/includes? % text)
         (text-nodes tree))))

(defn find-by-id
  [tree id]
  (some
   (fn [node]
     (when (= id (:id (attrs node)))
       node))
   (filter node? (hiccup-seq tree))))

;; -----------------------------------------------------------------------------
;; Response helpers
;; -----------------------------------------------------------------------------

(defn response-body
  [response]
  (or (:body response) ""))

(defn html-response?
  [response]
  (and (= 200 (:status response))
       (str/starts-with?
        (or (get-in response [:headers "content-type"]) "")
        "text/html")
       (string? (:body response))))

(defn body-contains?
  [response text]
  (str/includes? (response-body response) text))

(defn response-oob?
  [response dom-id]
  (and (body-contains? response (str "id=\"" dom-id "\""))
       (body-contains? response "hx-swap-oob=\"outerHTML\"")))

(defn body-has-input-value?
  [response name value]
  (body-contains?
   response
   (str "name=\"" name "\" value=\"" value "\"")))

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

(defn route-entry?
  [route-tree route method handler]
  (boolean
   (some #(= [route {method handler}] %)
         (tree-seq
          (fn [x]
            (and (sequential? x)
                 (not (string? x))))
          seq
          route-tree))))

;; -----------------------------------------------------------------------------
;; Request helpers
;; -----------------------------------------------------------------------------

(defn valid-create-params
  [overrides]
  (merge
   {"title" "Need help finding a rake"
    "area" "Garden"
    "details" "Looking for a sturdy rake for leaves."
    "customer-name" "Jon"}
   overrides))

(defn ctx-with-params
  [ctx params]
  (assoc ctx :params params))

(defn open-seed-request
  [ctx]
  (first
   (filter #(= :open (:request/status %))
           (model/all-requests ctx))))

(defn request-by-title
  [ctx title]
  (first
   (filter #(= title (:request/title %))
           (model/all-requests ctx))))

(defn owner-ctx-for
  [request]
  (assoc (base-ctx)
         :user/id (:request/customer-user-id request)
         :user/email (str (:request/customer-user-id request)
                          "@example.com")
         :session {:uid (:request/customer-user-id request)
                   :email (str (:request/customer-user-id request)
                               "@example.com")}))

(defn recording-fn
  [calls return-value]
  (fn [& args]
    (swap! calls conj args)
    return-value))

;; -----------------------------------------------------------------------------
;; Page boundary / current-user regression
;; -----------------------------------------------------------------------------

(deftest app-page-passes-real-email-to-page-shell-test
  (let [panels {:request-toolbar-panel [:div {:id views/request-toolbar-dom-id}
                                        "toolbar"]
                :request-list-panel [:div {:id views/request-list-dom-id}
                                     "list"]}]
    (with-redefs [app-live/page-panels
                  (fn [_view-state]
                    panels)

                  views/page
                  (fn [_ctx opts]
                    opts)

                  client-plumbing/current-user-id
                  (fn [_ctx]
                    "owner")

                  client-plumbing/current-user-email
                  (fn [_ctx]
                    "owner@example.com")]
      (let [result (app/app-page (base-ctx))]
        (is (= {:user/id "owner"
                :user/email "owner@example.com"}
               (:user result)))
        (is (= (:request-toolbar-panel panels)
               (:request-toolbar-panel result)))
        (is (= (:request-list-panel panels)
               (:request-list-panel result)))))))

(deftest app-page-does-not-treat-uuid-as-email-test
  (let [uuid "aef38467-ef20-4f58-b9ab-f9c0fb4c9bb2"]
    (with-redefs [app-live/page-panels
                  (fn [_view-state]
                    {:request-toolbar-panel [:div {:id views/request-toolbar-dom-id}]
                     :request-list-panel [:div {:id views/request-list-dom-id}]})

                  views/page
                  (fn [_ctx opts]
                    opts)

                  client-plumbing/current-user-id
                  (fn [_ctx]
                    uuid)

                  client-plumbing/current-user-email
                  (fn [_ctx]
                    uuid)]
      (let [result (app/app-page
                    (merge
                     (ctx-base)
                     {:anti-forgery-token "test-token"
                      :gesso.live/system ::live-system
                      :session {:uid uuid}}))]
        (is (= uuid (get-in result [:user :user/id])))
        (is (nil? (get-in result [:user :user/email])))))))

(deftest app-page-normalizes-request-view-state-through-page-panels-test
  (let [seen-view-state (atom nil)]
    (with-redefs [app-live/page-panels
                  (fn [view-state]
                    (reset! seen-view-state view-state)
                    {:request-toolbar-panel [:div {:id views/request-toolbar-dom-id}]
                     :request-list-panel [:div {:id views/request-list-dom-id}]})

                  views/page
                  (fn [_ctx opts]
                    opts)]
      (let [ctx (ctx-with-params
                 (base-ctx)
                 {"q" ["" "garden"]
                  "visible-revision" "2"})
            result (app/app-page ctx)]
        (is (= "garden" (get-in result [:view-state :search])))
        (is (= 2 (get-in result [:view-state :visible-revision])))
        (is (= (:view-state result) @seen-view-state))))))

;; -----------------------------------------------------------------------------
;; Fragment and stream handlers
;; -----------------------------------------------------------------------------

(deftest fragment-handlers-delegate-to-live-rendering-test
  (let [calls (atom [])]
    (with-redefs [app-live/render-fragment-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :headers {"content-type" "text/html; charset=utf-8"}
                     :body "<div>fragment</div>"})]
      (let [ctx (ctx-with-params
                 (base-ctx)
                 {"q" "garden"
                  "visible-revision" "3"})]
        (is (html-response? (app/request-toolbar-fragment ctx)))
        (is (html-response? (app/request-list-fragment ctx)))

        (let [[toolbar-call list-call] @calls
              [toolbar-ctx toolbar-fragment toolbar-opts] toolbar-call
              [list-ctx list-fragment list-opts] list-call]
          (is (= ctx toolbar-ctx))
          (is (= ctx list-ctx))
          (is (= :request-toolbar toolbar-fragment))
          (is (= :request-list list-fragment))

          (is (= "garden" (get-in toolbar-opts [:view-state :search])))
          (is (= 3 (get-in toolbar-opts [:view-state :visible-revision])))
          (is (= toolbar-opts list-opts)))))))

(deftest search-requests-delegates-to-request-list-fragment-test
  (let [calls (atom [])]
    (with-redefs [app-live/render-fragment-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :headers {"content-type" "text/html; charset=utf-8"}
                     :body "<div>list</div>"})]
      (let [ctx (ctx-with-params
                 (base-ctx)
                 {"q" ["old" "rake"]
                  "visible-revision" (str (model/latest-revision (base-ctx)))})
            response (app/search-requests ctx)
            [ctx' fragment opts] (first @calls)]
        (is (html-response? response))
        (is (= ctx ctx'))
        (is (= :request-list fragment))
        (is (= "rake" (get-in opts [:view-state :search])))))))

(deftest stream-handlers-delegate-to-live-streams-test
  (let [calls (atom [])]
    (with-redefs [app-live/stream-response
                  (fn [& args]
                    (swap! calls conj args)
                    {:status 200
                     :body ::stream})]
      (let [ctx (base-ctx)]
        (is (= {:status 200
                :body ::stream}
               (app/request-toolbar-stream ctx)))
        (is (= {:status 200
                :body ::stream}
               (app/request-list-stream ctx)))

        (let [[toolbar-call list-call] @calls
              [toolbar-live-system toolbar-ctx toolbar-fragment toolbar-opts] toolbar-call
              [list-live-system list-ctx list-fragment list-opts] list-call]
          (is (= ::live-system toolbar-live-system))
          (is (= ::live-system list-live-system))
          (is (= ctx toolbar-ctx))
          (is (= ctx list-ctx))
          (is (= :request-toolbar toolbar-fragment))
          (is (= :request-list list-fragment))
          (is (= toolbar-opts list-opts)))))))

(deftest stream-handler-requires-live-system-test
  (try
    (app/request-list-stream {:a 1 :b 2})
    (is false "Expected missing live-system to throw.")
    (catch clojure.lang.ExceptionInfo e
      (is (str/includes? (ex-message e)
                         "Human Help requires :gesso.live/system"))
      (is (= #{:a :b}
             (:ctx-keys (ex-data e)))))))

(deftest create-request-dialog-fragment-test
  (let [response (app/create-request-dialog-fragment (base-ctx))]
    (is (html-response? response))
    (is (body-contains? response views/create-request-dialog-id))
    (is (body-contains? response "data-dialog-open=\"true\""))
    (is (body-contains? response "Create request"))
    (is (body-contains? response "owner@example.com"))))

;; -----------------------------------------------------------------------------
;; Create request
;; -----------------------------------------------------------------------------

(deftest create-request-validation-error-test
  (let [notified (atom [])
        toasts (atom [])]
    (with-redefs [app-live/notify!
                  (recording-fn notified {:submitted true})

                  app-live/send-new-request-toast!
                  (recording-fn toasts {:sent 1})]
      (let [ctx (ctx-with-params
                 (base-ctx)
                 {"title" ""
                  "area" ""})
            before-revision (model/latest-revision ctx)
            response (app/create-request! ctx)]
        (is (html-response? response))
        (is (= before-revision
               (model/latest-revision ctx)))

        (is (response-oob? response views/create-request-dialog-id))
        (is (body-contains? response "Create request"))
        (is (body-contains? response "data-dialog-open=\"true\""))
        (is (body-contains? response "Request"))
        (is (body-contains? response "A short request is required."))
        (is (body-contains? response "Area"))
        (is (body-contains? response "Choose or describe an area of the store."))

        (is (empty? @notified))
        (is (empty? @toasts))))))

(deftest create-request-success-test
  (let [notified (atom [])
        request-toasts (atom [])
        client-sends (atom [])
        ctx (assoc (base-ctx)
                   :params {"title" "Need gloves"
                            "area" "Garden"
                            "details" "Large gloves"
                            "customer-name" "Avery"})
        before-revision (model/latest-revision ctx)
        before-count (count (model/all-requests ctx))]
    (with-redefs [app-live/notify!
                  (fn [& args]
                    (swap! notified conj args)
                    {:submitted true})

                  app-live/send-new-request-toast!
                  (fn [& args]
                    (swap! request-toasts conj args)
                    {:sent 1})

                  client-plumbing/send-to-scope-except-user!
                  (fn [& args]
                    (swap! client-sends conj args)
                    {:sent 1})]
      (let [response (app/create-request! ctx)
            latest (model/latest-revision ctx)
            created (first
                     (filter #(= "Need gloves" (:request/title %))
                             (model/all-requests ctx)))]
        (is (html-response? response))
        (is (= (inc before-revision) latest))
        (is (= (inc before-count) (count (model/all-requests ctx))))

        (is created)
        (is (= :open (:request/status created)))
        (is (= "owner" (:request/customer-user-id created)))
        (is (= "Avery" (:request/customer-name created)))
        (is (= "Garden" (:request/area created)))
        (is (= "Large gloves" (:request/details created)))

        ;; Create no longer emits the model-backed :request/created live
        ;; invalidation and no longer calls the old toast helper. The creator
        ;; is updated by this POST response; observers are notified through
        ;; client plumbing, excluding the creator.
        (is (empty? @notified))
        (is (empty? @request-toasts))
        (is (= 1 (count @client-sends)))

        (let [[scope excluded-user-id fragment-fn] (first @client-sends)]
          (is (= app-live/notification-scope scope))
          (is (= "owner" excluded-user-id))
          (is (fn? fragment-fn)))

        ;; The creator gets the refresh-equivalent board update immediately.
        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))
        (is (response-oob? response views/create-request-dialog-id))
        (is (response-oob? response views/board-state-form-id))

        (is (body-contains? response "Request created"))
        (is (body-contains? response "Need gloves"))
        (is (not (body-contains? response "+1 new")))
        (is (not (body-contains? response
                                  "New request data is available")))

        (is (body-contains?
             response
             (str "name=\"" routes/visible-revision-param
                  "\" value=\"" latest "\"")))))))

;; -----------------------------------------------------------------------------
;; Request list interactions
;; -----------------------------------------------------------------------------

(deftest refresh-requests-test
  (let [ctx (base-ctx)
        visible-before (model/latest-revision ctx)
        {:keys [request revision]}
        (model/create-request!
         ctx
         {:user {:user/id "creator"
                 :user/email "creator@example.com"}
          :input {:title "New hidden request"
                  :area "Garden"
                  :details nil
                  :customer-name "Creator"}})
        response (app/refresh-requests!
                  (ctx-with-params
                   ctx
                   {"q" "garden"
                    "visible-revision" (str visible-before)}))]
    (is request)
    (is (html-response? response))
    (is (response-oob? response views/request-toolbar-dom-id))
    (is (response-oob? response views/request-list-dom-id))
    (is (response-oob? response views/board-state-form-id))
    (is (body-contains? response "New hidden request"))
    (is (body-has-input-value? response
                               routes/visible-revision-param
                               (str revision)))))

(deftest request-list-fragment-respects-visible-revision-test
  (let [ctx (base-ctx)
        visible-before (model/latest-revision ctx)
        {:keys [request revision]}
        (model/create-request!
         ctx
         {:user {:user/id "creator"
                 :user/email "creator@example.com"}
          :input {:title "Hidden until refreshed"
                  :area "Garden"
                  :details nil
                  :customer-name "Creator"}})
        stale-response (app/request-list-fragment
                        (ctx-with-params
                         ctx
                         {"q" ""
                          "visible-revision" (str visible-before)}))
        fresh-response (app/request-list-fragment
                        (ctx-with-params
                         ctx
                         {"q" ""
                          "visible-revision" (str revision)}))]
    (is request)
    (is (html-response? stale-response))
    (is (html-response? fresh-response))
    (is (not (body-contains? stale-response "Hidden until refreshed")))
    (is (body-contains? fresh-response "Hidden until refreshed"))))

(deftest search-requests-response-test
  (let [ctx (base-ctx)
        latest (model/latest-revision ctx)
        rake-response (app/search-requests
                       (ctx-with-params
                        ctx
                        {"q" "rake"
                         "visible-revision" (str latest)}))
        clear-response (app/search-requests
                        (ctx-with-params
                         ctx
                         {"q" ""
                          "visible-revision" (str latest)}))]
    (is (html-response? rake-response))
    (is (body-contains? rake-response views/request-list-dom-id))
    (is (body-contains? rake-response "Need help finding a rake"))

    (is (html-response? clear-response))
    (is (body-contains? clear-response views/request-list-dom-id))
    (is (body-contains? clear-response "Need help finding a rake"))))

;; -----------------------------------------------------------------------------
;; Lifecycle actions
;; -----------------------------------------------------------------------------

(deftest claim-request-success-test
  (let [ctx (base-ctx)
        request (open-seed-request ctx)
        notified (atom [])]
    (with-redefs [app-live/notify!
                  (recording-fn notified {:submitted true})]
      (let [response (app/claim-request!
                      (assoc (helper-ctx)
                             :path-params {:request-id (:request/id request)}
                             :params {"visible-revision"
                                      (str (model/latest-revision ctx))}))
            updated (model/request-by-id ctx (:request/id request))]
        (is (html-response? response))
        (is (= :claimed (:request/status updated)))
        (is (= "helper" (:request/claimed-by updated)))

        (is (= 1 (count @notified)))
        (let [[live-system _ctx change] (first @notified)]
          (is (= ::live-system live-system))
          (is (= :request/claimed (:topic change)))
          (is (= :claim (:action change)))
          (is (= (:request/id request)
                 (:request/id change))))

        (is (response-oob? response views/request-toolbar-dom-id))
        (is (response-oob? response views/request-list-dom-id))
        (is (response-oob? response views/board-state-form-id))))))

(deftest claim-request-error-test
  (let [ctx (base-ctx)
        request (open-seed-request ctx)
        owner-ctx (owner-ctx-for request)
        before (model/request-by-id ctx (:request/id request))
        notified (atom [])]
    (with-redefs [app-live/notify!
                  (recording-fn notified {:submitted true})]
      (let [response (app/claim-request!
                      (assoc owner-ctx
                             :path-params {:request-id (:request/id request)}))
            after (model/request-by-id ctx (:request/id request))]
        (is (html-response? response))
        (is (body-contains? response "Request not updated"))
        (is (= before after))
        (is (empty? @notified))))))

(deftest lifecycle-specific-handlers-test
  (testing "the public handlers can execute through the lifecycle transition boundary"
    (let [ctx (base-ctx)
          request (open-seed-request ctx)
          request-id (:request/id request)]
      (with-redefs [app-live/notify!
                    (fn [& _args]
                      {:submitted true})]
        (is (html-response?
             (app/claim-request!
              (assoc (helper-ctx)
                     :path-params {:request-id request-id}))))

        (is (html-response?
             (app/unclaim-request!
              (assoc (helper-ctx)
                     :path-params {:request-id request-id}))))

        (is (html-response?
             (app/take-over-request!
              (assoc (helper-ctx)
                     :path-params {:request-id request-id}))))

        (is (html-response?
             (app/mark-request-done!
              (assoc (helper-ctx)
                     :path-params {:request-id request-id})))))

      (let [request2 (:request
                      (model/create-request!
                       ctx
                       {:user {:user/id "owner-2"
                               :user/email "owner-2@example.com"}
                        :input {:title "Cancel me"
                                :area "Garden"
                                :details nil
                                :customer-name "Owner 2"}}))]
        (with-redefs [app-live/notify!
                      (fn [& _args]
                        {:submitted true})]
          (is (html-response?
               (app/cancel-request!
                (assoc (helper-ctx)
                       :path-params {:request-id (:request/id request2)})))))))))

;; -----------------------------------------------------------------------------
;; Reset
;; -----------------------------------------------------------------------------

(deftest reset-demo-test
  (let [ctx (base-ctx)
        {:keys [request]} (model/create-request!
                           ctx
                           {:user {:user/id "creator"
                                   :user/email "creator@example.com"}
                            :input {:title "Temporary request"
                                    :area "Garden"
                                    :details nil
                                    :customer-name "Creator"}})]
    (is request)
    (is (request-by-title ctx "Temporary request"))

    (let [notified (atom [])
          reset-toasts (atom [])]
      (with-redefs [app-live/notify!
                    (recording-fn notified {:submitted true})

                    app-live/send-reset-toast!
                    (recording-fn reset-toasts {:sent 1})]
        (let [response (app/reset-demo! ctx)]
          (is (html-response? response))
          (is (nil? (request-by-title ctx "Temporary request")))
          (is (= 3 (model/latest-revision ctx)))

          (is (= 1 (count @notified)))
          (let [[live-system ctx' change] (first @notified)]
            (is (= ::live-system live-system))
            (is (= ctx ctx'))
            (is (= :humanhelp-demo/reset (:topic change)))
            (is (= model/store-id (:id change)))
            (is (= model/store-id (:store/id change)))
            (is (= 3 (:revision change))))

          (is (= 1 (count @reset-toasts)))

          (is (response-oob? response views/request-toolbar-dom-id))
          (is (response-oob? response views/request-list-dom-id))
          (is (response-oob? response views/board-state-form-id))
          (is (body-contains? response "Demo reset")))))))

;; -----------------------------------------------------------------------------
;; Module
;; -----------------------------------------------------------------------------

(deftest module-shape-test
  (is (= app-live/live-rules (:live-rules app/module)))

  (let [route-tree (first (:routes app/module))]
    (is (= routes/base-path (first route-tree)))
    (is (= {:middleware [mid/wrap-signed-in]}
           (second route-tree)))))

(deftest module-routes-test
  (let [strings (route-strings (:routes app/module))]
    (doseq [route [routes/base-path
                   routes/request-toolbar-fragment-route
                   routes/request-list-fragment-route
                   routes/create-request-dialog-fragment-route
                   routes/request-toolbar-stream-route
                   routes/request-list-stream-route
                   routes/create-request-route
                   routes/refresh-requests-route
                   routes/search-requests-route
                   routes/claim-request-route
                   routes/unclaim-request-route
                   routes/take-over-request-route
                   routes/done-request-route
                   routes/cancel-request-route
                   routes/reset-demo-route]]
      (is (contains? strings route)
          (str "Missing route: " route)))))

(deftest module-handler-shape-test
  (let [routes (:routes app/module)]
    (is (route-entry? routes routes/create-request-route :post app/create-request!))
    (is (route-entry? routes routes/refresh-requests-route :post app/refresh-requests!))
    (is (route-entry? routes routes/search-requests-route :get app/search-requests))

    (is (route-entry? routes routes/request-toolbar-fragment-route :get app/request-toolbar-fragment))
    (is (route-entry? routes routes/request-list-fragment-route :get app/request-list-fragment))
    (is (route-entry? routes routes/create-request-dialog-fragment-route :get app/create-request-dialog-fragment))

    (is (route-entry? routes routes/request-toolbar-stream-route :get app/request-toolbar-stream))
    (is (route-entry? routes routes/request-list-stream-route :get app/request-list-stream))

    (is (route-entry? routes routes/claim-request-route :post app/claim-request!))
    (is (route-entry? routes routes/unclaim-request-route :post app/unclaim-request!))
    (is (route-entry? routes routes/take-over-request-route :post app/take-over-request!))
    (is (route-entry? routes routes/done-request-route :post app/mark-request-done!))
    (is (route-entry? routes routes/cancel-request-route :post app/cancel-request!))
    (is (route-entry? routes routes/reset-demo-route :post app/reset-demo!))))
