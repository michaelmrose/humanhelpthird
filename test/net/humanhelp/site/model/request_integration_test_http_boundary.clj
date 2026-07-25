(ns net.humanhelp.site.model.request-integration-test
  "Real XTDB integration coverage for the Request create/read boundary.

   This namespace intentionally does not stub Graph, model.fx, Gesso Live
   transaction execution, HoneySQL formatting, or XTDB. It seeds only the
   prerequisite Organization, Location, and User documents, then exercises the
   public Request API end to end:

     request/create-request
       -> shared model.fx
       -> Gesso Live transaction execution
       -> XTDB
       -> request/request-document

   The raw XTDB assertion distinguishes persistence failure from a Request
   Graph/read failure."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.graph :as graph]
   [gesso.live.core :as live]
   [malli.core :as m]
   [malli.registry :as mr]
   [net.humanhelp.schema :as schema]
   [net.humanhelp.site.app :as site.app]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.organization.domain :as organization.domain]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.views.get-help.await-help :as await-help]
   [net.humanhelp.site.views.get-help.request-finished :as request-finished]
   [xtdb.api :as xt]
   [xtdb.node :as xtn])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed integration-test identities
;; =============================================================================

(def organization-id
  (UUID/fromString
   "91000000-0000-0000-0000-000000000001"))

(def location-id
  (UUID/fromString
   "92000000-0000-0000-0000-000000000001"))

(def user-id
  (UUID/fromString
   "93000000-0000-0000-0000-000000000001"))

(def fixture-time
  (Instant/parse
   "2026-07-24T12:00:00Z"))

(def request-content
  {:title "Integration test request"
   :details "Created through the real Request transaction path."
   :location-detail "Integration test location"})

;; =============================================================================
;; Real test runtime
;; =============================================================================

(defonce ^:private !runtime
  (atom nil))

(defn- runtime
  []
  (or
   @!runtime
   (throw
    (ex-info
     "Request integration-test runtime is not initialized."
     {}))))

(def model-modules
  [user/module
   organization/module
   request/module])

(def malli-opts
  {:registry
   (mr/composite-registry
    m/default-registry
    schema/schema)})

(defn- live-rules
  []
  [{:when-topic :request
    :expand
    (fn [_ctx change]
      [change])}])

(defn- with-runtime
  [f]
  (with-open [node (xtn/start-node {})]
    (let [live-system
          (live/create
           {:rules
            (live-rules)

            :dispatch-options
            {:threads 1
             :queue-size 32}})

          ctx
          {:biff/node node
           :biff/conn node
           :xtdb/node node

           :biff/modules
           model-modules

           :biff.fx/handlers
           (merge
            graph/fx-handlers
            model.fx/handlers)

           :biff/malli-opts
           malli-opts

           :gesso.live/system
           live-system}]

      (reset!
       !runtime
       {:node node
        :live-system live-system
        :ctx ctx})

      (try
        (f)
        (finally
          (reset! !runtime nil)
          (live/close! live-system))))))

(use-fixtures
 :each
 with-runtime)

;; =============================================================================
;; Prerequisite persisted documents
;; =============================================================================

(defn- command-document
  [command]
  (model.common/command-document
   command))

(defn- organization-document
  []
  (command-document
   (organization.domain/create-organization-command
    {:id organization-id
     :name "Request Integration Test Organization"
     :now fixture-time})))

(defn- location-document
  []
  (command-document
   (organization.domain/create-location-command
    {:id location-id
     :organization-id organization-id
     :parent-scope
     (organization/organization-scope
      organization-id)
     :name "Request Integration Test Location"
     :now fixture-time})))

(defn- user-document
  []
  (identity/new-user
   {:id user-id
    :email "request-integration@example.com"
    :email-verified? true
    :display-name "Request Integration User"
    :now fixture-time}))

(defn- seed-prerequisites!
  []
  (let [{:keys [node]}
        (runtime)]
    (xt/execute-tx
     node
     [[:put-docs
       :organization
       (organization-document)]

      [:put-docs
       :location
       (location-document)]

      [:put-docs
       :user
       (user-document)]])))

;; =============================================================================
;; Diagnostics
;; =============================================================================

(defn- raw-request-rows
  [request-id]
  (let [{:keys [node]}
        (runtime)]
    (xt/q
     node
     ["SELECT * FROM request WHERE _id = ?"
      request-id])))

;; =============================================================================
;; Integration contract
;; =============================================================================

(deftest created-request-is-persisted-and-readable-test
  (seed-prerequisites!)

  (let [{:keys [ctx]}
        (runtime)

        create-result
        (request/create-request
         (assoc
          ctx
          :current-user/id
          user-id)
         {:organization-id organization-id
          :location-id location-id
          :content request-content})

        created
        (:request create-result)

        request-id
        (request/request-id
         created)]

    (testing "the public create workflow commits successfully"
      (is
       (=
        :committed
        (get-in
         create-result
         [:transaction
          :commit/status])))

      (is
       (uuid?
        request-id)))

    (testing "the committed Request physically exists in XTDB"
      (let [rows
            (raw-request-rows
             request-id)]
        (is
         (=
          1
          (count rows))
         (str
          "Request create reported success, but XTDB did not contain exactly "
          "one current Request row. request-id="
          request-id
          " rows="
          (pr-str rows)))))

    (testing "the normal public Request read can immediately find it"
      (let [loaded
            (request/request-document
             ctx
             request-id)]
        (is
         (some?
          loaded)
         (str
          "XTDB contains the newly-created Request, but "
          "request/request-document returned nil. request-id="
          request-id))

        (when loaded
          (is
           (=
            request-id
            (request/request-id
             loaded)))

          (is
           (request/requested-by-user?
            loaded
            user-id))

          (is
           (=
            organization-id
            (request/organization-id
             loaded)))

          (is
           (=
            location-id
            (request/location-id
             loaded))))))))

(deftest created-request-is-reachable-through-site-request-page-test
  (seed-prerequisites!)

  (let [{:keys [ctx]}
        (runtime)

        create-result
        (request/create-request
         (assoc
          ctx
          :current-user/id
          user-id)
         {:organization-id organization-id
          :location-id location-id
          :content request-content})

        created
        (:request create-result)

        request-id
        (request/request-id
         created)

        rendered
        (fn [_ctx props]
          {:status 200
           :body props})]

    (testing "the site Request handler accepts normal Reitit keyword path params"
      (with-redefs
       [await-help/page rendered
        request-finished/page rendered]
        (let [response
              (site.app/request-page
               (assoc
                ctx
                :session
                {:uid user-id}

                ;; Reitit exposes named path parameters as keyword keys.
                :path-params
                {:request-id
                 (str request-id)}))]

          (is
           (=
            200
            (:status response))
           (str
            "A persisted Request owned by the signed-in User should render, "
            "not redirect. request-id="
            request-id
            " response="
            (pr-str response)))

          (when
           (=
            200
            (:status response))
            (is
             (=
              request-id
              (some->
               response
               :body
               :request-document
               request/request-id)))))))

    (testing "the handler also accepts string-keyed path params"
      (with-redefs
       [await-help/page rendered
        request-finished/page rendered]
        (let [response
              (site.app/request-page
               (assoc
                ctx
                :session
                {:uid user-id}

                :path-params
                {"request-id"
                 (str request-id)}))]

          (is
           (=
            200
            (:status response))
           (str
            "String-keyed request-id should also render. request-id="
            request-id
            " response="
            (pr-str response))))))))

