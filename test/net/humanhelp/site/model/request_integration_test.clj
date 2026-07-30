(ns net.humanhelp.site.model.request-integration-test
  "Real XTDB integration coverage for the rewritten Request create/read boundary.

   This suite deliberately keeps one end-to-end test seam that does not stub
   Request Graph reads, gesso.model transaction execution, Gesso Live, or XTDB.

   Prerequisite Organization, Location, and User documents are seeded with
   canonical model commands through gesso.model.tx. Request creation then
   exercises the current architecture:

     request/plan-create-request
       -> gesso.model.tx/transact!
       -> Gesso Live
       -> XTDB
       -> request/request

   The second test carries the committed Request through the actual site
   request-page handler so route parameter parsing and the public Request/User
   read boundaries remain covered."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.live.core :as live]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [malli.registry :as mr]
   [net.humanhelp.site.app :as site.app]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.organization.domain :as organization.domain]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.model.user.domain :as user.domain]
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

(def request-time
  (Instant/parse
   "2026-07-24T12:01:00Z"))

(def request-content
  {:title
   "Integration test request"

   :details
   "Created through the real Request transaction path."

   :location-detail
   "Integration test location"})

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

(def model-schema
  (merge
   user/schema
   organization/schema
   request/schema))

(def malli-opts
  {:registry
   (mr/composite-registry
    m/default-registry
    model-schema)})

(defn- live-rules
  []
  [{:when-topic
    :request

    :expand
    (fn [_ctx change]
      [change])}])

(defn- with-runtime
  [f]
  (with-open
   [node
    (xtn/start-node
     {})]

    (let [live-system
          (live/create
           {:rules
            (live-rules)

            :dispatch-options
            {:threads
             1

             :queue-size
             32}})

          ctx
          {:biff/node
           node

           :biff/conn
           node

           :xtdb/node
           node

           :biff/malli-opts
           malli-opts

           :gesso.live/system
           live-system}]

      (reset!
       !runtime
       {:node
        node

        :live-system
        live-system

        :ctx
        ctx})

      (try
        (f)

        (finally
          (reset!
           !runtime
           nil)

          (live/close!
           live-system))))))

(use-fixtures
 :each
 with-runtime)

;; =============================================================================
;; Prerequisite model commands
;; =============================================================================

(defn- organization-command
  []
  (organization.domain/create-organization-command
   {:id
    organization-id

    :name
    "Request Integration Test Organization"

    :now
    fixture-time}))

(defn- location-command
  []
  (organization.domain/create-location-command
   {:id
    location-id

    :organization-id
    organization-id

    :parent-scope
    (organization/organization-scope
     organization-id)

    :name
    "Request Integration Test Location"

    :now
    fixture-time}))

(defn- user-command
  []
  (user.domain/create-user-command
   {:id
    user-id

    :email
    "request-integration@example.com"

    :email-verified?
    true

    :display-name
    "Request Integration User"

    :now
    fixture-time}))

(defn- seed-prerequisites!
  []
  (let [{:keys
         [ctx]}
        (runtime)]
    (model.tx/transact!
     ctx
     {:commands
      [(organization-command)
       (location-command)
       (user-command)]

      :emit
      false})))

;; =============================================================================
;; Request create/commit helper
;; =============================================================================

(defn- transaction-plan
  [{:keys
    [transaction-fragment
     transaction-options]}]
  (merge
   transaction-fragment
   transaction-options))

(defn- create-request!
  [ctx]
  (let [ctx
        (assoc
         ctx

         :current-user/id
         user-id

         :biff.fx/now
         request-time

         :biff.fx/seed
         7)

        planned
        (request/plan-create-request
         ctx
         {:organization-id
          organization-id

          :location-id
          location-id

          :content
          request-content})

        transaction
        (model.tx/transact!
         ctx
         (transaction-plan
          planned))]

    (assoc
     (:result
      planned)

     :transaction
     transaction)))

(defn- consistency-ctx
  [fallback transaction]
  (or
   (:ctx
    transaction)

   fallback))

;; =============================================================================
;; Diagnostics
;; =============================================================================

(defn- raw-request-rows
  [request-id]
  (let [{:keys
         [node]}
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

  (let [{:keys
         [ctx]}
        (runtime)

        create-result
        (create-request!
         ctx)

        created
        (:request
         create-result)

        transaction
        (:transaction
         create-result)

        request-id
        (request/request-id
         created)

        read-ctx
        (consistency-ctx
         ctx
         transaction)]

    (testing
     "the current Request plan commits successfully"
      (is
       (=
        :committed
        (:commit/status
         transaction)))

      (is
       (uuid?
        request-id)))

    (testing
     "the committed Request physically exists in XTDB"
      (let [rows
            (raw-request-rows
             request-id)]
        (is
         (=
          1
          (count
           rows))
         (str
          "Request create committed, but XTDB did not contain exactly "
          "one current Request row. request-id="
          request-id
          " rows="
          (pr-str
           rows)))))

    (testing
     "the public Request read immediately finds the committed document"
      (let [loaded
            (request/request
             read-ctx
             request-id)]

        (is
         (some?
          loaded)
         (str
          "XTDB contains the newly-created Request, but "
          "request/request returned nil. request-id="
          request-id))

        (when
         loaded
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

  (let [{:keys
         [ctx]}
        (runtime)

        create-result
        (create-request!
         ctx)

        created
        (:request
         create-result)

        request-id
        (request/request-id
         created)

        read-ctx
        (consistency-ctx
         ctx
         (:transaction
          create-result))

        rendered
        (fn [_ctx props]
          {:status
           200

           :body
           props})]

    (testing
     "the site Request handler accepts Reitit keyword path params"
      (with-redefs
       [await-help/page
        rendered

        request-finished/page
        rendered]

        (let [response
              (site.app/request-page
               (assoc
                read-ctx

                :session
                {:uid
                 user-id}

                :path-params
                {:request-id
                 (str
                  request-id)}))]

          (is
           (=
            200
            (:status
             response))
           (str
            "A persisted Request owned by the signed-in User should render, "
            "not redirect. request-id="
            request-id
            " response="
            (pr-str
             response)))

          (when
           (=
            200
            (:status
             response))
            (is
             (=
              request-id
              (some->
               response
               :body
               :request-document
               request/request-id)))))))

    (testing
     "the handler also accepts string-keyed path params"
      (with-redefs
       [await-help/page
        rendered

        request-finished/page
        rendered]

        (let [response
              (site.app/request-page
               (assoc
                read-ctx

                :session
                {:uid
                 user-id}

                :path-params
                {"request-id"
                 (str
                  request-id)}))]

          (is
           (=
            200
            (:status
             response))
           (str
            "String-keyed request-id should also render. request-id="
            request-id
            " response="
            (pr-str
             response))))))))
