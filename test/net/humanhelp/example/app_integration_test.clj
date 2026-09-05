(ns net.humanhelp.example.app-integration-test
  "Clean-XTDB integration coverage for the globally selected HumanHelp proving app.

   This test deliberately keeps the production seams real: selected app routing,
   production fixture installation, production User/Organization/Request reads,
   Gesso model transactions, Gesso Live progression, and XTDB all execute for
   real. The fixture and Request create functions are wrapped only long enough to
   capture the exact values they return; their implementation is still invoked
   unchanged.

   Starting from an otherwise empty XTDB node, the globally selected app must:

     1. persist the authenticated production User and retain its commit
        progression for dependent reads;
     2. establish the fixed production Organization/Location fixtures and pass
        their composed post-commit progression into the app handler;
     3. create a real production Request through the selected app route;
     4. use that Request commit progression for the route's immediate
        read-your-writes success rendering;
     5. reread the Request through public production APIs at the correct
        Organization/Location only; and
     6. project the persisted Request's requestor through the production User
        model when the real example board/fragment renders.

   This is intentionally the integration seam that should catch a proving app
   which appears to work while discarding authoritative progression, querying a
   Request with the wrong location shape, or silently reviving parallel example
   model/User state."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.core :as live]
   [gesso.live.progression :as progression]
   [gesso.model.tx :as model.tx]
   [net.humanhelp :as humanhelp]
   [net.humanhelp.app :as selected-app]
   [net.humanhelp.example.app :as example.app]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.site.mock-data :as mock-data]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.model.user.domain :as user.domain]
   [reitit.ring :as ring]
   [xtdb.node :as xtn])
  (:import
   [java.time Instant]
   [java.util UUID]))

(def user-id
  (UUID/fromString
   "93000000-0000-0000-0000-000000000544"))

(def fixture-time
  (Instant/parse
   "2026-09-04T00:00:00Z"))

(def request-time
  (Instant/parse
   "2026-09-04T00:00:01Z"))

(def request-title
  "Selected example app integration request")

(def request-details
  "Created through the globally selected example app over production models.")

(def request-location-detail
  "North entrance")

(def user-display-name
  "Selected Example Integration User")

(defn- user-command
  []
  (user.domain/create-user-command
   {:id
    user-id

    :email
    "selected-example-integration@example.com"

    :email-verified?
    true

    :display-name
    user-display-name

    :now
    fixture-time}))

(defn- base-ctx
  [node live-system]
  {:biff.xtdb/node
   node

   :biff/malli-opts
   humanhelp/malli-opts

   :gesso.live/system
   live-system

   :biff.fx/now
   request-time

   :biff.fx/seed
   544})

(defn- selected-handler
  []
  (ring/ring-handler
   (ring/router
    (:routes
     selected-app/module))))

(defn- signed-in-request
  [ctx method uri]
  (assoc
   ctx
   :request-method method
   :uri uri
   :session {:uid user-id}))

(defn- seed-authenticated-user!
  [ctx]
  (model.tx/transact!
   ctx
   {:commands
    [(user-command)]

    :emit
    false}))

(defn- result-read-ctx
  "Compose one public production result's transaction progression onto ctx.

   Public model APIs deliberately expose progression rather than their internal
   transaction ctx. Tests that perform a dependent read should therefore use the
   same public composition rule as the application instead of relying on local
   XTDB timing."
  [ctx result]
  (if-let [committed-progression
           (:progression result)]
    (live/with-progression
     ctx
     (progression/compose
      (live/progression ctx)
      committed-progression))
    ctx))

(defn- requests-at
  [ctx location-id]
  (request/requests-for-location
   ctx
   {:organization-id
    mock-data/organization-id

    :location-id
    location-id

    :include-terminal?
    true}))

(deftest globally-selected-example-app-preserves-production-progression-and-projection-test
  (with-open
   [node
    (xtn/start-node
     {})]

    (let [live-system
          (live/create
           {:rules
            (humanhelp/gesso-live-rules)

            :dispatch-options
            {:threads
             1

             :queue-size
             32}})

          initial-ctx
          (base-ctx
           node
           live-system)

          handler
          (selected-handler)]

      (try
        (testing "the globally selected module is the example proving app"
          (is
           (identical?
            example.app/module
            selected-app/module)))

        (testing "the production fixture dependencies are genuinely absent initially"
          (is
           (nil?
            (organization/organization
             initial-ctx
             mock-data/organization-id)))

          (is
           (nil?
            (organization/location
             initial-ctx
             mock-data/default-location-id))))

        (let [user-transaction
              (seed-authenticated-user!
               initial-ctx)

              seeded-ctx
              (:ctx
               user-transaction)]

          (testing "the authenticated production User seed establishes an explicit read frontier"
            (is
             (= :committed
                (:commit/status
                 user-transaction)))

            (is
             (some?
              (:progression
               user-transaction)))

            (is
             (= (:progression user-transaction)
                (live/progression
                 seeded-ctx)))

            (is
             (= user-id
                (user/user-id
                 (user/require-user
                  seeded-ctx
                  user-id)))))

          (let [real-ensure!
                mock-data/ensure!

                fixture-result
                (atom nil)

                page-response
                (with-redefs
                 [mock-data/ensure!
                  (fn [actual-ctx]
                    (let [result
                          (real-ensure!
                           actual-ctx)]
                      (reset!
                       fixture-result
                       result)
                      result))]

                  (handler
                   (signed-in-request
                    seeded-ctx
                    :get
                    "/app")))

                {:keys
                 [created-count
                  transaction
                  ctx]}
                @fixture-result

                fixture-ctx
                ctx]

            (testing "entering the selected example app commits all missing production fixtures"
              ;; Route-local middleware intentionally returns the Hiccup page
              ;; here; top-level site middleware is tested separately by
              ;; HumanHelp's assembly tests.
              (is
               (vector?
                page-response))

              (is
               (= 4
                  created-count))

              (is
               (= :committed
                  (:commit/status
                   transaction)))

              (is
               (some?
                (:progression
                 transaction)))

              (is
               (some?
                (organization/organization
                 fixture-ctx
                 mock-data/organization-id)))

              (is
               (every?
                some?
                (map
                 #(organization/location
                   fixture-ctx
                   (:location/id %))
                 mock-data/locations))))

            (testing "fixture installation composes its commit progression onto the incoming User frontier"
              (is
               (= (progression/compose
                   (live/progression seeded-ctx)
                   (:progression transaction))
                  (live/progression fixture-ctx)))

              (is
               (not=
                (live/progression seeded-ctx)
                (live/progression fixture-ctx))
               "The fixture transaction must advance the authoritative observation frontier."))

            (let [real-create
                  request/create

                  create-call
                  (atom nil)

                  create-response
                  (with-redefs
                   [request/create
                    (fn [actual-ctx input]
                      (let [result
                            (real-create
                             actual-ctx
                             input)]
                        (reset!
                         create-call
                         {:ctx actual-ctx
                          :input input
                          :result result})
                        result))]

                    (handler
                     (assoc
                      (signed-in-request
                       fixture-ctx
                       :post
                       "/app/requests")
                      :form-params
                      {"title"
                       request-title

                       "details"
                       request-details

                       "location-detail"
                       request-location-detail})))

                  create-result
                  (:result
                   @create-call)

                  create-input
                  (:input
                   @create-call)

                  create-ctx
                  (:ctx
                   @create-call)

                  post-create-ctx
                  (result-read-ctx
                   create-ctx
                   create-result)

                  requests
                  (requests-at
                   post-create-ctx
                   mock-data/default-location-id)

                  request-document
                  (first
                   requests)

                  other-location-id
                  (:location/id
                   (second
                    mock-data/locations))

                  other-location-requests
                  (requests-at
                   post-create-ctx
                   other-location-id)

                  rows
                  (board/request-rows-for-location
                   post-create-ctx
                   mock-data/default-location-id)

                  request-row
                  (first
                   rows)

                  requestor-document
                  (:requestor-user
                   request-row)

                  create-body
                  (str
                   (:body
                    create-response))]

              (testing "the route delegates production-shaped authority to request.core/create"
                (is
                 (= mock-data/organization-id
                    (:organization-id
                     create-input)))

                (is
                 (= mock-data/default-location-id
                    (:location-id
                     create-input)))

                (is
                 (= user-id
                    (:current-user/id
                     create-ctx)))

                (is
                 (= (live/progression fixture-ctx)
                    (live/progression create-ctx))))

              (testing "the real production Request commit establishes a new authoritative progression"
                (is
                 (= 200
                    (:status
                     create-response)))

                (is
                 (= :committed
                    (:commit/status
                     create-result)))

                (is
                 (some?
                  (:progression
                   create-result)))

                (is
                 (= (progression/compose
                     (live/progression create-ctx)
                     (:progression create-result))
                    (live/progression post-create-ctx))))

              (testing "the create route immediately renders its own committed Request through read-your-writes progression"
                (is
                 (str/includes?
                  create-body
                  request-title))

                (is
                 (str/includes?
                  create-body
                  request-location-detail))

                (is
                 (str/includes?
                  create-body
                  user-display-name)))

              (testing "the public Request read sees exactly one Request at the production Location"
                (is
                 (= 1
                    (count
                     requests)))

                (is
                 (= mock-data/organization-id
                    (request/organization-id
                     request-document)))

                (is
                 (= mock-data/default-location-id
                    (request/location-id
                     request-document)))

                (is
                 (= user-id
                    (request/requestor-id
                     request-document)))

                (is
                 (= :open
                    (request/status
                     request-document)))

                (is
                 (= {:title
                     request-title

                     :details
                     request-details

                     :location-detail
                     request-location-detail}
                    (request/content
                     request-document)))

                (is
                 (empty?
                  other-location-requests)
                 "A Request created for Northgate must not leak into another production Location query."))

              (testing "the real example board projects the persisted requestor through production User"
                (is
                 (= 1
                    (count
                     rows)))

                (is
                 (= user-id
                    (board/row-requestor-user-id
                     request-row)))

                (is
                 (= user-id
                    (user/user-id
                     requestor-document)))

                (is
                 (= user-display-name
                    (user/user-display-name
                     requestor-document))))

              (testing "the real example fragment path rereads and renders the committed Request"
                (let [response
                      (handler
                       (signed-in-request
                        post-create-ctx
                        :get
                        "/app/fragments/requests"))

                      body
                      (str
                       (:body response))]

                  (is
                   (= 200
                      (:status response)))

                  (is
                   (str/includes?
                    body
                    "data-humanhelp-request-card"))

                  (is
                   (str/includes?
                    body
                    request-title))

                  (is
                   (str/includes?
                    body
                    request-location-detail))

                  (is
                   (str/includes?
                    body
                    user-display-name)))))))

        (finally
          (live/close!
           live-system))))))
