(ns net.humanhelp.example.app-integration-test
  "Clean-XTDB integration coverage for the globally selected HumanHelp proving app.

   This test deliberately does not stub the example board, production Request
   Graph reads, production fixture initialization, Gesso model transactions,
   Gesso Live, or XTDB.

   The only prerequisite seeded directly is the authenticated production User.
   Starting from an otherwise empty XTDB node, the globally selected app must:

     1. establish the production Organization/Location fixtures;
     2. render the example app for that authenticated production User;
     3. create a real production Request through the selected app route;
     4. persist that Request through request.core -> Gesso model tx -> XTDB; and
     5. render the committed Request through the real example board/fragment
        path.

   This is intentionally the integration seam that would have caught the old
   example.board call of request/requests-for-location with a bare Location UUID."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.core :as live]
   [gesso.model.tx :as model.tx]
   [net.humanhelp :as humanhelp]
   [net.humanhelp.app :as selected-app]
   [net.humanhelp.example.app :as example.app]
   [net.humanhelp.site.mock-data :as mock-data]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.domain :as user.domain]
   [reitit.ring :as ring]
   [xtdb.node :as xtn])
  (:import
   [java.time Instant]
   [java.util UUID]))

(def user-id
  (UUID/fromString
   "93000000-0000-0000-0000-000000000538"))

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
    "Selected Example Integration User"

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
   538})

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

(deftest globally-selected-example-app-renders-a-production-request-from-clean-xtdb-test
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

          ctx
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
             ctx
             mock-data/organization-id)))

          (is
           (nil?
            (organization/location
             ctx
             mock-data/default-location-id))))

        (seed-authenticated-user!
         ctx)

        (testing "entering the selected example app establishes production fixtures"
          (let [response
                (handler
                 (signed-in-request
                  ctx
                  :get
                  "/app"))]
            ;; Route-local middleware intentionally returns the Hiccup page here;
            ;; top-level site middleware is tested separately by HumanHelp's
            ;; assembly tests.
            (is
             (vector?
              response))

            (is
             (some?
              (organization/organization
               ctx
               mock-data/organization-id)))

            (is
             (some?
              (organization/location
               ctx
               mock-data/default-location-id)))))

        (testing "a real production Request is created through the selected app route"
          (let [response
                (handler
                 (assoc
                  (signed-in-request
                   ctx
                   :post
                   "/app/requests")
                  :form-params
                  {"title"
                   request-title

                   "details"
                   request-details

                   "location-detail"
                   request-location-detail}))

                requests
                (request/requests-for-location
                 ctx
                 {:organization-id
                  mock-data/organization-id

                  :location-id
                  mock-data/default-location-id

                  :include-terminal?
                  true})

                request-document
                (first
                 requests)]

            (is
             (= 200
                (:status response)))

            (is
             (= 1
                (count requests)))

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
                 request-document)))))

        (testing "the real example fragment path rereads and renders the committed Request"
          (let [response
                (handler
                 (signed-in-request
                  ctx
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
              request-location-detail))))

        (finally
          (live/close!
           live-system))))))
