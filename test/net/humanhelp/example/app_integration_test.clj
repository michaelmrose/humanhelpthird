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
        Organization/Location only;
     6. project the persisted Request's requestor through the production User
        model when the real example board/fragment renders;
     7. drive a claimed Request back to open through the production unclaim
        choreography and prove the authoritative post-commit reread; and
     8. drive a claimed Request through on-the-way to done, preserving the
        primary assignment across the intermediate transition and ending it
        atomically at completion.

   This is intentionally the integration seam that should catch a proving app
   which appears to work while discarding authoritative progression, querying a
   Request with the wrong location shape, or silently reviving parallel example
   model/User state."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.live.consistency.xtdb :as live.xtdb]
   [gesso.live.core :as live]
   [gesso.live.optimistic.protocol :as optimistic.protocol]
   [gesso.live.progression :as progression]
   [gesso.model.command :as model.command]
   [gesso.model.tx :as model.tx]
   [net.humanhelp :as humanhelp]
   [net.humanhelp.app :as selected-app]
   [net.humanhelp.example.app :as example.app]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.site.mock-data :as mock-data]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.membership.domain :as membership.domain]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
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

(def helper-membership-id
  (UUID/fromString
   "94000000-0000-0000-0000-000000000545"))

(def helper-role-assignment-id
  (UUID/fromString
   "95000000-0000-0000-0000-000000000545"))

(def claim-command-id
  (identity/command-id
   "humanhelp-example-app-integration-claim-command-545"))

(def claim-execution-id
  (identity/execution-id
   "humanhelp-example-app-integration-claim-execution-545"))

(def unclaim-command-id
  (identity/command-id
   "humanhelp-example-app-integration-unclaim-command-550"))

(def unclaim-execution-id
  (identity/execution-id
   "humanhelp-example-app-integration-unclaim-execution-550"))

(def mark-on-the-way-command-id
  (identity/command-id
   "humanhelp-example-app-integration-mark-on-the-way-command-551"))

(def mark-on-the-way-execution-id
  (identity/execution-id
   "humanhelp-example-app-integration-mark-on-the-way-execution-551"))

(def complete-command-id
  (identity/command-id
   "humanhelp-example-app-integration-complete-command-551"))

(def complete-execution-id
  (identity/execution-id
   "humanhelp-example-app-integration-complete-execution-551"))

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
  "Build a browser-shaped authenticated request context.

   Deliberately remove deterministic fixture FX inputs here. Real Ring requests
   do not carry :biff.fx/now or :biff.fx/seed; application mutation boundaries
   must acquire those through Biff FX execution rather than accidentally relying
   on test fixture context."
  [ctx method uri]
  (-> ctx
      (dissoc :biff.fx/now :biff.fx/seed)
      (assoc
       :request-method method
       :uri uri
       :session {:uid user-id})))

(defn- seed-authenticated-user!
  [ctx]
  (model.tx/transact!
   ctx
   {:commands
    [(user-command)]

    :emit
    false}))

(defn- seed-helper-authority!
  "Install a fixed active Membership plus exact :helper role at the demo Location.

   This is test fixture bootstrap, not application mutation behavior. The claim
   itself still crosses the public production Request choreography and model
   boundaries for real."
  [ctx]
  (let [membership-command
        (membership.domain/create-membership-command
         {:id helper-membership-id
          :user-id user-id
          :organization-id mock-data/organization-id
          :now request-time})

        membership-document
        (model.command/after membership-command)

        role-command
        (membership.domain/create-role-assignment-command
         membership-document
         {:id helper-role-assignment-id
          :role :helper
          :scope (organization/location-scope
                  mock-data/default-location-id)
          :now request-time})]
    (model.tx/transact!
     ctx
     {:commands [membership-command role-command]
      :emit false})))

(defn- lifecycle-command-wire
  [ctx request-document {:keys [command-id execution-id operation]}]
  (let [request-id
        (request/request-id request-document)

        basis
        (live.xtdb/strongest-required-basis
         (live/progression ctx))]
    (pr-str
     (optimistic.protocol/command->wire
      (optimistic.protocol/command
       {:command-id command-id
        :execution-id execution-id
        :operation operation
        :arguments {:request-id request-id}
        :observed-basis basis
        :scope [:request request-id]
        :fact-versions {:request/revision
                        (request/revision request-document)}})))))

(defn- claim-command-wire
  [ctx request-document]
  (lifecycle-command-wire
   ctx
   request-document
   {:command-id claim-command-id
    :execution-id claim-execution-id
    :operation request.choreo/claim-operation}))

(defn- unclaim-command-wire
  [ctx request-document]
  (lifecycle-command-wire
   ctx
   request-document
   {:command-id unclaim-command-id
    :execution-id unclaim-execution-id
    :operation request.choreo/unclaim-operation}))

(defn- mark-on-the-way-command-wire
  [ctx request-document]
  (lifecycle-command-wire
   ctx
   request-document
   {:command-id mark-on-the-way-command-id
    :execution-id mark-on-the-way-execution-id
    :operation request.choreo/mark-on-the-way-operation}))

(defn- complete-command-wire
  [ctx request-document]
  (lifecycle-command-wire
   ctx
   request-document
   {:command-id complete-command-id
    :execution-id complete-execution-id
    :operation request.choreo/complete-operation}))

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

(deftest globally-selected-example-app-claims-through-production-choreo-test
  (with-open
   [node
    (xtn/start-node
     {})]

    (let [live-system
          (live/create
           {:rules
            (humanhelp/gesso-live-rules)

            :dispatch-options
            {:threads 1
             :queue-size 32}})

          initial-ctx
          (base-ctx
           node
           live-system)

          handler
          (selected-handler)]

      (try
        (let [user-transaction
              (seed-authenticated-user!
               initial-ctx)

              seeded-ctx
              (:ctx user-transaction)

              fixture-result
              (mock-data/ensure!
               seeded-ctx)

              fixture-ctx
              (:ctx fixture-result)

              helper-transaction
              (seed-helper-authority!
               fixture-ctx)

              helper-ctx
              (:ctx helper-transaction)

              create-result
              (request/create
               (assoc helper-ctx :current-user/id user-id)
               {:organization-id mock-data/organization-id
                :location-id mock-data/default-location-id
                :content {:title request-title
                          :details request-details
                          :location-detail request-location-detail}})

              post-create-ctx
              (result-read-ctx
               helper-ctx
               create-result)

              request-document
              (first
               (requests-at
                post-create-ctx
                mock-data/default-location-id))

              request-id
              (request/request-id
               request-document)

              command-wire
              (claim-command-wire
               post-create-ctx
               request-document)

              real-claim
              request/claim

              claim-call
              (atom nil)

              claim-response
              (with-redefs
               [request/claim
                (fn [actual-ctx input]
                  (let [result
                        (real-claim
                         actual-ctx
                         input)]
                    (reset!
                     claim-call
                     {:ctx actual-ctx
                      :input input
                      :result result})
                    result))]

                (handler
                 (assoc
                  (signed-in-request
                   post-create-ctx
                   :post
                   (routes/claim-request-url request-id))
                  :form-params
                  {example.app/optimistic-command-param
                   command-wire})))

              claim-result
              (:result @claim-call)

              claim-ctx
              (:ctx @claim-call)

              post-claim-ctx
              (result-read-ctx
               claim-ctx
               claim-result)

              claimed-request
              (:request claim-result)

              primary-assignment
              (:primary-assignment claim-result)

              rows
              (board/request-rows-for-location
               post-claim-ctx
               mock-data/default-location-id)

              row
              (first rows)

              rendered-primary
              (board/row-primary-assignment row)

              claim-body
              (str (:body claim-response))]

          (testing "fixture authority makes the authenticated production User an effective helper"
            (is (= :committed
                   (:commit/status helper-transaction)))

            (is (some? (:progression helper-transaction)))

            (is (membership/helper?
                 helper-ctx
                 user-id
                 (organization/location-scope
                  mock-data/default-location-id)))

            (is (some?
                 (live.xtdb/strongest-required-basis
                  (live/progression helper-ctx)))))

          (testing "the browser-shaped protocol-v3 claim is bound to the concrete HTTP route"
            (is (uuid? request-id))

            (is (= 200
                   (:status claim-response)))

            (is (str/includes?
                 claim-body
                 "data-gesso-live-optimistic-settlement"))

            (is (= {:request-id request-id}
                   (:input @claim-call)))

            (is (= user-id
                   (:current-user/id claim-ctx)))

            (is (= (live/progression post-create-ctx)
                   (live/progression claim-ctx))))

          (testing "production Request Choreo commits the authoritative claim atomically"
            (is (= :committed
                   (:commit/status claim-result)))

            (is (some? (:progression claim-result)))

            (is (= request-id
                   (request/request-id claimed-request)))

            (is (= :claimed
                   (request/status claimed-request)))

            (is (= user-id
                   (request/assignment-helper-id primary-assignment)))

            (is (request/active-primary-assignment?
                 primary-assignment))

            (is (= (progression/compose
                    (live/progression claim-ctx)
                    (:progression claim-result))
                   (live/progression post-claim-ctx))))

          (testing "the real board rereads the committed claim through production relationships"
            (is (= 1 (count rows)))

            (is (= request-id
                   (board/row-request-id row)))

            (is (= :claimed
                   (request/status
                    (board/row-request row))))

            (is (some? rendered-primary))

            (is (= user-id
                   (request/assignment-helper-id
                    rendered-primary)))

            (is (request/active-primary-assignment?
                 rendered-primary)))

          (testing "the real fragment path renders the post-claim authoritative state"
            (let [response
                  (handler
                   (signed-in-request
                    post-claim-ctx
                    :get
                    "/app/fragments/requests"))

                  body
                  (str (:body response))]

              (is (= 200 (:status response)))

              (is (str/includes? body request-title))

              (is (str/includes? body "Claimed"))

              (is (str/includes? body "claimed by you")))))

        (finally
          (live/close!
           live-system))))))

(deftest globally-selected-example-app-unclaims-through-production-choreo-test
  (with-open
   [node
    (xtn/start-node
     {})]

    (let [live-system
          (live/create
           {:rules
            (humanhelp/gesso-live-rules)

            :dispatch-options
            {:threads 1
             :queue-size 32}})

          initial-ctx
          (base-ctx
           node
           live-system)

          handler
          (selected-handler)]

      (try
        (let [user-transaction
              (seed-authenticated-user!
               initial-ctx)

              seeded-ctx
              (:ctx user-transaction)

              fixture-result
              (mock-data/ensure!
               seeded-ctx)

              fixture-ctx
              (:ctx fixture-result)

              helper-transaction
              (seed-helper-authority!
               fixture-ctx)

              helper-ctx
              (:ctx helper-transaction)

              create-result
              (request/create
               (assoc helper-ctx :current-user/id user-id)
               {:organization-id mock-data/organization-id
                :location-id mock-data/default-location-id
                :content {:title request-title
                          :details request-details
                          :location-detail request-location-detail}})

              post-create-ctx
              (result-read-ctx
               helper-ctx
               create-result)

              open-request
              (first
               (requests-at
                post-create-ctx
                mock-data/default-location-id))

              request-id
              (request/request-id
               open-request)

              claim-result
              (request/claim
               (assoc post-create-ctx :current-user/id user-id)
               {:request-id request-id})

              post-claim-ctx
              (result-read-ctx
               post-create-ctx
               claim-result)

              claimed-request
              (:request claim-result)

              reread-primary-assignment
              (request/active-primary-assignment-for-request
               post-claim-ctx
               request-id)

              command-wire
              (unclaim-command-wire
               post-claim-ctx
               claimed-request)

              real-unclaim
              request/unclaim

              unclaim-call
              (atom nil)

              unclaim-response
              (with-redefs
               [request/unclaim
                (fn [actual-ctx input]
                  (let [result
                        (real-unclaim
                         actual-ctx
                         input)]
                    (reset!
                     unclaim-call
                     {:ctx actual-ctx
                      :input input
                      :result result})
                    result))]

                (handler
                 (assoc
                  (signed-in-request
                   post-claim-ctx
                   :post
                   (routes/unclaim-request-url request-id))
                  :form-params
                  {example.app/optimistic-command-param
                   command-wire})))

              unclaim-result
              (:result @unclaim-call)

              unclaim-ctx
              (:ctx @unclaim-call)

              post-unclaim-ctx
              (result-read-ctx
               unclaim-ctx
               unclaim-result)

              unclaimed-request
              (:request unclaim-result)

              ended-assignments
              (:assignments unclaim-result)

              active-primary-after-unclaim
              (request/active-primary-assignment-for-request
               post-unclaim-ctx
               request-id)

              rows
              (board/request-rows-for-location
               post-unclaim-ctx
               mock-data/default-location-id)

              row
              (first rows)

              unclaim-body
              (str (:body unclaim-response))]

          (testing "the claimed setup is visible at the composed post-commit progression"
            (is (= :committed
                   (:commit/status claim-result)))

            (is (= :claimed
                   (request/status claimed-request)))

            (is (some? reread-primary-assignment)
                "A dependent relationship read must observe the committed primary assignment.")

            (is (= user-id
                   (request/assignment-helper-id
                    reread-primary-assignment)))

            (is (request/active-primary-assignment?
                 reread-primary-assignment)))

          (testing "the browser-shaped protocol-v3 unclaim is bound to the concrete HTTP route"
            (is (= 200
                   (:status unclaim-response)))

            (is (str/includes?
                 unclaim-body
                 "data-gesso-live-optimistic-settlement"))

            (is (not
                 (str/includes?
                  unclaim-body
                  "data-humanhelp-request-card"))
                "The direct settlement response must not compete with managed Live for canonical card ownership.")

            (is (= {:request-id request-id}
                   (:input @unclaim-call)))

            (is (= user-id
                   (:current-user/id unclaim-ctx)))

            (is (= (live/progression post-claim-ctx)
                   (live/progression unclaim-ctx))))

          (testing "production Request Choreo atomically returns the Request to open and ends its assignment"
            (is (= :committed
                   (:commit/status unclaim-result)))

            (is (some? (:progression unclaim-result)))

            (is (= request-id
                   (request/request-id unclaimed-request)))

            (is (= :open
                   (request/status unclaimed-request)))

            (is (= 1
                   (count ended-assignments)))

            (is (every?
                 request/assignment-ended?
                 ended-assignments))

            (is (every?
                 #(= :request/unclaimed
                     (request/assignment-end-reason %))
                 ended-assignments))

            (is (= (progression/compose
                    (live/progression unclaim-ctx)
                    (:progression unclaim-result))
                   (live/progression post-unclaim-ctx))))

          (testing "the authoritative post-unclaim relationship and board reads contain no stale primary assignment"
            (is (nil? active-primary-after-unclaim))

            (is (= 1 (count rows)))

            (is (= request-id
                   (board/row-request-id row)))

            (is (= :open
                   (request/status
                    (board/row-request row))))

            (is (nil?
                 (board/row-primary-assignment row))))

          (testing "the next managed fragment reread installs the authoritative open card"
            (let [response
                  (handler
                   (signed-in-request
                    post-unclaim-ctx
                    :get
                    "/app/fragments/requests"))

                  body
                  (str (:body response))]

              (is (= 200 (:status response)))

              (is (str/includes? body request-title))

              (is (str/includes? body "Open"))

              (is (str/includes? body "Claim"))

              (is (not
                   (str/includes? body "claimed by you"))))))

        (finally
          (live/close!
           live-system))))))

(deftest globally-selected-example-app-progresses-on-the-way-through-completion-test
  (with-open
   [node
    (xtn/start-node
     {})]

    (let [live-system
          (live/create
           {:rules
            (humanhelp/gesso-live-rules)

            :dispatch-options
            {:threads 1
             :queue-size 32}})

          initial-ctx
          (base-ctx
           node
           live-system)

          handler
          (selected-handler)]

      (try
        (let [user-transaction
              (seed-authenticated-user!
               initial-ctx)

              seeded-ctx
              (:ctx user-transaction)

              fixture-result
              (mock-data/ensure!
               seeded-ctx)

              fixture-ctx
              (:ctx fixture-result)

              helper-transaction
              (seed-helper-authority!
               fixture-ctx)

              helper-ctx
              (:ctx helper-transaction)

              create-result
              (request/create
               (assoc helper-ctx :current-user/id user-id)
               {:organization-id mock-data/organization-id
                :location-id mock-data/default-location-id
                :content {:title request-title
                          :details request-details
                          :location-detail request-location-detail}})

              post-create-ctx
              (result-read-ctx
               helper-ctx
               create-result)

              open-request
              (first
               (requests-at
                post-create-ctx
                mock-data/default-location-id))

              request-id
              (request/request-id
               open-request)

              claim-result
              (request/claim
               (assoc post-create-ctx :current-user/id user-id)
               {:request-id request-id})

              post-claim-ctx
              (result-read-ctx
               post-create-ctx
               claim-result)

              claimed-request
              (:request claim-result)

              mark-command-wire
              (mark-on-the-way-command-wire
               post-claim-ctx
               claimed-request)

              real-mark-on-the-way
              request/mark-on-the-way

              mark-call
              (atom nil)

              mark-response
              (with-redefs
               [request/mark-on-the-way
                (fn [actual-ctx input]
                  (let [result
                        (real-mark-on-the-way
                         actual-ctx
                         input)]
                    (reset!
                     mark-call
                     {:ctx actual-ctx
                      :input input
                      :result result})
                    result))]

                (handler
                 (assoc
                  (signed-in-request
                   post-claim-ctx
                   :post
                   (routes/mark-on-the-way-request-url request-id))
                  :form-params
                  {example.app/optimistic-command-param
                   mark-command-wire})))

              mark-result
              (:result @mark-call)

              mark-ctx
              (:ctx @mark-call)

              post-mark-ctx
              (result-read-ctx
               mark-ctx
               mark-result)

              on-the-way-request
              (:request mark-result)

              primary-after-mark
              (request/active-primary-assignment-for-request
               post-mark-ctx
               request-id)

              mark-body
              (str (:body mark-response))

              mark-fragment-response
              (handler
               (signed-in-request
                post-mark-ctx
                :get
                "/app/fragments/requests"))

              mark-fragment-body
              (str (:body mark-fragment-response))

              complete-command-wire
              (complete-command-wire
               post-mark-ctx
               on-the-way-request)

              real-complete
              request/complete

              complete-call
              (atom nil)

              complete-response
              (with-redefs
               [request/complete
                (fn [actual-ctx input]
                  (let [result
                        (real-complete
                         actual-ctx
                         input)]
                    (reset!
                     complete-call
                     {:ctx actual-ctx
                      :input input
                      :result result})
                    result))]

                (handler
                 (assoc
                  (signed-in-request
                   post-mark-ctx
                   :post
                   (routes/complete-request-url request-id))
                  :form-params
                  {example.app/optimistic-command-param
                   complete-command-wire})))

              complete-result
              (:result @complete-call)

              complete-ctx
              (:ctx @complete-call)

              post-complete-ctx
              (result-read-ctx
               complete-ctx
               complete-result)

              completed-request
              (:request complete-result)

              ended-assignments
              (:assignments complete-result)

              active-primary-after-complete
              (request/active-primary-assignment-for-request
               post-complete-ctx
               request-id)

              completed-requests
              (requests-at
               post-complete-ctx
               mock-data/default-location-id)

              complete-body
              (str (:body complete-response))

              default-fragment-response
              (handler
               (signed-in-request
                post-complete-ctx
                :get
                "/app/fragments/requests"))

              default-fragment-body
              (str (:body default-fragment-response))

              terminal-fragment-response
              (handler
               (assoc
                (signed-in-request
                 post-complete-ctx
                 :get
                 "/app/fragments/requests")
                :query-params
                {routes/show-terminal-param
                 "true"}))

              terminal-fragment-body
              (str (:body terminal-fragment-response))]

          (testing "the claimed setup carries an active primary helper into the lifecycle route"
            (is (= :committed
                   (:commit/status claim-result)))

            (is (= :claimed
                   (request/status claimed-request)))

            (is (some?
                 (request/active-primary-assignment-for-request
                  post-claim-ctx
                  request-id))))

          (testing "the browser-shaped mark-on-the-way command crosses the concrete HTTP and Choreo boundary"
            (is (= 200
                   (:status mark-response)))

            (is (str/includes?
                 mark-body
                 "data-gesso-live-optimistic-settlement"))

            (is (not
                 (str/includes?
                  mark-body
                  "data-humanhelp-request-card"))
                "The direct settlement response must leave canonical card ownership to managed Live.")

            (is (= {:request-id request-id}
                   (:input @mark-call)))

            (is (= user-id
                   (:current-user/id mark-ctx)))

            (is (= (live/progression post-claim-ctx)
                   (live/progression mark-ctx))))

          (testing "mark-on-the-way commits the Request transition without ending its primary assignment"
            (is (= :committed
                   (:commit/status mark-result)))

            (is (some? (:progression mark-result)))

            (is (= :on-the-way
                   (request/status on-the-way-request)))

            (is (some? primary-after-mark))

            (is (= user-id
                   (request/assignment-helper-id
                    primary-after-mark)))

            (is (request/active-primary-assignment?
                 primary-after-mark))

            (is (= (progression/compose
                    (live/progression mark-ctx)
                    (:progression mark-result))
                   (live/progression post-mark-ctx))))

          (testing "the managed fragment reread installs the authoritative on-the-way card and completion affordance"
            (is (= 200
                   (:status mark-fragment-response)))

            (is (str/includes?
                 mark-fragment-body
                 request-title))

            (is (str/includes?
                 mark-fragment-body
                 "On the way"))

            (is (str/includes?
                 mark-fragment-body
                 "Done")))

          (testing "the browser-shaped complete command binds to the concrete route at the post-mark progression"
            (is (= 200
                   (:status complete-response)))

            (is (str/includes?
                 complete-body
                 "data-gesso-live-optimistic-settlement"))

            (is (not
                 (str/includes?
                  complete-body
                  "data-humanhelp-request-card"))
                "Completion settlement must not race managed Live with a canonical card.")

            (is (= {:request-id request-id}
                   (:input @complete-call)))

            (is (= user-id
                   (:current-user/id complete-ctx)))

            (is (= (live/progression post-mark-ctx)
                   (live/progression complete-ctx))))

          (testing "completion atomically marks the Request done and ends every active assignment"
            (is (= :committed
                   (:commit/status complete-result)))

            (is (some? (:progression complete-result)))

            (is (= request-id
                   (request/request-id completed-request)))

            (is (= :done
                   (request/status completed-request)))

            (is (= 1
                   (count ended-assignments)))

            (is (every?
                 request/assignment-ended?
                 ended-assignments))

            (is (every?
                 #(= :request/completed
                     (request/assignment-end-reason %))
                 ended-assignments))

            (is (= (progression/compose
                    (live/progression complete-ctx)
                    (:progression complete-result))
                   (live/progression post-complete-ctx))))

          (testing "authoritative post-completion reads contain the terminal Request and no stale primary assignment"
            (is (nil?
                 active-primary-after-complete))

            (is (= 1
                   (count completed-requests)))

            (is (= :done
                   (request/status
                    (first completed-requests)))))

          (testing "normal board filtering hides terminal Requests while an explicit terminal reread renders Done"
            (is (= 200
                   (:status default-fragment-response)))

            (is (not
                 (str/includes?
                  default-fragment-body
                  request-title)))

            (is (= 200
                   (:status terminal-fragment-response)))

            (is (str/includes?
                 terminal-fragment-body
                 request-title))

            (is (str/includes?
                 terminal-fragment-body
                 "Done"))))

        (finally
          (live/close!
           live-system))))))

