(ns net.humanhelp.example.live-test
  "Focused tests for the production-model-backed HumanHelp example Live boundary.

   The example app is now intended to validate the production HumanHelp models
   and production Request choreography directly. These tests protect the Live
   seam so it cannot quietly regress into the former demo model/event/revision
   system while the HTTP app is being cut over.

   In particular they verify that:

   - example.live has no dependency on net.humanhelp.example.model;
   - the example Live scope is the production development Location;
   - production :request changes expand to both Request fragments;
   - authoritative progression is preserved exactly through invalidation
     expansion;
   - fragment queries delegate to example.board and return production :rows;
   - Live observed basis is propagated as presentation/read context rather than
     replaced by a demo Request revision frontier."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.core :as live]
   [gesso.live.invalidation :as invalidation]
   [gesso.live.progression :as progression]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.live :as app-live]
   [net.humanhelp.site.mock-data :as mock-data])
  (:import
   [java.util UUID]))

(def request-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def viewer
  {:xt/id
   (UUID/fromString
    "40000000-0000-0000-0000-000000000001")
   :user/display-name "Example Viewer"})

(def render-context-key
  "Private implementation key used only to exercise the public fragment query
   functions directly. It is data, not a private Var dependency."
  :net.humanhelp.example.live/render-options)

(deftest example-live-is-not-a-parallel-model-test
  (let [dependencies
        (->> (ns-aliases
              'net.humanhelp.example.live)
             vals
             (map ns-name)
             set)]
    (is (not
         (contains?
          dependencies
          'net.humanhelp.example.model))
        "Production-backed Live wiring must never route through the old example model.")
    (is (contains?
         dependencies
         'net.humanhelp.example.board))
    (is (contains?
         dependencies
         'net.humanhelp.site.mock-data))))

(deftest live-scope-is-the-production-development-location-test
  (is (= mock-data/default-location-id
         app-live/location-id))
  (is (uuid? app-live/location-id))
  (is (= app-live/location-id
         app-live/store-id)
      "The temporary store-id alias must identify the production Location, not demo-store.")
  (is (true?
       (app-live/allow-example-location?
        {}
        app-live/location-id)))
  (is (false?
       (app-live/allow-example-location?
        {}
        (UUID/fromString
         "30000000-0000-0000-0000-000000000099")))))

(deftest production-request-change-expands-to-exact-fragment-scopes-test
  (let [change
        {:topic               :request
         :request/id          request-id
         :request/location-id app-live/location-id
         :request/operation   :request/claim}

        expanded
        (live/expand-change
         app-live/compiled-live
         {}
         change)]
    (is (= 2 (count expanded)))
    (is (= [{:topic :humanhelp/request-toolbar
             :id    app-live/location-id}
            {:topic :humanhelp/request-list
             :id    app-live/location-id}]
           (mapv
            #(select-keys % [:topic :id])
            expanded)))
    (is (= #{:request-toolbar
             :request-list}
           (set
            (map :gesso.live/scope expanded))))
    (is (= #{{:topic :humanhelp/request-toolbar
              :id    app-live/location-id}
             {:topic :humanhelp/request-list
              :id    app-live/location-id}}
           (set
            (map
             #(select-keys % [:topic :id])
             expanded))))))

(deftest authoritative-progression-survives-request-invalidation-expansion-test
  (let [basis
        {:xtdb/tx-id 42}

        requirement
        (progression/requirement basis)

        change
        {:topic               :request
         :request/id          request-id
         :request/location-id app-live/location-id
         :request/operation   :request/complete
         :progression         requirement}

        expanded
        (invalidation/expand
         app-live/live-rules
         {:test/context :live}
         change)]
    (is (= 2 (count expanded)))
    (is (= #{[:humanhelp/request-toolbar app-live/location-id]
             [:humanhelp/request-list app-live/location-id]}
           (set
            (map
             (juxt :topic :id)
             expanded))))
    (is (every?
         #(= requirement
             (:progression %))
         expanded)
        "Neither the app graph nor a fragment rule may weaken, replace, or invent the authoritative requirement.")))

(deftest request-list-query-delegates-to-production-board-test
  (let [calls
        (atom [])

        basis
        {:xtdb/tx-id 77}

        rows
        [{:request {:xt/id          request-id
                    :request/status :open}}]

        expected-board-data
        {:location-id    app-live/location-id
         :viewer         viewer
         :viewer-id      (:xt/id viewer)
         :view-state     {:search           "needle"
                          :created-order    :oldest
                          :mine-first?      true
                          :unclaimed-first? false
                          :show-terminal?   true}
         :observed-basis basis
         :rows           rows
         :total-count    1
         :active-count   1
         :terminal-count 0}

        ctx
        {:test/context :request-list
         render-context-key
         {:viewer     viewer
          :view-state {:search         "  needle  "
                       :created-order  "oldest"
                       :mine-first?    "on"
                       :show-terminal? "true"}}}]
    (with-redefs
     [board/board-data
      (fn [actual-ctx input]
        (swap! calls conj [actual-ctx input])
        expected-board-data)]

      (let [actual
            (app-live/request-list-query
             ctx
             app-live/location-id)]
        (is (= 1 (count @calls)))
        (is (= ctx
               (ffirst @calls)))
        (is (= {:location-id app-live/location-id
                :viewer      viewer
                :view-state
                {:search           "needle"
                 :created-order    :oldest
                 :mine-first?      true
                 :unclaimed-first? false
                 :show-terminal?   true}}
               (second
                (first @calls))))
        (is (= rows
               (:rows actual)))
        (is (= viewer
               (:viewer actual)))
        (is (= viewer
               (:user actual))
            "The temporary :user compatibility key must carry the same production User projection.")
        (is (= basis
               (:observed-basis actual)))
        (is (= basis
               (:latest-revision actual))
            "The temporary presentation key must mirror Live basis rather than resurrect an application revision frontier.")
        (is (= app-live/location-id
               (:store/id actual)))))))

(deftest request-toolbar-query-derives-counts-and-basis-from-board-data-test
  (let [basis
        {:xtdb/tx-id 88}

        ctx
        {:test/context :toolbar
         render-context-key
         {:viewer     viewer
          :view-state {:created-order :newest}}}]
    (with-redefs
     [board/board-data
      (fn [_ctx input]
        (is (= app-live/location-id
               (:location-id input)))
        (is (= viewer
               (:viewer input)))
        {:viewer         viewer
         :view-state     (board/normalize-view-state
                          (:view-state input))
         :observed-basis basis
         :active-count   3
         :total-count    5
         :terminal-count 2
         :rows           []})]

      (let [actual
            (app-live/request-toolbar-query
             ctx
             app-live/location-id)]
        (is (= 3
               (:open-count actual)))
        (is (= 0
               (:pending-open-count actual)))
        (is (false?
             (:stale? actual)))
        (is (= basis
               (:latest-revision actual)))
        (is (= viewer
               (:viewer actual)))
        (is (= viewer
               (:user actual)))))))

(deftest fragment-options-use-stable-production-fragment-contract-test
  (let [toolbar
        (app-live/fragment-options :request-toolbar)

        request-list
        (app-live/fragment-options :request-list)]
    (is (= (app-live/board-state-selector)
           (get-in toolbar [:root-attrs :hx-include])))
    (is (= (app-live/board-state-selector)
           (get-in request-list [:root-attrs :hx-include])))
    (is (= "outerHTML show:none focus-scroll:false"
           (:swap request-list)))
    (is (= app-live/request-list-client-continuity
           (:client-continuity request-list)))
    (is (string? (:fragment-url toolbar)))
    (is (string? (:stream-url toolbar)))
    (is (string? (:fragment-url request-list)))
    (is (string? (:stream-url request-list)))))

(deftest unknown-fragment-options-fail-closed-test
  (let [error
        (try
          (app-live/fragment-options :not-a-fragment)
          nil
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (some? error))
    (is (= :not-a-fragment
           (:fragment (ex-data error))))
    (is (= [:request-toolbar :request-list]
           (:known-fragments
            (ex-data error))))))
