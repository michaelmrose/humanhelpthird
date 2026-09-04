(ns net.humanhelp.example.views-test
  "Contract tests for the production-model-backed Request rendering seam in the
   example app.

   Request rendering consumes the :rows emitted by example.live/example.board.
   During the request-card namespace consolidation this test deliberately stubs
   both the canonical request-card.core var and the temporary
   request-card.production var so the repository stays green before and after
   the source-side require is switched. Once the temporary production namespace
   is deleted, requiring it here should be removed as part of that deletion
   cleanup; the semantic contract is the canonical example Request card, not a
   separate production component."
  (:require
   [clojure.test :refer [deftest is testing]]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.components.request-card.core :as request-card]
   [net.humanhelp.example.components.request-card.production :as temporary-production-card]
   [net.humanhelp.example.views :as views])
  (:import
   [java.util UUID]))

(def request-id
  (UUID/fromString "81000000-0000-0000-0000-000000000001"))

(def viewer-id
  (UUID/fromString "82000000-0000-0000-0000-000000000001"))

(def viewer
  {:xt/id viewer-id
   :user/display-name "Production Viewer"
   :user/email "viewer@example.test"})

(def production-row
  {:request
   {:xt/id request-id
    :request/status :open
    :request/content {:title "Need a ride"}}
   :primary-assignment nil
   :requestor-user viewer
   :primary-helper-user nil})

(defn- tree-elements
  [markup]
  (filter
   #(and (vector? %)
         (keyword? (first %)))
   (tree-seq coll? seq markup)))

(defn- attrs-for-id
  [markup id]
  (some
   (fn [element]
     (let [attrs (when (map? (second element))
                   (second element))]
       (when (= id (:id attrs))
         attrs)))
   (tree-elements markup)))

(defn- with-request-card-stub
  "Run f while both the canonical and temporary Request-card vars point at stub.

   This is intentionally migration-only support for the namespace consolidation:
   the currently installed views namespace may still call the temporary var,
   while the immediately following source revision will call request-card.core.
   It does not imply two semantic component implementations."
  [stub f]
  (with-redefs-fn
    {#'request-card/request-card stub
     #'temporary-production-card/request-card stub}
    f))

(deftest request-views-have-one-example-request-card-dependency-test
  (let [dependencies
        (->> (ns-aliases 'net.humanhelp.example.views)
             vals
             (map ns-name)
             set)
        card-dependencies
        (select-keys
         (zipmap dependencies (repeat true))
         ['net.humanhelp.example.components.request-card.core
          'net.humanhelp.example.components.request-card.production])]
    (is (contains? dependencies 'net.humanhelp.example.board))
    (is (not (contains? dependencies 'net.humanhelp.example.model)))
    (is (= 1 (count card-dependencies))
        "During the one-file-at-a-time consolidation, views must depend on exactly one Request-card implementation, never both.")))

(deftest request-card-node-delegates-production-row-without-translation-test
  (let [calls (atom [])
        ctx {:anti-forgery-token "token"}
        stub
        (fn [actual-ctx opts]
          (swap! calls conj [actual-ctx opts])
          [:article {:data-test-request-card true}])]
    (with-request-card-stub
      stub
      (fn []
        (is (= [:article {:data-test-request-card true}]
               (views/request-card-node
                ctx
                {:row production-row
                 :viewer viewer
                 :open? true})))
        (is (= [[ctx
                 {:row production-row
                  :viewer viewer
                  :board-state-selector (views/board-state-selector)
                  :open? true}]]
               @calls))))))

(deftest request-accordion-renders-every-production-row-through-example-card-test
  (let [second-row
        (assoc-in
         production-row
         [:request :xt/id]
         (UUID/fromString "81000000-0000-0000-0000-000000000002"))
        calls (atom [])
        stub
        (fn [_ctx {:keys [row viewer]}]
          (swap! calls conj [row viewer])
          [:div {:data-test-request-id
                 (str (get-in row [:request :xt/id]))}])]
    (with-request-card-stub
      stub
      (fn []
        (let [markup
              (views/request-accordion
               {:ctx {:request/context true}
                :viewer viewer
                :rows [production-row second-row]})]
          (is (= [[production-row viewer]
                  [second-row viewer]]
                 @calls))
          (is (= 2
                 (count
                  (filter
                   #(and (map? (second %))
                          (contains? (second %) :data-test-request-id))
                   (tree-elements markup))))))))))

(deftest request-list-fragment-preserves-live-basis-and-production-row-contract-test
  (let [basis {:xtdb/tx-id 901}
        calls (atom [])]
    (with-redefs
     [views/request-accordion
      (fn [opts]
        (swap! calls conj opts)
        [:section {:data-test-production-accordion true}])]
      (let [markup
            (views/request-list-fragment
             {:ctx {:request/context true}
              :viewer viewer
              :rows [production-row]
              :view-state board/default-view-state
              :latest-revision basis})
            attrs
            (attrs-for-id markup views/request-list-dom-id)]
        (is (= [{:ctx {:request/context true}
                 :viewer viewer
                 :rows [production-row]}]
               @calls))
        (is (= "request-list"
               (:data-humanhelp-fragment attrs)))
        (is (= basis
               (:data-latest-revision attrs))
            "The view must preserve the Live/XTDB basis supplied by the Live query rather than derive a Request revision.")))))

(deftest legacy-requests-input-is-not-a-request-rendering-fallback-test
  (let [legacy-request
        {:id "demo-request"
         :status :open
         :revision 999}
        card-called? (atom false)
        stub
        (fn [& _]
          (reset! card-called? true)
          [:div {:data-unexpected-legacy-card true}])]
    (with-request-card-stub
      stub
      (fn []
        (let [markup
              (views/request-list-fragment
               {:ctx {}
                :viewer viewer
                :requests [legacy-request]
                :view-state board/default-view-state
                :latest-revision {:xtdb/tx-id 1}})
              text
              (filter string? (tree-seq coll? seq markup))]
          (is (false? @card-called?)
              "Supplying the retired :requests key must never reach any Request-card renderer.")
          (is (some #{"No requests yet"} text)
              "Without production :rows, the fragment is empty rather than adapting a legacy demo Request."))))))

(deftest empty-production-row-set-is-presentation-only-test
  (testing "empty unfiltered boards render an ordinary empty state"
    (let [markup
          (views/request-list-fragment
           {:ctx {}
            :viewer viewer
            :rows []
            :view-state board/default-view-state
            :latest-revision {:xtdb/tx-id 2}})
          text (set (filter string? (tree-seq coll? seq markup)))]
      (is (contains? text "No requests yet"))))

  (testing "search changes only empty-state presentation"
    (let [markup
          (views/request-list-fragment
           {:ctx {}
            :viewer viewer
            :rows []
            :view-state (assoc board/default-view-state :search "needle")
            :latest-revision {:xtdb/tx-id 3}})
          text (set (filter string? (tree-seq coll? seq markup)))]
      (is (contains? text "No matching requests"))
      (is (contains?
           text
           "Try fewer words or a different person, area, request, or status.")))))
