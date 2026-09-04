(ns net.humanhelp.example.views-test
  "Contract tests for the production-only Request rendering seam in the example app.

   Request rendering must consume the production-backed :rows emitted by
   example.live/example.board and delegate every Request card to the production
   Request component. The former example-model :requests shape is deliberately
   not a compatibility input anymore."
  (:require
   [clojure.test :refer [deftest is testing]]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.components.request-card.production :as production-card]
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

(deftest request-views-have-only-production-request-dependencies-test
  (let [dependencies
        (->> (ns-aliases 'net.humanhelp.example.views)
             vals
             (map ns-name)
             set)]
    (is (contains? dependencies 'net.humanhelp.example.board))
    (is (contains?
         dependencies
         'net.humanhelp.example.components.request-card.production))
    (is (not (contains? dependencies 'net.humanhelp.example.model)))
    (is (not (contains?
              dependencies
              'net.humanhelp.example.components.request-card.core)))))

(deftest request-card-node-delegates-production-row-without-translation-test
  (let [calls (atom [])
        ctx {:anti-forgery-token "token"}]
    (with-redefs
     [production-card/request-card
      (fn [actual-ctx opts]
        (swap! calls conj [actual-ctx opts])
        [:article {:data-test-production-card true}])]
      (is (= [:article {:data-test-production-card true}]
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
             @calls)))))

(deftest request-accordion-renders-every-production-row-through-production-card-test
  (let [second-row
        (assoc-in
         production-row
         [:request :xt/id]
         (UUID/fromString "81000000-0000-0000-0000-000000000002"))
        calls (atom [])]
    (with-redefs
     [production-card/request-card
      (fn [_ctx {:keys [row viewer]}]
        (swap! calls conj [row viewer])
        [:div {:data-test-request-id
               (str (get-in row [:request :xt/id]))}])]
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
                 (tree-elements markup)))))))))

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
        production-card-called? (atom false)]
    (with-redefs
     [production-card/request-card
      (fn [& _]
        (reset! production-card-called? true)
        [:div {:data-unexpected-legacy-card true}])]
      (let [markup
            (views/request-list-fragment
             {:ctx {}
              :viewer viewer
              :requests [legacy-request]
              :view-state board/default-view-state
              :latest-revision {:xtdb/tx-id 1}})
            text
            (filter string? (tree-seq coll? seq markup))]
        (is (false? @production-card-called?)
            "Supplying the retired :requests key must never reach any Request-card renderer.")
        (is (some #{"No requests yet"} text)
            "Without production :rows, the fragment is empty rather than adapting a legacy demo Request.")))))

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
