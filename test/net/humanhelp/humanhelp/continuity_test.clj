(ns net.humanhelp.humanhelp.continuity-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [net.humanhelp.humanhelp.live :as human-live]
   [net.humanhelp.humanhelp.routes :as routes]
   [net.humanhelp.humanhelp.views :as views]))

;; -----------------------------------------------------------------------------
;; Hiccup helpers
;; -----------------------------------------------------------------------------

(defn attrs
  [node]
  (when (and (vector? node)
             (map? (second node)))
    (second node)))

(defn children
  [node]
  (let [xs (rest node)]
    (if (map? (first xs))
      (rest xs)
      xs)))

(defn hiccup-nodes
  [x]
  (cond
    (vector? x)
    (cons x (mapcat hiccup-nodes (children x)))

    (sequential? x)
    (mapcat hiccup-nodes x)

    :else
    nil))

(defn find-by-id
  [node id]
  (first
   (filter
    (fn [x]
      (= id (:id (attrs x))))
    (hiccup-nodes node))))

(defn find-first-attr
  [node k]
  (first
   (filter
    (fn [x]
      (contains? (attrs x) k))
    (hiccup-nodes node))))

(defn runtime-resource
  []
  (some io/resource
        ["public/js/gesso-live.js"
         "public/gesso/gesso-live.js"
         "js/gesso-live.js"
         "gesso/gesso-live.js"]))

;; -----------------------------------------------------------------------------
;; Human Help continuity wiring
;; -----------------------------------------------------------------------------

(deftest request-list-fragment-options-continuity-contract-test
  (let [opts (human-live/fragment-options :request-list)]
    (testing "Human Help declares continuity on the request-list panel"
      (is (= human-live/request-list-client-continuity
             (:client-continuity opts)))
      (is (= {:enabled true
              :preserve {:scroll {:selector "[data-humanhelp-request-card]"}
                         :focus true}}
             (:client-continuity opts))))

    (testing "board state remains ordinary server-relevant hx-include state"
      (is (= (str "#" views/board-state-form-id)
             (get-in opts [:root-attrs :hx-include]))))))

(deftest request-list-panel-renders-continuity-root-test
  (let [panel (human-live/request-list-panel)
        root-attrs (attrs panel)
        target (first (children panel))
        target-attrs (attrs target)
        config (:data-gesso-live-continuity-config root-attrs)]
    (testing "the outer root is stable and owns HTMX/SSE/continuity behavior"
      (is (= :div (first panel)))
      (is (= "sse" (:hx-ext root-attrs)))
      (is (= views/request-list-dom-id (:data-gesso-live-fragment root-attrs)))
      (is (= (routes/request-list-fragment-url) (:hx-get root-attrs)))
      (is (= "#humanhelp-request-list-fragment" (:hx-target root-attrs)))
      (is (= "outerHTML show:none focus-scroll:false" (:hx-swap root-attrs)))
      (is (= (str "#" views/board-state-form-id) (:hx-include root-attrs)))
      (is (= "true" (:data-gesso-live-continuity root-attrs)))
      (is (= views/request-list-dom-id
             (:data-gesso-live-continuity-fragment root-attrs)))
      (is (string? config))
      (is (str/includes? config "\"selector\":\"[data-humanhelp-request-card]\""))
      (is (str/includes? config "\"focus\":true")))

    (testing "the replaceable child is just the target id"
      (is (= :div (first target)))
      (is (= {:id views/request-list-dom-id}
             target-attrs)))))

(deftest request-list-replacement-paths-use-the-same-target-test
  (testing "search replaces the same inner request-list fragment target"
    (let [form (views/search-control nil {:view-state {}})
          form-attrs (attrs form)]
      (is (= views/board-state-form-id (:id form-attrs)))
      (is (= (str "#" views/request-list-dom-id) (:hx-target form-attrs)))
      (is (= "outerHTML" (:hx-swap form-attrs)))))

  (testing "prune/self-refresh keeps the same fragment id and includes board state"
    (let [node (views/request-list-fragment
                {:ctx nil
                 :user nil
                 :view-state {}
                 :requests []
                 :latest-revision 10
                 :next-prune-ms 250})
          node-attrs (attrs node)]
      (is (= views/request-list-dom-id (:id node-attrs)))
      (is (= (routes/request-list-fragment-url) (:hx-get node-attrs)))
      (is (= "load delay:250ms" (:hx-trigger node-attrs)))
      (is (= "outerHTML" (:hx-swap node-attrs)))
      (is (= (str "#" views/board-state-form-id) (:hx-include node-attrs)))))

  (testing "OOB replacement uses the same inner request-list fragment id"
    (let [payload [:div {:id views/request-list-dom-id} "request list"]
          node (views/replace-request-list-oob payload)
          node-attrs (attrs node)]
      (is (= views/request-list-dom-id (:id node-attrs)))
      (is (= "outerHTML" (:hx-swap-oob node-attrs))))))

(deftest request-list-fragment-renders-continuity-anchor-candidates-test
  ;; This test does not need to know every request-card detail. It asserts the
  ;; minimum contract required by Human Help's selector-based continuity config:
  ;;
  ;;   {:scroll {:selector "[data-humanhelp-request-card]"}}
  ;;
  ;; The request-card component should render at least one element matching that
  ;; selector and that element should have stable identity, usually an id.
  ;;
  ;; If this fails, the app-side continuity declaration and rendered HTML do not
  ;; agree.
  (let [request {:request/id #uuid "00000000-0000-0000-0000-000000000001"
                 :request/number 1
                 :request/status :open
                 :request/title "Need help finding a rake"
                 :request/customer-name "Ada"
                 :request/area "Garden"
                 :request/details "Aisle 4"
                 :request/created-at-ms 0}
        node (views/request-list-fragment
              {:ctx nil
               :user {:user/id #uuid "00000000-0000-0000-0000-000000000002"
                      :user/email "helper@example.com"}
               :view-state {}
               :requests [request]
               :latest-revision 1})
        card (find-first-attr node :data-humanhelp-request-card)
        card-attrs (attrs card)]
    (is card "request-list-fragment should render at least one data-humanhelp-request-card element")
    (when card
      (is (:data-humanhelp-request-card card-attrs))
      (is (or (:id card-attrs)
              (:data-gesso-continuity-key card-attrs)
              (:data-key card-attrs))
          "anchor-scroll needs stable card identity: id, data-gesso-continuity-key, or data-key"))))

(deftest packaged-gesso-live-runtime-supports-continuity-paths-test
  (let [resource (runtime-resource)]
    (is resource
        "Gessokit must package gesso-live.js under resources/public/js or resources/public/gesso")
    (when resource
      (let [source (slurp resource)]
        (testing "runtime is present"
          (is (str/includes? source "window.gessoLive"))
          (is (str/includes? source "continuity")))

        (testing "runtime observes ordinary and OOB HTMX replacement paths"
          (is (str/includes? source "htmx:beforeSwap"))
          (is (str/includes? source "htmx:afterSettle"))
          (is (str/includes? source "htmx:oobBeforeSwap"))
          (is (str/includes? source "htmx:oobAfterSwap")))

        (testing "runtime has raw scroll fallback in addition to anchor logic"
          (is (or (str/includes? source "rawWindowScrollState")
                  (str/includes? source "fallbackScroll")
                  (str/includes? source "pageYOffset"))))))))
