(ns net.humanhelp.example.components.request-card.core-test
  "Contract tests for the example Request card backed by production models and Choreo.

   The card is presentation only: it renders production Request/User documents,
   consumes production Choreo affordances from example.board, and may attach a
   Gesso optimistic action only when the board can justify an authoritative XTDB
   observation basis. It must never recover the former example.model semantics."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.ui :as ui]
   [gesso.model.command :as command]
   [net.humanhelp.example.board :as board]
   [net.humanhelp.example.components.request-card.core :as card]
   [net.humanhelp.example.routes :as routes]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.request.domain :as request.domain]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.model.user.domain :as user.domain])
  (:import
   [java.time Instant]
   [java.util UUID]))

(def request-id
  (UUID/fromString "71000000-0000-0000-0000-000000000001"))
(def organization-id
  (UUID/fromString "72000000-0000-0000-0000-000000000001"))
(def location-id
  (UUID/fromString "73000000-0000-0000-0000-000000000001"))
(def requestor-id
  (UUID/fromString "74000000-0000-0000-0000-000000000001"))
(def helper-id
  (UUID/fromString "74000000-0000-0000-0000-000000000002"))
(def created-at
  (Instant/parse "2026-09-03T20:00:00Z"))

(defn- production-user
  [id display-name]
  (command/after
   (user.domain/create-user-command
    {:id id
     :display-name display-name
     :email (str (str/lower-case display-name) "@example.test")
     :now created-at})))

(def requestor-user
  (production-user requestor-id "Requestor"))
(def helper-user
  (production-user helper-id "Helper"))

(def open-request
  (command/after
   (request.domain/create-request-command
    {:id request-id
     :organization-id organization-id
     :location-id location-id
     :requestor (request.domain/user-requestor requestor-id)
     :content {:title "Need groceries"
               :details "Please pick up milk."
               :location-detail "Front desk"}
     :now created-at})))

(def open-row
  {:request open-request
   :primary-assignment nil
   :requestor-user requestor-user
   :primary-helper-user nil})

(def claim-affordance
  {:operation request.choreo/claim-operation
   :capability request.choreo/claim-capability
   :arguments {:request-id request-id}})

(defn- elements
  [markup]
  (filter
   #(and (vector? %)
         (keyword? (first %)))
   (tree-seq coll? seq markup)))

(defn- first-element
  [markup tag]
  (some #(when (= tag (first %)) %) (elements markup)))

(defn- attrs
  [element]
  (when (map? (second element))
    (second element)))

(defn- button-attrs
  [markup]
  (some-> (first-element markup :button) attrs))

(defn- decoded-optimistic-action
  [markup]
  (some-> (button-attrs markup)
          (get ui/optimistic-action-attr)
          edn/read-string))

(deftest request-card-does-not-depend-on-example-model-test
  (let [dependencies
        (->> (ns-aliases
              'net.humanhelp.example.components.request-card.core)
             vals
             (map ns-name)
             set)]
    (is (not (contains? dependencies 'net.humanhelp.example.model)))
    (is (contains? dependencies 'net.humanhelp.example.board))
    (is (contains? dependencies 'net.humanhelp.site.model.request.core))
    (is (contains? dependencies 'net.humanhelp.site.model.user.core))))

(deftest request-card-operation-labels-use-production-vocabulary-test
  (is (= "Claim" (card/action-label :request/claim)))
  (is (= "Unclaim" (card/action-label :request/unclaim)))
  (is (= "On the way" (card/action-label :request/mark-on-the-way)))
  (is (= "Done" (card/action-label :request/complete)))
  (is (= "Cancel" (card/action-label :request/cancel)))
  (is (= "Reassign" (card/action-label :request/reassign)))
  (is (= "/app/requests/71000000-0000-0000-0000-000000000001/mark-on-the-way"
         (routes/operation-url request-id :request/mark-on-the-way)))
  (is (= "/app/requests/71000000-0000-0000-0000-000000000001/complete"
         (routes/operation-url request-id :request/complete))))

(deftest ordinary-action-remains-an-authoritative-htmx-post-without-basis-test
  (with-redefs [board/optimistic-binding (constantly nil)]
    (let [markup
          (card/action-button
           {:anti-forgery-token "token"}
           open-row
           claim-affordance
           "#humanhelp-board-state")
          button
          (button-attrs markup)]
      (is (= (routes/claim-request-url request-id)
             (:hx-post button)))
      (is (= "none" (:hx-swap button)))
      (is (= "claim"
             (:data-humanhelp-request-operation button)))
      (is (not (contains? button ui/optimistic-action-attr)))
      (is (nil? (decoded-optimistic-action markup))))))

(deftest optimistic-action-separates-production-capability-from-closed-binding-test
  (let [basis {:tx-id 81
               :system-time "2026-09-03T20:00:01Z"}
        revision (request/revision open-request)
        binding {:arguments {:request-id request-id}
                 :observed-basis basis
                 :scope [:request request-id]
                 :fact-versions {:request/revision revision}
                 :target-id (card/request-target-id open-row)
                 :capability request.choreo/claim-capability}]
    (with-redefs [board/optimistic-binding
                  (fn [_ctx row operation arguments target-id]
                    (is (= request-id (board/row-request-id row)))
                    (is (= :request/claim operation))
                    (is (= {:request-id request-id} arguments))
                    (is (= (card/request-target-id open-row) target-id))
                    binding)]
      (let [markup
            (card/action-button
             {:anti-forgery-token "token"}
             open-row
             claim-affordance
             "#humanhelp-board-state")
            button
            (button-attrs markup)
            action
            (decoded-optimistic-action markup)]
        (is (= (routes/claim-request-url request-id) (:hx-post button)))
        (is (= :request/claim (:operation action)))
        (is (= {:request-id request-id} (:arguments action)))
        (is (= basis (:observed-basis action)))
        (is (= [:request request-id] (:scope action)))
        (is (= {:request/revision revision} (:fact-versions action)))
        (is (= (card/request-target-id open-row) (:target-id action)))
        (is (not (contains? action :capability))
            "Capability-owned data must not leak into Gesso's closed per-render binding.")))))

(deftest request-card-consumes-board-affordances-test
  (let [calls (atom [])]
    (with-redefs [board/operation-affordances
                  (fn [row viewer-id]
                    (swap! calls conj [row viewer-id])
                    [claim-affordance])
                  board/optimistic-binding (constantly nil)]
      (let [markup
            (card/request-card
             {:anti-forgery-token "token"}
             {:row open-row
              :viewer helper-user
              :board-state-selector "#humanhelp-board-state"})
            button
            (button-attrs markup)]
        (is (= [[open-row helper-id]] @calls))
        (is (= (routes/claim-request-url request-id) (:hx-post button)))
        (is (some #{"Claim"}
                  (filter string? (tree-seq coll? seq markup))))))))

(deftest request-card-fails-closed-on-nonproduction-input-test
  (testing "a demo-shaped Request map is not accepted"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires a production Request document"
         (card/request-card
          {}
          {:row {:request {:id request-id
                           :status :open}}
           :viewer helper-user
           :board-state-selector "#humanhelp-board-state"}))))

  (testing "viewer identity must be a production User UUID"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires a production User viewer"
         (with-redefs [user/user-id (constantly "not-a-uuid")]
           (card/request-card
            {}
            {:row open-row
             :viewer helper-user
             :board-state-selector "#humanhelp-board-state"}))))))
