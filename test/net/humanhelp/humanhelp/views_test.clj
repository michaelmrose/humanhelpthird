(ns net.humanhelp.humanhelp.views-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [net.humanhelp.client-plumbing :as client-plumbing]
   [net.humanhelp.humanhelp.model :as model]
   [net.humanhelp.humanhelp.routes :as routes]
   [net.humanhelp.humanhelp.views :as views]
   [net.humanhelp.ui :as ui]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def ctx
  {:anti-forgery-token "test-token"})

(def owner
  {:user/id "user-owner"
   :user/email "owner@example.com"})

(def uuid-user
  {:user/id "aef38467-ef20-4f58-b9ab-f9c0fb4c9bb2"
   :user/email "aef38467-ef20-4f58-b9ab-f9c0fb4c9bb2"})

(def helper
  {:user/id "user-helper"
   :user/email "helper@example.com"})

(def view-state
  {:search "garden"
   :visible-revision 3
   :created-order :oldest
   :mine-first? true
   :unclaimed-first? false
   :show-terminal? true})

(defn request
  [overrides]
  (merge
   {:request/id "hh-req-1"
    :request/number 1
    :request/store-id model/store-id
    :request/title "Need help finding a rake"
    :request/area "Garden"
    :request/details "Looking for a sturdy rake for leaves."
    :request/customer-user-id "user-owner"
    :request/customer-name "Jon"
    :request/status :open
    :request/claimed-by nil
    :request/claimed-by-email nil
    :request/created-at-ms 1780471110000
    :request/updated-at-ms 1780471110000
    :request/created-revision 1
    :request/updated-revision 1}
   overrides))

(def open-request
  (request {}))

(def claimed-request
  (request
   {:request/id "hh-req-2"
    :request/number 2
    :request/title "Can someone help load soil?"
    :request/details "Six heavy bags near the entrance to the garden center."
    :request/customer-user-id "seed-user-2"
    :request/customer-name "Avery"
    :request/status :claimed
    :request/claimed-by "user-helper"
    :request/claimed-by-email "helper@example.com"
    :request/created-revision 2
    :request/updated-revision 2}))

(def done-request
  (request
   {:request/id "hh-req-3"
    :request/number 3
    :request/title "Done request"
    :request/status :done
    :request/terminal-at-ms 1780471210000
    :request/created-revision 3
    :request/updated-revision 3}))

;; -----------------------------------------------------------------------------
;; Hiccup inspection helpers
;; -----------------------------------------------------------------------------

(defn hiccup-branch?
  [x]
  (and (sequential? x)
       (not (string? x))
       (not (map? x))))

(defn hiccup-seq
  [x]
  (tree-seq hiccup-branch? seq x))

(defn node?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defn attrs
  [node]
  (when (and (vector? node)
             (map? (second node)))
    (second node)))

(defn children
  [node]
  (let [xs (if (map? (second node))
             (nnext node)
             (next node))]
    (remove nil? xs)))

(defn text-nodes
  [tree]
  (filter string? (hiccup-seq tree)))

(defn contains-text?
  [tree text]
  (boolean
   (some #(str/includes? % (str text))
         (text-nodes tree))))

(defn nodes
  [tree]
  (filter node? (hiccup-seq tree)))

(defn find-by-id
  [tree id]
  (some
   (fn [node]
     (when (= id (:id (attrs node)))
       node))
   (nodes tree)))

(defn find-by-attr
  [tree k v]
  (some
   (fn [node]
     (when (= v (get (attrs node) k))
       node))
   (nodes tree)))

(defn nodes-by-tag
  [tree tag]
  (filter #(= tag (first %))
          (nodes tree)))

(defn inputs
  [tree]
  (nodes-by-tag tree :input))

(defn selects
  [tree]
  (nodes-by-tag tree :select))

(defn controls
  [tree]
  (concat (inputs tree)
          (selects tree)
          (nodes-by-tag tree :textarea)
          (nodes-by-tag tree :button)))

(defn input-by-name
  [tree name]
  (some
   (fn [node]
     (when (= name (:name (attrs node)))
       node))
   (inputs tree)))

(defn control-by-name
  [tree name]
  (some
   (fn [node]
     (when (= name (:name (attrs node)))
       node))
   (controls tree)))

(defn hidden-input-value
  [tree name]
  (some
   (fn [node]
     (let [a (attrs node)]
       (when (and (= "hidden" (:type a))
                  (= name (:name a)))
         (:value a))))
   (inputs tree)))

(defn checked-control?
  [node]
  (let [checked (:checked (attrs node))]
    (or (= true checked)
        (= "checked" checked)
        (= "true" checked))))

(defn oob-by-id
  [tree id]
  (some
   (fn [node]
     (let [a (attrs node)]
       (when (and (= id (:id a))
                  (= "outerHTML" (:hx-swap-oob a)))
         node)))
   (nodes tree)))

(defn open-dialog?
  [node]
  (let [a (attrs node)]
    (or (= true (:data-dialog-open a))
        (= "true" (:data-dialog-open a))
        (= true (:open a)))))

;; -----------------------------------------------------------------------------
;; User helpers / small helpers
;; -----------------------------------------------------------------------------

(deftest user-email-test
  (is (= "owner@example.com"
         (views/user-email owner)))
  (is (= "owner@example.com"
         (views/account-email owner)))

  (testing "uuid-looking ids are not treated as display email"
    (is (nil? (views/user-email uuid-user)))
    (is (nil? (views/account-email uuid-user)))))

(deftest selector-test
  (is (= (str "#" views/board-state-form-id)
         (views/board-state-selector)))

  (is (= (str "#" views/board-state-form-id
              " input[name="
              routes/visible-revision-param
              "]")
         (views/board-state-input-selector
          routes/visible-revision-param)))

  (is (= (str "#" views/search-input-dom-id
              ", "
              (views/board-state-input-selector
               routes/visible-revision-param))
         (views/board-options-preserved-state-selector))))

(deftest hidden-input-test
  (is (= [:input {:type "hidden"
                  :name "x"
                  :value "y"}]
         (views/hidden-input "x" "y")))
  (is (nil? (views/hidden-input "x" nil))))

(deftest board-state-option-hidden-inputs-test
  (let [node (views/board-state-option-hidden-inputs view-state)]
    (is (= "oldest"
           (hidden-input-value node routes/created-order-param)))
    (is (= "true"
           (hidden-input-value node routes/mine-first-param)))
    (is (= "false"
           (hidden-input-value node routes/unclaimed-first-param)))
    (is (= "true"
           (hidden-input-value node routes/show-terminal-param)))))

(deftest view-state-hidden-inputs-test
  (let [node (views/view-state-hidden-inputs view-state)]
    (is (= "garden"
           (hidden-input-value node routes/search-param)))
    (is (= 3
           (hidden-input-value node routes/visible-revision-param)))
    (is (= "oldest"
           (hidden-input-value node routes/created-order-param)))
    (is (= "true"
           (hidden-input-value node routes/mine-first-param)))
    (is (= "false"
           (hidden-input-value node routes/unclaimed-first-param)))
    (is (= "true"
           (hidden-input-value node routes/show-terminal-param)))))

(deftest board-state-hidden-inputs-test
  (let [node (views/board-state-hidden-inputs view-state)]
    (testing "search is deliberately not duplicated as a hidden q input"
      (is (nil? (input-by-name node routes/search-param))))

    (is (= 3
           (hidden-input-value node routes/visible-revision-param)))
    (is (= "oldest"
           (hidden-input-value node routes/created-order-param)))
    (is (= "true"
           (hidden-input-value node routes/mine-first-param)))
    (is (= "false"
           (hidden-input-value node routes/unclaimed-first-param)))
    (is (= "true"
           (hidden-input-value node routes/show-terminal-param)))))

;; -----------------------------------------------------------------------------
;; Create request dialog/form
;; -----------------------------------------------------------------------------


(deftest create-request-form-contract-test
  (let [node (views/create-request-form
              ctx
              {:user owner
               :values {:customer-name "Jon"
                        :area "Garden"
                        :title "Need a rake"
                        :details "Near aisle 4"}
               :errors {}})]
    (is (= :form (first node)))
    (is (= (routes/create-request-url)
           (:hx-post (attrs node))))
    (is (= "none"
           (:hx-swap (attrs node))))
    (is (= (views/board-state-selector)
           (:hx-include (attrs node))))

    (is (= "Jon"
           (:value (attrs (find-by-id node "humanhelp-create-customer-name")))))
    (is (= "Garden"
           (:value (attrs (find-by-id node "humanhelp-create-area")))))
    (is (= "Need a rake"
           (:value (attrs (find-by-id node "humanhelp-create-title")))))

    ;; Details is a textarea, so its value is rendered as content rather than
    ;; as a :value attr.
    (is (contains-text? node "Near aisle 4"))

    (is (contains-text? node "Cancel"))
    (is (contains-text? node "Create"))))
(deftest create-request-form-error-test
  (let [node (views/create-request-form
              ctx
              {:user owner
               :values {:title ""
                        :area ""}
               :errors {:title "Title required."
                        :area "Area required."}})]
    (is (contains-text? node "Title required."))
    (is (contains-text? node "Area required."))))

(deftest create-request-dialog-test
  (let [node (views/create-request-dialog
              ctx
              {:user owner
               :values {}
               :errors {}
               :open? true})]
    (is (find-by-id node views/create-request-dialog-id))
    (is (find-by-id node views/create-request-dialog-body-id))
    (is (contains-text? node "Create request"))
    (is (open-dialog? (find-by-id node views/create-request-dialog-id)))))

(deftest create-request-dialog-fragment-test
  (let [node (views/create-request-dialog-fragment
              ctx
              {:user owner
               :values {}
               :errors {}
               :open? true})]
    (is (find-by-id node views/create-request-dialog-id))
    (is (find-by-id node views/create-request-dialog-body-id))
    (is (contains-text? node "Create request"))))

;; -----------------------------------------------------------------------------
;; Board options
;; -----------------------------------------------------------------------------

(deftest board-options-form-contract-test
  (let [node (views/board-options-form
              ctx
              {:view-state view-state})]
    (is (= :form (first node)))
    (is (= (routes/apply-board-options-url)
           (:hx-post (attrs node))))
    (is (= "none"
           (:hx-swap (attrs node))))
    (is (= (views/board-options-preserved-state-selector)
           (:hx-include (attrs node))))
    (is (= true
           (:data-humanhelp-board-options-form (attrs node))))

    (testing "options form edits option params directly"
      (is (control-by-name node routes/created-order-param))
      (is (control-by-name node routes/mine-first-param))
      (is (control-by-name node routes/unclaimed-first-param))
      (is (control-by-name node routes/show-terminal-param)))

    (is (contains-text? node "Done"))
    (is (contains-text? node "Cancel"))))

(deftest board-options-dialog-test
  (let [node (views/board-options-dialog
              ctx
              {:view-state view-state
               :open? true})]
    (is (find-by-id node views/board-options-dialog-id))
    (is (find-by-id node views/board-options-dialog-body-id))
    (is (contains-text? node "Board options"))
    (is (open-dialog? (find-by-id node views/board-options-dialog-id)))))

;; -----------------------------------------------------------------------------
;; Toolbar/search/list fragments
;; -----------------------------------------------------------------------------

(deftest refresh-form-contract-test
  (let [node (views/refresh-form ctx view-state true)]
    (is (= :form (first node)))
    (is (= (routes/refresh-requests-url)
           (:hx-post (attrs node))))
    (is (= "none"
           (:hx-swap (attrs node))))
    (is (= (views/board-state-selector)
           (:hx-include (attrs node))))
    (is (find-by-attr node :data-humanhelp-refresh-button-frame true))))

(deftest request-toolbar-heading-test
  (let [node (views/request-toolbar-heading
              {:open-count 2
               :pending-open-count 1})]
    (is (contains-text? node "Requests"))
    (is (contains-text? node "2 open"))
    (is (contains-text? node "+1 new"))))

(deftest request-toolbar-fragment-test
  (let [node (views/request-toolbar-fragment
              {:ctx ctx
               :user owner
               :view-state view-state
               :open-count 2
               :pending-open-count 1
               :stale? true
               :latest-revision 9})]
    (is (= views/request-toolbar-dom-id (:id (attrs node))))
    (is (= "request-toolbar" (:data-humanhelp-fragment (attrs node))))
    (is (= 9 (:data-latest-revision (attrs node))))
    (is (contains-text? node "Requests"))
    (is (contains-text? node "+1 new"))
    (is (contains-text? node "New request data is available"))
    (is (find-by-id node views/create-request-dialog-id))
    (is (find-by-id node views/board-options-dialog-id))))

(deftest search-control-contract-test
  (let [node (views/search-control ctx {:view-state view-state})]
    (is (= :form (first node)))
    (is (= views/board-state-form-id (:id (attrs node))))
    (is (= (routes/search-requests-url)
           (:hx-get (attrs node))))
    (is (= (str "#" views/request-list-dom-id)
           (:hx-target (attrs node))))
    (is (= "outerHTML"
           (:hx-swap (attrs node))))

    (testing "search form has exactly one q input: the visible search box"
      (is (= 1
             (count
              (filter #(= routes/search-param (:name (attrs %)))
                      (inputs node)))))
      (is (= views/search-input-dom-id
             (:id (attrs (input-by-name node routes/search-param)))))
      (is (= "garden"
             (:value (attrs (input-by-name node routes/search-param))))))

    (testing "search form preserves non-search board state"
      (is (= 3
             (hidden-input-value node routes/visible-revision-param)))
      (is (= "oldest"
             (hidden-input-value node routes/created-order-param)))
      (is (= "true"
             (hidden-input-value node routes/mine-first-param)))
      (is (= "false"
             (hidden-input-value node routes/unclaimed-first-param)))
      (is (= "true"
             (hidden-input-value node routes/show-terminal-param))))))

(deftest empty-request-list-test
  (is (contains-text?
       (views/empty-request-list {:view-state {:search ""}})
       "No requests yet"))
  (is (contains-text?
       (views/empty-request-list {:view-state {:search "rake"}})
       "No matching requests")))

(deftest request-list-fragment-test
  (let [node (views/request-list-fragment
              {:ctx ctx
               :user helper
               :view-state view-state
               :requests [open-request claimed-request]
               :latest-revision 9})]
    (is (= views/request-list-dom-id (:id (attrs node))))
    (is (= "request-list" (:data-humanhelp-fragment (attrs node))))
    (is (= 9 (:data-latest-revision (attrs node))))
    (is (contains-text? node "Need help finding a rake"))
    (is (contains-text? node "Can someone help load soil?"))
    (is (contains-text? node "claimed by helper@example.com"))

    (testing "request list renders a Gesso accordion but does not own selected state"
      (is (find-by-attr node :data-humanhelp-request-accordion true))
      (is (nil? (hidden-input-value node "selected"))))))

(deftest request-list-empty-fragment-test
  (let [node (views/request-list-fragment
              {:ctx ctx
               :user helper
               :view-state {:search ""}
               :requests []
               :latest-revision 9})]
    (is (= views/request-list-dom-id (:id (attrs node))))
    (is (contains-text? node "No requests yet"))))

;; -----------------------------------------------------------------------------
;; Page composition
;; -----------------------------------------------------------------------------

(deftest board-card-fallback-test
  (let [node (views/board-card
              ctx
              {:view-state view-state})]
    (is (find-by-id node views/request-toolbar-dom-id))
    (is (find-by-id node views/request-list-dom-id))
    (is (find-by-id node views/board-state-form-id))
    (is (contains-text? node "Request toolbar loading"))
    (is (contains-text? node "Request list loading"))))

(deftest page-composition-test
  (testing "page passes user to shell and installs the app listener with board-state include"
    (with-redefs [ui/page-shell
                  (fn [ctx opts & body]
                    (into [:page-shell {:ctx ctx
                                        :opts opts}]
                          body))

                  ui/container
                  (fn [& body]
                    (into [:container] body))

                  client-plumbing/listener
                  (fn [& args]
                    [:listener {:id "client-listener"
                                :args args}])]
      (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar panel"]
            request-list [:div {:id views/request-list-dom-id} "list panel"]
            node (views/page
                  ctx
                  {:user owner
                   :view-state view-state
                   :request-toolbar-panel toolbar
                   :request-list-panel request-list})
            listener (find-by-id node "client-listener")]
        (is (= :page-shell (first node)))
        (is (= ctx (:ctx (attrs node))))
        (is (= {:user owner} (:opts (attrs node))))

        (is listener)
        (is (= ctx (first (:args (attrs listener)))))
        (is (= {:trigger-attrs {:hx-include (views/board-state-selector)}}
               (second (:args (attrs listener)))))

        (is (find-by-id node views/request-toolbar-dom-id))
        (is (find-by-id node views/request-list-dom-id))
        (is (find-by-id node views/board-state-form-id))
        (is (contains-text? node "Human Help Fast."))
        (is (contains-text? node "toolbar panel"))
        (is (contains-text? node "list panel"))))))

;; -----------------------------------------------------------------------------
;; OOB / response helpers
;; -----------------------------------------------------------------------------

(deftest replace-oob-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        dialog [:div {:id views/create-request-dialog-id} "dialog"]
        toolbar-oob (views/replace-toolbar-oob toolbar)
        list-oob (views/replace-request-list-oob request-list)
        dialog-oob (views/replace-dialog-oob dialog)]
    (is (= views/request-toolbar-dom-id (:id (attrs toolbar-oob))))
    (is (= views/request-list-dom-id (:id (attrs list-oob))))
    (is (= views/create-request-dialog-id (:id (attrs dialog-oob))))
    (is (= "outerHTML" (:hx-swap-oob (attrs toolbar-oob))))
    (is (= "outerHTML" (:hx-swap-oob (attrs list-oob))))
    (is (= "outerHTML" (:hx-swap-oob (attrs dialog-oob))))))

(deftest board-state-oob-contract-test
  (let [node (views/replace-board-state-oob ctx view-state)]
    (is (= views/board-state-form-id (:id (attrs node))))
    (is (= "outerHTML" (:hx-swap-oob (attrs node))))
    (is (= (routes/search-requests-url)
           (:hx-get (attrs node))))
    (is (= "garden"
           (:value (attrs (find-by-id node views/search-input-dom-id)))))
    (is (= 3
           (hidden-input-value node routes/visible-revision-param)))
    (is (= "oldest"
           (hidden-input-value node routes/created-order-param)))
    (is (= "true"
           (hidden-input-value node routes/mine-first-param)))
    (is (= "false"
           (hidden-input-value node routes/unclaimed-first-param)))
    (is (= "true"
           (hidden-input-value node routes/show-terminal-param)))))

(deftest with-board-state-oob-contract-test
  (let [payload [:div {:id "payload"} "payload"]
        node (views/with-board-state-oob ctx view-state payload)
        kids (children node)
        board-state-oob (first kids)]
    (is (= :div (first node)))
    (is (= "contents" (get-in (attrs node) [:style :display])))

    (testing "board-state OOB comes first so later OOB fragments see current state"
      (is (= views/board-state-form-id (:id (attrs board-state-oob))))
      (is (= "outerHTML" (:hx-swap-oob (attrs board-state-oob)))))

    (is (find-by-id node "payload"))
    (is (contains-text? node "payload"))))

(deftest fragments-oob-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "list"]
        node (views/fragments-oob
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))))

(deftest create-validation-error-oob-contract-test
  (let [node (views/create-request-validation-error
              ctx
              {:user owner
               :values {:title ""
                        :area ""}
               :errors {:title "Title required."
                        :area "Area required."}})
        dialog (oob-by-id node views/create-request-dialog-id)]
    (is dialog)
    (is (= "outerHTML" (:hx-swap-oob (attrs dialog))))
    (is (contains-text? node "Title required."))
    (is (contains-text? node "Area required."))))

(deftest create-success-oob-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "request list"]
        node (views/create-request-success
              ctx
              {:request open-request
               :toolbar toolbar
               :request-list request-list})]
    (testing "successful create replaces the board fragments"
      (is (oob-by-id node views/request-toolbar-dom-id))
      (is (oob-by-id node views/request-list-dom-id)))

    (testing "successful create emits a success toast"
      (is (contains-text? node "Request created"))
      (is (contains-text? node "Request #1 is now on the board.")))))

(deftest refreshed-request-board-fragments-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "request list"]
        node (views/refreshed-request-board-fragments
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))))

(deftest lifecycle-result-oob-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "request list"]
        node (views/request-lifecycle-result
              {:action :claim
               :request claimed-request
               :toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (contains-text? node "Claim"))
    (is (contains-text? node "Claimed request #2."))))

(deftest request-action-error-contract-test
  (let [node (views/request-action-error
              {:result {:error {:message "Nope."}}})]
    (is (contains-text? node "Request not updated"))
    (is (contains-text? node "Nope."))))

(deftest reset-demo-result-contract-test
  (let [toolbar [:div {:id views/request-toolbar-dom-id} "toolbar"]
        request-list [:div {:id views/request-list-dom-id} "request list"]
        node (views/reset-demo-result
              {:toolbar toolbar
               :request-list request-list})]
    (is (oob-by-id node views/request-toolbar-dom-id))
    (is (oob-by-id node views/request-list-dom-id))
    (is (contains-text? node "Demo reset"))
    (is (contains-text? node "The Human Help request board was reset."))))
