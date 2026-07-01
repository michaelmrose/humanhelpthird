(ns net.humanhelp.example.model-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [net.humanhelp.example.model :as model]
   [malli.core :as m]
   [xtdb.node :as xtn]))

;; -----------------------------------------------------------------------------
;; XTDB fixture
;; -----------------------------------------------------------------------------

(defonce !ctx
  (atom nil))

(defn ctx
  []
  (or @!ctx
      (throw
       (ex-info "model-test ctx has not been initialized."
                {}))))

(defn xtdb-fixture
  [f]
  (with-open [node (xtn/start-node)]
    (reset! !ctx {:biff/node node
                  :biff/conn node
                  :xtdb/node node})
    (try
      (f)
      (finally
        (reset! !ctx nil)))))

(defn reset-model-fixture
  [f]
  (model/reset-demo-state! (ctx))
  (try
    (f)
    (finally
      (model/reset-demo-state! (ctx)))))

(use-fixtures :once xtdb-fixture)
(use-fixtures :each reset-model-fixture)

;; -----------------------------------------------------------------------------
;; Users / input fixtures
;; -----------------------------------------------------------------------------

(def user-owner
  {:user/id "user-owner"
   :user/email "owner@example.com"})

(def user-helper
  {:user/id "user-helper"
   :user/email "helper@example.com"})

(def user-other
  {:user/id "user-other"
   :user/email "other@example.com"})

(defn valid-input
  [overrides]
  (merge
   {:title "Need help finding a rake"
    :area "Garden"
    :details "Looking for a sturdy rake for leaves."
    :customer-name "Jon"}
   overrides))

(defn req
  [overrides]
  (merge
   {:request/id "hh-req-1"
    :request/number 1
    :request/store-id model/store-id
    :request/title "Need help finding a rake"
    :request/area "Garden"
    :request/details "Looking for a sturdy rake for bark and leaves."
    :request/customer-user-id "user-owner"
    :request/customer-name "Jon"
    :request/status :open
    :request/claimed-by nil
    :request/claimed-by-email nil
    :request/created-at-ms 1000000
    :request/updated-at-ms 1000000
    :request/terminal-at-ms nil
    :request/created-revision 1
    :request/updated-revision 1}
   overrides))

;; -----------------------------------------------------------------------------
;; Generic helpers
;; -----------------------------------------------------------------------------

(def required-request-keys
  #{:request/id
    :request/number
    :request/store-id
    :request/title
    :request/area
    :request/customer-user-id
    :request/status
    :request/created-at-ms
    :request/updated-at-ms
    :request/created-revision
    :request/updated-revision})

(def required-event-keys
  #{:event/id
    :event/store-id
    :event/kind
    :event/message
    :event/request-id
    :event/at-ms
    :event/revision})

(def valid-statuses
  #{:open :claimed :done :cancelled})

(def active-statuses
  #{:open :claimed})

(defn without-nil-vals
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn request-shape-valid?
  [request]
  (and
   (map? request)
   (every? #(contains? request %) required-request-keys)
   (string? (:request/id request))
   (pos-int? (:request/number request))
   (= model/store-id (:request/store-id request))
   (string? (:request/title request))
   (string? (:request/area request))
   (contains? valid-statuses (:request/status request))
   (integer? (:request/created-at-ms request))
   (integer? (:request/updated-at-ms request))
   (or (nil? (:request/terminal-at-ms request))
       (integer? (:request/terminal-at-ms request)))
   (integer? (:request/created-revision request))
   (integer? (:request/updated-revision request))))

(defn event-shape-valid?
  [event]
  (and
   (map? event)
   (every? #(contains? event %) required-event-keys)
   (string? (:event/id event))
   (= model/store-id (:event/store-id event))
   (keyword? (:event/kind event))
   (string? (:event/message event))
   (string? (:event/request-id event))
   (integer? (:event/at-ms event))
   (integer? (:event/revision event))))

(defn active-request?
  [request]
  (contains? active-statuses (:request/status request)))

(defn request-ids
  [requests]
  (mapv :request/id requests))

(defn request-titles
  [requests]
  (set (map :request/title requests)))

(defn requests-by-id
  [requests]
  (into {}
        (map (juxt :request/id identity))
        requests))

(defn find-request-by-title
  [title]
  (first
   (filter #(= title (:request/title %))
           (model/all-requests (ctx)))))

(defn find-event
  [{:keys [kind request-id action]}]
  (first
   (filter
    (fn [event]
      (and
       (or (nil? kind)
           (= kind (:event/kind event)))
       (or (nil? request-id)
           (= request-id (:event/request-id event)))
       (or (nil? action)
           (= action (:event/action event)))))
    (model/all-events (ctx)))))

(defn seeded-open-request
  []
  (first
   (filter #(= :open (:request/status %))
           (model/all-requests (ctx)))))

(defn seeded-claimed-request
  []
  (first
   (filter #(= :claimed (:request/status %))
           (model/all-requests (ctx)))))

(defn seeded-terminal-request
  []
  (first
   (filter #(contains? #{:done :cancelled}
                       (:request/status %))
           (model/all-requests (ctx)))))

(defn create-open-request!
  [overrides]
  (:request
   (model/create-request!
    (ctx)
    {:user user-owner
     :input (valid-input overrides)})))

(defn owner-user-for
  [request]
  {:user/id (:request/customer-user-id request)
   :user/email (or (:request/customer-name request)
                   (:request/customer-user-id request))})

(defn claimer-user-for
  [request]
  {:user/id (:request/claimed-by request)
   :user/email (:request/claimed-by-email request)})

;; -----------------------------------------------------------------------------
;; Constants and schemas
;; -----------------------------------------------------------------------------

(deftest constants-test
  (is (= "demo-store" model/store-id))
  (is (= "Human Help" model/store-name))
  (is (= #{:open :claimed :done :cancelled} model/statuses))
  (is (= #{:open :claimed} model/open-statuses))
  (is (= #{:done :cancelled} model/terminal-statuses))
  (is (= [:claim :unclaim :take-over :done :cancel]
         model/lifecycle-actions))
  (is (= :newest model/default-created-order))
  (is (false? model/default-mine-first?))
  (is (false? model/default-unclaimed-first?))
  (is (false? model/default-show-terminal?))
  (is (integer? model/terminal-fade-ms))
  (is (pos? model/terminal-fade-ms))
  (doseq [limit [model/request-title-max
                 model/request-area-max
                 model/request-details-max
                 model/request-customer-name-max]]
    (is (integer? limit))
    (is (pos? limit))))

(deftest schemas-test
  (is (true? (m/validate model/user-schema user-owner)))
  (is (false? (m/validate model/user-schema {:user/id "x"})))
  (is (true? (m/validate model/request-schema (req {}))))
  (is (false? (m/validate model/request-schema
                          (dissoc (req {}) :request/id)))))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(deftest present-test
  (is (false? (model/present? nil)))
  (is (false? (model/present? "")))
  (is (false? (model/present? "   ")))
  (is (true? (model/present? "x")))
  (is (true? (model/present? "  x  ")))
  (is (true? (model/present? 0)))
  (is (true? (model/present? false))))

(deftest trim-value-test
  (is (nil? (model/trim-value nil)))
  (is (= "" (model/trim-value "")))
  (is (= "" (model/trim-value "   ")))
  (is (= "abc" (model/trim-value "  abc  ")))
  (is (= "123" (model/trim-value 123))))

(deftest blank->nil-test
  (is (nil? (model/blank->nil nil)))
  (is (nil? (model/blank->nil "")))
  (is (nil? (model/blank->nil "   ")))
  (is (= "x" (model/blank->nil "x")))
  (is (= "  x  " (model/blank->nil "  x  "))))

(deftest request-param-test
  (let [params {:title "keyword title"
                "area" "string area"}]
    (is (= "keyword title" (model/request-param params :title)))
    (is (= "string area" (model/request-param params :area)))
    (is (nil? (model/request-param params :missing)))))

(deftest parse-long-value-test
  (is (nil? (model/parse-long-value nil)))
  (is (nil? (model/parse-long-value "")))
  (is (nil? (model/parse-long-value "   ")))
  (is (nil? (model/parse-long-value "abc")))
  (is (nil? (model/parse-long-value "1.5")))
  (is (= 0 (model/parse-long-value "0")))
  (is (= 42 (model/parse-long-value "42")))
  (is (= -2 (model/parse-long-value "-2")))
  (is (= 7 (model/parse-long-value 7))))

(deftest parse-visible-revision-test
  (is (nil? (model/parse-visible-revision nil)))
  (is (nil? (model/parse-visible-revision "")))
  (is (nil? (model/parse-visible-revision "abc")))
  (is (= 3 (model/parse-visible-revision "3")))
  (is (= 3 (model/parse-visible-revision 3))))

(deftest truthy-param-test
  (doseq [v [true "true" " TRUE " "on" "1"]]
    (is (true? (model/truthy-param? v))))
  (doseq [v [false nil "" "false" "off" "0" "yes"]]
    (is (false? (model/truthy-param? v)))))

(deftest scalar-value-test
  (is (nil? (model/scalar-value nil)))
  (is (= "x" (model/scalar-value "x")))
  (is (= "last" (model/scalar-value ["first" "last"])))
  (is (= "" (model/scalar-value ["old" ""])))
  (is (= {:a 1} (model/scalar-value {:a 1}))))

(deftest view-state-value-test
  (let [view-state {:search "keyword search"
                    "q" "string search"
                    :mine-first? ["false" "true"]}]
    (is (= "keyword search"
           (model/view-state-value view-state :search "q")))
    (is (= "string search"
           (model/view-state-value view-state :missing "q")))
    (is (= "true"
           (model/view-state-value view-state :mine-first?)))
    (is (nil? (model/view-state-value view-state :missing "missing")))))

(deftest show-terminal-test
  (is (true? (model/show-terminal? {:show-terminal? true})))
  (is (true? (model/show-terminal? {"show-terminal" "on"})))
  (is (false? (model/show-terminal? {:show-terminal? false})))
  (is (false? (model/show-terminal? {"show-terminal" "false"}))))

(deftest now-ms-test
  (let [before (System/currentTimeMillis)
        n (model/now-ms)
        after (System/currentTimeMillis)]
    (is (integer? n))
    (is (<= before n after))))

(deftest compact-map-test
  (is (= {:a 1
          :c false
          :d ""}
         (model/compact-map
          {:a 1
           :b nil
           :c false
           :d ""}))))

(deftest normalize-token-test
  (is (nil? (model/normalize-token nil)))
  (is (= "" (model/normalize-token "")))
  (is (= "garden" (model/normalize-token " Garden ")))
  (is (= ":open" (model/normalize-token :open))))

(deftest label-helpers-test
  (is (= "" (model/sentence-case "")))
  (is (= "" (model/sentence-case nil)))
  (is (= "Open" (model/sentence-case "open")))
  (is (= "Open" (model/labelize :open)))
  (is (= "Take over" (model/labelize :take-over)))
  (is (= "Already done" (model/labelize "already-done"))))

;; -----------------------------------------------------------------------------
;; Users
;; -----------------------------------------------------------------------------

(deftest user-accessors-test
  (is (= "user-owner" (model/user-id user-owner)))
  (is (= "owner@example.com" (model/user-email user-owner)))
  (is (nil? (model/user-id {:user/email "fallback@example.com"})))
  (is (= 123 (model/user-id {:user/id 123})))
  (is (nil? (model/user-email {:user/id "user-only"}))))

(deftest same-user-test
  (is (true? (model/same-user? "1" "1")))
  (is (true? (model/same-user? 1 "1")))
  (is (true? (model/same-user? :a ":a")))
  (is (false? (model/same-user? "1" "2")))
  (is (false? (model/same-user? nil "x"))))

;; -----------------------------------------------------------------------------
;; Create-request input parsing and validation
;; -----------------------------------------------------------------------------

(deftest parse-create-request-input-test
  (testing "string-key params are trimmed"
    (is (= {:title "Need a rake"
            :area "Garden"
            :details "Near aisle 4"
            :customer-name "Jon"}
           (model/parse-create-request-input
            {"title" "  Need a rake  "
             "area" " Garden "
             "details" " Near aisle 4 "
             "customer-name" " Jon "}))))

  (testing "keyword params are trimmed"
    (is (= {:title "Need gloves"
            :area "Hardware"
            :details "Large work gloves"
            :customer-name "Avery"}
           (model/parse-create-request-input
            {:title " Need gloves "
             :area " Hardware "
             :details " Large work gloves "
             :customer-name " Avery "}))))

  (testing "optional blanks become nil"
    (is (= {:title "Need a rake"
            :area "Garden"
            :details nil
            :customer-name nil}
           (model/parse-create-request-input
            {:title " Need a rake "
             :area " Garden "
             :details "   "
             :customer-name ""}))))

  (testing ":name is a fallback for customer-name"
    (is (= "Morgan"
           (:customer-name
            (model/parse-create-request-input
             {:title "Need paint"
              :area "Paint"
              :name " Morgan "}))))))

(deftest create-request-errors-test
  (testing "valid normalized input has no errors"
    (is (nil? (model/create-request-errors (valid-input {})))))

  (testing "title and area are required by nicer model errors"
    (let [errors (model/create-request-errors
                  (valid-input {:title ""
                                :area "   "}))]
      (is (= "A short request is required."
             (:title errors)))
      (is (= "Choose or describe an area of the store."
             (:area errors)))))

  (testing "max length errors use app-specific wording"
    (let [errors (model/create-request-errors
                  (valid-input
                   {:title (apply str (repeat (inc model/request-title-max) "x"))
                    :area (apply str (repeat (inc model/request-area-max) "x"))
                    :details (apply str (repeat (inc model/request-details-max) "x"))
                    :customer-name (apply str (repeat (inc model/request-customer-name-max) "x"))}))]
      (is (= (str "Use " model/request-title-max " characters or fewer.")
             (:title errors)))
      (is (= (str "Use " model/request-area-max " characters or fewer.")
             (:area errors)))
      (is (= (str "Use " model/request-details-max " characters or fewer.")
             (:details errors)))
      (is (= (str "Use " model/request-customer-name-max " characters or fewer.")
             (:customer-name errors))))))

(deftest valid-create-request-input-test
  (let [normal-input (valid-input {})
        blank-area-input (valid-input {:area "   "})
        empty-title-input (valid-input {:title ""})
        optional-nil-input (valid-input {:details nil
                                         :customer-name nil})
        blank-area-errors (model/create-request-errors blank-area-input)]
    (is (true? (model/valid-create-request-input? normal-input)))

    (testing "blank strings currently satisfy the raw Malli predicate"
      (is (true? (model/valid-create-request-input? blank-area-input)))
      (is (= "Choose or describe an area of the store."
             (:area blank-area-errors))))

    (is (false? (model/valid-create-request-input? empty-title-input)))
    (is (true? (model/valid-create-request-input? optional-nil-input)))))

;; -----------------------------------------------------------------------------
;; Request model helpers
;; -----------------------------------------------------------------------------

(deftest request-state-predicates-test
  (is (true? (model/request-open? (req {:request/status :open}))))
  (is (true? (model/request-open? (req {:request/status :claimed}))))
  (is (false? (model/request-open? (req {:request/status :done}))))
  (is (false? (model/request-open? (req {:request/status :cancelled}))))

  (is (false? (model/request-terminal? (req {:request/status :open}))))
  (is (false? (model/request-terminal? (req {:request/status :claimed}))))
  (is (true? (model/request-terminal? (req {:request/status :done}))))
  (is (true? (model/request-terminal? (req {:request/status :cancelled})))))

(deftest request-ownership-test
  (let [r (req {:request/customer-user-id "user-owner"
                :request/status :claimed
                :request/claimed-by "user-helper"
                :request/claimed-by-email "helper@example.com"})]
    (is (true? (model/request-owner? user-owner r)))
    (is (false? (model/request-owner? user-helper r)))
    (is (true? (model/request-claimed? r)))
    (is (true? (model/request-claimed-by-user? user-helper r)))
    (is (false? (model/request-claimed-by-user? user-owner r)))
    (is (true? (model/request-claimed-by-other? user-owner r)))
    (is (false? (model/request-claimed-by-other? user-helper r)))))

(deftest request-labels-test
  (is (= "Open" (model/request-status-label (req {:request/status :open}))))
  (is (= "Claimed" (model/request-status-label (req {:request/status :claimed}))))
  (is (= "Done" (model/request-status-label (req {:request/status :done}))))
  (is (= "Cancelled" (model/request-status-label (req {:request/status :cancelled})))))

(deftest request-counts-and-revisions-test
  (let [requests [(req {:request/id "open"
                        :request/status :open
                        :request/updated-revision 2})
                  (req {:request/id "claimed"
                        :request/status :claimed
                        :request/updated-revision 8})
                  (req {:request/id "done"
                        :request/status :done
                        :request/updated-revision 4})]]
    (is (= 2 (model/open-request-count requests)))
    (is (= 8 (model/newest-revision requests)))
    (is (= 0 (model/newest-revision [])))))

;; -----------------------------------------------------------------------------
;; Visibility and board freshness
;; -----------------------------------------------------------------------------

(deftest revision-visibility-test
  (let [existing (req {:request/id "existing"
                       :request/created-revision 1
                       :request/updated-revision 10})
        newly-created (req {:request/id "new"
                            :request/created-revision 4
                            :request/updated-revision 4})]
    (is (true? (model/request-visible-at-revision? 3 existing)))
    (is (false? (model/request-visible-at-revision? 3 newly-created)))
    (is (true? (model/request-visible-at-revision? 4 newly-created)))
    (is (true? (model/request-visible-at-revision? nil newly-created)))))

(deftest board-stale-test
  (is (false? (model/board-stale? nil 10)))
  (is (true? (model/board-stale? 9 10)))
  (is (false? (model/board-stale? 10 10)))
  (is (false? (model/board-stale? 11 10))))

(deftest pending-open-request-count-test
  (let [requests [(req {:request/id "old-open"
                        :request/status :open
                        :request/created-revision 1})
                  (req {:request/id "old-claimed"
                        :request/status :claimed
                        :request/created-revision 1})
                  (req {:request/id "new-open"
                        :request/status :open
                        :request/created-revision 4})
                  (req {:request/id "new-claimed"
                        :request/status :claimed
                        :request/created-revision 5})
                  (req {:request/id "new-done"
                        :request/status :done
                        :request/created-revision 6})
                  (req {:request/id "new-cancelled"
                        :request/status :cancelled
                        :request/created-revision 7})]]
    (is (= 2 (model/pending-open-request-count requests 3)))
    (is (= 0 (model/pending-open-request-count requests nil)))
    (is (= 0 (model/pending-open-request-count requests 5)))))

;; -----------------------------------------------------------------------------
;; Search
;; -----------------------------------------------------------------------------

(deftest parse-search-test
  (is (= [] (model/parse-search nil)))
  (is (= [] (model/parse-search "")))
  (is (= [] (model/parse-search "   ")))
  (is (= ["jon" "rake" "garden"]
         (model/parse-search " Jon   rake Garden ")))
  (is (= ["jon" "rake"]
         (model/parse-search "jon jon rake"))))

(deftest request-search-fields-test
  (let [r (req {:request/status :claimed
                :request/claimed-by-email "helper@example.com"})]
    (is (= ["1"
            "Need help finding a rake"
            "Garden"
            "Looking for a sturdy rake for bark and leaves."
            "Jon"
            "helper@example.com"
            "Claimed"]
           (model/request-search-fields r)))
    (is (str/includes? (model/request-search-text r)
                       "helper@example.com"))))

(deftest request-matches-search-test
  (let [r (req {})]
    (is (true? (model/request-matches-search? r nil)))
    (is (true? (model/request-matches-search? r "")))
    (is (true? (model/request-matches-search? r "jon rake garden")))
    (is (true? (model/request-matches-search? r "jon bark rake garden")))
    (is (true? (model/request-matches-search? r "JON GARDEN")))
    (is (true? (model/request-matches-search? r "rak gar jon")))
    (is (false? (model/request-matches-search? r "jon purple rake garden")))
    (is (false? (model/request-matches-search? r "rak gar xyz")))))

(deftest filter-requests-test
  (let [now 100000
        r1 (req {:request/id "r1"
                 :request/title "Need a rake"
                 :request/area "Garden"
                 :request/status :open
                 :request/created-revision 1})
        r2 (req {:request/id "r2"
                 :request/title "Need blue paint"
                 :request/area "Paint"
                 :request/status :open
                 :request/created-revision 2})
        r3 (req {:request/id "r3"
                 :request/title "Need mulch"
                 :request/area "Garden"
                 :request/status :open
                 :request/created-revision 5})
        terminal-old (req {:request/id "terminal-old"
                           :request/title "Need garden soil"
                           :request/area "Garden"
                           :request/status :done
                           :request/terminal-at-ms (- now model/terminal-fade-ms 1)
                           :request/created-revision 1})
        terminal-recent (req {:request/id "terminal-recent"
                              :request/title "Need garden hose"
                              :request/area "Garden"
                              :request/status :done
                              :request/terminal-at-ms (- now 1000)
                              :request/created-revision 1})]
    (testing "visible revision and search are applied before terminal visibility"
      (is (= ["r1"]
             (request-ids
              (model/filter-requests
               [r1 r2 r3]
               {:search "garden"
                :visible-revision 3}))))
      (is (= ["r1" "r3"]
             (request-ids
              (model/filter-requests
               [r1 r2 r3]
               {:search "garden"
                :visible-revision nil})))))

    (testing "terminal requests are hidden by default unless they are still fading"
      (is (= ["r1" "terminal-recent"]
             (request-ids
              (model/filter-requests
               [r1 terminal-old terminal-recent]
               {:view-state {:search "garden"
                             :visible-revision nil}
                :now-ms now})))))

    (testing "show-terminal includes terminal requests regardless of fade age"
      (is (= ["r1" "terminal-old" "terminal-recent"]
             (request-ids
              (model/filter-requests
               [r1 terminal-old terminal-recent]
               {:view-state {:search "garden"
                             :visible-revision nil
                             :show-terminal? true}
                :now-ms now})))))))

(deftest sort-requests-for-board-test
  (let [old-open (req {:request/id "old-open"
                       :request/number 1
                       :request/status :open
                       :request/updated-at-ms 9000})
        unclaimed-new (req {:request/id "unclaimed-new"
                            :request/number 6
                            :request/status :open
                            :request/claimed-by nil
                            :request/updated-at-ms 1000})
        mine (req {:request/id "mine"
                   :request/number 3
                   :request/status :claimed
                   :request/claimed-by "user-helper"
                   :request/claimed-by-email "helper@example.com"})
        mine-created (req {:request/id "mine-created"
                           :request/number 2
                           :request/status :open
                           :request/customer-user-id "user-helper"
                           :request/claimed-by nil
                           :request/claimed-by-email nil})
        other-claimed (req {:request/id "other-claimed"
                            :request/number 5
                            :request/status :claimed
                            :request/claimed-by "user-other"
                            :request/claimed-by-email "other@example.com"})
        done (req {:request/id "done"
                   :request/number 4
                   :request/status :done})
        requests [old-open done other-claimed unclaimed-new mine mine-created]]

    (testing "default sort is newest-created first with stable id tie-breaker"
      (is (= ["unclaimed-new" "other-claimed" "done" "mine" "mine-created" "old-open"]
             (request-ids
              (model/sort-requests-for-board
               requests
               {:view-state {:created-order :newest}})))))

    (testing "oldest created order is selectable"
      (is (= ["old-open" "mine-created" "mine" "done" "other-claimed" "unclaimed-new"]
             (request-ids
              (model/sort-requests-for-board
               requests
               {:view-state {:created-order :oldest}})))))

    (testing "mine-first is a priority sort key before created order"
      (is (= ["mine" "mine-created" "unclaimed-new" "other-claimed" "done" "old-open"]
             (request-ids
              (model/sort-requests-for-board
               requests
               {:user user-helper
                :view-state {:created-order :newest
                             :mine-first? true}})))))

    (testing "unclaimed-first is a priority sort key before created order"
      (is (= ["unclaimed-new" "mine-created" "old-open" "other-claimed" "done" "mine"]
             (request-ids
              (model/sort-requests-for-board
               requests
               {:user user-helper
                :view-state {:created-order :newest
                             :unclaimed-first? true}})))))

    (testing "priority key order is code-defined: mine first, then unclaimed"
      (is (= ["mine-created" "mine" "unclaimed-new" "old-open" "other-claimed" "done"]
             (request-ids
              (model/sort-requests-for-board
               requests
               {:user user-helper
                :view-state {:created-order :newest
                             :mine-first? true
                             :unclaimed-first? true}})))))))

(deftest visible-board-requests-test
  (let [now 100000
        r1 (req {:request/id "r1"
                 :request/number 1
                 :request/title "Need rake"
                 :request/area "Garden"
                 :request/status :open
                 :request/updated-at-ms 1000
                 :request/created-revision 1})
        r2 (req {:request/id "r2"
                 :request/number 2
                 :request/title "Need paint"
                 :request/area "Paint"
                 :request/status :open
                 :request/updated-at-ms 2000
                 :request/created-revision 2})
        r3 (req {:request/id "r3"
                 :request/number 3
                 :request/title "Need mulch"
                 :request/area "Garden"
                 :request/status :done
                 :request/terminal-at-ms (- now model/terminal-fade-ms 1)
                 :request/updated-at-ms 3000
                 :request/created-revision 3})
        r4 (req {:request/id "r4"
                 :request/number 4
                 :request/title "Need gloves"
                 :request/area "Garden"
                 :request/status :open
                 :request/updated-at-ms 4000
                 :request/created-revision 4})
        recent-terminal (req {:request/id "recent-terminal"
                              :request/number 5
                              :request/title "Need garden cart"
                              :request/area "Garden"
                              :request/status :cancelled
                              :request/terminal-at-ms (- now 1000)
                              :request/updated-at-ms 5000
                              :request/created-revision 5})]

    (testing "default board hides old terminal requests"
      (is (= ["r1"]
             (request-ids
              (model/visible-board-requests
               [r1 r2 r3 r4]
               {:view-state {:search "garden"
                             :visible-revision 3}
                :now-ms now}))))
      (is (= ["r4" "r1"]
             (request-ids
              (model/visible-board-requests
               [r1 r2 r3 r4]
               {:view-state {:search "garden"
                             :visible-revision 4}
                :now-ms now})))))

    (testing "show-terminal includes done/cancelled requests as ordinary history"
      (let [requests (model/visible-board-requests
                      [r1 r2 r3 r4]
                      {:view-state {:search "garden"
                                    :visible-revision 4
                                    :show-terminal? true}
                       :now-ms now})]
        (is (= ["r4" "r3" "r1"]
               (request-ids requests)))
        (is (not-any? :board/fading-terminal? requests))))

    (testing "recent terminal requests stay visible during fade grace"
      (let [requests (model/visible-board-requests
                      [r1 r2 r3 r4 recent-terminal]
                      {:view-state {:search "garden"
                                    :visible-revision 5}
                       :now-ms now})
            by-id (requests-by-id requests)]
        (is (= ["recent-terminal" "r4" "r1"]
               (request-ids requests)))
        (is (true? (get-in by-id ["recent-terminal" :board/fading-terminal?])))
        (is (= 1000
               (- model/terminal-fade-ms
                  (get-in by-id ["recent-terminal" :board/terminal-fade-remaining-ms]))))))))

(deftest elapsed-minutes-test
  (is (= 0 (model/elapsed-minutes 1000 1000)))
  (is (= 0 (model/elapsed-minutes 1000 59999)))
  (is (= 1 (model/elapsed-minutes 1000 61000)))
  (is (= 17 (model/elapsed-minutes 0 (* 17 60000))))
  (is (= 0 (model/elapsed-minutes 10000 1000))))

(deftest waiting-label-test
  (let [now 10000000]
    (is (= "just now"
           (model/waiting-label (req {:request/created-at-ms now}) now)))
    (is (= "just now"
           (model/waiting-label (req {:request/created-at-ms (- now 59000)}) now)))
    (is (= "1 min"
           (model/waiting-label (req {:request/created-at-ms (- now 60000)}) now)))
    (is (= "17 min"
           (model/waiting-label (req {:request/created-at-ms (- now (* 17 60000))}) now)))
    (is (= "59 min"
           (model/waiting-label (req {:request/created-at-ms (- now (* 59 60000))}) now)))
    (is (= "1 hr"
           (model/waiting-label (req {:request/created-at-ms (- now (* 60 60000))}) now)))
    (is (= "2 hr"
           (model/waiting-label (req {:request/created-at-ms (- now (* 125 60000))}) now)))))

;; -----------------------------------------------------------------------------
;; Available actions and pure errors
;; -----------------------------------------------------------------------------

(deftest available-actions-test
  (is (= [:done :cancel]
         (model/available-actions
          (req {:request/status :open
                :request/customer-user-id "user-owner"})
          user-owner)))

  (is (= [:claim]
         (model/available-actions
          (req {:request/status :open
                :request/customer-user-id "user-owner"})
          user-helper)))

  (is (= [:done :unclaim :cancel]
         (model/available-actions
          (req {:request/status :claimed
                :request/customer-user-id "user-owner"
                :request/claimed-by "user-helper"})
          user-helper)))

  (is (= [:take-over]
         (model/available-actions
          (req {:request/status :claimed
                :request/customer-user-id "user-owner"
                :request/claimed-by "user-helper"})
          user-owner)))

  (is (= []
         (model/available-actions
          (req {:request/status :done})
          user-owner)))

  (is (= []
         (model/available-actions
          (req {:request/status :cancelled})
          user-owner))))

(deftest action-available-test
  (let [open-owned (req {:request/status :open
                         :request/customer-user-id "user-owner"})]
    (is (true? (model/action-available? open-owned user-owner :done)))
    (is (false? (model/action-available? open-owned user-owner :claim)))))

(deftest transition-error-test
  (is (= :humanhelp/request-not-found
         (:error/type (model/transition-error nil :claim user-helper))))

  (is (= :humanhelp/unknown-action
         (:error/type (model/transition-error (req {}) :explode user-helper))))

  (is (= :humanhelp/request-closed
         (:error/type
          (model/transition-error
           (req {:request/status :done})
           :claim
           user-helper))))

  (is (= :humanhelp/action-not-allowed
         (:error/type
          (model/transition-error
           (req {:request/status :open
                 :request/customer-user-id "user-owner"})
           :claim
           user-owner)))))

;; -----------------------------------------------------------------------------
;; Pure transitions
;; -----------------------------------------------------------------------------

(deftest transition-claim-test
  (let [result (model/transition-request
                (req {:request/status :open
                      :request/customer-user-id "user-owner"})
                :claim
                user-helper
                {:now-ms 2000000
                 :revision 4})
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= :claimed (:request/status request')))
    (is (= "user-helper" (:request/claimed-by request')))
    (is (= "helper@example.com" (:request/claimed-by-email request')))
    (is (= 2000000 (:request/updated-at-ms request')))
    (is (= 4 (:request/updated-revision request')))
    (is (= :open (get-in result [:previous :request/status])))))

(deftest transition-unclaim-test
  (let [result (model/transition-request
                (req {:request/status :claimed
                      :request/claimed-by "user-helper"
                      :request/claimed-by-email "helper@example.com"})
                :unclaim
                user-helper
                {:now-ms 2000000
                 :revision 5})
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= :open (:request/status request')))
    (is (nil? (:request/claimed-by request')))
    (is (nil? (:request/claimed-by-email request')))
    (is (= 5 (:request/updated-revision request')))))

(deftest transition-take-over-test
  (let [result (model/transition-request
                (req {:request/status :claimed
                      :request/claimed-by "user-helper"
                      :request/claimed-by-email "helper@example.com"})
                :take-over
                user-other
                {:now-ms 2000000
                 :revision 6})
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= :claimed (:request/status request')))
    (is (= "user-other" (:request/claimed-by request')))
    (is (= "other@example.com" (:request/claimed-by-email request')))))

(deftest transition-done-test
  (let [owned-open (req {:request/status :open
                         :request/customer-user-id "user-owner"})
        claimed-by-helper (req {:request/status :claimed
                                :request/claimed-by "user-helper"})]
    (let [result (model/transition-request
                  owned-open
                  :done
                  user-owner
                  {:now-ms 2000000
                   :revision 7})]
      (is (= :ok (:status result)))
      (is (= :done (get-in result [:request :request/status])))
      (is (= 2000000 (get-in result [:request :request/terminal-at-ms]))))
    (is (= :ok
           (:status (model/transition-request
                     claimed-by-helper
                     :done
                     user-helper
                     {:revision 8}))))
    (is (= :error
           (:status (model/transition-request
                     owned-open
                     :done
                     user-helper
                     {:revision 9}))))))

(deftest transition-cancel-test
  (let [owned-open (req {:request/status :open
                         :request/customer-user-id "user-owner"})
        claimed-by-helper (req {:request/status :claimed
                                :request/claimed-by "user-helper"})]
    (let [result (model/transition-request
                  owned-open
                  :cancel
                  user-owner
                  {:now-ms 2000000
                   :revision 7})]
      (is (= :ok (:status result)))
      (is (= :cancelled (get-in result [:request :request/status])))
      (is (= 2000000 (get-in result [:request :request/terminal-at-ms]))))
    (is (= :ok
           (:status (model/transition-request
                     claimed-by-helper
                     :cancel
                     user-helper
                     {:revision 8}))))
    (is (= :error
           (:status (model/transition-request
                     owned-open
                     :cancel
                     user-helper
                     {:revision 9}))))))

(deftest transition-rejection-test
  (is (= :error
         (:status (model/transition-request
                   (req {:request/status :open
                         :request/customer-user-id "user-owner"})
                   :claim
                   user-owner
                   {:revision 4}))))

  (is (= :error
         (:status (model/transition-request
                   (req {:request/status :claimed
                         :request/claimed-by "user-helper"})
                   :unclaim
                   user-other
                   {:revision 4}))))

  (is (= :error
         (:status (model/transition-request
                   (req {:request/status :claimed
                         :request/claimed-by "user-helper"})
                   :take-over
                   user-helper
                   {:revision 4}))))

  (doseq [status [:done :cancelled]]
    (is (= :error
           (:status (model/transition-request
                     (req {:request/status status})
                     :claim
                     user-helper
                     {:revision 4}))))))

(deftest transition-default-timestamp-and-revision-test
  (let [before (System/currentTimeMillis)
        result (model/transition-request
                (req {:request/status :open
                      :request/customer-user-id "user-owner"
                      :request/updated-revision 11})
                :done
                user-owner)
        after (System/currentTimeMillis)
        request' (:request result)]
    (is (= :ok (:status result)))
    (is (= 11 (:request/updated-revision request')))
    (is (<= before (:request/updated-at-ms request') after))
    (is (<= before (:request/terminal-at-ms request') after))))

(deftest patch-helper-test
  (is (= {:request/status :claimed
          :request/claimed-by "user-helper"
          :request/claimed-by-email "helper@example.com"}
         (model/claim-fields user-helper)))

  (is (= {:request/status :open
          :request/claimed-by nil
          :request/claimed-by-email nil}
         (model/clear-claim-fields)))

  (is (= {:request/status :done
          :request/terminal-at-ms 1234}
         (model/terminal-fields :done 1234)))
  (is (= {:request/status :cancelled}
         (model/terminal-fields :cancelled nil))))

(deftest action-label-test
  (is (= "Claim" (model/action-label :claim)))
  (is (= "Unclaim" (model/action-label :unclaim)))
  (is (= "Take over" (model/action-label :take-over)))
  (is (= "Done" (model/action-label :done)))
  (is (= "Cancel" (model/action-label :cancel)))
  (is (= "Custom action" (model/action-label :custom-action))))

(deftest action-result-message-test
  (let [r (req {:request/number 42})]
    (is (= "Claimed request #42."
           (model/action-result-message :claim r)))
    (is (= "Unclaimed request #42."
           (model/action-result-message :unclaim r)))
    (is (= "Took over request #42."
           (model/action-result-message :take-over r)))
    (is (= "Marked request #42 done."
           (model/action-result-message :done r)))
    (is (= "Cancelled request #42."
           (model/action-result-message :cancel r)))
    (is (= "Updated request #42."
           (model/action-result-message :whatever r)))))

;; -----------------------------------------------------------------------------
;; Reset / seeded persisted state
;; -----------------------------------------------------------------------------

(deftest reset-demo-state-test
  (testing "reset returns the deterministic demo state"
    (let [result (model/reset-demo-state! (ctx))]
      (is (= :ok (:status result)))
      (is (= 3 (:revision result)))
      (is (= 3 (model/latest-revision (ctx))))
      (is (= 3 (count (model/all-requests (ctx)))))
      (is (= 3 (count (model/all-events (ctx)))))))

  (testing "reset restores the exact seeded collection after mutations"
    (let [initial {:revision (model/latest-revision (ctx))
                   :requests (model/all-requests (ctx))
                   :events (model/all-events (ctx))}]
      (model/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "Temporary request"})})

      (is (not= initial
                {:revision (model/latest-revision (ctx))
                 :requests (model/all-requests (ctx))
                 :events (model/all-events (ctx))}))

      (model/reset-demo-state! (ctx))

      (is (= initial
             {:revision (model/latest-revision (ctx))
              :requests (model/all-requests (ctx))
              :events (model/all-events (ctx))}))))

  (testing "seeded requests and events have valid shapes"
    (is (every? request-shape-valid? (model/all-requests (ctx))))
    (is (every? event-shape-valid? (model/all-events (ctx)))))

  (testing "seeded request ids and numbers are unique"
    (let [requests (model/all-requests (ctx))
          ids (request-ids requests)
          numbers (mapv :request/number requests)]
      (is (= (count ids) (count (set ids))))
      (is (= (count numbers) (count (set numbers))))))

  (testing "seeded state includes open, claimed, and terminal examples"
    (let [statuses (set (map :request/status (model/all-requests (ctx))))]
      (is (contains? statuses :open))
      (is (contains? statuses :claimed))
      (is (some statuses [:done :cancelled])))))

(deftest latest-revision-test
  (testing "latest revision is integer and non-negative"
    (is (integer? (model/latest-revision (ctx))))
    (is (not (neg? (model/latest-revision (ctx))))))

  (testing "pure reads do not advance the revision"
    (let [before (model/latest-revision (ctx))]
      (model/all-requests (ctx))
      (model/all-events (ctx))
      (model/recent-events (ctx))
      (model/summary (ctx))
      (model/board-data (ctx)
                        {:search ""
                         :visible-revision before})
      (model/toolbar-data (ctx)
                          {:search ""
                           :visible-revision before})
      (is (= before (model/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Read APIs
;; -----------------------------------------------------------------------------

(deftest all-requests-test
  (testing "all-requests returns sorted request maps"
    (let [requests (model/all-requests (ctx))]
      (is (vector? requests))
      (is (every? map? requests))
      (is (= (sort (map :request/number requests))
             (map :request/number requests)))))

  (testing "all-requests does not expose duplicate ids"
    (let [ids (request-ids (model/all-requests (ctx)))]
      (is (= (count ids) (count (set ids)))))))

(deftest request-by-id-test
  (testing "existing ids return the same maps found in all-requests"
    (doseq [request (model/all-requests (ctx))]
      (is (= request
             (model/request-by-id (ctx) (:request/id request))))))

  (testing "missing or blank ids return nil"
    (is (nil? (model/request-by-id (ctx) "missing-request-id")))
    (is (nil? (model/request-by-id (ctx) nil)))
    (is (nil? (model/request-by-id (ctx) "")))))

(deftest events-test
  (testing "all-events returns event maps newest-first"
    (let [events (model/all-events (ctx))]
      (is (vector? events))
      (is (every? event-shape-valid? events))
      (is (= (sort-by :event/at-ms > events)
             events))))

  (testing "recent-events returns the requested prefix"
    (let [events (model/all-events (ctx))]
      (is (= (take 2 events)
             (seq (model/recent-events (ctx) 2))))
      (is (= (take 20 events)
             (seq (model/recent-events (ctx))))))))

(deftest summary-test
  (testing "summary reflects current persisted request state"
    (let [requests (model/all-requests (ctx))
          summary (model/summary (ctx))]
      (is (= model/store-id (:store/id summary)))
      (is (= model/store-name (:store/name summary)))
      (is (= (model/latest-revision (ctx)) (:revision summary)))
      (is (= (count requests) (:total summary)))
      (is (= (count (filter active-request? requests))
             (:open summary)))
      (is (= (frequencies (map :request/status requests))
             (:by-status summary)))
      (is (fn? (:pending-open summary))))))

;; -----------------------------------------------------------------------------
;; View-state normalization
;; -----------------------------------------------------------------------------

(deftest normalize-view-state-test
  (testing "nil view-state normalizes to a complete usable shape"
    (is (= {:search ""
            :visible-revision (model/latest-revision (ctx))
            :created-order :newest
            :mine-first? false
            :unclaimed-first? false
            :show-terminal? false}
           (model/normalize-view-state (ctx) nil))))

  (testing "blank search normalizes to an empty string"
    (doseq [search [nil "" "   "]]
      (is (= ""
             (:search
              (model/normalize-view-state
               (ctx)
               {:search search}))))))

  (testing "search is trimmed"
    (is (= "garden"
           (:search
            (model/normalize-view-state
             (ctx)
             {:search "  garden  "}))))
    (is (= "garden rake"
           (:search
            (model/normalize-view-state
             (ctx)
             {:search "  garden rake  "})))))

  (testing "visible revision defaults to latest only when absent"
    (let [latest (model/latest-revision (ctx))]
      (is (= latest
             (:visible-revision
              (model/normalize-view-state (ctx) nil))))
      (is (= latest
             (:visible-revision
              (model/normalize-view-state (ctx) {}))))
      (is (= 1
             (:visible-revision
              (model/normalize-view-state
               (ctx)
               {:visible-revision 1}))))))

  (testing "board options normalize from raw form values"
    (is (= {:search "garden"
            :visible-revision 2
            :created-order :oldest
            :mine-first? true
            :unclaimed-first? false
            :show-terminal? true}
           (model/normalize-view-state
            (ctx)
            {"q" " garden "
             "visible-revision" "2"
             "created-order" "oldest"
             "mine-first" "on"
             "unclaimed-first" "false"
             "show-terminal" "true"}))))

  (testing "invalid created-order falls back to default"
    (is (= :newest
           (:created-order
            (model/normalize-view-state
             (ctx)
             {:created-order "sideways"}))))))

(deftest create-request-success-test
  (testing "create-request! appends a new open request"
    (let [before-revision (model/latest-revision (ctx))
          before-count (count (model/all-requests (ctx)))
          result (model/create-request!
                  (ctx)
                  {:user user-owner
                   :input (valid-input
                           {:title "Need help finding gloves"
                            :area "Hardware"
                            :details "Large work gloves"
                            :customer-name "Avery"})})
          request (:request result)]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= (:revision result) (model/latest-revision (ctx))))
      (is (= (inc before-count) (count (model/all-requests (ctx)))))

      (is (request-shape-valid? request))
      (is (= :open (:request/status request)))
      (is (= model/store-id (:request/store-id request)))
      (is (= "Need help finding gloves" (:request/title request)))
      (is (= "Hardware" (:request/area request)))
      (is (= "Large work gloves" (:request/details request)))
      (is (= "Avery" (:request/customer-name request)))
      (is (= (:user/id user-owner) (:request/customer-user-id request)))
      (is (nil? (:request/claimed-by request)))
      (is (nil? (:request/claimed-by-email request)))
      (is (= (:revision result) (:request/created-revision request)))
      (is (= (:revision result) (:request/updated-revision request)))
      (is (= (without-nil-vals request)
             (model/request-by-id (ctx) (:request/id request)))))))

(deftest create-request-numbering-test
  (testing "created request numbers and ids are unique and monotonic"
    (let [r1 (:request
              (model/create-request!
               (ctx)
               {:user user-owner
                :input (valid-input {:title "First new request"})}))
          r2 (:request
              (model/create-request!
               (ctx)
               {:user user-owner
                :input (valid-input {:title "Second new request"})}))]
      (is (not= (:request/id r1) (:request/id r2)))
      (is (not= (:request/number r1) (:request/number r2)))
      (is (< (:request/number r1) (:request/number r2)))
      (is (< (:request/created-revision r1)
             (:request/created-revision r2))))))

(deftest create-request-default-customer-name-test
  (testing "customer-name falls back to user email when omitted"
    (let [result (model/create-request!
                  (ctx)
                  {:user user-owner
                   :input (valid-input
                           {:title "Need help"
                            :area "Paint"
                            :details nil
                            :customer-name nil})})
          request (:request result)]
      (is (= :ok (:status result)))
      (is (= "Need help" (:request/title request)))
      (is (= "Paint" (:request/area request)))
      (is (nil? (:request/details request)))
      (is (= "owner@example.com" (:request/customer-name request))))))

(deftest create-request-service-assumes-validated-input-test
  (testing "service layer assumes app/model validation happened before create"
    (let [result (model/create-request!
                  (ctx)
                  {:user user-owner
                   :input {:title ""
                           :area ""
                           :details nil
                           :customer-name nil}})]
      (is (= :ok (:status result)))
      (is (= "" (get-in result [:request :request/title])))
      (is (= "" (get-in result [:request :request/area]))))))

(deftest create-request-event-test
  (testing "create-request! records a request-created event"
    (let [{:keys [request revision]} (model/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Event target"})})
          event (find-event {:kind :request/created
                             :request-id (:request/id request)})]
      (is event)
      (is (= revision (:event/revision event)))
      (is (= (:request/id request) (:event/request-id event)))
      (is (nil? (:event/action event))))))

;; -----------------------------------------------------------------------------
;; Persisted counts / toolbar data
;; -----------------------------------------------------------------------------

(deftest persisted-open-request-count-test
  (testing "open-request-count matches active statuses in all requests"
    (let [expected (count (filter active-request?
                                  (model/all-requests (ctx))))]
      (is (= expected
             (model/open-request-count (model/all-requests (ctx)))))))

  (testing "creating an open request increments open count"
    (let [before (model/open-request-count (model/all-requests (ctx)))]
      (model/create-request!
       (ctx)
       {:user user-owner
        :input (valid-input {:title "New active request"})})
      (is (= (inc before)
             (model/open-request-count (model/all-requests (ctx))))))))

(deftest toolbar-data-test
  (testing "toolbar-data exposes the values needed by the toolbar fragment"
    (let [latest (model/latest-revision (ctx))
          toolbar (model/toolbar-data
                   (ctx)
                   {:search ""
                    :visible-revision latest})]
      (is (= model/store-id (:store/id toolbar)))
      (is (= model/store-name (:store/name toolbar)))
      (is (= latest (:latest-revision toolbar)))
      (is (= latest (:visible-revision toolbar)))
      (is (= latest (get-in toolbar [:view-state :visible-revision])))
      (is (= :newest (get-in toolbar [:view-state :created-order])))
      (is (= (model/open-request-count (model/all-requests (ctx)))
             (:open-count toolbar)))
      (is (= 0 (:pending-open-count toolbar)))
      (is (false? (:stale? toolbar)))
      (is (vector? (:created-order-options toolbar)))
      (is (vector? (:priority-sort-options toolbar)))
      (is (map? (:terminal-visibility-option toolbar)))))

  (testing "toolbar-data reports stale board and pending open requests"
    (let [visible-before (model/latest-revision (ctx))
          {:keys [request]} (model/create-request!
                             (ctx)
                             {:user user-owner
                              :input (valid-input
                                      {:title "Pending new request"})})
          toolbar (model/toolbar-data
                   (ctx)
                   {:search ""
                    :visible-revision visible-before})]
      (is (= (model/latest-revision (ctx)) (:latest-revision toolbar)))
      (is (true? (:stale? toolbar)))
      (is (= 1 (:pending-open-count toolbar)))
      (is (= (model/open-request-count (model/all-requests (ctx)))
             (:open-count toolbar)))
      (is (= :open (:request/status request)))))

  (testing "terminal newly-created requests do not count as pending open or stale"
    (let [visible-before (model/latest-revision (ctx))
          {:keys [request]} (model/create-request!
                             (ctx)
                             {:user user-owner
                              :input (valid-input {:title "Soon terminal"})})]
      (model/cancel-request!
       (ctx)
       {:request-id (:request/id request)
        :user user-owner})
      (let [toolbar (model/toolbar-data
                     (ctx)
                     {:search ""
                      :visible-revision visible-before})]
        (is (false? (:stale? toolbar)))
        (is (= 0 (:pending-open-count toolbar)))))))

;; -----------------------------------------------------------------------------
;; Board data / stale visibility semantics
;; -----------------------------------------------------------------------------

(deftest board-data-shape-test
  (testing "board-data exposes the stable fields needed by request-list"
    (let [latest (model/latest-revision (ctx))
          view-state {:search ""
                      :visible-revision latest}
          board (model/board-data (ctx) view-state)]
      (is (map? board))
      (is (= model/store-id (:store/id board)))
      (is (= model/store-name (:store/name board)))
      (is (= latest (:latest-revision board)))
      (is (= latest (:visible-revision board)))
      (is (= (model/open-request-count (model/all-requests (ctx)))
             (:open-count board)))
      (is (= 0 (:pending-open-count board)))
      (is (false? (:stale? board)))
      (is (vector? (:requests board)))
      (is (nil? (:next-prune-ms board)))
      (is (map? (:view-state board)))
      (is (= latest
             (get-in board [:view-state :visible-revision])))
      (is (= :newest
             (get-in board [:view-state :created-order])))
      (is (vector? (:created-order-options board)))
      (is (vector? (:priority-sort-options board)))
      (is (map? (:terminal-visibility-option board))))))

(deftest board-data-new-request-visibility-test
  (testing "new requests are hidden from an older visible revision"
    (let [visible-before (model/latest-revision (ctx))
          {:keys [request revision]} (model/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Hidden until refresh"})})
          stale-board (model/board-data
                       (ctx)
                       {:search ""
                        :visible-revision visible-before})
          fresh-board (model/board-data
                       (ctx)
                       {:search ""
                        :visible-revision revision})]
      (is (true? (:stale? stale-board)))
      (is (not (contains? (set (request-ids (:requests stale-board)))
                          (:request/id request))))
      (is (contains? (set (request-ids (:requests fresh-board)))
                     (:request/id request))))))

(deftest board-data-existing-request-update-visibility-test
  (testing "already-visible requests remain visible after lifecycle updates"
    (let [open-request (seeded-open-request)
          visible-before (model/latest-revision (ctx))
          result (model/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          stale-board (model/board-data
                       (ctx)
                       {:search ""
                        :visible-revision visible-before})
          updated-card (first
                        (filter #(= (:request/id open-request)
                                    (:request/id %))
                                (:requests stale-board)))]
      (is (= :ok (:status result)))
      (is updated-card)
      (is (= :claimed (:request/status updated-card)))
      (is (= (:user/id user-helper)
             (:request/claimed-by updated-card))))))

(deftest board-data-search-test
  (testing "search terms match collectively across customer, title, area, and details"
    (model/create-request!
     (ctx)
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Customer near front doors"
               :customer-name "Mina"})})
    (let [latest (model/latest-revision (ctx))
          board (model/board-data
                 (ctx)
                 {:search "mina purple seasonal front"
                  :visible-revision latest})]
      (is (contains? (request-titles (:requests board))
                     "Need a purple snow shovel"))))

  (testing "missing terms exclude requests"
    (model/create-request!
     (ctx)
     {:user user-owner
      :input (valid-input
              {:title "Need a purple snow shovel"
               :area "Seasonal"
               :details "Customer near front doors"
               :customer-name "Mina"})})
    (let [latest (model/latest-revision (ctx))
          board (model/board-data
                 (ctx)
                 {:search "mina purple seasonal unicorn"
                  :visible-revision latest})]
      (is (not (contains? (request-titles (:requests board))
                          "Need a purple snow shovel"))))))

(deftest board-data-search-respects-visible-revision-test
  (testing "search cannot reveal a new request hidden behind an older revision"
    (let [visible-before (model/latest-revision (ctx))
          {:keys [request revision]} (model/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Need chartreuse grout"
                                                :area "Tile"
                                                :details "Rare color"
                                                :customer-name "Mina"})})
          stale-board (model/board-data
                       (ctx)
                       {:search "chartreuse grout mina"
                        :visible-revision visible-before})
          fresh-board (model/board-data
                       (ctx)
                       {:search "chartreuse grout mina"
                        :visible-revision revision})]
      (is (not (contains? (set (request-ids (:requests stale-board)))
                          (:request/id request))))
      (is (contains? (set (request-ids (:requests fresh-board)))
                     (:request/id request))))))

(deftest board-data-terminal-visibility-test
  (testing "board-data hides old terminal requests by default and includes them when requested"
    (let [latest (model/latest-revision (ctx))
          default-board (model/board-data
                         (ctx)
                         {:search ""
                          :visible-revision latest})
          history-board (model/board-data
                         (ctx)
                         {:search ""
                          :visible-revision latest
                          :show-terminal? true})]
      (is (not-any? model/request-terminal? (:requests default-board)))
      (is (some model/request-terminal? (:requests history-board)))
      (is (false? (get-in default-board [:view-state :show-terminal?])))
      (is (true? (get-in history-board [:view-state :show-terminal?]))))))

(deftest board-data-terminal-fade-test
  (testing "newly terminal requests remain visible briefly and schedule a prune"
    (let [created (:request
                   (model/create-request!
                    (ctx)
                    {:user user-owner
                     :input (valid-input {:title "Fade after done"})}))
          result (model/mark-request-done!
                  (ctx)
                  {:request-id (:request/id created)
                   :user user-owner})
          terminal-at (get-in result [:request :request/terminal-at-ms])
          board (model/board-data
                 (ctx)
                 {:view-state {:search "fade"
                               :visible-revision (:revision result)}
                  :now-ms (+ terminal-at 1000)})
          card (first (:requests board))]
      (is (= :ok (:status result)))
      (is (= (:request/id created) (:request/id card)))
      (is (true? (:board/fading-terminal? card)))
      (is (= (- model/terminal-fade-ms 1000)
             (:board/terminal-fade-remaining-ms card)))
      (is (= (- model/terminal-fade-ms 1000)
             (:next-prune-ms board))))))

(deftest board-option-metadata-test
  (testing "board option metadata is GUI-safe and reflects active options"
    (let [view-state {:created-order :oldest
                      :mine-first? true
                      :unclaimed-first? false
                      :show-terminal? true}
          metadata (model/board-option-metadata view-state)]
      (is (= [:newest :oldest]
             (mapv :id (:created-order-options metadata))))
      (is (= :oldest
             (:id (first (filter :active?
                                  (:created-order-options metadata))))))
      (is (= [{:id :mine-first
               :checked? true}
              {:id :unclaimed-first
               :checked? false}]
             (mapv #(select-keys % [:id :checked?])
                   (:priority-sort-options metadata))))
      (is (= {:id :show-terminal
              :enabled-key :show-terminal?
              :checked? true}
             (select-keys (:terminal-visibility-option metadata)
                          [:id :enabled-key :checked?])))
      (is (not-any? :order/key (:created-order-options metadata)))
      (is (not-any? :priority/key (:priority-sort-options metadata))))))

(deftest board-helper-test
  (testing "board-requests delegates visible filtering"
    (let [visible-before (model/latest-revision (ctx))
          {:keys [request revision]} (model/create-request!
                                      (ctx)
                                      {:user user-owner
                                       :input (valid-input
                                               {:title "Visible helper target"})})]
      (is (not (contains? (set (request-ids
                                (model/board-requests
                                 (ctx)
                                 {:search ""
                                  :visible-revision visible-before})))
                          (:request/id request))))
      (is (contains? (set (request-ids
                           (model/board-requests
                            (ctx)
                            {:search ""
                             :visible-revision revision})))
                     (:request/id request))))))

;; -----------------------------------------------------------------------------
;; Persisted lifecycle actions: claim
;; -----------------------------------------------------------------------------

(deftest claim-request-success-test
  (testing "non-owner can claim an open request"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          result (model/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (model/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= open-request (:previous result)))
      (is (= updated (:request result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :claimed (:request/status updated)))
      (is (= (:user/id user-helper) (:request/claimed-by updated)))
      (is (= (:user/email user-helper) (:request/claimed-by-email updated)))
      (is (= (:revision result) (:request/updated-revision updated))))))

(deftest claim-request-event-test
  (testing "successful claim records a claimed event with action metadata"
    (let [open-request (seeded-open-request)
          result (model/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          event (find-event {:kind :request/claimed
                             :request-id (:request/id open-request)
                             :action :claim})]
      (is (= :ok (:status result)))
      (is event)
      (is (= (:revision result) (:event/revision event))))))

(deftest claim-request-error-test
  (testing "owner cannot claim own open request"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          before-requests (model/all-requests (ctx))
          before-events (model/all-events (ctx))
          result (model/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx))))
      (is (= before-requests (model/all-requests (ctx))))
      (is (= before-events (model/all-events (ctx))))))

  (testing "missing request id returns error and does not advance revision"
    (let [before-revision (model/latest-revision (ctx))
          result (model/claim-request!
                  (ctx)
                  {:request-id "missing"
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Persisted lifecycle actions: unclaim
;; -----------------------------------------------------------------------------

(deftest unclaim-request-success-test
  (testing "claimer can unclaim a claimed request"
    (let [open-request (seeded-open-request)
          claim (model/claim-request!
                 (ctx)
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-unclaim (model/latest-revision (ctx))
          result (model/unclaim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (model/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-unclaim) (:revision result)))
      (is (= :open (:request/status updated)))
      (is (nil? (:request/claimed-by updated)))
      (is (nil? (:request/claimed-by-email updated)))
      (is (find-event {:kind :request/unclaimed
                       :request-id (:request/id open-request)
                       :action :unclaim})))))

(deftest unclaim-request-error-test
  (testing "non-claimer cannot unclaim"
    (let [claimed-request (seeded-claimed-request)
          before-revision (model/latest-revision (ctx))
          result (model/unclaim-request!
                  (ctx)
                  {:request-id (:request/id claimed-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx))))))

  (testing "missing request id returns error"
    (let [before-revision (model/latest-revision (ctx))
          result (model/unclaim-request!
                  (ctx)
                  {:request-id "missing"
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Persisted lifecycle actions: take over
;; -----------------------------------------------------------------------------

(deftest take-over-request-success-test
  (testing "another user can take over a claimed request"
    (let [claimed-request (seeded-claimed-request)
          before-revision (model/latest-revision (ctx))
          result (model/take-over-request!
                  (ctx)
                  {:request-id (:request/id claimed-request)
                   :user user-other})
          updated (model/request-by-id (ctx) (:request/id claimed-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :claimed (:request/status updated)))
      (is (= (:user/id user-other) (:request/claimed-by updated)))
      (is (= (:user/email user-other) (:request/claimed-by-email updated)))
      (is (find-event {:kind :request/taken-over
                       :request-id (:request/id claimed-request)
                       :action :take-over})))))

(deftest take-over-request-error-test
  (testing "current claimer cannot take over their own claim"
    (let [claimed-request (seeded-claimed-request)
          claimer (claimer-user-for claimed-request)
          before-revision (model/latest-revision (ctx))
          result (model/take-over-request!
                  (ctx)
                  {:request-id (:request/id claimed-request)
                   :user claimer})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx))))))

  (testing "open request cannot be taken over"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          result (model/take-over-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Persisted lifecycle actions: done
;; -----------------------------------------------------------------------------

(deftest mark-request-done-success-test
  (testing "owner can mark an open request done"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          result (model/mark-request-done!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})
          updated (model/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :done (:request/status updated)))
      (is (= (:revision result) (:request/updated-revision updated)))
      (is (integer? (:request/terminal-at-ms updated)))
      (is (find-event {:kind :request/done
                       :request-id (:request/id open-request)
                       :action :done}))))

  (testing "claimer can mark a claimed request done"
    (let [open-request (create-open-request!
                        {:title "Claimed request to mark done"})
          claim (model/claim-request!
                 (ctx)
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-revision (model/latest-revision (ctx))
          result (model/mark-request-done!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (model/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :done (:request/status updated))))))

(deftest mark-request-done-error-test
  (testing "unrelated user cannot mark open request done"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          result (model/mark-request-done!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx))))))

  (testing "terminal request cannot be marked done again"
    (let [terminal (seeded-terminal-request)
          before-revision (model/latest-revision (ctx))
          result (model/mark-request-done!
                  (ctx)
                  {:request-id (:request/id terminal)
                   :user user-owner})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Persisted lifecycle actions: cancel
;; -----------------------------------------------------------------------------

(deftest cancel-request-success-test
  (testing "owner can cancel an open request"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          result (model/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})
          updated (model/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :cancelled (:request/status updated)))
      (is (integer? (:request/terminal-at-ms updated)))
      (is (find-event {:kind :request/cancelled
                       :request-id (:request/id open-request)
                       :action :cancel}))))

  (testing "claimer can cancel a claimed request"
    (let [open-request (create-open-request!
                        {:title "Claimed request to cancel"})
          claim (model/claim-request!
                 (ctx)
                 {:request-id (:request/id open-request)
                  :user user-helper})
          before-revision (model/latest-revision (ctx))
          result (model/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          updated (model/request-by-id (ctx) (:request/id open-request))]
      (is (= :ok (:status claim)))
      (is (= :ok (:status result)))
      (is (= (inc before-revision) (:revision result)))
      (is (= :cancelled (:request/status updated))))))

(deftest cancel-request-error-test
  (testing "unrelated user cannot cancel an open request"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          result (model/cancel-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-other})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx))))))

  (testing "terminal request cannot be cancelled again"
    (let [terminal (seeded-terminal-request)
          before-revision (model/latest-revision (ctx))
          result (model/cancel-request!
                  (ctx)
                  {:request-id (:request/id terminal)
                   :user user-owner})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx)))))))

;; -----------------------------------------------------------------------------
;; Mutation invariants
;; -----------------------------------------------------------------------------

(deftest successful-transition-updates-only-targeted-request-test
  (testing "successful transition updates only the targeted request"
    (let [open-request (seeded-open-request)
          before-requests (requests-by-id (model/all-requests (ctx)))
          result (model/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user user-helper})
          after-requests (requests-by-id (model/all-requests (ctx)))]
      (is (= :ok (:status result)))
      (doseq [[id before] before-requests
              :when (not= id (:request/id open-request))]
        (is (= before (get after-requests id))
            (str "Untargeted request changed: " id))))))

(deftest failed-transition-invariants-test
  (testing "failed transition does not change requests, events, or revision"
    (let [open-request (seeded-open-request)
          before-revision (model/latest-revision (ctx))
          before-requests (model/all-requests (ctx))
          before-events (model/all-events (ctx))
          result (model/claim-request!
                  (ctx)
                  {:request-id (:request/id open-request)
                   :user (owner-user-for open-request)})]
      (is (= :error (:status result)))
      (is (= before-revision (model/latest-revision (ctx))))
      (is (= before-requests (model/all-requests (ctx))))
      (is (= before-events (model/all-events (ctx)))))))

(deftest revision-monotonicity-test
  (testing "successful mutations advance revision by exactly one each"
    (let [r0 (model/latest-revision (ctx))
          created (:request
                   (model/create-request!
                    (ctx)
                    {:user user-owner
                     :input (valid-input {:title "Revision test"})}))
          r1 (model/latest-revision (ctx))
          claim-result (model/claim-request!
                        (ctx)
                        {:request-id (:request/id created)
                         :user user-helper})
          r2 (model/latest-revision (ctx))
          done-result (model/mark-request-done!
                       (ctx)
                       {:request-id (:request/id created)
                        :user user-helper})
          r3 (model/latest-revision (ctx))]
      (is (= (inc r0) r1))
      (is (= :ok (:status claim-result)))
      (is (= (inc r1) r2))
      (is (= :ok (:status done-result)))
      (is (= (inc r2) r3)))))

(deftest reset-after-many-mutations-test
  (testing "reset cleans up created and mutated demo data"
    (let [initial-requests (model/all-requests (ctx))
          initial-events (model/all-events (ctx))
          initial-revision (model/latest-revision (ctx))
          created (:request
                   (model/create-request!
                    (ctx)
                    {:user user-owner
                     :input (valid-input {:title "Request to disappear"})}))]
      (model/claim-request!
       (ctx)
       {:request-id (:request/id created)
        :user user-helper})
      (model/mark-request-done!
       (ctx)
       {:request-id (:request/id created)
        :user user-helper})

      (is (not= initial-revision (model/latest-revision (ctx))))
      (is (some #(= (:request/id created) (:request/id %))
                (model/all-requests (ctx))))

      (model/reset-demo-state! (ctx))

      (is (= initial-revision (model/latest-revision (ctx))))
      (is (= initial-requests (model/all-requests (ctx))))
      (is (= initial-events (model/all-events (ctx))))
      (is (nil? (model/request-by-id (ctx) (:request/id created)))))))
