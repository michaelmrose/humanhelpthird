(ns net.humanhelp.site.model.fx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.tools.logging :as log]
   [com.biffweb.experimental :as biffx]
   [gesso.live.core :as live]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx])
  (:import
   [java.time Instant]
   [java.util UUID]))

(defn uuid [value] (UUID/fromString value))

(def document-id
  (uuid "10000000-0000-0000-0000-000000000001"))

(def other-document-id
  (uuid "10000000-0000-0000-0000-000000000002"))

(def authorization-id
  (uuid "20000000-0000-0000-0000-000000000001"))

(def other-authorization-id
  (uuid "20000000-0000-0000-0000-000000000002"))

(def t0
  (Instant/parse "2026-07-01T12:00:00Z"))

(def t1
  (Instant/parse "2026-07-01T12:01:00Z"))

(def entity-version
  {:revision-key :example/revision
   :created-at-key :example/created-at
   :updated-at-key :example/updated-at})

(defn example-document
  ([]
   (example-document document-id 0 t0 "Original"))
  ([id revision updated-at name]
   {:xt/id id
    :example/name name
    :example/revision revision
    :example/created-at t0
    :example/updated-at updated-at}))

(def create-document
  (example-document))

(def updated-document
  (example-document document-id 1 t1 "Updated"))

(def create-command
  (model.common/create-command
   :example
   create-document
   entity-version))

(def update-command
  (model.common/update-command
   :example
   :rename
   create-document
   updated-document
   entity-version))

(def other-create-command
  (model.common/create-command
   :example
   (example-document other-document-id 0 t0 "Other")
   entity-version))

(def authorization-expected
  {:model/id authorization-id
   :model/revision-key :location/revision
   :model/revision 4
   :model/updated-at-key :location/updated-at
   :model/updated-at t0})

(def authorization-guard
  {:model/entity-type :location
   :model/expected authorization-expected})

(def other-authorization-guard
  {:model/entity-type :user
   :model/expected
   {:model/id other-authorization-id
    :model/revision-key :user/revision
    :model/revision 2
    :model/updated-at-key :user/updated-at
    :model/updated-at t1}})

(def explicit-assertion
  (model.fx/assert-none
   :example-lock
   [:= :example-lock/key :test/lock]))

(def primary-change
  {:topic :example
   :id document-id
   :change/kind :created})

(defn error-type
  [f]
  (try
    (f)
    ::did-not-throw
    (catch Throwable error
      (loop [error error]
        (when error
          (or (:error/type (ex-data error))
              (recur (ex-cause error))))))))

(defn base-plan
  ([]
   (base-plan {}))
  ([overrides]
   (merge
    {:commands [create-command]
     :changes [primary-change]
     :emit :sync}
    overrides)))

(defn silent-plan
  ([]
   (silent-plan {}))
  ([overrides]
   (merge
    {:commands [create-command]
     :changes []
     :emit false}
    overrides)))

(deftest assertion-helper-test
  (is
   (=
    {:assert
     [:= 0
      {:select [[[:count '*']]]
       :from 'example
       :where [:= :xt/id document-id]}]}
    (model.fx/assert-document-absent
     :example
     document-id)))

  (is
   (=
    {:assert
     [:= 1
      {:select [[[:count '*']]]
       :from 'location
       :where
       [:and
        [:= :xt/id authorization-id]
        [:= :location/revision 4]
        [:= :location/updated-at t0]]}]}
    (model.fx/assert-document-current
     :location
     authorization-expected)))

  (is
   (=
    :model.fx/invalid-document-id
    (error-type
     #(model.fx/assert-document-absent
       :example
       "bad"))))

  (is
   (=
    :model.fx/invalid-expected-version
    (error-type
     #(model.fx/assert-document-current
       :location
       (dissoc authorization-expected :model/revision))))))

(deftest authorization-version-test
  (testing "shape and target"
    (is
     (model.fx/authorization-version?
      authorization-guard))

    (is
     (false?
      (model.fx/authorization-version?
       (assoc authorization-guard :extra true))))

    (is
     (=
      [:location authorization-id]
      (model.fx/authorization-version-target
       authorization-guard))))

  (testing "normalization preserves order and deduplicates"
    (is
     (=
      [authorization-guard
       other-authorization-guard]
      (model.fx/normalize-authorization-versions
       [authorization-guard
        authorization-guard
        other-authorization-guard]))))

  (testing "invalid collections and guards fail"
    (is
     (=
      :model.fx/invalid-authorization-versions
      (error-type
       #(model.fx/normalize-authorization-versions
         #{authorization-guard}))))

    (is
     (=
      :model.fx/invalid-authorization-version
      (error-type
       #(model.fx/normalize-authorization-versions
         [{:model/entity-type :location}])))))

  (testing "conflicting versions fail"
    (is
     (=
      :model.fx/conflicting-authorization-versions
      (error-type
       #(model.fx/normalize-authorization-versions
         [authorization-guard
          (assoc-in
           authorization-guard
           [:model/expected :model/revision]
           5)])))))

  (testing "guards become current-document assertions"
    (is
     (=
      [(model.fx/assert-document-current
        :location
        authorization-expected)

       (model.fx/assert-document-current
        :user
        (:model/expected other-authorization-guard))]
      (model.fx/authorization-version-assertions
       [authorization-guard
        other-authorization-guard])))))

(deftest command-translation-test
  (is
   (=
    (model.fx/assert-document-absent
     :example
     document-id)
    (model.fx/command-precondition
     create-command)))

  (is
   (=
    (model.fx/assert-document-current
     :example
     (:model/expected update-command))
    (model.fx/command-precondition
     update-command)))

  (is
   (=
    [:put-docs :example create-document]
    (model.fx/command->tx-op
     create-command)))

  (is
   (=
    [:put-docs :example updated-document]
    (model.fx/command->tx-op
     update-command))))

(deftest transaction-operation-order-test
  (let [operations
        (model.fx/transaction-ops
         {:commands [create-command other-create-command]
          :authorization-versions
          [authorization-guard
           authorization-guard
           other-authorization-guard]
          :assertions [explicit-assertion]})]
    (is
     (=
      [explicit-assertion
       (model.fx/assert-document-current
        :location
        authorization-expected)
       (model.fx/assert-document-current
        :user
        (:model/expected other-authorization-guard))
       (model.fx/command-precondition create-command)
       (model.fx/command-precondition other-create-command)
       (model.fx/command->tx-op create-command)
       (model.fx/command->tx-op other-create-command)]
      operations))))

(deftest duplicate-command-target-test
  (is
   (=
    :model.fx/duplicate-command-targets
    (error-type
     #(model.fx/transaction-ops
       {:commands [create-command update-command]
        :authorization-versions []
        :assertions []})))))

(defn- no-op-transaction
  [plan]
  (with-redefs
   [biffx/validate-tx
    (fn [& _] nil)

    biffx/format-query
    identity

    live/execute-tx!
    (fn [& _]
      {:tx-result {:tx-id 1}
       :consistency {:tx-id 1}})]

    (model.fx/transact!
     {:biff/malli-opts {}
      :gesso.live/system ::live-system}
     plan)))

(deftest transaction-plan-validation-test
  (is
   (=
    :model.fx/invalid-transaction-plan
    (error-type
     #(model.fx/transact! {} nil))))

  (is
   (=
    :model.fx/empty-transaction
    (error-type
     #(no-op-transaction
       {:commands []
        :changes []
        :emit false}))))

  (is
   (=
    :model.fx/invalid-assertions
    (error-type
     #(no-op-transaction
       (silent-plan
        {:assertions [{:where [:= :xt/id document-id]}]})))))

  (is
   (=
    :model.fx/missing-live-changes
    (error-type
     #(no-op-transaction
       {:commands [create-command]
        :changes []
        :emit :async}))))

  (is
   (=
    :model.fx/ambiguous-entry
    (error-type
     #(no-op-transaction
       (base-plan
        {:entry {:coalesce-key [:example document-id]}
         :entry-fn (fn [_] {})})))))

  (is
   (=
    :model.fx/invalid-tx-options
    (error-type
     #(no-op-transaction
       (base-plan
        {:tx-options [:bad]})))))

  (is
   (=
    :model.fx/conflicting-authorization-versions
    (error-type
     #(no-op-transaction
       (silent-plan
        {:authorization-versions
         [authorization-guard
          (assoc-in
           authorization-guard
           [:model/expected :model/revision]
           99)]}))))))

(deftest prepare-transaction-operations-test
  (let [validated
        (atom nil)

        formatted
        (atom [])

        normalized-plan
        {:commands [create-command]
         :authorization-versions [authorization-guard]
         :assertions [explicit-assertion]
         :changes []
         :emit false
         :entry nil
         :entry-fn nil
         :tx-options nil}

        expected
        (model.fx/transaction-ops
         normalized-plan)]
    (with-redefs
     [biffx/validate-tx
      (fn [operations malli-options]
        (reset! validated [operations malli-options]))

      biffx/format-query
      (fn [operation]
        (swap! formatted conj operation)
        [:formatted operation])]

      (is
       (=
        (mapv
         (fn [operation]
           [:formatted operation])
         expected)
        (model.fx/prepare-tx-ops
         {:biff/malli-opts
          (atom {:registry :test})}
         normalized-plan))))

    (is
     (=
      [expected {:registry :test}]
      @validated))

    (is
     (=
      expected
      @formatted))))

(deftest silent-transaction-test
  (let [executed
        (atom nil)

        result
        (with-redefs
         [biffx/validate-tx
          (fn [& _] nil)

          biffx/format-query
          identity

          live/execute-tx!
          (fn [ctx operations tx-options]
            (reset! executed [ctx operations tx-options])
            {:tx-result {:tx-id 21}
             :consistency {:tx-id 21}})]

         (model.fx/transact!
          {:biff/malli-opts {}}
          (silent-plan
           {:authorization-versions [authorization-guard]
            :tx-options {:default-tz "UTC"}})))]
    (is
     (=
      :committed
      (:commit/status result)))

    (is
     (=
      false
      (:emit result)))

    (is
     (=
      {:status :not-requested
       :mode false
       :results []}
      (:publication result)))

    (is
     (=
      {:default-tz "UTC"}
      (nth @executed 2)))))

(deftest synchronous-publication-test
  (let [emitted
        (atom [])

        result
        (with-redefs
         [biffx/validate-tx
          (fn [& _] nil)

          biffx/format-query
          identity

          live/execute-tx!
          (fn [& _]
            {:tx-result {:tx-id 22}
             :consistency {:tx-id 22}})

          live/with-consistency
          (fn [ctx consistency]
            (assoc ctx :test/consistency consistency))

          live/attach-consistency
          (fn [change consistency]
            (assoc change :test/consistency consistency))

          live/emit-expanded!
          (fn [system ctx change]
            (swap! emitted conj [system ctx change])
            {:status :emitted
             :source/id (:id change)
             :count 3
             :ignored true})]

         (model.fx/transact!
          {:biff/malli-opts {}
           :gesso.live/system ::live-system}
          (base-plan
           {:authorization-versions [authorization-guard]})))]
    (is
     (=
      :committed
      (:commit/status result)))

    (is
     (=
      {:status :emitted
       :mode :sync
       :results
       [{:status :emitted
         :source/id document-id
         :count 3}]}
      (:publication result)))

    (is
     (=
      {:tx-id 22}
      (get-in
       (nth (first @emitted) 2)
       [:test/consistency])))))

(deftest asynchronous-publication-test
  (let [submitted
        (atom [])

        result
        (with-redefs
         [biffx/validate-tx
          (fn [& _] nil)

          biffx/format-query
          identity

          live/execute-tx!
          (fn [& _]
            {:tx-result {:tx-id 23}
             :consistency {:tx-id 23}})

          live/with-consistency
          (fn [ctx consistency]
            (assoc ctx :test/consistency consistency))

          live/attach-consistency
          (fn [change consistency]
            (assoc change :test/consistency consistency))

          live/submit-expanded!
          (fn [system ctx change entry]
            (swap! submitted conj [system ctx change entry])
            {:status :submitted
             :reason :accepted
             :job-id 9
             :coalesce-key (:coalesce-key entry)
             :ignored true})]

         (model.fx/transact!
          {:biff/malli-opts {}
           :live/system ::alternate-live-system}
          (base-plan
           {:emit :async
            :entry-fn
            (fn [change]
              {:coalesce-key
               [(:topic change)
                (:id change)]})})))]
    (is
     (=
      {:status :submitted
       :mode :async
       :results
       [{:status :submitted
         :reason :accepted
         :job-id 9
         :coalesce-key [:example document-id]}]}
      (:publication result)))

    (is
     (=
      ::alternate-live-system
      (ffirst @submitted)))

    (is
     (=
      {:coalesce-key [:example document-id]}
      (nth (first @submitted) 3)))))

(deftest dropped-publication-test
  (let [result
        (with-redefs
         [biffx/validate-tx
          (fn [& _] nil)

          biffx/format-query
          identity

          live/execute-tx!
          (fn [& _]
            {:tx-result {:tx-id 24}
             :consistency {:tx-id 24}})

          live/with-consistency
          (fn [ctx consistency]
            (assoc ctx :test/consistency consistency))

          live/attach-consistency
          (fn [change consistency]
            (assoc change :test/consistency consistency))

          live/submit-expanded!
          (fn [& _]
            {:status :dropped
             :reason :queue-full
             :coalesce-key [:example document-id]})]

         (model.fx/transact!
          {:biff/malli-opts {}
           :gesso.live/system ::live-system}
          (base-plan
           {:emit :async
            :entry
            {:coalesce-key [:example document-id]}})))]
    (is
     (=
      :committed
      (:commit/status result)))

    (is
     (=
      :incomplete
      (get-in result [:publication :status])))

    (is
     (=
      0
      (get-in result [:publication :dropped 0 :index])))))

(deftest publication-failure-does-not-hide-commit-test
  (let [result
        (with-redefs
         [log/enabled?
          (fn [& _] false)

          biffx/validate-tx
          (fn [& _] nil)

          biffx/format-query
          identity

          live/execute-tx!
          (fn [& _]
            {:tx-result {:tx-id 25}
             :consistency {:tx-id 25}})

          live/with-consistency
          (fn [ctx consistency]
            (assoc ctx :test/consistency consistency))

          live/attach-consistency
          (fn [change consistency]
            (assoc change :test/consistency consistency))

          live/emit-expanded!
          (fn [& _]
            (throw
             (RuntimeException.
              "publication failed")))]

         (model.fx/transact!
          {:biff/malli-opts {}
           :gesso.live/system ::live-system}
          (base-plan)))]
    (is
     (=
      :committed
      (:commit/status result)))

    (is
     (=
      :failed
      (get-in result [:publication :status])))

    (is
     (=
      "publication failed"
      (get-in result [:publication :error :message])))))

(deftest transaction-failure-prevents-publication-test
  (let [published?
        (atom false)]
    (with-redefs
     [biffx/validate-tx
      (fn [& _] nil)

      biffx/format-query
      identity

      live/execute-tx!
      (fn [& _]
        (throw
         (ex-info
          "transaction failed"
          {:error/type :test/transaction-failed})))

      live/emit-expanded!
      (fn [& _]
        (reset! published? true)
        {:status :emitted})]

      (is
       (=
        :test/transaction-failed
        (error-type
         #(model.fx/transact!
           {:biff/malli-opts {}
            :gesso.live/system ::live-system}
           (base-plan)))))

      (is
       (false?
        @published?)))))

(deftest missing-runtime-context-test
  (is
   (=
    :model.fx/missing-live-system
    (error-type
     #(model.fx/transact!
       {:biff/malli-opts {}}
       (base-plan)))))

  (is
   (=
    :model.fx/missing-malli-options
    (error-type
     #(model.fx/prepare-tx-ops
       {}
       {:commands [create-command]
        :authorization-versions []
        :assertions []
        :changes []
        :emit false
        :entry nil
        :entry-fn nil
        :tx-options nil})))))

(deftest module-contract-test
  (is
   (=
    {model.fx/transact-effect
     model.fx/transact!}
    model.fx/handlers))

  (is
   (=
    {:biff.fx/handlers
     model.fx/handlers}
    model.fx/module)))
