(ns net.humanhelp.site.model.fx
  "The shared transaction boundary for HumanHelp model workflows.

   Model-specific FX namespaces submit transaction plans through
   `transact-effect`:

     {:commands               [...]
      :authorization-versions [...]
      :assertions             [...]
      :changes                [...]
      :emit                   :async}

   An authorization-version guard has this generic shape:

     {:model/entity-type entity-type
      :model/expected    {:model/id             uuid
                          :model/revision-key   keyword
                          :model/revision       non-negative-integer
                          :model/updated-at-key keyword
                          :model/updated-at     timestamp}}

   Model-specific FX decides which documents establish authorization and places
   their guards in :authorization-versions. This namespace validates,
   deduplicates, conflict-checks, and converts those guards into XTDB2 ASSERT
   operations.

   This namespace owns the generic infrastructure required by every model:

   - optimistic-concurrency assertions for :model/* commands;
   - reusable HoneySQL ASSERT helpers;
   - generic authorization-version guard normalization;
   - conversion of domain commands to XTDB2 operations;
   - Biff Malli validation and HoneySQL formatting;
   - synchronous XTDB2 execution with Gesso consistency metadata;
   - best-effort Gesso Live expansion and publication after commit.

   The database commit and Live publication cannot be atomic. Once XTDB2 has
   committed, publication failures are returned as publication metadata rather
   than thrown as transaction failures. Callers must not retry a committed
   transaction merely because Live publication failed.

   It does not own model-specific authorization decisions, proof discovery,
   queries, workflow planning, domain transitions, or UI rendering."
  (:require
   [clojure.tools.logging :as log]
   [com.biffweb.experimental :as biffx]
   [gesso.live.core :as live]
   [net.humanhelp.site.model.common :as model.common]))

(def transact-effect
  "Gesso FX handler key for one atomic model transaction plan."
  ::transact)

(def valid-emit-modes
  #{:async :sync false})

(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type error-type}
       (some? details) (assoc :error/details details))))))

(defn- deref-if-needed
  [value]
  (if (instance? clojure.lang.IDeref value)
    @value
    value))

;; =============================================================================
;; Generic transaction assertions
;; =============================================================================

(defn- table-symbol
  [entity-type]
  (when-not (keyword? entity-type)
    (fail! :model.fx/invalid-entity-type
           "A model entity type must be a keyword."
           {:entity-type entity-type}))
  (symbol (name entity-type)))

(defn- count-subquery
  [entity-type where]
  {:select [[[:count '*']]]
   :from (table-symbol entity-type)
   :where where})

(defn assert-none
  "Returns an XTDB2 HoneySQL ASSERT requiring zero current matches."
  [entity-type where]
  {:assert
   [:= 0 (count-subquery entity-type where)]})

(defn assert-one
  "Returns an XTDB2 HoneySQL ASSERT requiring exactly one current match."
  [entity-type where]
  {:assert
   [:= 1 (count-subquery entity-type where)]})

(defn assert-at-most-one
  "Returns an XTDB2 HoneySQL ASSERT requiring at most one current match."
  [entity-type where]
  {:assert
   [:>= 1 (count-subquery entity-type where)]})

(defn assert-document-absent
  "Requires no current document with `id` in `entity-type`."
  [entity-type id]
  (when-not (uuid? id)
    (fail! :model.fx/invalid-document-id
           "A model document ID must be a UUID."
           {:entity-type entity-type
            :id id}))
  (assert-none entity-type [:= :xt/id id]))

(defn- expected-version?
  [expected]
  (let [{:model/keys [id revision-key revision updated-at-key updated-at]}
        expected]
    (and (map? expected)
         (uuid? id)
         (keyword? revision-key)
         (nat-int? revision)
         (keyword? updated-at-key)
         (model.common/timestamp-value? updated-at))))

(defn assert-document-current
  "Requires the current document to match generic expected-version metadata.

   `expected` must have the shape returned by model.common/expected-version:

     {:model/id             uuid
      :model/revision-key   keyword
      :model/revision       non-negative integer
      :model/updated-at-key keyword
      :model/updated-at     timestamp}"
  [entity-type expected]
  (when-not (expected-version? expected)
    (fail! :model.fx/invalid-expected-version
           "Expected-version metadata is invalid."
           {:entity-type entity-type
            :expected expected}))
  (let [{:model/keys [id revision-key revision updated-at-key updated-at]}
        expected]
    (assert-one
     entity-type
     [:and
      [:= :xt/id id]
      [:= revision-key revision]
      [:= updated-at-key updated-at]])))

(defn- assertion-form?
  [value]
  (and (map? value)
       (contains? value :assert)))

;; =============================================================================
;; Authorization-version guards
;; =============================================================================

(def authorization-version-keys
  #{:model/entity-type
    :model/expected})

(defn authorization-version?
  "Returns true when `value` is one generic authorization-version guard.

   Authorization policy remains model-specific. This predicate validates only
   the generic document-version guard shape understood by model.fx."
  [value]
  (and (map? value)
       (= authorization-version-keys
          (set (keys value)))
       (keyword? (:model/entity-type value))
       (expected-version? (:model/expected value))))

(defn- require-authorization-version!
  [guard]
  (when-not (authorization-version? guard)
    (fail! :model.fx/invalid-authorization-version
           "An authorization-version guard is invalid."
           {:authorization-version guard}))
  guard)

(defn authorization-version-target
  "Returns the stable [entity-type document-id] target for one valid guard."
  [guard]
  (let [{:model/keys [entity-type expected]}
        (require-authorization-version! guard)]
    [entity-type (:model/id expected)]))

(defn normalize-authorization-versions
  "Validates and normalizes authorization-version guards.

   Identical guards for the same [entity-type document-id] are deduplicated in
   first-seen order. Different expected versions for the same target are
   rejected because one transaction cannot honestly claim authorization from
   two conflicting snapshots."
  [guards]
  (let [guards (or guards [])]
    (when-not (sequential? guards)
      (fail! :model.fx/invalid-authorization-versions
             "Transaction :authorization-versions must be sequential."
             {:authorization-versions guards}))
    (let [{:keys [order guards-by-target]}
          (reduce
           (fn [{:keys [order guards-by-target] :as state} guard]
             (let [guard (require-authorization-version! guard)
                   target (authorization-version-target guard)
                   existing (get guards-by-target target)]
               (cond
                 (nil? existing)
                 {:order (conj order target)
                  :guards-by-target (assoc guards-by-target target guard)}

                 (= (:model/expected existing)
                    (:model/expected guard))
                 state

                 :else
                 (fail! :model.fx/conflicting-authorization-versions
                        "The same authorization document was loaded at conflicting versions."
                        {:target target
                         :authorization-versions [existing guard]}))))
           {:order []
            :guards-by-target {}}
           guards)]
      (mapv guards-by-target order))))

(defn authorization-version-assertions
  "Converts authorization-version guards to XTDB2 ASSERT forms.

   The input is normalized so this public helper is safe to call directly."
  [guards]
  (mapv
   (fn [{:model/keys [entity-type expected]}]
     (assert-document-current entity-type expected))
   (normalize-authorization-versions guards)))

;; =============================================================================
;; Domain-command translation
;; =============================================================================

(defn- validate-command!
  [command]
  (when-not (map? command)
    (fail! :model.fx/invalid-command
           "A model command must be a map."
           {:command command}))

  (let [{:model/keys [entity-type operation id expected]}
        command
        document
        (model.common/command-document command)]
    (when-not (keyword? entity-type)
      (fail! :model.fx/invalid-command
             "A model command requires a keyword :model/entity-type."
             {:command command}))

    (when-not (keyword? operation)
      (fail! :model.fx/invalid-command
             "A model command requires a keyword :model/operation."
             {:command command}))

    (when-not (uuid? id)
      (fail! :model.fx/invalid-command
             "A model command requires a UUID :model/id."
             {:command command}))

    (when-not (map? document)
      (fail! :model.fx/invalid-command
             "A model command must contain a document."
             {:command command}))

    (when-not (= id (:xt/id document))
      (fail! :model.fx/invalid-command
             "The command ID must equal the document :xt/id."
             {:command-id id
              :document-id (:xt/id document)}))

    (if (= :create operation)
      (when (some? expected)
        (fail! :model.fx/invalid-command
               "A create command must not contain :model/expected."
               {:command command}))
      (when-not (map? expected)
        (fail! :model.fx/invalid-command
               "A non-create command requires :model/expected metadata."
               {:command command}))))

  command)

(defn command-precondition
  "Returns the generic optimistic-concurrency assertion for `command`."
  [command]
  (let [{:model/keys [entity-type operation id expected]}
        (validate-command! command)]
    (if (= :create operation)
      (assert-document-absent entity-type id)
      (assert-document-current entity-type expected))))

(defn command->tx-op
  "Converts one model command to an XTDB2 :put-docs operation."
  [command]
  (let [{:model/keys [entity-type]}
        (validate-command! command)]
    [:put-docs
     entity-type
     (model.common/command-document command)]))

(defn- duplicate-command-targets
  [commands]
  (->> commands
       (map (juxt :model/entity-type :model/id))
       frequencies
       (keep (fn [[target n]]
               (when (< 1 n)
                 target)))
       set))

(defn transaction-ops
  "Builds unformatted Biff/XTDB2 operations from a normalized plan.

   Operations are ordered as:

   1. explicit model-specific assertions;
   2. authorization-version assertions;
   3. generic command optimistic-concurrency preconditions;
   4. document writes.

   Each model document may be written at most once in a transaction."
  [{:keys [assertions authorization-versions commands]}]
  (let [commands (mapv validate-command! commands)
        authorization-assertions
        (authorization-version-assertions authorization-versions)
        duplicates (duplicate-command-targets commands)]
    (when (seq duplicates)
      (fail! :model.fx/duplicate-command-targets
             "A transaction may write each model document at most once."
             {:targets duplicates}))

    (into []
          (concat assertions
                  authorization-assertions
                  (map command-precondition commands)
                  (map command->tx-op commands)))))

;; =============================================================================
;; Transaction-plan normalization
;; =============================================================================

(defn- normalize-plan
  [plan]
  (when-not (map? plan)
    (fail! :model.fx/invalid-transaction-plan
           "A model transaction plan must be a map."
           {:plan plan}))

  (let [commands (vec (or (:commands plan) []))
        authorization-versions
        (normalize-authorization-versions
         (:authorization-versions plan))
        assertions (vec (or (:assertions plan) []))
        changes (vec (or (:changes plan) []))
        emit (if (contains? plan :emit)
               (:emit plan)
               :async)
        entry (:entry plan)
        entry-fn (:entry-fn plan)
        tx-options (:tx-options plan)]
    (when (empty? commands)
      (fail! :model.fx/empty-transaction
             "A model transaction requires at least one command."))

    (when-not (every? assertion-form? assertions)
      (fail! :model.fx/invalid-assertions
             "Model transaction assertions must be HoneySQL :assert maps."
             {:assertions assertions}))

    (when-not (every? map? changes)
      (fail! :model.fx/invalid-changes
             "Gesso Live primary changes must be maps."
             {:changes changes}))

    (when-not (contains? valid-emit-modes emit)
      (fail! :model.fx/invalid-emit-mode
             "Transaction :emit must be :async, :sync, or false."
             {:emit emit}))

    (when (and (not= false emit)
               (empty? changes))
      (fail! :model.fx/missing-live-changes
             "A publishing transaction requires at least one primary change. Use :emit false for an intentionally silent write."))

    (when-not (or (nil? entry)
                  (map? entry))
      (fail! :model.fx/invalid-entry
             "A Gesso Live dispatch :entry must be a map when supplied."
             {:entry entry}))

    (when-not (or (nil? entry-fn)
                  (ifn? entry-fn))
      (fail! :model.fx/invalid-entry-fn
             "A Gesso Live :entry-fn must be callable when supplied."
             {:entry-fn entry-fn}))

    (when (and entry entry-fn)
      (fail! :model.fx/ambiguous-entry
             "Supply either :entry or :entry-fn, not both."))

    (when-not (or (nil? tx-options)
                  (map? tx-options))
      (fail! :model.fx/invalid-tx-options
             "XTDB2 transaction options must be a map when supplied."
             {:tx-options tx-options}))

    {:commands commands
     :authorization-versions authorization-versions
     :assertions assertions
     :changes changes
     :emit emit
     :entry entry
     :entry-fn entry-fn
     :tx-options tx-options}))

(defn- malli-opts!
  [ctx]
  (or (some-> (:biff/malli-opts ctx)
              deref-if-needed)
      (fail! :model.fx/missing-malli-options
             "Model transactions require :biff/malli-opts.")))

(defn prepare-tx-ops
  "Validates model documents with Biff and formats HoneySQL operations for
   XTDB2/Gesso execution."
  [ctx normalized-plan]
  (let [tx-ops (transaction-ops normalized-plan)]
    (biffx/validate-tx tx-ops (malli-opts! ctx))
    (mapv biffx/format-query tx-ops)))

;; =============================================================================
;; Post-commit Live publication
;; =============================================================================

(defn- live-system!
  [ctx]
  (or (:gesso.live/system ctx)
      (:live/system ctx)
      (fail! :model.fx/missing-live-system
             "Publishing model transactions require the application Gesso Live system."
             {:expected-one-of
              [:gesso.live/system :live/system]})))

(defn- dispatch-entry-for!
  [entry entry-fn change]
  (let [dispatch-entry
        (cond
          entry-fn (entry-fn change)
          entry entry
          :else nil)]
    (when-not (or (nil? dispatch-entry)
                  (map? dispatch-entry))
      (fail! :model.fx/invalid-dispatch-entry
             "A Gesso Live dispatch entry must be a map when supplied."
             {:change change
              :entry dispatch-entry}))
    dispatch-entry))

(defn- error-summary
  [^Exception error]
  {:class (.getName (class error))
   :message (.getMessage error)})

(defn- summarize-publication-result
  [emit result]
  (case emit
    :async
    (select-keys result
                 [:status :reason :job-id :coalesce-key])

    :sync
    (select-keys result
                 [:status :source/id :count])))

(defn- publish-one!
  [system ctx emit change dispatch-entry]
  (summarize-publication-result
   emit
   (case emit
     :sync
     (live/emit-expanded! system ctx change)

     :async
     (live/submit-expanded! system ctx change dispatch-entry))))

(defn- dropped-submissions
  [results]
  (->> results
       (keep-indexed
        (fn [index result]
          (when (= :dropped (:status result))
            {:index index
             :result result})))
       vec))

(defn- publish-changes!
  [system ctx emit changes entry entry-fn]
  (if (= false emit)
    {:status :not-requested
     :mode false
     :results []}
    (loop [index 0
           remaining changes
           results []]
      (if-let [change (first remaining)]
        (let [outcome
              (try
                {:result
                 (publish-one!
                  system
                  ctx
                  emit
                  change
                  (when (= :async emit)
                    (dispatch-entry-for! entry entry-fn change)))}
                (catch Exception error
                  {:error error}))]
          (if-let [error (:error outcome)]
            (do
              (log/error
               error
               (str "Model transaction committed, but Gesso Live publication failed"
                    " at change index " index "."))
              {:status :failed
               :mode emit
               :results results
               :failed-index index
               :failed-change change
               :error (error-summary error)})
            (recur (inc index)
                   (next remaining)
                   (conj results (:result outcome)))))
        (if (= :sync emit)
          {:status :emitted
           :mode :sync
           :results results}
          (let [dropped (dropped-submissions results)]
            (if (seq dropped)
              (do
                (log/warn
                 (str "Model transaction committed, but "
                      (count dropped)
                      " Gesso Live submissions were dropped."))
                {:status :incomplete
                 :mode :async
                 :results results
                 :dropped dropped})
              {:status :submitted
               :mode :async
               :results results})))))))

;; =============================================================================
;; Shared Gesso FX handler
;; =============================================================================

(defn transact!
  "Executes one model transaction through the shared Biff/Gesso boundary.

   XTDB2 transaction failures are thrown and no commit result is returned.
   Once XTDB2 commits, Live publication is best-effort: publication problems are
   logged and returned under :publication instead of being thrown as apparent
   transaction failures.

   The returned value intentionally excludes the application ctx:

     {:commit/status :committed
      :tx-result ...
      :consistency ...
      :changes ...
      :emit ...
      :publication ...}"
  [ctx plan]
  (let [{:keys [changes emit entry entry-fn tx-options] :as normalized-plan}
        (normalize-plan plan)

        system
        (when (not= false emit)
          (live-system! ctx))

        tx-ops
        (prepare-tx-ops ctx normalized-plan)

        {:keys [tx-result consistency]}
        (live/execute-tx! ctx tx-ops tx-options)

        consistency-ctx
        (live/with-consistency ctx consistency)

        attached-changes
        (mapv #(live/attach-consistency % consistency) changes)

        publication
        (publish-changes!
         system
         consistency-ctx
         emit
         attached-changes
         entry
         entry-fn)]
    {:commit/status :committed
     :tx-result tx-result
     :consistency consistency
     :changes attached-changes
     :emit emit
     :publication publication}))

(def handlers
  "Gesso FX handler contribution."
  {transact-effect transact!})

(def module
  "Biff module contribution that installs the shared model transaction effect."
  {:biff.fx/handlers handlers})
