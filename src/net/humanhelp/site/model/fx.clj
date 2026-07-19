(ns net.humanhelp.site.model.fx
  "The shared transaction boundary for HumanHelp model workflows.

   Model-specific FX namespaces describe one atomic database change with a
   transaction fragment:

     {:commands               [...]
      :authorization-versions [...]
      :assertions             [...]
      :changes                [...]}

   Transaction fragments may be composed before commit. The final transaction
   plan may additionally contain transaction-level execution options:

     {:emit       :async|:sync|false
      :entry      dispatch-entry
      :entry-fn   attached-change->dispatch-entry
      :tx-options xtdb-options}

   Model-specific code decides which documents establish authorization, which
   commands express the domain transition, which model-specific ASSERT forms
   are required, and which semantic changes describe the committed operation.

   This namespace owns transaction-fragment construction and composition,
   generic authorization and command guards, transaction operation assembly,
   Biff validation and formatting, and one final delegation to
   gesso.live.core/transact-and-notify!.

   Gesso Live owns transaction execution, consistency attachment, change
   expansion, dispatch construction, coalescing, and publication."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.live.core :as live]
   [net.humanhelp.site.model.common :as model.common]))

(def transact-effect
  "Gesso FX handler key for one atomic model transaction plan."
  ::transact)

(def valid-emit-modes
  #{:async :sync false})

(def transaction-fragment-keys
  "Keys that may appear in one composable atomic transaction fragment."
  #{:commands
    :authorization-versions
    :assertions
    :changes})

(def transaction-option-keys
  "Transaction-level options chosen once after fragments compose."
  #{:emit
    :entry
    :entry-fn
    :tx-options})

(def transaction-plan-keys
  "All keys accepted by transact!."
  (into transaction-fragment-keys
        transaction-option-keys))

(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type error-type}
       (some? details)
       (assoc :error/details details))))))

(defn- deref-if-needed
  [value]
  (if (instance? clojure.lang.IDeref value)
    @value
    value))

(defn- unknown-keys
  [allowed value]
  (when (map? value)
    (seq
     (remove allowed
             (keys value)))))

(defn- sequential-value!
  [key value]
  (let [value (or value [])]
    (when-not (sequential? value)
      (fail! :model.fx/invalid-transaction-fragment
             "Transaction fragment collections must be sequential."
             {:key key
              :value value}))
    (vec value)))

;; =============================================================================
;; Composable transaction fragments
;; =============================================================================

(defn transaction-fragment
  "Constructs one canonical composable transaction fragment.

   Missing collections become empty vectors and collection order is preserved.

   This validates only the fragment container. Authorization versions,
   commands, assertions, and changes are intentionally left uninterpreted until
   the final transaction boundary sees the complete composed operation."
  [fragment]
  (when-not (map? fragment)
    (fail! :model.fx/invalid-transaction-fragment
           "A transaction fragment must be a map."
           {:fragment fragment}))

  (when-let [unknown (unknown-keys transaction-fragment-keys fragment)]
    (fail! :model.fx/unknown-transaction-fragment-keys
           "A transaction fragment contains unsupported keys."
           {:keys (set unknown)
            :allowed-keys transaction-fragment-keys}))

  {:commands
   (sequential-value! :commands
                      (:commands fragment))

   :authorization-versions
   (sequential-value! :authorization-versions
                      (:authorization-versions fragment))

   :assertions
   (sequential-value! :assertions
                      (:assertions fragment))

   :changes
   (sequential-value! :changes
                      (:changes fragment))})

(def empty-transaction-fragment
  "Canonical empty fragment used as the identity for composition."
  (transaction-fragment {}))

(defn compose-transaction-fragments
  "Concatenates transaction fragments in argument order.

   Composition preserves every command, authorization version, assertion, and
   semantic change. It does not deduplicate or resolve conflicts. The final
   transaction boundary must see all supplied evidence so it can reject
   conflicting authorization versions and duplicate command targets honestly.

   With no arguments, returns empty-transaction-fragment."
  [& fragments]
  (reduce
   (fn [combined fragment]
     (let [fragment (transaction-fragment fragment)]
       {:commands
        (into (:commands combined)
              (:commands fragment))

        :authorization-versions
        (into (:authorization-versions combined)
              (:authorization-versions fragment))

        :assertions
        (into (:assertions combined)
              (:assertions fragment))

        :changes
        (into (:changes combined)
              (:changes fragment))}))
   empty-transaction-fragment
   fragments))

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
   [:= 0
    (count-subquery entity-type where)]})

(defn assert-one
  "Returns an XTDB2 HoneySQL ASSERT requiring exactly one current match."
  [entity-type where]
  {:assert
   [:= 1
    (count-subquery entity-type where)]})

(defn assert-at-most-one
  "Returns an XTDB2 HoneySQL ASSERT requiring at most one current match."
  [entity-type where]
  {:assert
   [:>= 1
    (count-subquery entity-type where)]})

(defn assert-document-absent
  "Requires no current document with id in entity-type."
  [entity-type id]
  (when-not (uuid? id)
    (fail! :model.fx/invalid-document-id
           "A model document ID must be a UUID."
           {:entity-type entity-type
            :id id}))
  (assert-none entity-type
               [:= :xt/id id]))

(defn- expected-version?
  [expected]
  (let [{:model/keys [id
                      revision-key
                      revision
                      updated-at-key
                      updated-at]}
        expected]
    (and (map? expected)
         (uuid? id)
         (keyword? revision-key)
         (nat-int? revision)
         (keyword? updated-at-key)
         (model.common/timestamp-value? updated-at))))

(defn assert-document-current
  "Requires the current document to match generic expected-version metadata.

   expected must have the shape returned by model.common/expected-version:

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

  (let [{:model/keys [id
                      revision-key
                      revision
                      updated-at-key
                      updated-at]}
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
  "Returns true when value is one generic authorization-version guard.

   Authorization policy remains model-specific. This validates only the
   generic document-version guard shape understood by model.fx."
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
    [entity-type
     (:model/id expected)]))

(defn normalize-authorization-versions
  "Validates and normalizes authorization-version guards.

   Identical guards for the same [entity-type document-id] are deduplicated in
   first-seen order. Different expected versions for the same target are
   rejected because one transaction cannot honestly claim authorization from
   conflicting snapshots."
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
                  :guards-by-target
                  (assoc guards-by-target target guard)}

                 (= (:model/expected existing)
                    (:model/expected guard))
                 state

                 :else
                 (fail!
                  :model.fx/conflicting-authorization-versions
                  "The same authorization document was loaded at conflicting versions."
                  {:target target
                   :authorization-versions [existing guard]}))))
           {:order []
            :guards-by-target {}}
           guards)]
      (mapv guards-by-target
            order))))

(defn authorization-version-assertions
  "Converts authorization-version guards to XTDB2 ASSERT forms.

   The input is normalized so this public helper is safe to call directly."
  [guards]
  (mapv
   (fn [{:model/keys [entity-type expected]}]
     (assert-document-current entity-type
                              expected))
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

  (let [{:model/keys [entity-type
                      operation
                      id
                      expected]}
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

    (when-not (= id
                 (:xt/id document))
      (fail! :model.fx/invalid-command
             "The command ID must equal the document :xt/id."
             {:command-id id
              :document-id (:xt/id document)}))

    (if (= :create operation)
      (when (some? expected)
        (fail! :model.fx/invalid-command
               "A create command must not contain :model/expected."
               {:command command}))
      (when-not (expected-version? expected)
        (fail! :model.fx/invalid-command
               "A non-create command requires valid :model/expected metadata."
               {:command command}))))

  command)

(defn command-precondition
  "Returns the generic optimistic-concurrency assertion for command."
  [command]
  (let [{:model/keys [entity-type
                      operation
                      id
                      expected]}
        (validate-command! command)]
    (if (= :create operation)
      (assert-document-absent entity-type
                              id)
      (assert-document-current entity-type
                               expected))))

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
       (map
        (juxt :model/entity-type
              :model/id))
       frequencies
       (keep
        (fn [[target count]]
          (when (< 1 count)
            target)))
       set))

(defn transaction-ops
  "Builds unformatted Biff/XTDB2 operations from one final transaction plan.

   Operations are ordered as:

   1. explicit model-specific assertions;
   2. authorization-version assertions;
   3. generic command optimistic-concurrency preconditions;
   4. document writes.

   Each model document may be written at most once in a transaction."
  [{:keys [assertions
           authorization-versions
           commands]}]
  (let [commands
        (mapv validate-command!
              commands)

        authorization-assertions
        (authorization-version-assertions authorization-versions)

        duplicates
        (duplicate-command-targets commands)]

    (when (seq duplicates)
      (fail! :model.fx/duplicate-command-targets
             "A transaction may write each model document at most once."
             {:targets duplicates}))

    (into
     []
     (concat assertions
             authorization-assertions
             (map command-precondition commands)
             (map command->tx-op commands)))))

;; =============================================================================
;; Final transaction-plan normalization
;; =============================================================================

(defn- normalize-plan
  [plan]
  (when-not (map? plan)
    (fail! :model.fx/invalid-transaction-plan
           "A model transaction plan must be a map."
           {:plan plan}))

  (when-let [unknown (unknown-keys transaction-plan-keys plan)]
    (fail! :model.fx/unknown-transaction-plan-keys
           "A model transaction plan contains unsupported keys."
           {:keys (set unknown)
            :allowed-keys transaction-plan-keys}))

  (let [{:keys [commands
                authorization-versions
                assertions
                changes]}
        (transaction-fragment
         (select-keys plan
                      transaction-fragment-keys))

        authorization-versions
        (normalize-authorization-versions authorization-versions)

        emit
        (if (contains? plan :emit)
          (:emit plan)
          :async)

        entry
        (:entry plan)

        entry-fn
        (:entry-fn plan)

        tx-options
        (:tx-options plan)]

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
  (let [tx-ops
        (transaction-ops normalized-plan)]
    (biffx/validate-tx
     tx-ops
     (malli-opts! ctx))

    (mapv biffx/format-query
          tx-ops)))

(defn- live-system-for!
  [ctx emit]
  (when (not= false emit)
    (or (:gesso.live/system ctx)
        (:live/system ctx)
        (fail! :model.fx/missing-live-system
               "Publishing model transactions require the application Gesso Live system."
               {:expected-one-of
                [:gesso.live/system
                 :live/system]}))))

(defn- request-biff-listener-poll!
  "Asks Biff's optional XTDB2 listener to poll immediately.

   This is only a latency optimization: the listener polls on its own. A poll
   hook failure must not make a successfully committed transaction appear to
   have failed."
  [ctx]
  (when-some [poll-now (:biff.xtdb.listener/poll-now ctx)]
    (try
      (poll-now)
      (catch Throwable _
        nil))))

;; =============================================================================
;; Shared Gesso FX handler
;; =============================================================================

(defn transact!
  "Executes one atomic model transaction through Biff and Gesso Live.

   Model transaction assembly ends here. This function validates and formats
   the XTDB2 operations, then delegates execution, consistency attachment,
   change expansion, dispatch, coalescing, and publication to
   gesso.live.core/transact-and-notify!.

   The returned map is Gesso Live's result without its consistency-aware ctx,
   plus :commit/status :committed.

   Publication-failure semantics belong to Gesso Live. They must be fixed in
   Gesso Live if it can throw after a successful commit; model.fx intentionally
   does not reimplement the publication pipeline to change those semantics."
  [ctx plan]
  (let [{:keys [changes
                emit
                entry
                entry-fn
                tx-options]
         :as normalized-plan}
        (normalize-plan plan)

        tx-ops
        (prepare-tx-ops ctx
                        normalized-plan)

        result
        (live/transact-and-notify!
         (live-system-for! ctx emit)
         ctx
         {:tx-ops tx-ops
          :tx-options tx-options
          :changes changes
          :emit emit
          :entry entry
          :entry-fn entry-fn})]

    (request-biff-listener-poll! ctx)

    (-> result
        (dissoc :ctx)
        (assoc :commit/status :committed))))

(def handlers
  "Gesso FX handler contribution."
  {transact-effect transact!})

(def module
  "Biff module contribution that installs the shared model transaction effect."
  {:biff.fx/handlers handlers})
