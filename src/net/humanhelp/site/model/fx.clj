(ns net.humanhelp.site.model.fx
  "Shared execution of pure model commands through Biff's XTDB2 transaction API.

   Domain namespaces describe creates and updates with :model/* command maps.
   Model-specific FX namespaces add business assertions and submit commands
   through transact-effect. This namespace owns only the generic translation
   from those commands to one atomic Biff transaction."
  (:require
   [com.biffweb.experimental :as biffx]
   [net.humanhelp.site.model.common :as model.common]
   [xtdb.api :as xt]))

(def transact-effect
  ::transact)

(defn- table-symbol
  [entity-type]
  (symbol (name entity-type)))

(defn count-subquery
  [entity-type where]
  {:select [[[:count '*']]]
   :from (table-symbol entity-type)
   :where where})

(defn assert-none
  "Returns an XTDB SQL ASSERT form requiring zero current matching documents."
  [entity-type where]
  {:assert
   [:= 0
    (count-subquery entity-type where)]})

(defn assert-one
  "Returns an XTDB SQL ASSERT form requiring exactly one current match."
  [entity-type where]
  {:assert
   [:= 1
    (count-subquery entity-type where)]})

(defn- create-precondition
  [{:model/keys [entity-type id]}]
  (assert-none
   entity-type
   [:= :xt/id id]))

(defn- update-precondition
  [{:model/keys [entity-type expected]}]
  (let [{:model/keys
         [id
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

(defn command-precondition
  [command]
  (if
   (= :create
      (:model/operation command))
    (create-precondition command)
    (update-precondition command)))

(defn command->tx-op
  [command]
  [:put-docs
   (:model/entity-type command)
   (model.common/command-document command)])

(defn transaction-ops
  "Builds one Biff/XTDB transaction.

   assertions are model-specific HoneySQL ASSERT forms. Generic document
   existence/version assertions are generated from each command."
  [{:keys [assertions commands]}]
  (let [commands
        (vec commands)]
    (into
     (vec assertions)
     (mapcat
      (fn [command]
        [(command-precondition command)
         (command->tx-op command)]))
     commands)))

(defn- deref-if-needed
  [value]
  (if (instance? clojure.lang.IDeref value)
    @value
    value))

(defn transact!
  [{:keys [biff/conn biff/node biff/malli-opts]} transaction]
  (let [tx-ops
        (transaction-ops transaction)

        connectable
        (or conn node
            (throw
             (ex-info
              "Model FX requires :biff/conn or :biff/node."
              {:error/type :model.fx/missing-connectable})))]
    (biffx/validate-tx
     tx-ops
     (deref-if-needed malli-opts))

    (xt/execute-tx
     connectable
     (mapv biffx/format-query tx-ops))))

(def handlers
  {transact-effect
   transact!})
