(ns net.humanhelp.site.model.user.fx
  "User-specific read dependencies and transaction planning.

   User FX owns only the effectful concerns surrounding canonical User domain
   commands:

   - loading the current User when a mutation or cross-model decision needs it;
   - generating a User ID for creation;
   - protecting globally unique phone/email facts;
   - attaching semantic User changes for Gesso Live;
   - returning composable gesso.model transaction fragments.

   It does not own User transition semantics, authorization, Memberships,
   Organizations, Invitations, authentication policy, or transaction commit."
  (:require
   [com.biffweb.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.user.domain :as user]
   [net.humanhelp.site.model.user.schema :as user.schema]))

;; =============================================================================
;; Errors and FX context
;; =============================================================================

(defn- fail!
  ([type message]
   (fail! type message nil))
  ([type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type type}
       (some? details)
       (assoc :error/details details))))))

(defn- now!
  [ctx]
  (or
   (:biff.fx/now ctx)
   (fail!
    :user.fx/missing-now
    "User planning requires :biff.fx/now.")))

(defn- seed!
  [ctx]
  (or
   (:biff.fx/seed ctx)
   (fail!
    :user.fx/missing-seed
    "User creation requires :biff.fx/seed.")))

(defn- generated-id
  [ctx]
  (first
   (fx/uuid7
    (seed! ctx)
    (now! ctx))))

(defn- require-user-id!
  [user-id]
  (when-not
   (uuid? user-id)
    (fail!
     :user/invalid-user-id
     "User ID must be a UUID."
     {:user/id user-id}))
  user-id)

;; =============================================================================
;; Conventional persisted User reads
;; =============================================================================

(defn- load-user
  [ctx user-id]
  (model/load-by-id
   user.schema/user-descriptor
   ctx
   (require-user-id! user-id)))

(defn- require-user!
  [ctx user-id]
  (or
   (load-user ctx user-id)
   (fail!
    :user/not-found
    "The User does not exist."
    {:user/id user-id})))

;; =============================================================================
;; Guarded User dependencies
;; =============================================================================

(defn user-dependency
  "Returns the current User and a guard-only transaction fragment.

   Use this across model boundaries when a decision depends on current User
   state. The guard keeps the exact User version used by the decision current
   until the composed transaction commits.

   Returns nil when the User does not exist."
  [ctx user-id]
  (when-let [user-document
             (load-user ctx user-id)]
    {:user
     user-document

     :transaction-fragment
     (model.tx/guards-fragment
      (command/guard
       user/entity-type
       user-document
       user/version))}))

(defn require-user-dependency
  "Returns user-dependency or throws when the User does not exist."
  [ctx user-id]
  (or
   (user-dependency ctx user-id)
   (fail!
    :user/not-found
    "The User does not exist."
    {:user/id user-id})))

;; =============================================================================
;; User persistence invariants
;; =============================================================================

(defn- newly-set-value?
  [model-command key]
  (let [before
        (command/before model-command)

        after
        (command/after model-command)

        before-value
        (get before key)

        after-value
        (get after key)]
    (and
     (some? after-value)
     (not= before-value after-value))))

(defn- uniqueness-assertions
  "Derives User-global contact uniqueness from command before/after values.

   This intentionally does not enumerate operation names. Any present or future
   User command that introduces a different phone or email receives the same
   persistence invariant automatically."
  [model-command]
  (let [after
        (command/after model-command)]
    (cond-> []
      (newly-set-value? model-command :user/phone)
      (conj
       (model.tx/assert-none
        user/entity-type
        [:= :user/phone (:user/phone after)]))

      (newly-set-value? model-command :user/email)
      (conj
       (model.tx/assert-none
        user/entity-type
        [:= :user/email (:user/email after)])))))

;; =============================================================================
;; Semantic User changes
;; =============================================================================

(defn- user-change
  [model-command]
  (let [document
        (command/after model-command)]
    {:topic
     :user

     :id
     (:xt/id document)

     :change/kind
     (if
      (command/create? model-command)
       :created
       :updated)

     :user/operation
     (command/operation model-command)

     :user/id
     (:xt/id document)

     :user/status
     (:user/status document)

     :user/revision
     (:user/revision document)}))

(defn- change-entry
  [{:keys [topic id]}]
  {:coalesce-key
   [topic id]})

(def ^:private transaction-options
  {:entry-fn change-entry})

;; =============================================================================
;; Generic User command planning
;; =============================================================================

(defn- mutation-fragment
  [model-command]
  (model.tx/fragment
   {:commands
    [model-command]

    :assertions
    (uniqueness-assertions model-command)

    :changes
    [(user-change model-command)]}))

(defn- planned
  [model-command]
  {:result
   {:user
    (command/after model-command)}

   :transaction-fragment
   (mutation-fragment model-command)

   :transaction-options
   transaction-options})

(defn- plan-update
  [ctx command-fn {:keys [user-id] :as input}]
  (let [user-document
        (require-user! ctx user-id)

        command-input
        (-> input
            (dissoc :user-id)
            (assoc :now (now! ctx)))

        model-command
        (command-fn
         user-document
         command-input)]
    (planned model-command)))

;; =============================================================================
;; Creation
;; =============================================================================

(defn plan-create-user
  "Plans creation of one global User identity.

   Phone/email remain optional, but any supplied contact facts are protected by
   atomic User-global uniqueness assertions."
  [ctx input]
  (planned
   (user/create-user-command
    (assoc
     input
     :id (generated-id ctx)
     :now (now! ctx)))))

;; =============================================================================
;; Profile and contact planning
;; =============================================================================

(defn plan-edit-profile
  [ctx input]
  (plan-update
   ctx
   user/edit-profile-command
   input))

(defn plan-replace-phone
  [ctx input]
  (plan-update
   ctx
   user/replace-phone-command
   input))

(defn plan-replace-email
  [ctx input]
  (plan-update
   ctx
   user/replace-email-command
   input))

(defn plan-remove-phone
  [ctx input]
  (plan-update
   ctx
   user/remove-phone-command
   input))

(defn plan-remove-email
  [ctx input]
  (plan-update
   ctx
   user/remove-email-command
   input))

(defn plan-verify-phone
  [ctx input]
  (plan-update
   ctx
   user/verify-phone-command
   input))

(defn plan-verify-email
  [ctx input]
  (plan-update
   ctx
   user/verify-email-command
   input))

;; =============================================================================
;; Lifecycle planning
;; =============================================================================

(defn plan-suspend-user
  [ctx input]
  (plan-update
   ctx
   user/suspend-user-command
   input))

(defn plan-reactivate-user
  [ctx input]
  (plan-update
   ctx
   user/reactivate-user-command
   input))

(defn plan-delete-user
  [ctx input]
  (plan-update
   ctx
   user/delete-user-command
   input))
