(ns net.humanhelp.site.model.user.fx
  "User-specific read dependencies and transaction planning.

   User FX performs the effectful reads and persistence-level checks needed
   around canonical User domain commands.

   It owns:

   - current User read dependencies for cross-model atomic composition;
   - generated User IDs for creation;
   - phone/email uniqueness assertions;
   - semantic Gesso Live User changes;
   - composable transaction plans for User mutations.

   It does not authorize Organization actions, manage Memberships or
   Invitations, or commit transactions. Callers compose returned fragments and
   commit through gesso.model.tx."
  (:require
   [gesso.fx :as fx]
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
   (uuid?
    user-id)
    (fail!
     :user/invalid-user-id
     "User ID must be a UUID."
     {:user/id user-id}))
  user-id)

(defn- load-user
  [ctx user-id]
  (model/load-by-id
   user.schema/user-descriptor
   ctx
   (require-user-id!
    user-id)))

(defn- require-user-document!
  [ctx user-id]
  (or
   (load-user
    ctx
    user-id)

   (fail!
    :user/not-found
    "The User does not exist."
    {:user/id user-id})))

;; =============================================================================
;; Guarded User dependencies
;; =============================================================================

(defn user-dependency
  "Returns the current User and the guard-only transaction fragment that keeps
   the exact User version used by a cross-model decision current until commit.

   Returns nil when the User does not exist."
  [ctx user-id]
  (when-let [user-document
             (load-user
              ctx
              user-id)]
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
   (user-dependency
    ctx
    user-id)

   (fail!
    :user/not-found
    "The User does not exist."
    {:user/id user-id})))

;; =============================================================================
;; Persistence-level uniqueness
;; =============================================================================

(defn- changed-to-value?
  [before after key]
  (let [before-value
        (get
         before
         key)

        after-value
        (get
         after
         key)]
    (and
     (some?
      after-value)

     (not=
      before-value
      after-value))))

(defn- uniqueness-assertions
  "Returns uniqueness assertions implied by the command's resulting contacts.

   This is intentionally derived from before/after values rather than from an
   operation-name list. Any future User command that introduces a new phone or
   email automatically receives the same persistence invariant."
  [model-command]
  (let [before
        (command/before
         model-command)

        after
        (command/after
         model-command)]
    (cond-> []
      (changed-to-value?
       before
       after
       :user/phone)
      (conj
       (model.tx/assert-none
        user/entity-type
        [:=
         :user/phone
         (:user/phone after)]))

      (changed-to-value?
       before
       after
       :user/email)
      (conj
       (model.tx/assert-none
        user/entity-type
        [:=
         :user/email
         (:user/email after)])))))

;; =============================================================================
;; Semantic changes
;; =============================================================================

(defn- user-change
  [model-command]
  (let [document
        (command/after
         model-command)]
    {:topic
     :user

     :id
     (:xt/id document)

     :change/kind
     (if
      (command/create?
       model-command)
       :created
       :updated)

     :user/operation
     (command/operation
      model-command)

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

(def transaction-options
  "Transaction-wide Live dispatch options to apply after any cross-model
   fragment composition."
  {:entry-fn
   change-entry})

;; =============================================================================
;; Plan construction
;; =============================================================================

(defn- planned
  [model-command]
  {:result
   {:user
    (command/after
     model-command)}

   :transaction-fragment
   (model.tx/fragment
    {:commands
     [model-command]

     :assertions
     (uniqueness-assertions
      model-command)

     :changes
     [(user-change
       model-command)]})

   :transaction-options
   transaction-options})

;; =============================================================================
;; Domain command dispatch
;; =============================================================================

(def ^:private update-command-fns
  {:edit-profile
   #'user/edit-profile-command

   :replace-phone
   #'user/replace-phone-command

   :replace-email
   #'user/replace-email-command

   :remove-phone
   #'user/remove-phone-command

   :remove-email
   #'user/remove-email-command

   :verify-phone
   #'user/verify-phone-command

   :verify-email
   #'user/verify-email-command

   :suspend
   #'user/suspend-user-command

   :reactivate
   #'user/reactivate-user-command

   :delete
   #'user/delete-user-command})

(defn- update-command!
  [operation user-document input]
  (if-let [command-fn
           (get
            update-command-fns
            operation)]
    (command-fn
     user-document
     input)

    (fail!
     :user.fx/unsupported-update
     "The requested User update is not supported."
     {:operation operation})))

(defn- plan-update
  [ctx operation {:keys [user-id] :as input}]
  (let [user-document
        (require-user-document!
         ctx
         user-id)

        model-command
        (update-command!
         operation
         user-document
         (-> input
             (dissoc
              :user-id)
             (assoc
              :now
              (now! ctx))))]
    (planned
     model-command)))

;; =============================================================================
;; Creation
;; =============================================================================

(defn plan-create-user
  "Plans creation of one global User identity.

   Phone/email are optional User facts, but any supplied values are protected
   by atomic uniqueness assertions."
  [ctx input]
  (planned
   (user/create-user-command
    (assoc
     input
     :id
     (generated-id ctx)
     :now
     (now! ctx)))))

;; =============================================================================
;; Profile and contact planning
;; =============================================================================

(defn plan-edit-profile
  [ctx input]
  (plan-update
   ctx
   :edit-profile
   input))

(defn plan-replace-phone
  [ctx input]
  (plan-update
   ctx
   :replace-phone
   input))

(defn plan-replace-email
  [ctx input]
  (plan-update
   ctx
   :replace-email
   input))

(defn plan-remove-phone
  [ctx input]
  (plan-update
   ctx
   :remove-phone
   input))

(defn plan-remove-email
  [ctx input]
  (plan-update
   ctx
   :remove-email
   input))

(defn plan-verify-phone
  [ctx input]
  (plan-update
   ctx
   :verify-phone
   input))

(defn plan-verify-email
  [ctx input]
  (plan-update
   ctx
   :verify-email
   input))

;; =============================================================================
;; Lifecycle planning
;; =============================================================================

(defn plan-suspend-user
  [ctx input]
  (plan-update
   ctx
   :suspend
   input))

(defn plan-reactivate-user
  [ctx input]
  (plan-update
   ctx
   :reactivate
   input))

(defn plan-delete-user
  [ctx input]
  (plan-update
   ctx
   :delete
   input))
