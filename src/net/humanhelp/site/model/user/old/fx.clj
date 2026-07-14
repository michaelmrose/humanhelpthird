(ns net.humanhelp.site.model.user.fx
  "Effectful workflows for the HumanHelp user model.

   These machines coordinate:

   - Graph reads
   - pure domain validation and transitions
   - model command construction
   - persistence through an injected commit implementation

   This namespace intentionally does not implement actor authorization.
   Organization and location authorization facts are not complete yet. Route or
   service code must authorize the actor before invoking these low-level model
   commands.

   Commits are never performed with an unconditional built-in put. The
   application must install ::commit-command! so uniqueness constraints,
   expected-version checks, persistence, and Gesso Live publication can be
   performed atomically.

   Gesso FX injects Instant timestamps. Existing user documents use
   ZonedDateTime, so this namespace converts injected times using ::zone-id,
   defaulting to UTC."
  (:require
   [gesso.fx :as fx]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.capability :as capability]
   [net.humanhelp.site.model.user.graph :as user.graph]
   [net.humanhelp.site.model.user.identity :as identity]
   [net.humanhelp.site.model.user.invitation :as invitation]
   [net.humanhelp.site.model.user.membership :as membership]
   [net.humanhelp.site.model.user.role :as role])
  (:import
   [java.time Instant ZoneId ZoneOffset ZonedDateTime]))

;; =============================================================================
;; Effect contract
;; =============================================================================

(def commit-effect
  ::commit)

(def commit-command-key
  "Required ctx key containing:

     (fn [ctx command] commit-result)

   The function must enforce relevant uniqueness constraints and atomically
   compare :model/expected for updates.

   It should also publish the resulting model change only after persistence
   succeeds.

   Supported results:

     {:ok? true
      :value backend-result}

     {:ok? false
      :error error-key
      :errors optional-field-errors}

   A non-result value is treated as a successful backend value."
  ::commit-command!)

(def zone-id-key
  "Optional ctx key containing a java.time.ZoneId or zone-name string.

   Defaults to UTC."
  ::zone-id)

(defn- normalize-commit-result
  [result]
  (if
   (and
    (map? result)
    (contains? result :ok?))

    result

    {:ok? true
     :value result}))

(defn- handle-commit
  [ctx command]
  (if-some [commit-command!
            (get ctx commit-command-key)]

    (normalize-commit-result
     (commit-command! ctx command))

    (throw
     (ex-info
      "User FX requires an atomic model command commit implementation."
      {:error/type
       :user.fx/missing-commit-command

       :expected-ctx-key
       commit-command-key

       :command
       command}))))

(def handlers
  {commit-effect
   handle-commit})

;; =============================================================================
;; FX time
;; =============================================================================

(defn- zone-id
  [ctx]
  (let [value
        (get ctx
             zone-id-key
             ZoneOffset/UTC)]
    (cond
      (instance? ZoneId value)
      value

      (string? value)
      (ZoneId/of value)

      :else
      (throw
       (ex-info
        "Invalid HumanHelp user-model time zone."
        {:error/type
         :user.fx/invalid-zone-id

         :value
         value})))))

(defn- fx-now->zdt
  [{:biff.fx/keys [now]
    :as ctx}]
  (when-not
   (instance? Instant now)
    (throw
     (ex-info
      "Gesso FX did not supply a valid Instant."
      {:error/type
       :user.fx/invalid-now

       :value
       now})))

  (ZonedDateTime/ofInstant
   now
   (zone-id ctx)))

;; =============================================================================
;; Shared machine state
;; =============================================================================

(defn- not-found-result
  [error]
  {:biff.fx/return
   {:ok? false
    :error error}})

(defn- prepare-create
  [entity-type result-key document]
  {:user-model/after
   document

   :user-model/result-key
   result-key

   :user-model/command
   (model.common/create-command
    entity-type
    document)

   :biff.fx/next
   :commit})

(defn- prepare-update
  [{:keys
    [entity-type
     operation
     expected-fn
     result-key]}
   before
   result]
  (if-not
   (:ok? result)

    {:biff.fx/return
     result}

    (let [after
          (get result result-key)]
      {:user-model/after
       after

       :user-model/result-key
       result-key

       :user-model/command
       (model.common/update-command
        entity-type
        operation
        (expected-fn before)
        before
        after)

       :biff.fx/next
       :commit})))

(defn- commit-state
  [{:user-model/keys
    [after
     result-key
     command]}]
  {:user-model/after
   after

   :user-model/result-key
   result-key

   :user-model/commit-result
   [commit-effect command]

   :biff.fx/next
   :finish})

(defn- finish-state
  [{:user-model/keys
    [after
     result-key
     commit-result]}]
  (if
   (:ok? commit-result)

    {:biff.fx/return
     {:ok? true
      result-key after
      :commit-result
      (:value commit-result)}}

    {:biff.fx/return
     (cond->
      {:ok? false
       :error
       (or
        (:error commit-result)
        :model/commit-failed)}

       (:errors commit-result)
       (assoc
        :errors
        (:errors commit-result))

       (contains? commit-result :value)
       (assoc
        :commit-result
        (:value commit-result)))}))

;; =============================================================================
;; Generic machine constructors
;; =============================================================================

(defn- create-machine
  [machine-name
   {:keys
    [entity-type
     result-key
     input-key
     prepare-input
     input-errors
     new-document]}]
  (fx/machine
   machine-name

   :start
   (fn
     [{:biff.fx/keys [seed now]
       :as ctx}]
     (let [[id _next-seed]
           (fx/uuid7 seed now)

           created-at
           (fx-now->zdt ctx)

           input
           (prepare-input
            ctx
            id
            created-at)]
       {:user-model/input
        input

        :biff.fx/next
        :validate}))

   :validate
   (fn
     [{:user-model/keys [input]}]
     (let [errors
           (input-errors input)]
       (if
        (seq errors)

         {:biff.fx/return
          {:ok? false
           :error
           :user-model/invalid-input

           :errors
           errors}}

         {:user-model/input
          input

          :biff.fx/next
          :build})))

   :build
   (fn
     [{:user-model/keys [input]}]
     (prepare-create
      entity-type
      result-key
      (new-document input)))

   :commit
   commit-state

   :finish
   finish-state))

(defn- update-machine
  [machine-name
   {:keys
    [entity-type
     result-key
     operation
     query-input
     query
     found-key
     document-key
     not-found-error
     expected-fn
     apply-update]}]
  (fx/machine
   machine-name

   :start
   (fn
     [ctx]
     {:user-model/facts
      [:biff.graph.fx/query
       (query-input ctx)
       query]

      :biff.fx/next
      :apply})

   :apply
   (fn
     [{:user-model/keys [facts]
       :as ctx}]
     (if-not
      (get facts found-key)

       (not-found-result
        not-found-error)

       (let [before
             (get facts document-key)

             now
             (fx-now->zdt ctx)

             result
             (apply-update
              before
              ctx
              now)]
         (prepare-update
          {:entity-type
           entity-type

           :operation
           operation

           :expected-fn
           expected-fn

           :result-key
           result-key}

          before
          result))))

   :commit
   commit-state

   :finish
   finish-state))

;; =============================================================================
;; Create-input preparation
;; =============================================================================

(defn- prepare-create-input
  [ctx input-key id now]
  (assoc
   (or
    (get ctx input-key)
    {})

   :id
   id

   :now
   now))

;; =============================================================================
;; Graph lookup inputs
;; =============================================================================

(defn- user-query-input
  [ctx]
  (user.graph/user-query-input
   {:user-id
    (:user/id ctx)}))

(defn- membership-query-input
  [ctx]
  (user.graph/membership-query-input
   {:membership-id
    (:membership/id ctx)}))

(defn- role-assignment-query-input
  [ctx]
  (user.graph/role-assignment-query-input
   {:role-assignment-id
    (:role-assignment/id ctx)}))

(defn- invitation-query-input
  [ctx]
  (user.graph/invitation-query-input
   {:invitation-id
    (:invitation/id ctx)

    :token-hash
    (:invitation/token-hash ctx)}))

(defn- request-capability-query-input
  [ctx]
  (user.graph/request-capability-query-input
   {:request-capability-id
    (:request-capability/id ctx)

    :token-hash
    (:request-capability/token-hash ctx)}))

;; =============================================================================
;; User creation
;; =============================================================================

(def create-user
  (create-machine
   ::create-user
   {:entity-type
    identity/entity-type

    :result-key
    :user

    :input-key
    :user/create-input

    :prepare-input
    (fn
      [ctx id now]
      (prepare-create-input
       ctx
       :user/create-input
       id
       now))

    :input-errors
    identity/create-input-errors

    :new-document
    identity/new-user}))

(def create-verified-phone-user
  (create-machine
   ::create-verified-phone-user
   {:entity-type
    identity/entity-type

    :result-key
    :user

    :input-key
    :user/create-input

    :prepare-input
    (fn
      [ctx id now]
      (assoc
       (prepare-create-input
        ctx
        :user/create-input
        id
        now)

       :phone-verified-at
       now))

    :input-errors
    identity/create-input-errors

    :new-document
    identity/new-user}))

;; =============================================================================
;; User updates
;; =============================================================================

(def edit-user-profile
  (update-machine
   ::edit-user-profile
   {:entity-type
    identity/entity-type

    :result-key
    :user

    :operation
    :edit-profile

    :query-input
    user-query-input

    :query
    user.graph/user-summary-query

    :found-key
    :user/found?

    :document-key
    :user/doc

    :not-found-error
    :user/not-found

    :expected-fn
    identity/expected-version

    :apply-update
    (fn
      [before ctx now]
      (identity/edit-profile-doc
       before
       (:user/profile-input ctx)
       now))}))

(defn- user-transition-machine
  [machine-name action]
  (update-machine
   machine-name
   {:entity-type
    identity/entity-type

    :result-key
    :user

    :operation
    action

    :query-input
    user-query-input

    :query
    user.graph/user-summary-query

    :found-key
    :user/found?

    :document-key
    :user/doc

    :not-found-error
    :user/not-found

    :expected-fn
    identity/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (identity/transition-user
       before
       action
       now))}))

(def suspend-user
  (user-transition-machine
   ::suspend-user
   :suspend))

(def reactivate-user
  (user-transition-machine
   ::reactivate-user
   :reactivate))

(def delete-user
  (user-transition-machine
   ::delete-user
   :delete))

;; =============================================================================
;; Membership creation
;; =============================================================================

(def create-membership
  (create-machine
   ::create-membership
   {:entity-type
    membership/entity-type

    :result-key
    :membership

    :input-key
    :membership/create-input

    :prepare-input
    (fn
      [ctx id now]
      (prepare-create-input
       ctx
       :membership/create-input
       id
       now))

    :input-errors
    membership/create-input-errors

    :new-document
    membership/new-membership}))

;; =============================================================================
;; Membership lifecycle
;; =============================================================================

(defn- membership-transition-machine
  [machine-name action]
  (update-machine
   machine-name
   {:entity-type
    membership/entity-type

    :result-key
    :membership

    :operation
    action

    :query-input
    membership-query-input

    :query
    user.graph/membership-command-query

    :found-key
    :membership/found?

    :document-key
    :membership/doc

    :not-found-error
    :membership/not-found

    :expected-fn
    membership/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (membership/transition-membership
       before
       action
       now))}))

(def suspend-membership
  (membership-transition-machine
   ::suspend-membership
   :suspend))

(def reactivate-membership
  (membership-transition-machine
   ::reactivate-membership
   :reactivate))

(def revoke-membership
  (membership-transition-machine
   ::revoke-membership
   :revoke))

;; =============================================================================
;; Role-assignment creation
;; =============================================================================

(def create-role-assignment
  (create-machine
   ::create-role-assignment
   {:entity-type
    role/entity-type

    :result-key
    :role-assignment

    :input-key
    :role-assignment/create-input

    :prepare-input
    (fn
      [ctx id now]
      (prepare-create-input
       ctx
       :role-assignment/create-input
       id
       now))

    :input-errors
    role/create-input-errors

    :new-document
    role/new-role-assignment}))

;; =============================================================================
;; Role-assignment lifecycle
;; =============================================================================

(def revoke-role-assignment
  (update-machine
   ::revoke-role-assignment
   {:entity-type
    role/entity-type

    :result-key
    :role-assignment

    :operation
    :revoke

    :query-input
    role-assignment-query-input

    :query
    user.graph/role-assignment-command-query

    :found-key
    :role-assignment/found?

    :document-key
    :role-assignment/doc

    :not-found-error
    :role-assignment/not-found

    :expected-fn
    role/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (role/revoke-role-assignment-doc
       before
       now))}))

;; =============================================================================
;; Invitation creation
;; =============================================================================

(def create-invitation
  (create-machine
   ::create-invitation
   {:entity-type
    invitation/entity-type

    :result-key
    :invitation

    :input-key
    :invitation/create-input

    :prepare-input
    (fn
      [ctx id now]
      (prepare-create-input
       ctx
       :invitation/create-input
       id
       now))

    :input-errors
    invitation/create-input-errors

    :new-document
    invitation/new-invitation}))

;; =============================================================================
;; Invitation lifecycle
;; =============================================================================

(def accept-invitation
  (update-machine
   ::accept-invitation
   {:entity-type
    invitation/entity-type

    :result-key
    :invitation

    :operation
    :accept

    :query-input
    invitation-query-input

    :query
    user.graph/invitation-command-query

    :found-key
    :invitation/found?

    :document-key
    :invitation/doc

    :not-found-error
    :invitation/not-found

    :expected-fn
    invitation/expected-version

    :apply-update
    (fn
      [before ctx now]
      (invitation/accept-invitation-doc
       before
       (:current-user/id ctx)
       now))}))

(def revoke-invitation
  (update-machine
   ::revoke-invitation
   {:entity-type
    invitation/entity-type

    :result-key
    :invitation

    :operation
    :revoke

    :query-input
    invitation-query-input

    :query
    user.graph/invitation-command-query

    :found-key
    :invitation/found?

    :document-key
    :invitation/doc

    :not-found-error
    :invitation/not-found

    :expected-fn
    invitation/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (invitation/revoke-invitation-doc
       before
       now))}))

(def expire-invitation
  (update-machine
   ::expire-invitation
   {:entity-type
    invitation/entity-type

    :result-key
    :invitation

    :operation
    :expire

    :query-input
    invitation-query-input

    :query
    user.graph/invitation-command-query

    :found-key
    :invitation/found?

    :document-key
    :invitation/doc

    :not-found-error
    :invitation/not-found

    :expected-fn
    invitation/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (invitation/expire-invitation-doc
       before
       now))}))

;; =============================================================================
;; Request-capability creation
;; =============================================================================

(def create-request-capability
  (create-machine
   ::create-request-capability
   {:entity-type
    capability/entity-type

    :result-key
    :request-capability

    :input-key
    :request-capability/create-input

    :prepare-input
    (fn
      [ctx id now]
      (prepare-create-input
       ctx
       :request-capability/create-input
       id
       now))

    :input-errors
    capability/create-input-errors

    :new-document
    capability/new-capability}))

;; =============================================================================
;; Request-capability updates
;; =============================================================================

(def record-request-capability-use
  (update-machine
   ::record-request-capability-use
   {:entity-type
    capability/entity-type

    :result-key
    :request-capability

    :operation
    :record-use

    :query-input
    request-capability-query-input

    :query
    user.graph/request-capability-command-query

    :found-key
    :request-capability/found?

    :document-key
    :request-capability/doc

    :not-found-error
    :request-capability/not-found

    :expected-fn
    capability/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (capability/record-use-doc
       before
       now))}))

(def revoke-request-capability
  (update-machine
   ::revoke-request-capability
   {:entity-type
    capability/entity-type

    :result-key
    :request-capability

    :operation
    :revoke

    :query-input
    request-capability-query-input

    :query
    user.graph/request-capability-command-query

    :found-key
    :request-capability/found?

    :document-key
    :request-capability/doc

    :not-found-error
    :request-capability/not-found

    :expected-fn
    capability/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (capability/revoke-capability-doc
       before
       now))}))

(def expire-request-capability
  (update-machine
   ::expire-request-capability
   {:entity-type
    capability/entity-type

    :result-key
    :request-capability

    :operation
    :expire

    :query-input
    request-capability-query-input

    :query
    user.graph/request-capability-command-query

    :found-key
    :request-capability/found?

    :document-key
    :request-capability/doc

    :not-found-error
    :request-capability/not-found

    :expected-fn
    capability/expected-version

    :apply-update
    (fn
      [before _ctx now]
      (capability/expire-capability-doc
       before
       now))}))
