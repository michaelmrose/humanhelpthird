(ns net.humanhelp.site.model.request.fx
  "Effectful Request workflows.

   Request FX coordinates the pure Request domain with the public Organization
   and User model interfaces.

   Organization supplies one authoritative Location hierarchy context and the
   authorization-version guards for every Organization document that established
   it. User supplies current identity, Membership, and role facts. Request FX
   rechecks operation policy, coordinates Request Assignment records when
   helper participation changes, and delegates the final atomic transaction to
   the shared model.fx effect.

   Request documents own lifecycle. Request Assignment documents own the
   primary helper and collaborators. Capability-authenticated Request writes
   remain intentionally unsupported."
  (:require
   [gesso.fx :as fx]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.assignment :as assignment]
   [net.humanhelp.site.model.request.domain :as request]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Supported workflow policy
;; =============================================================================

(def operational-location-operations
  "Operations that require the Request Location hierarchy to remain
   operational at commit time."
  #{:create
    :edit
    :claim
    :mark-on-the-way
    :complete
    :add-collaborator
    :reassign})

(def owner-operations
  "Operations reserved for the authenticated User requestor."
  #{:edit
    :cancel})

(def effective-helper-operations
  "Operations requiring the active primary helper to still have effective
   helper authority at the Location. Claim establishes the primary assignment."
  #{:mark-on-the-way
    :complete
    :add-collaborator})

(def primary-helper-operations
  "Operations requiring active primary-assignment ownership but not a still
   effective helper role."
  #{:unclaim
    :remove-collaborator})

;; =============================================================================
;; Errors and machine plumbing
;; =============================================================================

(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info
     message
     (cond->
      {:error/type error-type}

       (some? details)
       (assoc :error/details details))))))

(defn- require-uuid!
  [value error-type message details]
  (when-not
   (uuid? value)
    (fail!
     error-type
     message
     details))
  value)

(defn- require-operation!
  [operation]
  (when-not
   (request/operation? operation)
    (fail!
     :request.fx/unsupported-operation
     "The requested Request operation is not supported."
     {:operation operation}))
  operation)

(defn- require-authenticated-user-id!
  [ctx]
  (require-uuid!
   (:current-user/id ctx)
   :request/not-authenticated
   "A signed-in User is required."
   {:current-user/id
    (:current-user/id ctx)}))

(defn- command-document
  [command]
  (model.common/command-document
   command))

(defn- commit-state
  [{:request.fx/keys
    [result
     transaction-plan]}]
  {:request.fx/result
   result

   :request.fx/transaction
   [model.fx/transact-effect
    transaction-plan]

   :biff.fx/next
   :finish})

(defn- finish-state
  [{:request.fx/keys
    [result
     transaction]}]
  {:biff.fx/return
   (assoc
    result
    :transaction
    transaction)})

;; =============================================================================
;; Cross-model version metadata
;; =============================================================================

(def user-version
  {:revision-key :user/revision
   :created-at-key :user/created-at
   :updated-at-key :user/updated-at})

(def membership-version
  {:revision-key :membership/revision
   :created-at-key :membership/created-at
   :updated-at-key :membership/updated-at})

(def role-assignment-version
  {:revision-key :role-assignment/revision
   :created-at-key :role-assignment/created-at
   :updated-at-key :role-assignment/updated-at})

(defn- document-authorization-version
  [entity-type version document]
  (model.common/authorization-version
   entity-type
   document
   version))

;; =============================================================================
;; Organization Location authorization
;; =============================================================================

(defn- require-location-authorization-versions!
  [authorization-versions location-id]
  (when-not
   (sequential? authorization-versions)
    (fail!
     :request.fx/invalid-location-authorization-versions
     "Organization Location authorization versions must be sequential."
     {:location/id location-id
      :authorization-versions authorization-versions}))

  (let [authorization-versions
        (vec authorization-versions)

        targets
        (mapv
         model.fx/authorization-version-target
         authorization-versions)

        location-target
        [organization/location-entity-type
         location-id]]

    (when
     (empty? authorization-versions)
      (fail!
       :request.fx/missing-location-authorization-versions
       "Organization must supply authorization-version guards for the Request Location."
       {:location/id location-id}))

    (when-not
     (some
      #(= location-target %)
      targets)
      (fail!
       :request.fx/missing-location-version
       "Organization authorization guards must include the Request Location document."
       {:location/id location-id
        :authorization-targets targets}))

    authorization-versions))

(defn- require-location-authorization!
  "Loads and validates Organization's stable Location authorization contract.

   This intentionally consumes organization/location-context rather than
   rebuilding hierarchy facts from separate reads. Its scope context,
   operational decision, and authorization-version guards therefore describe
   one coherent Organization Graph result."
  [ctx organization-id location-id require-operational?]
  (require-uuid!
   organization-id
   :request/invalid-organization
   "A Request requires an Organization UUID."
   {:organization/id organization-id})

  (require-uuid!
   location-id
   :request/invalid-location
   "A Request requires a Location UUID."
   {:location/id location-id})

  (let [facts
        (organization/location-context
         ctx
         {:organization-id organization-id
          :location-id location-id})]

    (when-not
     (true?
      (:location/found? facts))
      (fail!
       :location/not-found
       "The Request Location no longer exists."
       {:organization/id organization-id
        :location/id location-id}))

    (let [location
          (:location/doc facts)

          scope-context
          (:location/scope-context facts)

          authorization-versions
          (require-location-authorization-versions!
           (:location/authorization-versions facts)
           location-id)

          expected-scope
          (organization/location-scope
           location-id)]

      (when-not
       (organization/location-document?
        location)
        (fail!
         :request.fx/invalid-location-document
         "Organization returned an invalid Location document."
         {:organization/id organization-id
          :location/id location-id
          :location location}))

      (when-not
       (=
        location-id
        (organization/location-id location))
        (fail!
         :request.fx/inconsistent-location-context
         "Organization returned a different Location document."
         {:expected-location-id location-id
          :actual-location-id
          (organization/location-id location)}))

      (when-not
       (=
        organization-id
        (organization/location-organization-id location))
        (fail!
         :location/organization-mismatch
         "The Request Location belongs to another Organization."
         {:expected-organization-id organization-id
          :actual-organization-id
          (organization/location-organization-id location)
          :location/id location-id}))

      (when-not
       (organization/scope-context?
        scope-context)
        (fail!
         :request.fx/invalid-location-context
         "Organization returned an invalid Location scope context."
         {:organization/id organization-id
          :location/id location-id
          :scope-context scope-context}))

      (when-not
       (=
        organization-id
        (:organization/id scope-context))
        (fail!
         :request.fx/inconsistent-location-context
         "The Location scope context belongs to another Organization."
         {:expected-organization-id organization-id
          :actual-organization-id
          (:organization/id scope-context)
          :location/id location-id}))

      (when-not
       (organization/same-scope?
        expected-scope
        (:scope/target scope-context))
        (fail!
         :request.fx/inconsistent-location-context
         "Organization returned a scope context for another Location."
         {:expected-scope expected-scope
          :actual-scope
          (:scope/target scope-context)}))

      (when
       (and
        require-operational?
        (not
         (true?
          (:scope/operational? scope-context))))
        (fail!
         :location/not-operational
         "The Request Location is not operational."
         {:organization/id organization-id
          :location/id location-id}))

      {:location location
       :scope-context scope-context
       :authorization-versions authorization-versions})))

;; =============================================================================
;; User authorization evidence
;; =============================================================================

(defn- require-active-user-document!
  [ctx user-id]
  (let [document
        (user/require-user
         ctx
         {:user-id user-id})]
    (when-not
     (user/user-active?
      document)
      (fail!
       :user/not-active
       "Only an active User may change a Request."
       {:user/id user-id}))
    document))

(defn- active-user-authorization
  [ctx user-id]
  (let [document
        (require-active-user-document!
         ctx
         user-id)]
    {:user/id user-id
     :user/document document
     :authorization-versions
     [(document-authorization-version
       user/user-entity-type
       user-version
       document)]}))

(defn- distinct-documents-by-id
  [documents]
  (reduce
   (fn [result document]
     (if
      (some
       #(=
         (:xt/id %)
         (:xt/id document))
       result)
       result
       (conj
        result
        document)))
   []
   documents))

(defn- role-documents-at-scopes
  [ctx organization-id scopes]
  (distinct-documents-by-id
   (mapcat
    (fn [scope]
      (user/active-role-assignment-documents-at-scope
       ctx
       organization-id
       scope))
    scopes)))

(defn- role-authorization
  "Reloads and guards the User, Membership, and one effective role assignment
   that establishes one of expected-roles at scope-context."
  [ctx user-id scope-context expected-roles]
  (let [organization-id (:organization/id scope-context)
        scopes (vec (:scope/applicable scope-context))
        public-access
        (user/access-context
         ctx
         {:user-id user-id
          :scope-context scope-context})
        membership-id
        (or
         (:membership/id public-access)
         (fail!
          :request.fx/incomplete-user-authorization
          "User access did not identify an active Membership."
          {:user/id user-id
           :organization/id organization-id}))
        user-document
        (require-active-user-document! ctx user-id)
        membership-document
        (user/require-membership ctx membership-id)
        assignments
        (role-documents-at-scopes ctx organization-id scopes)
        role-assignment
        (some
         #(user/effective-assignment-for-role
           user-document
           membership-document
           assignments
           scopes
           %)
         expected-roles)]
    (when-not role-assignment
      (fail!
       :user/not-authorized
       "The User does not have the required role at this Location."
       {:user/id user-id
        :organization/id organization-id
        :scope/target (:scope/target scope-context)
        :required-roles (set expected-roles)}))
    {:user/id user-id
     :organization/id organization-id
     :scope/target (:scope/target scope-context)
     :user/document user-document
     :membership/document membership-document
     :role-assignment/document role-assignment
     :authorization-versions
     [(document-authorization-version
       user/user-entity-type user-version user-document)
      (document-authorization-version
       user/membership-entity-type membership-version membership-document)
      (document-authorization-version
       user/role-assignment-entity-type
       role-assignment-version
       role-assignment)]}))

(defn- helper-authorization
  [ctx user-id scope-context]
  (role-authorization ctx user-id scope-context [:helper]))

(defn- manager-authorization
  [ctx user-id scope-context]
  (role-authorization ctx user-id scope-context [:admin :supervisor]))

;; =============================================================================
;; Request actor policy
;; =============================================================================

(defn- owner-authorization
  [ctx request-document]
  (let [user-id
        (require-authenticated-user-id!
         ctx)

        requestor
        (request/requestor
         request-document)]

    (when
     (request/capability-requestor?
      requestor)
      (fail!
       :request/capability-authorization-unavailable
       "Capability-owned Request writes are not implemented yet."
       {:request/id
        (request/request-id request-document)
        :requestor requestor}))

    (when-not
     (request/requested-by-user?
      request-document
      user-id)
      (fail!
       :request/not-authorized
       "Only the User who created this Request may perform this action."
       {:request/id
        (request/request-id request-document)
        :user/id user-id}))

    (active-user-authorization
     ctx
     user-id)))

(defn- assignment-documents
  [request-facts]
  (mapv :request-assignment/doc
        (or (:request/assignments request-facts) [])))

(defn- require-active-assignments!
  [request-document request-facts]
  (let [request-id (request/request-id request-document)
        assignments (assignment-documents request-facts)]
    (doseq [assignment-document assignments]
      (assignment/require-document assignment-document)
      (when-not (and (assignment/active? assignment-document)
                     (assignment/for-request? assignment-document request-id))
        (fail!
         :request.fx/invalid-active-assignment
         "Request Graph returned an invalid active Request Assignment."
         {:request/id request-id
          :request-assignment/id
          (assignment/assignment-id assignment-document)})))
    (cond
      (request/lifecycle-expects-primary-assignment? request-document)
      (when-not (assignment/active-primary-assignment assignments)
        (fail!
         :request/missing-primary-assignment
         "The Request lifecycle requires an active primary helper assignment."
         {:request/id request-id
          :request/status (request/status request-document)}))

      (seq assignments)
      (fail!
       :request.fx/assignment-state-mismatch
       "The Request lifecycle does not allow active assignments."
       {:request/id request-id
        :request/status (request/status request-document)
        :request/active-assignment-count (count assignments)}))
    assignments))

(defn- require-primary-assignment!
  [request-document assignments]
  (or (assignment/active-primary-assignment assignments)
      (fail!
       :request/missing-primary-assignment
       "The Request requires an active primary helper assignment."
       {:request/id (request/request-id request-document)
        :request/status (request/status request-document)})))

(defn- require-primary-owned-by!
  [request-document assignments user-id]
  (let [primary (require-primary-assignment! request-document assignments)]
    (when-not (assignment/for-helper? primary user-id)
      (fail!
       :request/not-authorized
       "Only the active primary helper may perform this action."
       {:request/id (request/request-id request-document)
        :user/id user-id
        :request/primary-helper (assignment/helper-id primary)}))
    primary))

(defn- primary-helper-authorization
  [ctx request-document assignments scope-context require-effective-helper?]
  (let [user-id (require-authenticated-user-id! ctx)
        primary (require-primary-owned-by!
                 request-document assignments user-id)
        authorization
        (if require-effective-helper?
          (helper-authorization ctx user-id scope-context)
          (active-user-authorization ctx user-id))]
    (assoc authorization :request/primary-assignment primary)))

(defn- normalize-required-skill!
  [value]
  (when (some? value)
    (let [skill (user/normalize-skill value)]
      (when-not (user/skill? skill)
        (fail!
         :request/invalid-skill
         "The requested helper skill is invalid."
         {:skill value}))
      skill)))

(defn- eligible-helper-authorization
  [ctx helper-id scope-context skill]
  (let [authorization (helper-authorization ctx helper-id scope-context)]
    (when (and skill
               (not (user/membership-has-skill?
                     (:membership/document authorization)
                     skill)))
      (fail!
       :request/helper-missing-skill
       "The selected helper does not have the required Organization skill."
       {:user/id helper-id
        :organization/id (:organization/id scope-context)
        :skill skill}))
    authorization))

(defn- assignment-authorization-version
  [assignment-document]
  (document-authorization-version
   assignment/entity-type
   assignment/version
   assignment-document))

(defn- distinct-authorization-versions
  [authorization-versions]
  (second
   (reduce
    (fn [[seen result] authorization-version]
      (let [target (model.fx/authorization-version-target authorization-version)]
        (if (contains? seen target)
          [seen result]
          [(conj seen target) (conj result authorization-version)])))
    [#{} []]
    authorization-versions)))

(defn- claim-authorization
  [ctx input scope-context]
  (let [actor-id (require-authenticated-user-id! ctx)
        target-helper-id (or (:helper-id input) actor-id)
        skill (normalize-required-skill! (:skill input))]
    (if (= actor-id target-helper-id)
      (let [target-authorization
            (eligible-helper-authorization
             ctx target-helper-id scope-context skill)]
        {:actor-id actor-id
         :target-helper-id target-helper-id
         :required-skill skill
         :authorization-versions
         (:authorization-versions target-authorization)})
      (let [manager
            (manager-authorization ctx actor-id scope-context)
            target-authorization
            (eligible-helper-authorization
             ctx target-helper-id scope-context skill)]
        {:actor-id actor-id
         :target-helper-id target-helper-id
         :required-skill skill
         :authorization-versions
         (distinct-authorization-versions
          (concat
           (:authorization-versions manager)
           (:authorization-versions target-authorization)))}))))

(defn- operation-authorization
  [ctx operation request-document assignments scope-context]
  (cond
    (contains? owner-operations operation)
    (owner-authorization ctx request-document)

    (contains? primary-helper-operations operation)
    (primary-helper-authorization
     ctx request-document assignments scope-context false)

    (contains? effective-helper-operations operation)
    (primary-helper-authorization
     ctx request-document assignments scope-context true)

    :else
    (fail!
     :request.fx/unsupported-operation
     "The Request operation has no authorization policy."
     {:operation operation})))

;; =============================================================================
;; Semantic changes
;; =============================================================================

(defn- without-nils
  [value]
  (into
   {}
   (remove
    (comp nil? val))
   value))

(defn- change-entry
  [{:keys [topic id]}]
  {:coalesce-key
   [topic id]})

(defn- request-change
  [before after operation]
  (without-nils
   {:topic
    :request

    :id
    (request/request-id after)

    :change/kind
    (if before
      :updated
      :created)

    :request/operation
    operation

    :request/id
    (request/request-id after)

    :organization/id
    (request/organization-id after)

    :location/id
    (request/location-id after)

    :request/requestor-type
    (request/requestor-type after)

    :request/requestor-id
    (request/requestor-id after)

    :request/status
    (request/status after)

    :request/previous-status
    (some->
     before
     request/status)

    :request/revision
    (request/revision after)}))

(defn- assignment-change
  [before after operation]
  {:topic :request
   :id (assignment/request-id after)
   :change/kind :updated
   :request/operation operation
   :request/id (assignment/request-id after)
   :request-assignment/id (assignment/assignment-id after)
   :request-assignment/helper (assignment/helper-id after)
   :request-assignment/role (assignment/role after)
   :request-assignment/status (assignment/status after)
   :request-assignment/previous-status (some-> before assignment/status)})

(defn- transaction-plan
  [fragment]
  (assoc
   (model.fx/transaction-fragment
    fragment)
   :entry-fn
   change-entry))

;; =============================================================================
;; Pure transaction planning
;; =============================================================================

(defn- require-command!
  [command expected-operation]
  (when-not
   (map?
    command)
    (fail!
     :request.fx/invalid-command
     "A Request workflow requires a model command."
     {:command command}))

  (when-not
   (=
    request/request-entity-type
    (:model/entity-type command))
    (fail!
     :request.fx/invalid-command
     "The command does not target a Request."
     {:command command}))

  (when-not
   (=
    expected-operation
    (:model/operation command))
    (fail!
     :request.fx/invalid-command
     "The command operation does not match the workflow."
     {:expected-operation expected-operation
      :actual-operation
      (:model/operation command)}))

  command)

(defn plan-create-request
  "Constructs one atomic Request-create plan from an already-authorized domain
   command and the cross-model versions that established authorization."
  [{:keys
    [command
     location-authorization-versions
     actor-authorization-versions]}]
  (require-command!
   command
   :create)

  (let [document
        (command-document
         command)]

    (when-not
     (request/request-document-consistent?
      document)
      (fail!
       :request.fx/invalid-command-document
       "The create command contains an invalid Request."
       {:request document}))

    {:transaction-plan
     (transaction-plan
      (model.fx/compose-transaction-fragments
       {:authorization-versions
        location-authorization-versions}

       {:authorization-versions
        actor-authorization-versions}

       {:commands
        [command]

        :changes
        [(request-change
          nil
          document
          :create)]}))

     :result
     {:request document}}))

(defn plan-update-request
  "Constructs one atomic Request-update plan from the current Request, an
   already-authorized domain command, and all cross-model versions that
   established authorization."
  [{:keys
    [before
     command
     assignment-commands
     assignment-changes
     assertions
     location-authorization-versions
     actor-authorization-versions]}]
  (request/require-request-document
   before)

  (let [operation
        (require-operation!
         (:model/operation command))]

    (when
     (=
      :create
      operation)
      (fail!
       :request.fx/invalid-command
       "An update workflow cannot execute a create command."
       {:command command}))

    (require-command!
     command
     operation)

    (doseq [assignment-command assignment-commands]
      (when-not (= assignment/entity-type
                   (:model/entity-type assignment-command))
        (fail!
         :request.fx/invalid-assignment-command
         "The workflow contains an invalid Request Assignment command."
         {:command assignment-command})))

    (let [after
          (command-document
           command)

          expected-before
          (model.common/expected-version
           before
           request/request-version)]

      (when-not
       (request/request-document-consistent?
        after)
        (fail!
         :request.fx/invalid-command-document
         "The update command contains an invalid Request."
         {:request after}))

      (when-not
       (=
        (request/request-id before)
        (request/request-id after))
        (fail!
         :request.fx/command-target-mismatch
         "The Request command changed its target."
         {:before-id
          (request/request-id before)
          :after-id
          (request/request-id after)}))

      (when-not
       (=
        expected-before
        (:model/expected command))
        (fail!
         :request.fx/stale-command-origin
         "The Request command was not derived from the supplied current Request."
         {:request/id
          (request/request-id before)
          :expected expected-before
          :actual
          (:model/expected command)}))

      {:transaction-plan
       (transaction-plan
        (model.fx/compose-transaction-fragments
         {:authorization-versions
          location-authorization-versions}

         {:authorization-versions
          actor-authorization-versions}

         {:assertions assertions
          :commands
          (into [command] assignment-commands)

          :changes
          (into
           [(request-change before after operation)]
           assignment-changes)}))

       :result
       {:request after}})))

(defn plan-assignment-operation
  "Constructs an assignment-only Request transaction plan."
  [{:keys [request-document operation assignment-commands assignment-changes
           assertions location-authorization-versions
           actor-authorization-versions target-authorization-versions]}]
  (request/require-request-document request-document)

  (when-not (request/assignment-operation? operation)
    (fail!
     :request.fx/invalid-assignment-operation
     "The operation is not a Request Assignment operation."
     {:operation operation}))

  (doseq [assignment-command assignment-commands]
    (when-not (= assignment/entity-type (:model/entity-type assignment-command))
      (fail!
       :request.fx/invalid-assignment-command
       "The workflow contains an invalid Request Assignment command."
       {:command assignment-command})))

  {:transaction-plan
   (transaction-plan
    (model.fx/compose-transaction-fragments
     {:authorization-versions location-authorization-versions}
     {:authorization-versions actor-authorization-versions}
     {:authorization-versions target-authorization-versions}
     {:assertions assertions
      :commands assignment-commands
      :changes assignment-changes}))

   :result
   {:request request-document
    :assignments (mapv command-document assignment-commands)}})

;; =============================================================================
;; Assignment transaction helpers
;; =============================================================================

(defn- assert-no-active-assignment-for-helper
  [request-id helper-id]
  (model.fx/assert-none
   assignment/entity-type
   [:and
    [:= :request-assignment/request request-id]
    [:= :request-assignment/helper helper-id]
    [:= :request-assignment/status :active]]))

(defn- assert-no-active-assignments
  [request-id]
  (model.fx/assert-none
   assignment/entity-type
   [:and
    [:= :request-assignment/request request-id]
    [:= :request-assignment/status :active]]))

(defn- create-assignment-command
  [id request-id helper-id role source actor-id now]
  (assignment/create-command
   {:id id
    :request-id request-id
    :helper-id helper-id
    :role role
    :source source
    :actor-id actor-id
    :now now}))

(defn- end-assignment-commands
  [assignments actor-id reason now]
  (mapv
   #(assignment/end-command
     %
     {:actor-id actor-id
      :reason reason
      :now now})
   assignments))

(defn- end-assignment-changes
  [assignments commands operation]
  (mapv
   (fn [before command]
     (assignment-change before (command-document command) operation))
   assignments
   commands))

;; =============================================================================
;; Domain-command selection
;; =============================================================================

(defn- update-command
  [operation request-document input now]
  (case operation
    :edit
    (request/edit-command
     request-document
     {:content
      (:content input)

      :now
      now})

    :claim
    (request/claim-command
     request-document
     {:now now})

    :unclaim
    (request/unclaim-command
     request-document
     {:now now})

    :mark-on-the-way
    (request/mark-on-the-way-command
     request-document
     {:now now})

    :complete
    (request/complete-command
     request-document
     {:now now})

    :cancel
    (request/cancel-command
     request-document
     {:now
      now

      :reason
      (:reason input)})

    (fail!
     :request.fx/unsupported-operation
     "The requested Request update is not supported."
     {:operation operation})))

(defn- require-request-document!
  [facts request-id]
  (when-not
   (true?
    (:request/found? facts))
    (fail!
     :request/not-found
     "The Request no longer exists."
     {:request/id request-id}))

  (let [document
        (:request/doc facts)]

    (when-not
     document
      (fail!
       :request.fx/incomplete-request-result
       "Request Graph reported a found Request without its document."
       {:request/id request-id}))

    (request/require-request-document
     document)

    (when-not
     (=
      request-id
      (request/request-id document))
      (fail!
       :request.fx/inconsistent-request-result
       "Request Graph returned a different Request."
       {:expected-request-id request-id
        :actual-request-id
        (request/request-id document)}))

    document))

;; =============================================================================
;; Create workflow
;; =============================================================================

(fx/defmachine create-request-machine
  :start
  (fn [ctx]
    (let [input
          (:request.fx/input ctx)

          now
          (:biff.fx/now ctx)

          seed
          (:biff.fx/seed ctx)

          user-id
          (require-authenticated-user-id!
           ctx)

          organization-id
          (:organization-id input)

          location-id
          (:location-id input)

          [request-id _]
          (fx/uuid7
           seed
           now)

          location-authorization
          (require-location-authorization!
           ctx
           organization-id
           location-id
           true)

          actor-authorization
          (active-user-authorization
           ctx
           user-id)

          command
          (request/create-command
           {:id request-id
            :organization-id organization-id
            :location-id location-id
            :requestor
            (request/user-requestor
             user-id)
            :content
            (:content input)
            :now now})

          plan
          (plan-create-request
           {:command command
            :location-authorization-versions
            (:authorization-versions
             location-authorization)
            :actor-authorization-versions
            (:authorization-versions
             actor-authorization)})]

      {:request.fx/result
       (:result plan)

       :request.fx/transaction-plan
       (:transaction-plan plan)

       :biff.fx/next
       :commit}))

  :commit
  commit-state

  :finish
  finish-state)

(defn create-request
  "Creates one User-owned Request at an operational Location.

   input:
     {:organization-id uuid
      :location-id     uuid
      :content         {:title string
                        :details string-or-nil
                        :location-detail string-or-nil}}"
  [ctx input]
  (create-request-machine
   (assoc
    ctx
    :request.fx/input
    input)))

;; =============================================================================
;; Existing-Request workflow
;; =============================================================================

(fx/defmachine update-request-machine
  :start
  (fn [ctx]
    (let [input (:request.fx/input ctx)
          operation (require-operation! (:request.fx/operation ctx))
          request-id
          (require-uuid!
           (:request-id input)
           :request/invalid-request-id
           "A Request UUID is required."
           {:request/id (:request-id input)})]
      {:request.fx/base-ctx ctx
       :request.fx/input input
       :request.fx/operation operation
       :request.fx/request-id request-id
       :request.fx/request-facts
       [:biff.graph.fx/query
        (assoc (request.graph/request-query-input {:request-id request-id})
               :request-assignment/include-ended? false)
        request.graph/request-command-query]
       :biff.fx/next :plan}))

  :plan
  (fn [{:request.fx/keys
        [base-ctx input operation request-id request-facts]}]
    (let [document (require-request-document! request-facts request-id)
          assignments (require-active-assignments! document request-facts)
          organization-id (request/organization-id document)
          location-id (request/location-id document)
          location-authorization
          (require-location-authorization!
           base-ctx
           organization-id
           location-id
           (contains? operational-location-operations operation))
          scope-context (:scope-context location-authorization)
          actor-id (require-authenticated-user-id! base-ctx)
          now (:biff.fx/now base-ctx)
          seed (:biff.fx/seed base-ctx)]
      (case operation
        :claim
        (do
          (when (seq assignments)
            (fail!
             :request/assignments-already-active
             "An open Request cannot be claimed while active assignments exist."
             {:request/id request-id}))
          (let [{:keys
                 [actor-id
                  target-helper-id
                  required-skill
                  authorization-versions]}
                (claim-authorization base-ctx input scope-context)
                [assignment-id _] (fx/uuid7 seed now)
                request-command (update-command operation document input now)
                assignment-command
                (create-assignment-command
                 assignment-id
                 request-id
                 target-helper-id
                 :primary
                 (if (= actor-id target-helper-id)
                   :request/claim
                   :request/manager-claim)
                 actor-id
                 now)
                assignment-document (command-document assignment-command)
                plan
                (plan-update-request
                 {:before document
                  :command request-command
                  :assignment-commands [assignment-command]
                  :assignment-changes
                  [(assignment-change nil assignment-document :claim)]
                  :assertions
                  [(assert-no-active-assignments request-id)]
                  :location-authorization-versions
                  (:authorization-versions location-authorization)
                  :actor-authorization-versions authorization-versions})]
            {:request.fx/result
             (cond-> (assoc (:result plan)
                            :primary-assignment assignment-document)
               required-skill
               (assoc :required-skill required-skill))
             :request.fx/transaction-plan (:transaction-plan plan)
             :biff.fx/next :commit}))

        :unclaim
        (let [actor-authorization
              (operation-authorization
               base-ctx operation document assignments scope-context)
              request-command (update-command operation document input now)
              assignment-commands
              (end-assignment-commands
               assignments actor-id :request/unclaimed now)
              assignment-changes
              (end-assignment-changes
               assignments assignment-commands :unclaim)
              plan
              (plan-update-request
               {:before document
                :command request-command
                :assignment-commands assignment-commands
                :assignment-changes assignment-changes
                :location-authorization-versions
                (:authorization-versions location-authorization)
                :actor-authorization-versions
                (:authorization-versions actor-authorization)})]
          {:request.fx/result (:result plan)
           :request.fx/transaction-plan (:transaction-plan plan)
           :biff.fx/next :commit})

        :complete
        (let [actor-authorization
              (operation-authorization
               base-ctx operation document assignments scope-context)
              request-command (update-command operation document input now)
              assignment-commands
              (end-assignment-commands
               assignments actor-id :request/completed now)
              assignment-changes
              (end-assignment-changes
               assignments assignment-commands :complete)
              plan
              (plan-update-request
               {:before document
                :command request-command
                :assignment-commands assignment-commands
                :assignment-changes assignment-changes
                :location-authorization-versions
                (:authorization-versions location-authorization)
                :actor-authorization-versions
                (:authorization-versions actor-authorization)})]
          {:request.fx/result (:result plan)
           :request.fx/transaction-plan (:transaction-plan plan)
           :biff.fx/next :commit})

        :cancel
        (let [actor-authorization
              (operation-authorization
               base-ctx operation document assignments scope-context)
              request-command (update-command operation document input now)
              assignment-commands
              (end-assignment-commands
               assignments actor-id :request/cancelled now)
              assignment-changes
              (end-assignment-changes
               assignments assignment-commands :cancel)
              plan
              (plan-update-request
               {:before document
                :command request-command
                :assignment-commands assignment-commands
                :assignment-changes assignment-changes
                :location-authorization-versions
                (:authorization-versions location-authorization)
                :actor-authorization-versions
                (:authorization-versions actor-authorization)})]
          {:request.fx/result (:result plan)
           :request.fx/transaction-plan (:transaction-plan plan)
           :biff.fx/next :commit})

        :add-collaborator
        (let [actor-authorization
              (operation-authorization
               base-ctx operation document assignments scope-context)
              primary (:request/primary-assignment actor-authorization)
              helper-id
              (require-uuid!
               (:helper-id input)
               :request/invalid-helper
               "A collaborator User UUID is required."
               {:helper-id (:helper-id input)})
              _ (when (= actor-id helper-id)
                  (fail!
                   :request/helper-already-assigned
                   "The primary helper is already assigned to this Request."
                   {:request/id request-id :helper-id helper-id}))
              _ (when (assignment/active-assignment-for-helper
                       assignments helper-id)
                  (fail!
                   :request/helper-already-assigned
                   "The helper already has an active assignment on this Request."
                   {:request/id request-id :helper-id helper-id}))
              skill (normalize-required-skill! (:skill input))
              target-authorization
              (eligible-helper-authorization
               base-ctx helper-id scope-context skill)
              [assignment-id _] (fx/uuid7 seed now)
              assignment-command
              (create-assignment-command
               assignment-id request-id helper-id :collaborator
               :request/collaboration actor-id now)
              assignment-document (command-document assignment-command)
              plan
              (plan-assignment-operation
               {:request-document document
                :operation :add-collaborator
                :assignment-commands [assignment-command]
                :assignment-changes
                [(assignment-change
                  nil assignment-document :add-collaborator)]
                :assertions
                [(assert-no-active-assignment-for-helper request-id helper-id)]
                :location-authorization-versions
                (:authorization-versions location-authorization)
                :actor-authorization-versions
                (conj (:authorization-versions actor-authorization)
                      (assignment-authorization-version primary))
                :target-authorization-versions
                (:authorization-versions target-authorization)})]
          {:request.fx/result
           (assoc (:result plan)
                  :collaborator-assignment assignment-document
                  :required-skill skill)
           :request.fx/transaction-plan (:transaction-plan plan)
           :biff.fx/next :commit})

        :remove-collaborator
        (let [actor-authorization
              (operation-authorization
               base-ctx operation document assignments scope-context)
              primary (:request/primary-assignment actor-authorization)
              helper-id
              (require-uuid!
               (:helper-id input)
               :request/invalid-helper
               "A collaborator User UUID is required."
               {:helper-id (:helper-id input)})
              collaborator
              (or
               (some
                #(when (and (assignment/active-collaborator? %)
                            (assignment/for-helper? % helper-id))
                   %)
                assignments)
               (fail!
                :request/collaborator-not-found
                "The helper is not an active collaborator on this Request."
                {:request/id request-id :helper-id helper-id}))
              assignment-command
              (assignment/end-command
               collaborator
               {:actor-id actor-id
                :reason :request/collaborator-removed
                :now now})
              after (command-document assignment-command)
              plan
              (plan-assignment-operation
               {:request-document document
                :operation :remove-collaborator
                :assignment-commands [assignment-command]
                :assignment-changes
                [(assignment-change
                  collaborator after :remove-collaborator)]
                :location-authorization-versions
                (:authorization-versions location-authorization)
                :actor-authorization-versions
                (conj (:authorization-versions actor-authorization)
                      (assignment-authorization-version primary))})]
          {:request.fx/result
           (assoc (:result plan) :collaborator-assignment after)
           :request.fx/transaction-plan (:transaction-plan plan)
           :biff.fx/next :commit})

        :reassign
        (let [_ (when-not (request/claimed? document)
                  (fail!
                   :request/not-reassignable
                   "Primary reassignment currently requires a claimed Request."
                   {:request/id request-id
                    :request/status (request/status document)}))
              actor-authorization
              (manager-authorization base-ctx actor-id scope-context)
              current-primary
              (require-primary-assignment! document assignments)
              target-helper-id
              (require-uuid!
               (:helper-id input)
               :request/invalid-helper
               "A replacement primary helper User UUID is required."
               {:helper-id (:helper-id input)})
              _ (when (assignment/for-helper? current-primary target-helper-id)
                  (fail!
                   :request/helper-already-primary
                   "The selected helper is already the primary helper."
                   {:request/id request-id
                    :helper-id target-helper-id}))
              target-existing
              (assignment/active-assignment-for-helper
               assignments target-helper-id)
              _ (when (and target-existing
                           (not (assignment/active-collaborator?
                                 target-existing)))
                  (fail!
                   :request/helper-already-assigned
                   "The selected helper already has an incompatible active assignment."
                   {:request/id request-id
                    :helper-id target-helper-id}))
              skill (normalize-required-skill! (:skill input))
              target-authorization
              (eligible-helper-authorization
               base-ctx target-helper-id scope-context skill)
              [assignment-id _] (fx/uuid7 seed now)
              assignments-to-end
              (cond-> [current-primary]
                target-existing
                (conj target-existing))
              end-commands
              (end-assignment-commands
               assignments-to-end actor-id :request/reassigned now)
              create-command
              (create-assignment-command
               assignment-id request-id target-helper-id :primary
               :request/reassignment actor-id now)
              new-primary (command-document create-command)
              assignment-commands
              (conj end-commands create-command)
              assignment-changes
              (into
               (end-assignment-changes
                assignments-to-end end-commands :reassign)
               [(assignment-change nil new-primary :reassign)])
              plan
              (plan-assignment-operation
               {:request-document document
                :operation :reassign
                :assignment-commands assignment-commands
                :assignment-changes assignment-changes
                :location-authorization-versions
                (:authorization-versions location-authorization)
                :actor-authorization-versions
                (:authorization-versions actor-authorization)
                :target-authorization-versions
                (:authorization-versions target-authorization)})]
          {:request.fx/result
           (cond-> (assoc (:result plan)
                          :previous-primary-assignment
                          (command-document (first end-commands))
                          :primary-assignment new-primary)
             skill
             (assoc :required-skill skill))
           :request.fx/transaction-plan (:transaction-plan plan)
           :biff.fx/next :commit})

        ;; edit and mark-on-the-way remain Request-only changes.
        (let [actor-authorization
              (operation-authorization
               base-ctx operation document assignments scope-context)
              command (update-command operation document input now)
              primary (:request/primary-assignment actor-authorization)
              authorization-versions
              (cond-> (:authorization-versions actor-authorization)
                (and (= :mark-on-the-way operation) primary)
                (conj (assignment-authorization-version primary)))
              plan
              (plan-update-request
               {:before document
                :command command
                :location-authorization-versions
                (:authorization-versions location-authorization)
                :actor-authorization-versions authorization-versions})]
          {:request.fx/result (:result plan)
           :request.fx/transaction-plan (:transaction-plan plan)
           :biff.fx/next :commit}))))

  :commit
  commit-state

  :finish
  finish-state)

(defn- run-update
  [ctx operation input]
  (update-request-machine
   (assoc
    ctx
    :request.fx/operation
    operation
    :request.fx/input
    input)))

(defn edit-request
  "Edits an active User-owned Request while its Location remains operational."
  [ctx input]
  (run-update
   ctx
   :edit
   input))

(defn claim-request
  "Claims an open Request.

   With no :helper-id, the signed-in effective helper claims it personally.
   With a different :helper-id, the signed-in actor must be an effective
   supervisor or administrator and the selected User must be an effective
   helper. Optional :skill constrains the selected helper."
  [ctx input]
  (run-update
   ctx
   :claim
   input))

(defn unclaim-request
  "Returns the signed-in helper's Request to open.

   Location operational state and a still-current helper role are not required.
   Current assignment ownership and an active User identity are required."
  [ctx input]
  (run-update
   ctx
   :unclaim
   input))

(defn mark-request-on-the-way
  "Marks the signed-in effective helper's assigned Request on the way."
  [ctx input]
  (run-update
   ctx
   :mark-on-the-way
   input))

(defn complete-request
  "Completes the signed-in effective helper's claimed or on-the-way Request."
  [ctx input]
  (run-update
   ctx
   :complete
   input))

(defn cancel-request
  "Cancels an active User-owned Request.

   The Location must still exist and match the Request, but the hierarchy need
   not remain operational."
  [ctx input]
  (run-update
   ctx
   :cancel
   input))

(defn add-collaborator
  "Adds one effective Location helper as a collaborator.

   The signed-in primary helper remains responsible for the Request. :skill is
   optional and, when supplied, must match an organization-local Membership
   skill on the collaborator."
  [ctx input]
  (run-update ctx :add-collaborator input))

(defn remove-collaborator
  "Ends one collaborator assignment. The signed-in actor must be the active
   primary helper."
  [ctx input]
  (run-update ctx :remove-collaborator input))

(defn reassign-request
  "Replaces the active primary helper on a claimed Request.

   The signed-in actor must be an effective supervisor or administrator at the
   Request Location. A target helper who is currently a collaborator is
   promoted atomically by ending that collaborator assignment and creating the
   new primary assignment."
  [ctx input]
  (run-update ctx :reassign input))

(defn perform-action
  "Dispatches one supported existing-Request operation."
  [ctx operation input]
  (case operation
    :edit
    (edit-request
     ctx
     input)

    :claim
    (claim-request
     ctx
     input)

    :unclaim
    (unclaim-request
     ctx
     input)

    :mark-on-the-way
    (mark-request-on-the-way
     ctx
     input)

    :complete
    (complete-request
     ctx
     input)

    :cancel
    (cancel-request
     ctx
     input)

    :add-collaborator
    (add-collaborator
     ctx
     input)

    :remove-collaborator
    (remove-collaborator
     ctx
     input)

    :reassign
    (reassign-request
     ctx
     input)

    (fail!
     :request.fx/unsupported-operation
     "The requested Request action is not supported."
     {:operation operation})))

(def operations
  "Public Request operation registry."
  {:request/create
   #'create-request

   :request/edit
   #'edit-request

   :request/claim
   #'claim-request

   :request/unclaim
   #'unclaim-request

   :request/mark-on-the-way
   #'mark-request-on-the-way

   :request/complete
   #'complete-request

   :request/cancel
   #'cancel-request

   :request/add-collaborator
   #'add-collaborator

   :request/remove-collaborator
   #'remove-collaborator

   :request/reassign
   #'reassign-request})
