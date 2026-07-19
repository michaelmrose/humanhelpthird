(ns net.humanhelp.site.model.request.fx
  "Effectful Request workflows.

   Request FX coordinates the pure Request domain with the public Organization
   and User model interfaces.

   Organization supplies one authoritative Location hierarchy context and the
   authorization-version guards for every Organization document that established
   it. User supplies current identity, Membership, and role facts. Request FX
   rechecks the operation policy, constructs one Request command, and delegates
   the final atomic transaction to the shared model.fx effect.

   This namespace does not query XTDB directly, format transactions, execute
   transactions, dispatch Gesso Live invalidations, or duplicate the shared
   authorization-version machinery. Capability-authenticated Request writes and
   supervisor overrides remain intentionally unsupported."
  (:require
   [gesso.fx :as fx]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.organization.core :as organization]
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
    :complete})

(def owner-operations
  "Operations reserved for the authenticated User requestor."
  #{:edit
    :cancel})

(def effective-helper-operations
  "Operations requiring a currently effective helper role at the Location."
  #{:claim
    :mark-on-the-way
    :complete})

(def assigned-helper-operations
  "Cleanup operations requiring current assignment ownership but not a still
   effective helper role."
  #{:unclaim})

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

(defn- helper-authorization
  "Reloads and rechecks every User document that establishes effective helper
   authority. The compact access context supplies the current Membership ID;
   the final authorization decision is recomputed from the loaded User,
   Membership, and role-assignment documents that are guarded in the same
   transaction."
  [ctx user-id scope-context]
  (let [organization-id
        (:organization/id scope-context)

        scopes
        (vec
         (:scope/applicable scope-context))

        public-access
        (user/access-context
         ctx
         {:user-id user-id
          :scope-context scope-context})]

    (when-not
     (true?
      (:user/helper? public-access))
      (fail!
       :user/not-authorized
       "Effective helper authority at this Location is required."
       {:user/id user-id
        :organization/id organization-id
        :scope/target
        (:scope/target scope-context)}))

    (let [membership-id
          (or
           (:membership/id public-access)
           (fail!
            :request.fx/incomplete-helper-authorization
            "Helper access did not identify a Membership."
            {:user/id user-id
             :organization/id organization-id}))

          user-document
          (require-active-user-document!
           ctx
           user-id)

          membership-document
          (user/require-membership
           ctx
           membership-id)

          assignments
          (role-documents-at-scopes
           ctx
           organization-id
           scopes)

          helper-assignment
          (user/effective-assignment-for-role
           user-document
           membership-document
           assignments
           scopes
           :helper)]

      (when-not
       helper-assignment
        (fail!
         :user/not-authorized
         "Helper authority changed while it was being loaded."
         {:user/id user-id
          :organization/id organization-id
          :scope/target
          (:scope/target scope-context)}))

      {:user/id user-id
       :organization/id organization-id
       :scope/target
       (:scope/target scope-context)

       :authorization-versions
       [(document-authorization-version
         user/user-entity-type
         user-version
         user-document)

        (document-authorization-version
         user/membership-entity-type
         membership-version
         membership-document)

        (document-authorization-version
         user/role-assignment-entity-type
         role-assignment-version
         helper-assignment)]})))

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

(defn- assigned-helper-authorization
  [ctx request-document]
  (let [user-id
        (require-authenticated-user-id!
         ctx)]

    (when-not
     (request/assigned-to?
      request-document
      user-id)
      (fail!
       :request/not-authorized
       "Only the currently assigned helper may perform this action."
       {:request/id
        (request/request-id request-document)
        :user/id user-id
        :request/helper
        (request/helper-id request-document)}))

    (active-user-authorization
     ctx
     user-id)))

(defn- operation-authorization
  [ctx operation request-document scope-context]
  (cond
    (contains?
     owner-operations
     operation)
    (owner-authorization
     ctx
     request-document)

    (contains?
     assigned-helper-operations
     operation)
    (assigned-helper-authorization
     ctx
     request-document)

    (contains?
     effective-helper-operations
     operation)
    (helper-authorization
     ctx
     (require-authenticated-user-id! ctx)
     scope-context)

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

    :request/helper
    (request/helper-id after)

    :request/previous-helper
    (some->
     before
     request/helper-id)

    :request/revision
    (request/revision after)}))

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

         {:commands
          [command]

          :changes
          [(request-change
            before
            after
            operation)]}))

       :result
       {:request after}})))

;; =============================================================================
;; Domain-command selection
;; =============================================================================

(defn- update-command
  [operation request-document input user-id now]
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
     {:helper-id
      user-id

      :now
      now})

    :unclaim
    (request/unclaim-command
     request-document
     {:helper-id
      user-id

      :now
      now})

    :mark-on-the-way
    (request/mark-on-the-way-command
     request-document
     {:helper-id
      user-id

      :now
      now})

    :complete
    (request/complete-command
     request-document
     {:helper-id
      user-id

      :now
      now})

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
    (let [input
          (:request.fx/input ctx)

          operation
          (require-operation!
           (:request.fx/operation ctx))

          request-id
          (require-uuid!
           (:request-id input)
           :request/invalid-request-id
           "A Request UUID is required."
           {:request/id
            (:request-id input)})]

      {:request.fx/base-ctx
       ctx

       :request.fx/input
       input

       :request.fx/operation
       operation

       :request.fx/request-id
       request-id

       :request.fx/request-facts
       [:biff.graph.fx/query
        (request.graph/request-query-input
         {:request-id request-id})
        request.graph/request-command-query]

       :biff.fx/next
       :plan}))

  :plan
  (fn [{:request.fx/keys
        [base-ctx
         input
         operation
         request-id
         request-facts]}]
    (let [document
          (require-request-document!
           request-facts
           request-id)

          organization-id
          (request/organization-id
           document)

          location-id
          (request/location-id
           document)

          location-authorization
          (require-location-authorization!
           base-ctx
           organization-id
           location-id
           (contains?
            operational-location-operations
            operation))

          actor-authorization
          (operation-authorization
           base-ctx
           operation
           document
           (:scope-context
            location-authorization))

          user-id
          (require-authenticated-user-id!
           base-ctx)

          command
          (update-command
           operation
           document
           input
           user-id
           (:biff.fx/now base-ctx))

          plan
          (plan-update-request
           {:before document
            :command command
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
  "Claims an open Request for the signed-in effective helper."
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
   #'cancel-request})
