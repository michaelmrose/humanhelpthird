(ns net.humanhelp.site.model.request.fx
  "Effectful Request workflows.

   This slice supports signed-in User creation/edit/cancel and helper
   claim/unclaim/on-the-way/complete. Organization supplies authoritative
   Location context, User supplies current authorization proof, Request supplies
   pure commands, and model.fx validates authorization-version guards and
   commits the command plus semantic change.

   Request FX decides which documents establish authorization. It does not
   normalize guards or translate them into assertions; those generic concerns
   belong to model.fx.

   Capability-owned writes and supervisor overrides remain unsupported until
   those models and policies exist."
  (:require
   [gesso.fx :as fx]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.domain.core :as request]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Workflow vocabulary
;; =============================================================================

(def operation-order
  [:create :edit :claim :unclaim :mark-on-the-way :complete :cancel])

(def operations-set
  (set operation-order))

(def operational-location-operations
  #{:create :edit :claim :mark-on-the-way :complete})

(def owner-operations
  #{:edit :cancel})

(def helper-operations
  #{:claim :mark-on-the-way :complete})

(defn operation?
  [value]
  (contains? operations-set value))

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
     (cond-> {:error/type error-type}
       (some? details) (assoc :error/details details))))))

(defn- require-uuid!
  [value error-type message details]
  (when-not (uuid? value)
    (fail! error-type message details))
  value)

(defn- require-operation!
  [operation]
  (when-not (operation? operation)
    (fail! :request.fx/unsupported-operation
           "The requested Request operation is not supported."
           {:operation operation}))
  operation)

(defn- require-authenticated-user-id!
  [ctx]
  (require-uuid!
   (:current-user/id ctx)
   :request/not-authenticated
   "A signed-in User is required."
   {:current-user/id (:current-user/id ctx)}))

(defn- command-document
  [command]
  (model.common/command-document command))

(defn- commit-state
  [{:request.fx/keys [result transaction-plan]}]
  {:request.fx/result result
   :request.fx/transaction [model.fx/transact-effect transaction-plan]
   :biff.fx/next :finish})

(defn- finish-state
  [{:request.fx/keys [result transaction]}]
  {:biff.fx/return (assoc result :transaction transaction)})

;; =============================================================================
;; Authorization-version construction
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
  "Builds one generic model.fx authorization-version guard from a concrete
   document whose current version established this Request authorization
   decision."
  [entity-type version document]
  {:model/entity-type entity-type
   :model/expected
   (model.common/expected-version
    document
    version)})

(defn- require-authorization-version-sequence!
  "Validates only the model-specific collection contract.

   Generic guard shape validation, deduplication, conflict detection, and ASSERT
   generation belong to model.fx."
  [authorization-versions error-type message details]
  (when-not
   (sequential? authorization-versions)
    (fail!
     error-type
     message
     (assoc
      details
      :authorization-versions
      authorization-versions)))

  authorization-versions)

;; =============================================================================
;; Organization Location proof
;; =============================================================================

(defn- require-location-authorization!
  [ctx organization-id location-id require-operational?]
  (require-uuid! organization-id
                 :request/invalid-organization
                 "A Request requires an Organization UUID."
                 {:organization/id organization-id})
  (require-uuid! location-id
                 :request/invalid-location
                 "A Request requires a Location UUID."
                 {:location/id location-id})

  (let [facts (organization/location-context
               ctx
               {:organization-id organization-id
                :location-id location-id})]
    (when-not (true? (:location/found? facts))
      (fail! :location/not-found
             "The Request Location no longer exists."
             {:organization/id organization-id
              :location/id location-id}))

    (let [location
          (or (:location/doc facts)
              (fail! :request.fx/incomplete-location-context
                     "Organization reported a found Location without its document."
                     {:location/id location-id}))

          scope-context
          (or (:location/scope-context facts)
              (fail! :request.fx/incomplete-location-context
                     "Organization did not return the Location scope context."
                     {:location/id location-id}))

          authorization-versions
          (or (:location/authorization-versions facts)
              (fail! :request.fx/incomplete-location-context
                     "Organization did not return Location authorization versions."
                     {:location/id location-id}))

          expected-scope
          (organization/location-scope location-id)]

      (when-not (= location-id (:xt/id location))
        (fail! :request.fx/inconsistent-location-context
               "Organization returned a different Location document."
               {:expected-location-id location-id
                :actual-location-id (:xt/id location)}))

      (when-not (= organization-id (:location/organization-id facts))
        (fail! :location/organization-mismatch
               "The Request Location belongs to another Organization."
               {:expected-organization-id organization-id
                :actual-organization-id (:location/organization-id facts)
                :location/id location-id}))

      (when-not (organization/scope-context? scope-context)
        (fail! :request.fx/invalid-location-context
               "Organization returned an invalid Location scope context."
               {:scope-context scope-context}))

      (when-not (= organization-id (:organization/id scope-context))
        (fail! :request.fx/inconsistent-location-context
               "The Location scope context belongs to another Organization."
               {:expected-organization-id organization-id
                :actual-organization-id (:organization/id scope-context)}))

      (when-not (organization/same-scope?
                 expected-scope
                 (:scope/target scope-context))
        (fail! :request.fx/inconsistent-location-context
               "Organization returned a scope context for another Location."
               {:expected-scope expected-scope
                :actual-scope (:scope/target scope-context)}))

      (when (and require-operational?
                 (not (true? (:scope/operational? scope-context))))
        (fail! :location/not-operational
               "The Request Location is not operational."
               {:organization/id organization-id
                :location/id location-id}))

      {:location location
       :scope-context scope-context
       :authorization-versions
       (vec
        (require-authorization-version-sequence!
         authorization-versions
         :request.fx/invalid-location-authorization-versions
         "Organization Location authorization versions must be sequential."
         {:organization/id organization-id
          :location/id location-id}))})))

;; =============================================================================
;; User proof
;; =============================================================================

(defn- require-public-document!
  [facts found-key document-key error-type message details]
  (when-not (true? (get facts found-key))
    (fail! error-type message details))
  (or (get facts document-key)
      (fail! :request.fx/incomplete-user-result
             "User core reported a found document without returning it."
             {:document-key document-key
              :details details})))

(defn- require-user-document!
  [ctx user-id]
  (require-public-document!
   (user/user-facts ctx {:user-id user-id})
   :user/found?
   :user/doc
   :user/not-found
   "The signed-in User no longer exists."
   {:user/id user-id}))

(defn- require-active-user-document!
  [ctx user-id]
  (let [document (require-user-document! ctx user-id)]
    (when-not (user/user-active? document)
      (fail! :user/not-active
             "Only an active User may change a Request."
             {:user/id user-id}))
    document))

(defn- active-user-authorization
  [ctx user-id]
  (let [document (require-active-user-document! ctx user-id)]
    {:user/id user-id
     :user/authorization-versions
     [(document-authorization-version user/user-entity-type user-version document)]}))

(defn- role-documents-at-scopes
  [ctx organization-id scopes]
  (reduce
   (fn [documents document]
     (if (some #(= (:xt/id %) (:xt/id document)) documents)
       documents
       (conj documents document)))
   []
   (mapcat
    (fn [scope]
      (let [facts (user/active-role-assignments-at-scope
                   ctx organization-id scope)
            values (:user/active-role-assignments-at-scope facts)]
        (when-not (sequential? values)
          (fail! :request.fx/incomplete-helper-authorization
                 "User core did not return active role assignments for a scope."
                 {:organization/id organization-id
                  :scope scope
                  :facts facts}))
        (map
         (fn [value]
           (or (:role-assignment/doc value)
               (fail! :request.fx/incomplete-helper-authorization
                      "User core returned a role assignment without its document."
                      {:scope scope
                       :value value})))
         values)))
    scopes)))

(defn- helper-authorization
  [ctx user-id scope-context]
  (let [organization-id (:organization/id scope-context)
        scopes (vec (:scope/applicable scope-context))
        access-context
        (user/access-context
         ctx
         {:user-id user-id
          :scope-context scope-context})]

    (when-not (true? (:user/helper? access-context))
      (fail! :user/not-authorized
             "Effective helper authority at this Location is required."
             {:user/id user-id
              :organization/id organization-id
              :scope/target (:scope/target scope-context)}))

    (let [membership-id
          (or (:membership/id access-context)
              (fail! :request.fx/incomplete-helper-authorization
                     "Helper access did not identify a Membership."
                     {:user/id user-id
                      :organization/id organization-id}))

          user-document
          (require-active-user-document! ctx user-id)

          membership-document
          (require-public-document!
           (user/membership-facts ctx membership-id)
           :membership/found?
           :membership/doc
           :membership/not-found
           "The helper Membership no longer exists."
           {:membership/id membership-id})

          assignments
          (role-documents-at-scopes ctx organization-id scopes)

          helper-assignment
          (user/effective-assignment-for-role
           user-document
           membership-document
           assignments
           scopes
           :helper)]

      (when-not helper-assignment
        (fail! :user/not-authorized
               "Helper authority changed while it was being loaded."
               {:user/id user-id
                :organization/id organization-id
                :scope/target (:scope/target scope-context)}))

      {:user/id user-id
       :organization/id organization-id
       :scope/target (:scope/target scope-context)
       :user/authorization-versions
       [(document-authorization-version user/user-entity-type
                        user-version
                        user-document)
        (document-authorization-version user/membership-entity-type
                        membership-version
                        membership-document)
        (document-authorization-version user/role-assignment-entity-type
                        role-assignment-version
                        helper-assignment)]})))

;; =============================================================================
;; Request actor policy
;; =============================================================================

(defn- owner-authorization
  [ctx request-document]
  (let [user-id (require-authenticated-user-id! ctx)]
    (when (request/capability-requestor?
           (request/requestor request-document))
      (fail! :request/capability-authorization-unavailable
             "Capability-owned Request writes are not implemented yet."
             {:request/id (request/request-id request-document)
              :requestor (request/requestor request-document)}))

    (when-not (request/requested-by-user? request-document user-id)
      (fail! :request/not-authorized
             "Only the User who created this Request may perform this action."
             {:request/id (request/request-id request-document)
              :user/id user-id}))

    (active-user-authorization ctx user-id)))

(defn- assigned-helper-authorization
  [ctx request-document]
  (let [user-id (require-authenticated-user-id! ctx)]
    (when-not (request/assigned-to? request-document user-id)
      (fail! :request/not-authorized
             "Only the currently assigned helper may perform this action."
             {:request/id (request/request-id request-document)
              :user/id user-id
              :request/helper (request/helper-id request-document)}))
    (active-user-authorization ctx user-id)))

(defn- operation-authorization
  [ctx operation request-document scope-context]
  (cond
    (contains? owner-operations operation)
    (owner-authorization ctx request-document)

    (= :unclaim operation)
    (assigned-helper-authorization ctx request-document)

    (contains? helper-operations operation)
    (helper-authorization
     ctx
     (require-authenticated-user-id! ctx)
     scope-context)

    :else
    (fail! :request.fx/unsupported-operation
           "The Request operation has no authorization policy."
           {:operation operation})))

;; =============================================================================
;; Semantic changes and transaction planning
;; =============================================================================

(defn- without-nils
  [m]
  (into {} (remove (comp nil? val)) m))

(defn- change-entry
  [{:keys [topic id]}]
  {:coalesce-key [topic id]})

(defn- request-change
  [before after operation]
  (without-nils
   {:topic :request
    :id (request/request-id after)
    :change/kind (if before :updated :created)
    :request/operation operation
    :request/id (request/request-id after)
    :organization/id (request/organization-id after)
    :location/id (request/location-id after)
    :request/requestor-type (request/requestor-type after)
    :request/requestor-id (request/requestor-id after)
    :request/status (request/status after)
    :request/previous-status (some-> before request/status)
    :request/helper (request/helper-id after)
    :request/previous-helper (some-> before request/helper-id)
    :request/revision (request/revision after)}))

(defn- transaction-plan
  [command authorization-versions change]
  {:commands [command]
   :authorization-versions
   (vec
    (require-authorization-version-sequence!
     authorization-versions
     :request.fx/invalid-authorization-versions
     "Request transaction authorization versions must be sequential."
     {}))
   :changes [change]
   :entry-fn change-entry})

(defn- require-command!
  [command expected-operation]
  (when-not (map? command)
    (fail! :request.fx/invalid-command
           "A Request workflow requires a model command."
           {:command command}))
  (when-not (= request/entity-type (:model/entity-type command))
    (fail! :request.fx/invalid-command
           "The command does not target a Request."
           {:command command}))
  (when-not (= expected-operation (:model/operation command))
    (fail! :request.fx/invalid-command
           "The command operation does not match the workflow."
           {:expected-operation expected-operation
            :actual-operation (:model/operation command)}))
  command)

(defn plan-create-request
  "Builds an atomic transaction plan for an already-authorized create command."
  [{:keys [command authorization-versions]}]
  (require-command! command :create)
  (let [document (command-document command)]
    (when-not (request/request-consistent? document)
      (fail! :request.fx/invalid-command-document
             "The create command contains an invalid Request."
             {:request document}))
    {:transaction-plan
     (transaction-plan
      command
      authorization-versions
      (request-change nil document :create))
     :result
     {:request document}}))

(defn plan-update-request
  "Builds an atomic transaction plan for an already-authorized update command."
  [{:keys [before command authorization-versions]}]
  (when-not (request/request-consistent? before)
    (fail! :request.fx/invalid-before
           "The current Request is invalid."
           {:request before}))

  (let [operation (:model/operation command)]
    (require-operation! operation)
    (when (= :create operation)
      (fail! :request.fx/invalid-command
             "An update workflow cannot execute a create command."
             {:command command}))
    (require-command! command operation)

    (let [after (command-document command)]
      (when-not (request/request-consistent? after)
        (fail! :request.fx/invalid-command-document
               "The update command contains an invalid Request."
               {:request after}))
      (when-not (= (request/request-id before)
                   (request/request-id after))
        (fail! :request.fx/command-target-mismatch
               "The Request command changed its target."
               {:before-id (request/request-id before)
                :after-id (request/request-id after)}))

      {:transaction-plan
       (transaction-plan
        command
        authorization-versions
        (request-change before after operation))
       :result
       {:request after}})))

;; =============================================================================
;; Command selection and Request loading
;; =============================================================================

(defn- update-command
  [operation request-document input user-id now]
  (case operation
    :edit
    (request/edit-command
     request-document
     {:content (:content input)
      :now now})

    :claim
    (request/claim-command
     request-document
     {:helper-id user-id
      :now now})

    :unclaim
    (request/unclaim-command
     request-document
     {:helper-id user-id
      :now now})

    :mark-on-the-way
    (request/mark-on-the-way-command
     request-document
     {:helper-id user-id
      :now now})

    :complete
    (request/complete-command
     request-document
     {:helper-id user-id
      :now now})

    :cancel
    (request/cancel-command
     request-document
     {:now now
      :reason (:reason input)})

    (fail! :request.fx/unsupported-operation
           "The requested Request update is not supported."
           {:operation operation})))

(defn- require-request-document!
  [facts request-id]
  (when-not (true? (:request/found? facts))
    (fail! :request/not-found
           "The Request no longer exists."
           {:request/id request-id}))

  (let [document
        (or (:request/doc facts)
            (fail! :request.fx/incomplete-request-result
                   "Request Graph reported a found Request without its document."
                   {:request/id request-id}))]
    (when-not (= request-id (request/request-id document))
      (fail! :request.fx/inconsistent-request-result
             "Request Graph returned a different Request."
             {:expected-request-id request-id
              :actual-request-id (request/request-id document)}))
    (request/require-request-consistent document)))

;; =============================================================================
;; Create workflow
;; =============================================================================

(fx/defmachine create-request-machine
  :start
  (fn [ctx]
    (let [input (:request.fx/input ctx)
          now (:biff.fx/now ctx)
          seed (:biff.fx/seed ctx)
          user-id (require-authenticated-user-id! ctx)
          organization-id (:organization-id input)
          location-id (:location-id input)
          [request-id _] (fx/uuid7 seed now)

          location-auth
          (require-location-authorization!
           ctx organization-id location-id true)

          user-auth
          (active-user-authorization ctx user-id)

          command
          (request/create-command
           {:id request-id
            :organization-id organization-id
            :location-id location-id
            :requestor (request/user-requestor user-id)
            :content (:content input)
            :now now})

          plan
          (plan-create-request
           {:command command
            :authorization-versions
            (into
             []
             (concat
              (:authorization-versions location-auth)
              (:user/authorization-versions user-auth)))})]

      {:request.fx/result (:result plan)
       :request.fx/transaction-plan (:transaction-plan plan)
       :biff.fx/next :commit}))

  :commit commit-state
  :finish finish-state)

(defn create-request
  "Creates one User-owned Request at an operational Location.

   input:
     {:organization-id uuid
      :location-id     uuid
      :content         {:title ...
                        :details ...
                        :location-detail ...}}"
  [ctx input]
  (create-request-machine
   (assoc ctx :request.fx/input input)))

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
        (request.graph/request-query-input {:request-id request-id})
        request.graph/request-command-query]
       :biff.fx/next :plan}))

  :plan
  (fn [{:request.fx/keys
        [base-ctx input operation request-id request-facts]}]
    (let [document
          (require-request-document! request-facts request-id)

          organization-id
          (request/organization-id document)

          location-id
          (request/location-id document)

          location-auth
          (require-location-authorization!
           base-ctx
           organization-id
           location-id
           (contains? operational-location-operations operation))

          actor-auth
          (operation-authorization
           base-ctx
           operation
           document
           (:scope-context location-auth))

          user-id
          (require-authenticated-user-id! base-ctx)

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
            :authorization-versions
            (into
             []
             (concat
              (:authorization-versions location-auth)
              (:user/authorization-versions actor-auth)))})]

      {:request.fx/result (:result plan)
       :request.fx/transaction-plan (:transaction-plan plan)
       :biff.fx/next :commit}))

  :commit commit-state
  :finish finish-state)

(defn- run-update
  [ctx operation input]
  (update-request-machine
   (assoc ctx
          :request.fx/operation operation
          :request.fx/input input)))

(defn edit-request
  "Edits an active User-owned Request."
  [ctx input]
  (run-update ctx :edit input))

(defn claim-request
  "Claims an open Request for the signed-in effective helper."
  [ctx input]
  (run-update ctx :claim input))

(defn unclaim-request
  "Returns the signed-in helper's Request to open.

   Location operational state and current helper role are not required for this
   cleanup action; current assignment ownership and an active User are."
  [ctx input]
  (run-update ctx :unclaim input))

(defn mark-request-on-the-way
  "Marks the signed-in helper's Request on the way."
  [ctx input]
  (run-update ctx :mark-on-the-way input))

(defn complete-request
  "Completes the signed-in helper's claimed or on-the-way Request."
  [ctx input]
  (run-update ctx :complete input))

(defn cancel-request
  "Cancels an active User-owned Request.

   The Location must still exist and match the Request, but it need not remain
   operational."
  [ctx input]
  (run-update ctx :cancel input))

(defn perform-action
  "Dispatches one supported existing-Request operation."
  [ctx operation input]
  (case operation
    :edit (edit-request ctx input)
    :claim (claim-request ctx input)
    :unclaim (unclaim-request ctx input)
    :mark-on-the-way (mark-request-on-the-way ctx input)
    :complete (complete-request ctx input)
    :cancel (cancel-request ctx input)
    (fail! :request.fx/unsupported-operation
           "The requested Request action is not supported."
           {:operation operation})))

(def operation-handlers
  "Public Request operation registry."
  {:request/create #'create-request
   :request/edit #'edit-request
   :request/claim #'claim-request
   :request/unclaim #'unclaim-request
   :request/mark-on-the-way #'mark-request-on-the-way
   :request/complete #'complete-request
   :request/cancel #'cancel-request})
