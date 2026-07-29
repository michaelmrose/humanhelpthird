(ns net.humanhelp.site.model.request.fx
  "Request-specific dependencies, authorization, and transaction planning.

   Request FX coordinates four model boundaries:

   - Request owns Request lifecycle and RequestAssignment participation;
   - User owns global identity and account lifecycle;
   - Membership owns Organization-local membership, roles, skills, and
     authorization;
   - Organization owns Location hierarchy, scope ancestry, and operational
     state.

   Cross-model dependencies are consumed only through those models' public
   core namespaces.

   This namespace does not commit transactions. Every planner returns:

     {:result               {...}
      :transaction-fragment {:commands [...]
                             :guards [...]
                             :assertions [...]
                             :changes [...]}
      :transaction-options  {:entry-fn ...}}

   Callers compose any additional requirements and commit the resulting plan
   through gesso.model.tx.

   Request is the concurrency root for its RequestAssignment aggregate. Every
   supported mutation of the active assignment set also updates the Request
   revision. Consequently concurrent Request-assignment workflows serialize on
   the Request document instead of attempting to encode the entire active
   assignment set as independent persistence assertions."
  (:require
   [gesso.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.domain :as request]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Workflow policy
;; =============================================================================

(def operational-location-operations
  "Request operations requiring the Location hierarchy to remain operational
   at commit time."
  #{:create
    :edit
    :claim
    :mark-on-the-way
    :complete
    :add-collaborator
    :reassign})

(def owner-operations
  "Operations reserved for the User requestor."
  #{:edit
    :cancel})

(def effective-primary-helper-operations
  "Operations requiring both active primary participation and current helper
   authority at the Request Location."
  #{:mark-on-the-way
    :complete
    :add-collaborator})

(def primary-helper-operations
  "Operations requiring primary assignment ownership but not a still-effective
   helper role.

   This permits a helper whose Organization role was removed to unwind an
   existing Request relationship."
  #{:unclaim
    :remove-collaborator})

;; =============================================================================
;; Errors and FX context
;; =============================================================================

(defn- fail!
  ([type message]
   (fail!
    type
    message
    nil))

  ([type message details]
   (throw
    (ex-info
     message
     (cond->
      {:error/type
       type}

       (some?
        details)
       (assoc
        :error/details
        details))))))

(defn- now!
  [ctx]
  (or
   (:biff.fx/now
    ctx)

   (fail!
    :request.fx/missing-now
    "Request planning requires :biff.fx/now.")))

(defn- seed!
  [ctx]
  (or
   (:biff.fx/seed
    ctx)

   (fail!
    :request.fx/missing-seed
    "Request planning that creates an entity requires :biff.fx/seed.")))

(defn- generated-id
  [ctx]
  (first
   (fx/uuid7
    (seed!
     ctx)
    (now!
     ctx))))

(defn- require-uuid!
  [value type message details]
  (when-not
   (uuid?
    value)
    (fail!
     type
     message
     details))
  value)

(defn- actor-id!
  [ctx]
  (require-uuid!
   (:current-user/id
    ctx)

   :request/not-authenticated

   "A signed-in User is required."

   {:current-user/id
    (:current-user/id
     ctx)}))

(defn- require-request-id!
  [request-id]
  (require-uuid!
   request-id

   :request/invalid-request-id

   "Request ID must be a UUID."

   {:request/id
    request-id}))

(defn- require-helper-id!
  [helper-id]
  (require-uuid!
   helper-id

   :request/invalid-helper

   "Helper ID must be a User UUID."

   {:helper-id
    helper-id}))

;; =============================================================================
;; Semantic Gesso Live changes
;; =============================================================================

(defn- without-nils
  [value]
  (into
   {}
   (remove
    (comp
     nil?
     val))
   value))

(defn- change-entry
  [{:keys
    [topic
     id]}]
  {:coalesce-key
   [topic
    id]})

(defn- request-change
  [before after operation]
  (without-nils
   {:topic
    :request

    :id
    (request/request-id
     after)

    :change/kind
    (if
     before
      :updated
      :created)

    :request/operation
    operation

    :request/id
    (request/request-id
     after)

    :request/organization-id
    (request/organization-id
     after)

    :request/location-id
    (request/location-id
     after)

    :request/requestor-type
    (request/requestor-type
     after)

    :request/requestor-id
    (request/requestor-id
     after)

    :request/status
    (request/request-status
     after)

    :request/previous-status
    (some->
     before
     request/request-status)

    :request/revision
    (request/request-revision
     after)}))

(defn- assignment-change
  [before after operation]
  (without-nils
   {:topic
    :request

    ;; Assignment changes deliberately coalesce with the owning Request.
    :id
    (request/assignment-request-id
     after)

    :change/kind
    :updated

    :request/operation
    operation

    :request/id
    (request/assignment-request-id
     after)

    :request-assignment/id
    (request/assignment-id
     after)

    :request-assignment/helper
    (request/assignment-helper-id
     after)

    :request-assignment/role
    (request/assignment-role
     after)

    :request-assignment/status
    (request/assignment-status
     after)

    :request-assignment/previous-status
    (some->
     before
     request/assignment-status)}))

;; =============================================================================
;; Plan assembly
;; =============================================================================

(defn- plan
  [{:keys
    [result
     dependencies
     commands
     assertions
     changes]}]
  {:result
   result

   :transaction-fragment
   (apply
    model.tx/compose
    (concat
     dependencies

     [(apply
       model.tx/commands-fragment
       commands)

      (apply
       model.tx/assertions-fragment
       assertions)

      (apply
       model.tx/changes-fragment
       changes)]))

   :transaction-options
   {:entry-fn
    change-entry}})

;; =============================================================================
;; Guarded Request dependencies
;; =============================================================================

(defn request-dependency
  "Returns the current Request and a guard-only transaction fragment.

   Returns nil when Request does not exist."
  [ctx request-id]
  (when-let [request-document
             (request.graph/request-document
              ctx
              (require-request-id!
               request-id))]
    {:request
     request-document

     :transaction-fragment
     (model.tx/guards-fragment
      (command/guard
       request/request-entity-type
       request-document
       request/request-version))}))

(defn require-request-dependency
  "Returns request-dependency or throws when Request does not exist."
  [ctx request-id]
  (or
   (request-dependency
    ctx
    request-id)

   (fail!
    :request/not-found
    "The Request does not exist."
    {:request/id
     request-id})))

(defn assignment-dependency
  "Returns one RequestAssignment and a guard-only transaction fragment.

   Returns nil when the assignment does not exist."
  [ctx assignment-id]
  (when-not
   (uuid?
    assignment-id)
    (fail!
     :request-assignment/invalid-assignment-id
     "RequestAssignment ID must be a UUID."
     {:request-assignment/id
      assignment-id}))

  (when-let [assignment
             (request.graph/request-assignment-document
              ctx
              assignment-id)]
    {:assignment
     assignment

     :transaction-fragment
     (model.tx/guards-fragment
      (command/guard
       request/assignment-entity-type
       assignment
       request/assignment-version))}))

(defn require-assignment-dependency
  "Returns assignment-dependency or throws when RequestAssignment is absent."
  [ctx assignment-id]
  (or
   (assignment-dependency
    ctx
    assignment-id)

   (fail!
    :request-assignment/not-found
    "The RequestAssignment does not exist."
    {:request-assignment/id
     assignment-id})))

(defn request-snapshot-dependency
  "Returns Request's complete active Request-owned aggregate snapshot plus
   guards for the Request and every active RequestAssignment observed.

   Request is the aggregate concurrency root. Every supported operation that
   changes active RequestAssignments also updates Request revision.

   Returns nil when Request does not exist."
  [ctx request-id]
  (when-let [{:keys
              [request
               assignments
               primary-assignment
               active-helper-ids
               active-collaborator-helper-ids]}
             (request.graph/request-snapshot
              ctx
              (require-request-id!
               request-id))]

    {:request
     request

     :assignments
     assignments

     :primary-assignment
     primary-assignment

     :active-helper-ids
     active-helper-ids

     :active-collaborator-helper-ids
     active-collaborator-helper-ids

     :transaction-fragment
     (apply
      model.tx/guards-fragment
      (cons
       (command/guard
        request/request-entity-type
        request
        request/request-version)

       (map
        #(command/guard
          request/assignment-entity-type
          %
          request/assignment-version)
        assignments)))}))

(defn require-request-snapshot-dependency
  "Returns request-snapshot-dependency or throws when Request is absent."
  [ctx request-id]
  (or
   (request-snapshot-dependency
    ctx
    request-id)

   (fail!
    :request/not-found
    "The Request does not exist."
    {:request/id
     request-id})))

;; =============================================================================
;; Organization Location dependency
;; =============================================================================

(defn- location-dependency
  [ctx organization-id location-id require-operational?]
  (require-uuid!
   organization-id

   :request/invalid-organization

   "Request Organization ID must be a UUID."

   {:organization/id
    organization-id})

  (require-uuid!
   location-id

   :request/invalid-location

   "Request Location ID must be a UUID."

   {:location/id
    location-id})

  (let [scope
        (organization/location-scope
         location-id)

        {:keys
         [scope-context
          transaction-fragment]
         :as dependency}
        (organization/require-scope-dependency
         ctx
         scope)

        actual-organization-id
        (organization/scope-context-organization-id
         scope-context)]

    (when-not
     (=
      organization-id
      actual-organization-id)
      (fail!
       :request/location-organization-mismatch
       "The Request Location belongs to another Organization."
       {:request/organization-id
        organization-id

        :location/id
        location-id

        :location/organization-id
        actual-organization-id}))

    (when-not
     (organization/same-scope?
      scope
      (organization/scope-context-target
       scope-context))
      (fail!
       :request.fx/inconsistent-location-dependency
       "Organization returned a scope dependency for another Location."
       {:expected-scope
        scope

        :actual-scope
        (organization/scope-context-target
         scope-context)}))

    (when
     (and
      require-operational?

      (not
       (organization/scope-context-operational?
        scope-context)))
      (fail!
       :request/location-not-operational
       "The Request Location is not operational."
       {:organization/id
        organization-id

        :location/id
        location-id}))

    (assoc
     dependency
     :scope
     scope
     :scope-context
     scope-context
     :transaction-fragment
     transaction-fragment)))

(defn- request-location-dependency
  [ctx request-document operation]
  (location-dependency
   ctx
   (request/organization-id
    request-document)
   (request/location-id
    request-document)
   (contains?
    operational-location-operations
    operation)))

;; =============================================================================
;; User identity dependency
;; =============================================================================

(defn- active-user-dependency
  [ctx user-id]
  (let [{:keys
         [user
          transaction-fragment]
         :as dependency}
        (user/require-user-dependency
         ctx
         user-id)]

    (when-not
     (user/active?
      user)
      (fail!
       :user/not-active
       "Only an active User may perform this Request operation."
       {:user/id
        user-id

        :user/status
        (user/user-status
         user)}))

    (assoc
     dependency
     :user
     user
     :transaction-fragment
     transaction-fragment)))

;; =============================================================================
;; Request owner authorization
;; =============================================================================

(defn- owner-dependency
  [ctx request-document]
  (let [actor-id
        (actor-id!
         ctx)

        requestor
        (request/requestor
         request-document)]

    (when
     (request/capability-requestor?
      requestor)
      (fail!
       :request/capability-authorization-unavailable
       "Capability-owned Request writes are not implemented."
       {:request/id
        (request/request-id
         request-document)

        :requestor
        requestor}))

    (when-not
     (request/requested-by-user?
      request-document
      actor-id)
      (fail!
       :request/not-authorized
       "Only the User who created this Request may perform this operation."
       {:request/id
        (request/request-id
         request-document)

        :user/id
        actor-id}))

    (active-user-dependency
     ctx
     actor-id)))

;; =============================================================================
;; Membership role authorization
;; =============================================================================

(defn- helper-dependency
  [ctx helper-id scope]
  (membership/require-helper-dependency
   ctx
   helper-id
   scope))

(defn- manager-dependency
  "Returns a dependency proving exact supervisor or administrator authority.

   HumanHelp deliberately defines no implicit role hierarchy. Either exact
   role is sufficient."
  [ctx actor-id scope]
  (or
   (membership/supervisor-dependency
    ctx
    actor-id
    scope)

   (membership/admin-dependency
    ctx
    actor-id
    scope)

   (fail!
    :request/not-authorized
    "Supervisor or administrator authority is required."
    {:user/id
     actor-id

     :scope
     scope

     :required-roles
     #{:supervisor
       :admin}})))

;; =============================================================================
;; Skill requirement
;; =============================================================================

(defn- normalize-required-skill!
  [value]
  (when
   (some?
    value)
    (let [skill
          (membership/normalize-skill
           value)]
      (when-not
       (membership/skill?
        skill)
        (fail!
         :request/invalid-skill
         "The requested helper skill is invalid."
         {:skill
          value}))
      skill)))

(defn- require-helper-skill!
  [helper-dependency required-skill]
  (when
   required-skill
    (let [membership-document
          (:membership
           helper-dependency)]

      (when-not
       membership-document
        (fail!
         :request.fx/incomplete-helper-dependency
         "Membership did not return the Membership that established helper authority."
         {:helper-id
          (some->
           helper-dependency
           :user
           user/user-id)

          :skill
          required-skill}))

      (when-not
       (membership/membership-has-skill?
        membership-document
        required-skill)
        (fail!
         :request/helper-missing-skill
         "The selected helper does not have the required Organization skill."
         {:user/id
          (membership/membership-user-id
           membership-document)

          :organization/id
          (membership/membership-organization-id
           membership-document)

          :skill
          required-skill}))))

  helper-dependency)

(defn- eligible-helper-dependency
  [ctx helper-id scope skill]
  (-> (helper-dependency
       ctx
       helper-id
       scope)
      (require-helper-skill!
       skill)))

;; =============================================================================
;; Primary assignment authorization
;; =============================================================================

(defn- require-primary!
  [snapshot]
  (or
   (:primary-assignment
    snapshot)

   (fail!
    :request/missing-primary-assignment
    "The Request requires an active primary helper."
    {:request/id
     (some->
      snapshot
      :request
      request/request-id)

     :request/status
     (some->
      snapshot
      :request
      request/request-status)})))

(defn- require-primary-owned-by!
  [snapshot user-id]
  (let [primary
        (require-primary!
         snapshot)]

    (when-not
     (request/assignment-for-helper?
      primary
      user-id)
      (fail!
       :request/not-authorized
       "Only the active primary helper may perform this operation."
       {:request/id
        (request/request-id
         (:request
          snapshot))

        :user/id
        user-id

        :request/primary-helper
        (request/assignment-helper-id
         primary)}))

    primary))

(defn- primary-user-dependency
  "Proves primary participation and current active User identity.

   This intentionally does not require current helper role authority."
  [ctx snapshot]
  (let [actor-id
        (actor-id!
         ctx)

        primary
        (require-primary-owned-by!
         snapshot
         actor-id)

        user-dependency
        (active-user-dependency
         ctx
         actor-id)]

    {:actor-id
     actor-id

     :primary-assignment
     primary

     :user
     (:user
      user-dependency)

     :transaction-fragment
     (:transaction-fragment
      user-dependency)}))

(defn- effective-primary-helper-dependency
  "Proves primary participation plus current helper authority."
  [ctx snapshot scope]
  (let [actor-id
        (actor-id!
         ctx)

        primary
        (require-primary-owned-by!
         snapshot
         actor-id)

        helper
        (helper-dependency
         ctx
         actor-id
         scope)]

    {:actor-id
     actor-id

     :primary-assignment
     primary

     :helper
     helper

     :transaction-fragment
     (:transaction-fragment
      helper)}))

;; =============================================================================
;; Request aggregate serialization
;; =============================================================================

(defn- touch-request-command
  "Creates a semantic Request update whose only persisted change is version.

   Assignment-only operations still mutate the Request aggregate, so Request
   revision serializes all supported changes to the active RequestAssignment
   set.

   The operation remains visible in the canonical model command and Live
   change even though Request lifecycle/status is unchanged."
  [request-document operation now]
  (command/update-command
   request/request-entity-type
   operation
   request-document
   (command/bump-version
    request-document
    request/request-version
    now)
   request/request-version))

;; =============================================================================
;; Assignment command helpers
;; =============================================================================

(defn- create-assignment-command
  [ctx {:keys
        [request-id
         helper-id
         role
         source
         actor-id]}]
  (request/create-assignment-command
   {:id
    (generated-id
     ctx)

    :request-id
    request-id

    :helper-id
    helper-id

    :role
    role

    :source
    source

    :actor-id
    actor-id

    :now
    (now!
     ctx)}))

(defn- end-assignment-commands
  [assignments actor-id reason now]
  (mapv
   #(request/end-assignment-command
     %
     {:actor-id
      actor-id

      :reason
      reason

      :now
      now})
   assignments))

(defn- assignment-changes
  [model-commands operation]
  (mapv
   #(assignment-change
     (command/before
      %)
     (command/after
      %)
     operation)
   model-commands))

;; =============================================================================
;; Persistence assertions
;; =============================================================================

(defn- assert-no-active-assignments
  [request-id]
  (model.tx/assert-none
   request/assignment-entity-type
   [:and
    [:=
     :request-assignment/request
     request-id]

    [:=
     :request-assignment/status
     :active]]))

(defn- assert-no-active-assignment-for-helper
  [request-id helper-id]
  (model.tx/assert-none
   request/assignment-entity-type
   [:and
    [:=
     :request-assignment/request
     request-id]

    [:=
     :request-assignment/helper
     helper-id]

    [:=
     :request-assignment/status
     :active]]))

;; =============================================================================
;; Creation
;; =============================================================================

(defn plan-create-request
  "Plans creation of a User-owned Request at an operational Location.

   input:

     {:organization-id uuid
      :location-id     uuid
      :content         {:title string
                        :details optional-string
                        :location-detail optional-string}}"
  [ctx {:keys
        [organization-id
         location-id
         content]}]
  (let [now
        (now!
         ctx)

        actor-id
        (actor-id!
         ctx)

        actor
        (active-user-dependency
         ctx
         actor-id)

        location
        (location-dependency
         ctx
         organization-id
         location-id
         true)

        model-command
        (request/create-request-command
         {:id
          (generated-id
           ctx)

          :organization-id
          organization-id

          :location-id
          location-id

          :requestor
          (request/user-requestor
           actor-id)

          :content
          content

          :now
          now})

        document
        (command/after
         model-command)]

    (plan
     {:result
      {:request
       document}

      :dependencies
      [(:transaction-fragment
        actor)

       (:transaction-fragment
        location)]

      :commands
      [model-command]

      :assertions
      []

      :changes
      [(request-change
        nil
        document
        :create)]})))

;; =============================================================================
;; Edit
;; =============================================================================

(defn plan-edit-request
  "Plans editing Request-owned customer content.

   Only the active User requestor may edit. The Request Location must remain
   operational."
  [ctx {:keys
        [request-id
         content]}]
  (let [now
        (now!
         ctx)

        {:keys
         [request
          transaction-fragment]}
        (require-request-dependency
         ctx
         request-id)

        location
        (request-location-dependency
         ctx
         request
         :edit)

        owner
        (owner-dependency
         ctx
         request)

        model-command
        (request/edit-request-command
         request
         {:content
          content

          :now
          now})

        after
        (command/after
         model-command)]

    (plan
     {:result
      {:request
       after}

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        owner)]

      :commands
      [model-command]

      :assertions
      []

      :changes
      [(request-change
        request
        after
        :edit)]})))

;; =============================================================================
;; Claim
;; =============================================================================

(defn plan-claim-request
  "Plans claiming an open Request and creating its primary RequestAssignment.

   Without :helper-id, the signed-in helper claims personally.

   With another :helper-id, the actor must hold exact supervisor or admin
   authority at the Location and the target must hold exact helper authority.

   Optional :skill requires the target helper's active Membership to carry that
   Organization-local skill."
  [ctx {:keys
        [request-id
         helper-id
         skill]}]
  (let [now
        (now!
         ctx)

        actor-id
        (actor-id!
         ctx)

        {:keys
         [request
          assignments
          transaction-fragment]
         :as snapshot}
        (require-request-snapshot-dependency
         ctx
         request-id)

        _
        (when
         (seq
          assignments)
          (fail!
           :request/assignments-already-active
           "An open Request cannot be claimed while active assignments exist."
           {:request/id
            request-id

            :request/active-assignment-ids
            (mapv
             request/assignment-id
             assignments)}))

        location
        (request-location-dependency
         ctx
         request
         :claim)

        scope
        (:scope
         location)

        target-helper-id
        (if
         (some?
          helper-id)
          (require-helper-id!
           helper-id)
          actor-id)

        required-skill
        (normalize-required-skill!
         skill)

        self-claim?
        (=
         actor-id
         target-helper-id)

        manager
        (when-not
         self-claim?
          (manager-dependency
           ctx
           actor-id
           scope))

        target-helper
        (eligible-helper-dependency
         ctx
         target-helper-id
         scope
         required-skill)

        request-command
        (request/claim-request-command
         request
         {:now
          now})

        assignment-command
        (create-assignment-command
         ctx
         {:request-id
          request-id

          :helper-id
          target-helper-id

          :role
          :primary

          :source
          (if
           self-claim?
            :request/claim
            :request/manager-claim)

          :actor-id
          actor-id})

        after-request
        (command/after
         request-command)

        primary-assignment
        (command/after
         assignment-command)]

    (plan
     {:result
      (cond->
       {:request
        after-request

        :primary-assignment
        primary-assignment}

        required-skill
        (assoc
         :required-skill
         required-skill))

      :dependencies
      (cond->
       [transaction-fragment

        (:transaction-fragment
         location)

        (:transaction-fragment
         target-helper)]

        manager
        (conj
         (:transaction-fragment
          manager)))

      :commands
      [request-command
       assignment-command]

      :assertions
      [(assert-no-active-assignments
        request-id)]

      :changes
      [(request-change
        request
        after-request
        :claim)

       (assignment-change
        nil
        primary-assignment
        :claim)]})))

;; =============================================================================
;; Unclaim
;; =============================================================================

(defn plan-unclaim-request
  "Plans returning a claimed Request to open and ending every active
   RequestAssignment.

   The actor must own the active primary assignment and still be an active User.
   A current helper role is deliberately not required so role removal cannot
   strand an assigned Request."
  [ctx {:keys
        [request-id]}]
  (let [now
        (now!
         ctx)

        {:keys
         [request
          assignments
          transaction-fragment]
         :as snapshot}
        (require-request-snapshot-dependency
         ctx
         request-id)

        location
        (request-location-dependency
         ctx
         request
         :unclaim)

        primary-user
        (primary-user-dependency
         ctx
         snapshot)

        actor-id
        (:actor-id
         primary-user)

        request-command
        (request/unclaim-request-command
         request
         {:now
          now})

        assignment-commands
        (end-assignment-commands
         assignments
         actor-id
         :request/unclaimed
         now)

        after-request
        (command/after
         request-command)]

    (plan
     {:result
      {:request
       after-request

       :assignments
       (mapv
        command/after
        assignment-commands)}

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        primary-user)]

      :commands
      (into
       [request-command]
       assignment-commands)

      :assertions
      []

      :changes
      (into
       [(request-change
         request
         after-request
         :unclaim)]

       (assignment-changes
        assignment-commands
        :unclaim))})))

;; =============================================================================
;; Mark on the way
;; =============================================================================

(defn plan-mark-request-on-the-way
  "Plans moving a claimed Request to :on-the-way.

   The actor must be the active primary helper and still have effective helper
   authority at the operational Request Location."
  [ctx {:keys
        [request-id]}]
  (let [now
        (now!
         ctx)

        {:keys
         [request
          transaction-fragment]
         :as snapshot}
        (require-request-snapshot-dependency
         ctx
         request-id)

        location
        (request-location-dependency
         ctx
         request
         :mark-on-the-way)

        primary-helper
        (effective-primary-helper-dependency
         ctx
         snapshot
         (:scope
          location))

        model-command
        (request/mark-on-the-way-command
         request
         {:now
          now})

        after
        (command/after
         model-command)]

    (plan
     {:result
      {:request
       after}

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        primary-helper)]

      :commands
      [model-command]

      :assertions
      []

      :changes
      [(request-change
        request
        after
        :mark-on-the-way)]})))

;; =============================================================================
;; Complete
;; =============================================================================

(defn plan-complete-request
  "Plans completing a claimed or on-the-way Request and ending every active
   RequestAssignment.

   The actor must be the active primary helper and still have effective helper
   authority at the operational Location."
  [ctx {:keys
        [request-id]}]
  (let [now
        (now!
         ctx)

        {:keys
         [request
          assignments
          transaction-fragment]
         :as snapshot}
        (require-request-snapshot-dependency
         ctx
         request-id)

        location
        (request-location-dependency
         ctx
         request
         :complete)

        primary-helper
        (effective-primary-helper-dependency
         ctx
         snapshot
         (:scope
          location))

        actor-id
        (:actor-id
         primary-helper)

        request-command
        (request/complete-request-command
         request
         {:now
          now})

        assignment-commands
        (end-assignment-commands
         assignments
         actor-id
         :request/completed
         now)

        after-request
        (command/after
         request-command)]

    (plan
     {:result
      {:request
       after-request

       :assignments
       (mapv
        command/after
        assignment-commands)}

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        primary-helper)]

      :commands
      (into
       [request-command]
       assignment-commands)

      :assertions
      []

      :changes
      (into
       [(request-change
         request
         after-request
         :complete)]

       (assignment-changes
        assignment-commands
        :complete))})))

;; =============================================================================
;; Cancel
;; =============================================================================

(defn plan-cancel-request
  "Plans cancellation by the active User requestor.

   The Location must still exist and belong to the Request Organization, but it
   need not remain operational. Every active RequestAssignment ends atomically
   with cancellation."
  [ctx {:keys
        [request-id
         reason]}]
  (let [now
        (now!
         ctx)

        {:keys
         [request
          assignments
          transaction-fragment]}
        (require-request-snapshot-dependency
         ctx
         request-id)

        location
        (request-location-dependency
         ctx
         request
         :cancel)

        owner
        (owner-dependency
         ctx
         request)

        actor-id
        (actor-id!
         ctx)

        request-command
        (request/cancel-request-command
         request
         {:now
          now

          :reason
          reason})

        assignment-commands
        (end-assignment-commands
         assignments
         actor-id
         :request/cancelled
         now)

        after-request
        (command/after
         request-command)]

    (plan
     {:result
      {:request
       after-request

       :assignments
       (mapv
        command/after
        assignment-commands)}

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        owner)]

      :commands
      (into
       [request-command]
       assignment-commands)

      :assertions
      []

      :changes
      (into
       [(request-change
         request
         after-request
         :cancel)]

       (assignment-changes
        assignment-commands
        :cancel))})))

;; =============================================================================
;; Add collaborator
;; =============================================================================

(defn plan-add-collaborator
  "Plans adding one effective helper as a collaborator.

   The Request must currently be assigned. The actor must be the active primary
   helper and retain effective helper authority.

   input:

     {:request-id uuid
      :helper-id  uuid
      :skill      optional organization-local skill}"
  [ctx {:keys
        [request-id
         helper-id
         skill]}]
  (let [now
        (now!
         ctx)

        actor-id
        (actor-id!
         ctx)

        helper-id
        (require-helper-id!
         helper-id)

        {:keys
         [request
          assignments
          transaction-fragment]
         :as snapshot}
        (require-request-snapshot-dependency
         ctx
         request-id)

        _
        (when-not
         (or
          (request/claimed?
           request)

          (request/on-the-way?
           request))
          (fail!
           :request/not-assigned
           "Collaborators can be added only to an assigned Request."
           {:request/id
            request-id

            :request/status
            (request/request-status
             request)}))

        location
        (request-location-dependency
         ctx
         request
         :add-collaborator)

        primary-helper
        (effective-primary-helper-dependency
         ctx
         snapshot
         (:scope
          location))

        _
        (when
         (=
          actor-id
          helper-id)
          (fail!
           :request/helper-already-assigned
           "The primary helper is already assigned to this Request."
           {:request/id
            request-id

            :helper-id
            helper-id}))

        _
        (when
         (request/active-assignment-for-helper
          assignments
          helper-id)
          (fail!
           :request/helper-already-assigned
           "The helper already has an active assignment on this Request."
           {:request/id
            request-id

            :helper-id
            helper-id}))

        required-skill
        (normalize-required-skill!
         skill)

        target-helper
        (eligible-helper-dependency
         ctx
         helper-id
         (:scope
          location)
         required-skill)

        request-command
        (touch-request-command
         request
         :add-collaborator
         now)

        collaborator-command
        (create-assignment-command
         ctx
         {:request-id
          request-id

          :helper-id
          helper-id

          :role
          :collaborator

          :source
          :request/collaboration

          :actor-id
          actor-id})

        after-request
        (command/after
         request-command)

        collaborator
        (command/after
         collaborator-command)]

    (plan
     {:result
      (cond->
       {:request
        after-request

        :collaborator-assignment
        collaborator}

        required-skill
        (assoc
         :required-skill
         required-skill))

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        primary-helper)

       (:transaction-fragment
        target-helper)]

      :commands
      [request-command
       collaborator-command]

      :assertions
      [(assert-no-active-assignment-for-helper
        request-id
        helper-id)]

      :changes
      [(request-change
        request
        after-request
        :add-collaborator)

       (assignment-change
        nil
        collaborator
        :add-collaborator)]})))

;; =============================================================================
;; Remove collaborator
;; =============================================================================

(defn plan-remove-collaborator
  "Plans ending one active collaborator assignment.

   The actor must be the active primary helper. Current helper-role authority
   and Location operational state are intentionally unnecessary, allowing an
   existing assignment relationship to be cleaned up after access changes."
  [ctx {:keys
        [request-id
         helper-id]}]
  (let [now
        (now!
         ctx)

        helper-id
        (require-helper-id!
         helper-id)

        {:keys
         [request
          assignments
          transaction-fragment]
         :as snapshot}
        (require-request-snapshot-dependency
         ctx
         request-id)

        _
        (when-not
         (or
          (request/claimed?
           request)

          (request/on-the-way?
           request))
          (fail!
           :request/not-assigned
           "A collaborator can be removed only from an assigned Request."
           {:request/id
            request-id

            :request/status
            (request/request-status
             request)}))

        location
        (request-location-dependency
         ctx
         request
         :remove-collaborator)

        primary-user
        (primary-user-dependency
         ctx
         snapshot)

        actor-id
        (:actor-id
         primary-user)

        collaborator
        (or
         (some
          #(when
            (and
             (request/active-collaborator-assignment?
              %)

             (request/assignment-for-helper?
              %
              helper-id))
            %)
          assignments)

         (fail!
          :request/collaborator-not-found
          "The helper is not an active collaborator on this Request."
          {:request/id
           request-id

           :helper-id
           helper-id}))

        request-command
        (touch-request-command
         request
         :remove-collaborator
         now)

        collaborator-command
        (request/end-assignment-command
         collaborator
         {:actor-id
          actor-id

          :reason
          :request/collaborator-removed

          :now
          now})

        after-request
        (command/after
         request-command)

        ended-collaborator
        (command/after
         collaborator-command)]

    (plan
     {:result
      {:request
       after-request

       :collaborator-assignment
       ended-collaborator}

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        primary-user)]

      :commands
      [request-command
       collaborator-command]

      :assertions
      []

      :changes
      [(request-change
        request
        after-request
        :remove-collaborator)

       (assignment-change
        collaborator
        ended-collaborator
        :remove-collaborator)]})))

;; =============================================================================
;; Reassign primary helper
;; =============================================================================

(defn plan-reassign-request
  "Plans replacement of the primary helper on a claimed Request.

   The actor must hold exact supervisor or administrator authority at the
   Request Location. The selected User must hold effective helper authority.

   If the selected helper is currently a collaborator, that assignment is
   ended before the new primary assignment is created in the same atomic
   transaction.

   Reassignment deliberately remains limited to :claimed Requests. Once a
   helper has marked a Request :on-the-way, changing responsibility is a
   distinct product workflow rather than silently reusing manager reassignment."
  [ctx {:keys
        [request-id
         helper-id
         skill]}]
  (let [now
        (now!
         ctx)

        actor-id
        (actor-id!
         ctx)

        target-helper-id
        (require-helper-id!
         helper-id)

        {:keys
         [request
          assignments
          primary-assignment
          transaction-fragment]}
        (require-request-snapshot-dependency
         ctx
         request-id)

        _
        (when-not
         (request/claimed?
          request)
          (fail!
           :request/not-reassignable
           "Primary reassignment requires a claimed Request."
           {:request/id
            request-id

            :request/status
            (request/request-status
             request)}))

        current-primary
        (or
         primary-assignment

         (fail!
          :request/missing-primary-assignment
          "The claimed Request has no active primary helper."
          {:request/id
           request-id}))

        _
        (when
         (request/assignment-for-helper?
          current-primary
          target-helper-id)
          (fail!
           :request/helper-already-primary
           "The selected helper is already the primary helper."
           {:request/id
            request-id

            :helper-id
            target-helper-id}))

        location
        (request-location-dependency
         ctx
         request
         :reassign)

        manager
        (manager-dependency
         ctx
         actor-id
         (:scope
          location))

        required-skill
        (normalize-required-skill!
         skill)

        target-helper
        (eligible-helper-dependency
         ctx
         target-helper-id
         (:scope
          location)
         required-skill)

        target-existing
        (request/active-assignment-for-helper
         assignments
         target-helper-id)

        _
        (when
         (and
          target-existing

          (not
           (request/active-collaborator-assignment?
            target-existing)))
          (fail!
           :request/helper-already-assigned
           "The selected helper already has an incompatible active assignment."
           {:request/id
            request-id

            :helper-id
            target-helper-id

            :request-assignment/id
            (request/assignment-id
             target-existing)

            :request-assignment/role
            (request/assignment-role
             target-existing)}))

        assignments-to-end
        (cond->
         [current-primary]

          target-existing
          (conj
           target-existing))

        end-commands
        (end-assignment-commands
         assignments-to-end
         actor-id
         :request/reassigned
         now)

        request-command
        (touch-request-command
         request
         :reassign
         now)

        new-primary-command
        (create-assignment-command
         ctx
         {:request-id
          request-id

          :helper-id
          target-helper-id

          :role
          :primary

          :source
          :request/reassignment

          :actor-id
          actor-id})

        after-request
        (command/after
         request-command)

        ended-primary
        (command/after
         (first
          end-commands))

        new-primary
        (command/after
         new-primary-command)

        all-assignment-commands
        (conj
         end-commands
         new-primary-command)]

    (plan
     {:result
      (cond->
       {:request
        after-request

        :previous-primary-assignment
        ended-primary

        :primary-assignment
        new-primary}

        target-existing
        (assoc
         :previous-collaborator-assignment
         (command/after
          (second
           end-commands)))

        required-skill
        (assoc
         :required-skill
         required-skill))

      :dependencies
      [transaction-fragment

       (:transaction-fragment
        location)

       (:transaction-fragment
        manager)

       (:transaction-fragment
        target-helper)]

      :commands
      (into
       [request-command]
       all-assignment-commands)

      :assertions
      []

      :changes
      (into
       [(request-change
         request
         after-request
         :reassign)]

       (concat
        (assignment-changes
         end-commands
         :reassign)

        [(assignment-change
          nil
          new-primary
          :reassign)]))})))
