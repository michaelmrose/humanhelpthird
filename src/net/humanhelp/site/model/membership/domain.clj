(ns net.humanhelp.site.model.membership.domain
  "Pure rules for HumanHelp Organization membership and role assignments.

   Membership owns the durable relationship between one User and one
   Organization. It also owns organization-local skills and the RoleAssignment
   entities that grant authority to that membership at Organization scopes.

   Organization remains the authority for scope structure and hierarchy
   semantics. User remains the authority for global identity/account state.

   This namespace is pure. It performs no database reads, authorization reads,
   Graph queries, transaction execution, or cross-model mutation. Cross-model
   existence, operational-state, and concurrency checks belong to
   membership.fx and are obtained through the other models' public cores."
  (:require
   [clojure.string :as str]
   [gesso.model.command :as command]
   [net.humanhelp.site.model.organization.core :as organization])
  (:import
   [java.time Instant]
   [java.util Locale]))

;; =============================================================================
;; Entity identity and versioning
;; =============================================================================

(def membership-entity-type
  :membership)

(def membership-version
  {:revision-key :membership/revision
   :created-at-key :membership/created-at
   :updated-at-key :membership/updated-at})

(def role-assignment-entity-type
  :role-assignment)

(def role-assignment-version
  {:revision-key :role-assignment/revision
   :created-at-key :role-assignment/created-at
   :updated-at-key :role-assignment/updated-at})

;; =============================================================================
;; Organization-local skill values
;; =============================================================================

(def skill-max
  120)

(defn normalize-skill
  "Canonicalizes an organization-local skill name.

   Skills are case-insensitive organization-local labels. HumanHelp deliberately
   assigns no universal meaning to the canonical string.

   Blank strings normalize to nil. Non-string values are left unchanged so
   validation rejects them rather than silently discarding malformed input."
  [value]
  (cond
    (nil? value)
    nil

    (string? value)
    (let [value
          (.toLowerCase
           ^String
           (str/trim value)
           Locale/ROOT)]
      (when-not
       (str/blank? value)
        value))

    :else
    value))

(defn skill?
  [value]
  (and
   (string? value)
   (= value (normalize-skill value))
   (<= (count value) skill-max)))

(defn normalize-skills
  "Canonicalizes a collection of organization-local skills to a set.

   Invalid elements remain invalid after normalization so skills? can reject
   the complete value."
  [values]
  (cond
    (nil? values)
    nil

    (coll? values)
    (into
     #{}
     (map normalize-skill)
     values)

    :else
    values))

(defn skills?
  [value]
  (and
   (set? value)
   (every? skill? value)))

;; =============================================================================
;; Role values
;; =============================================================================

(def ^:private roles
  #{:helper
    :supervisor
    :admin})

(defn role?
  [value]
  (contains? roles value))

;; =============================================================================
;; Shared local invariant helpers
;; =============================================================================

(defn- instant?
  [value]
  (instance? Instant value))

(defn- at-or-before?
  [^Instant left ^Instant right]
  (not
   (.isAfter left right)))

(defn- optional-uuid?
  [value]
  (or
   (nil? value)
   (uuid? value)))

(defn- optional-reason?
  [value]
  (or
   (nil? value)
   (qualified-keyword? value)))

(defn- none-present?
  [document keys]
  (every?
   #(nil? (get document %))
   keys))

(defn- optional-time-within?
  [^Instant created-at value ^Instant updated-at]
  (or
   (nil? value)
   (and
    (instant? value)
    (at-or-before? created-at value)
    (at-or-before? value updated-at))))

(defn- version-consistent?
  [document version]
  (let [{:keys
         [created-at-key
          updated-at-key]}
        version

        created-at
        (get document created-at-key)

        updated-at
        (get document updated-at-key)]
    (and
     (command/versioned-document?
      document
      version)
     (instant? created-at)
     (instant? updated-at)
     (at-or-before? created-at updated-at))))

(defn- valid-change-time?
  [document version now]
  (let [updated-at
        (get
         document
         (:updated-at-key version))]
    (and
     (instant? now)
     (instant? updated-at)
     (at-or-before? updated-at now))))

(defn- audit-consistent?
  [document at-key by-key reason-key]
  (let [at
        (get document at-key)

        by
        (get document by-key)

        reason
        (get document reason-key)]
    (and
     (or
      (nil? at)
      (instant? at))

     (optional-uuid? by)

     (optional-reason? reason)

     (or
      (some? at)
      (and
       (nil? by)
       (nil? reason))))))

(defn- fail!
  [error-type message errors context]
  (throw
   (ex-info
    message
    {:error/type error-type
     :error/details
     {:errors errors
      :context context}})))

(defn- ensure!
  [test error-type message errors context]
  (when-not test
    (fail!
     error-type
     message
     errors
     context)))

;; =============================================================================
;; Membership lifecycle and facts
;; =============================================================================

(def ^:private membership-statuses
  #{:active
    :suspended
    :revoked})

(def ^:private membership-transitions
  {[:active :suspend] :suspended
   [:suspended :reactivate] :active
   [:active :revoke] :revoked
   [:suspended :revoke] :revoked})

(defn membership-status?
  [value]
  (contains? membership-statuses value))

(defn membership-id
  [membership]
  (:xt/id membership))

(defn membership-user-id
  [membership]
  (:membership/user membership))

(defn membership-organization-id
  [membership]
  (:membership/organization membership))

(defn membership-status
  [membership]
  (:membership/status membership))

(defn membership-skills
  [membership]
  (:membership/skills membership))

(defn membership-active?
  [membership]
  (= :active (membership-status membership)))

(defn membership-suspended?
  [membership]
  (= :suspended (membership-status membership)))

(defn membership-revoked?
  [membership]
  (= :revoked (membership-status membership)))

(defn membership-for-user?
  [membership expected-user-id]
  (= expected-user-id
     (membership-user-id membership)))

(defn membership-for-organization?
  [membership expected-organization-id]
  (= expected-organization-id
     (membership-organization-id membership)))

(defn membership-relates?
  [membership expected-user-id expected-organization-id]
  (and
   (membership-for-user? membership expected-user-id)
   (membership-for-organization? membership expected-organization-id)))

(defn membership-has-skill?
  [membership skill]
  (let [skill
        (normalize-skill skill)]
    (and
     (skill? skill)
     (contains?
      (membership-skills membership)
      skill))))

(defn- membership-can-transition?
  [membership operation]
  (contains?
   membership-transitions
   [(membership-status membership)
    operation]))

;; =============================================================================
;; Membership document invariants
;; =============================================================================

(defn membership-document-consistent?
  "Returns true when membership satisfies every local persisted invariant.

   This does not establish that the referenced User or Organization exists,
   that either is operational, or that the User has only one current membership
   in the Organization."
  [membership]
  (and
   (map? membership)

   (uuid?
    (membership-id membership))

   (version-consistent?
    membership
    membership-version)

   (uuid?
    (membership-user-id membership))

   (uuid?
    (membership-organization-id membership))

   (skills?
    (membership-skills membership))

   (membership-status?
    (membership-status membership))

   (let [created-at
         (:membership/created-at membership)

         updated-at
         (:membership/updated-at membership)]
     (and
      (optional-time-within?
       created-at
       (:membership/suspended-at membership)
       updated-at)

      (optional-time-within?
       created-at
       (:membership/revoked-at membership)
       updated-at)))

   (audit-consistent?
    membership
    :membership/suspended-at
    :membership/suspended-by
    :membership/suspension-reason)

   (audit-consistent?
    membership
    :membership/revoked-at
    :membership/revoked-by
    :membership/revocation-reason)

   (case
    (membership-status membership)

    :active
    (none-present?
     membership
     [:membership/suspended-at
      :membership/suspended-by
      :membership/suspension-reason
      :membership/revoked-at
      :membership/revoked-by
      :membership/revocation-reason])

    :suspended
    (and
     (some?
      (:membership/suspended-at membership))

     (none-present?
      membership
      [:membership/revoked-at
       :membership/revoked-by
       :membership/revocation-reason]))

    :revoked
    (and
     (some?
      (:membership/revoked-at membership))

     (none-present?
      membership
      [:membership/suspended-at
       :membership/suspended-by
       :membership/suspension-reason]))

    false)))

(defn- membership-context
  [membership]
  {:membership/id
   (membership-id membership)

   :membership/user
   (membership-user-id membership)

   :membership/organization
   (membership-organization-id membership)

   :membership/status
   (membership-status membership)})

(defn- ensure-membership-document!
  [membership]
  (ensure!
   (membership-document-consistent? membership)
   :membership/invalid-document
   "The membership operation is invalid."
   {:membership
    "The membership document is internally inconsistent."}
   (membership-context membership))
  membership)

(defn- ensure-membership-audit-input!
  [membership {:keys [actor-id reason]}]
  (let [context
        (membership-context membership)]
    (ensure!
     (optional-uuid? actor-id)
     :membership/invalid-input
     "The membership operation is invalid."
     {:actor-id
      "The actor must be a UUID when supplied."}
     context)

    (ensure!
     (optional-reason? reason)
     :membership/invalid-input
     "The membership operation is invalid."
     {:reason
      "The reason must be a qualified keyword when supplied."}
     context)))

(defn- update-membership
  [membership now f]
  (ensure-membership-document!
   membership)

  (ensure!
   (valid-change-time?
    membership
    membership-version
    now)
   :membership/invalid-time
   "The membership operation is invalid."
   {:now
    "The change time must not precede the last update."}
   (membership-context membership))

  (let [changed
        (f membership)]
    (ensure!
     (not= membership changed)
     :membership/unchanged
     "The membership operation is invalid."
     {:membership
      "The operation would not change the membership."}
     (membership-context membership))

    (ensure-membership-document!
     (command/bump-version
      changed
      membership-version
      now))))

;; =============================================================================
;; Membership construction
;; =============================================================================

(defn- normalize-membership-create-input
  [input]
  (let [input
        (or input {})]
    {:id
     (:id input)

     :user-id
     (:user-id input)

     :organization-id
     (:organization-id input)

     :skills
     (normalize-skills
      (get input :skills #{}))

     :now
     (:now input)}))

(defn- membership-create-input-errors
  [{:keys
    [id
     user-id
     organization-id
     skills
     now]}]
  (cond-> {}
    (not
     (uuid? id))
    (assoc
     :id
     "A membership UUID is required.")

    (not
     (uuid? user-id))
    (assoc
     :user-id
     "A User UUID is required.")

    (not
     (uuid? organization-id))
    (assoc
     :organization-id
     "An Organization UUID is required.")

    (not
     (skills? skills))
    (assoc
     :skills
     "Skills must be canonical non-blank organization-local names.")

    (not
     (instant? now))
    (assoc
     :now
     "A valid membership creation time is required.")))

(defn- new-membership
  [input]
  (let [{:keys
         [id
          user-id
          organization-id
          skills
          now]
         :as normalized}
        (normalize-membership-create-input input)

        errors
        (membership-create-input-errors normalized)]
    (when
     (seq errors)
      (fail!
       :membership/invalid-create-input
       "A valid Organization membership could not be created."
       errors
       {:membership/id id
        :membership/user user-id
        :membership/organization organization-id}))

    (ensure-membership-document!
     {:xt/id
      id

      :membership/user
      user-id

      :membership/organization
      organization-id

      :membership/skills
      skills

      :membership/status
      :active

      :membership/revision
      0

      :membership/created-at
      now

      :membership/updated-at
      now})))

;; =============================================================================
;; Membership skill transitions
;; =============================================================================

(defn- ensure-membership-changeable!
  [membership]
  (ensure-membership-document!
   membership)

  (ensure!
   (not
    (membership-revoked? membership))
   :membership/revoked
   "The membership operation is invalid."
   {:status
    "A revoked membership cannot be changed."}
   (membership-context membership))

  membership)

(defn- add-membership-skill
  [membership {:keys [skill now]}]
  (ensure-membership-changeable!
   membership)

  (let [skill
        (normalize-skill skill)]
    (ensure!
     (skill? skill)
     :membership/invalid-skill
     "The membership operation is invalid."
     {:skill
      "The skill must be a non-blank organization-local skill name."}
     (membership-context membership))

    (ensure!
     (not
      (contains?
       (membership-skills membership)
       skill))
     :membership/skill-already-present
     "The membership operation is invalid."
     {:skill
      "The membership already has this skill."}
     (membership-context membership))

    (update-membership
     membership
     now
     #(update
       %
       :membership/skills
       conj
       skill))))

(defn- remove-membership-skill
  [membership {:keys [skill now]}]
  (ensure-membership-changeable!
   membership)

  (let [skill
        (normalize-skill skill)]
    (ensure!
     (skill? skill)
     :membership/invalid-skill
     "The membership operation is invalid."
     {:skill
      "The skill must be a non-blank organization-local skill name."}
     (membership-context membership))

    (ensure!
     (contains?
      (membership-skills membership)
      skill)
     :membership/skill-missing
     "The membership operation is invalid."
     {:skill
      "The membership does not have this skill."}
     (membership-context membership))

    (update-membership
     membership
     now
     #(update
       %
       :membership/skills
       disj
       skill))))

;; =============================================================================
;; Membership lifecycle transitions
;; =============================================================================

(defn- suspend-membership
  [membership {:keys [now actor-id reason] :as input}]
  (ensure-membership-document!
   membership)

  (ensure-membership-audit-input!
   membership
   input)

  (ensure!
   (membership-active? membership)
   (cond
     (membership-revoked? membership)
     :membership/revoked

     (membership-suspended? membership)
     :membership/already-suspended

     :else
     :membership/not-active)
   "The membership operation is invalid."
   {:status
    "Only an active membership can be suspended."}
   (membership-context membership))

  (update-membership
   membership
   now
   #(cond->
     (assoc
      %
      :membership/status
      :suspended

      :membership/suspended-at
      now)

     actor-id
     (assoc
      :membership/suspended-by
      actor-id)

     reason
     (assoc
      :membership/suspension-reason
      reason))))

(defn- reactivate-membership
  [membership {:keys [now]}]
  (ensure-membership-document!
   membership)

  (ensure!
   (membership-suspended? membership)
   (cond
     (membership-revoked? membership)
     :membership/revoked

     (membership-active? membership)
     :membership/already-active

     :else
     :membership/not-suspended)
   "The membership operation is invalid."
   {:status
    "Only a suspended membership can be reactivated."}
   (membership-context membership))

  (update-membership
   membership
   now
   #(-> %
        (assoc
         :membership/status
         :active)
        (dissoc
         :membership/suspended-at
         :membership/suspended-by
         :membership/suspension-reason))))

(defn- revoke-membership
  [membership {:keys [now actor-id reason] :as input}]
  (ensure-membership-document!
   membership)

  (ensure-membership-audit-input!
   membership
   input)

  (ensure!
   (membership-can-transition?
    membership
    :revoke)
   :membership/revoked
   "The membership operation is invalid."
   {:status
    "The membership is already revoked."}
   (membership-context membership))

  (update-membership
   membership
   now
   #(cond->
     (-> %
         (assoc
          :membership/status
          :revoked

          :membership/revoked-at
          now)
         (dissoc
          :membership/suspended-at
          :membership/suspended-by
          :membership/suspension-reason))

     actor-id
     (assoc
      :membership/revoked-by
      actor-id)

     reason
     (assoc
      :membership/revocation-reason
      reason))))

;; =============================================================================
;; Canonical Membership commands
;; =============================================================================

(defn create-membership-command
  [input]
  (command/create
   membership-entity-type
   (new-membership input)
   membership-version))

(defn- membership-update-command
  [operation membership transition input]
  (command/update-command
   membership-entity-type
   operation
   membership
   (transition membership input)
   membership-version))

(defn add-skill-command
  [membership input]
  (membership-update-command
   :add-skill
   membership
   add-membership-skill
   input))

(defn remove-skill-command
  [membership input]
  (membership-update-command
   :remove-skill
   membership
   remove-membership-skill
   input))

(defn suspend-membership-command
  [membership input]
  (membership-update-command
   :suspend
   membership
   suspend-membership
   input))

(defn reactivate-membership-command
  [membership input]
  (membership-update-command
   :reactivate
   membership
   reactivate-membership
   input))

(defn revoke-membership-command
  [membership input]
  (membership-update-command
   :revoke
   membership
   revoke-membership
   input))

;; =============================================================================
;; RoleAssignment lifecycle and facts
;; =============================================================================

(def ^:private role-assignment-statuses
  #{:active
    :revoked})

(defn role-assignment-status?
  [value]
  (contains?
   role-assignment-statuses
   value))

(defn role-assignment-id
  [role-assignment]
  (:xt/id role-assignment))

(defn role-assignment-membership-id
  [role-assignment]
  (:role-assignment/membership role-assignment))

(defn assigned-role
  [role-assignment]
  (:role-assignment/role role-assignment))

(defn role-assignment-status
  [role-assignment]
  (:role-assignment/status role-assignment))

(defn role-assignment-scope
  [role-assignment]
  {:scope/type
   (:role-assignment/scope-type role-assignment)

   :scope/id
   (:role-assignment/scope-id role-assignment)})

(defn role-assignment-active?
  [role-assignment]
  (= :active
     (role-assignment-status role-assignment)))

(defn role-assignment-revoked?
  [role-assignment]
  (= :revoked
     (role-assignment-status role-assignment)))

(defn role-assignment-for-membership?
  [role-assignment expected-membership-id]
  (= expected-membership-id
     (role-assignment-membership-id role-assignment)))

(defn role-assignment-grants-role?
  [role-assignment expected-role]
  (= expected-role
     (assigned-role role-assignment)))

(defn role-assignment-at-scope?
  [role-assignment expected-scope]
  (organization/same-scope?
   (role-assignment-scope role-assignment)
   expected-scope))

(defn role-assignment-grants?
  "Returns true when an active assignment exactly matches membership, role,
   and scope.

   Hierarchical scope applicability is handled separately through an
   authoritative Organization scope context."
  [role-assignment
   expected-membership-id
   expected-role
   expected-scope]
  (and
   (role-assignment-active?
    role-assignment)

   (role-assignment-for-membership?
    role-assignment
    expected-membership-id)

   (role-assignment-grants-role?
    role-assignment
    expected-role)

   (role-assignment-at-scope?
    role-assignment
    expected-scope)))

;; =============================================================================
;; RoleAssignment document invariants
;; =============================================================================

(defn role-assignment-document-consistent?
  "Returns true when role-assignment satisfies every local persisted invariant.

   The role assignment intentionally does not persist Organization ID. Its
   Organization is the Organization of the referenced Membership. FX proves
   that the assigned Organization scope belongs to that same Organization
   before constructing a new assignment."
  [role-assignment]
  (and
   (map? role-assignment)

   (uuid?
    (role-assignment-id role-assignment))

   (version-consistent?
    role-assignment
    role-assignment-version)

   (uuid?
    (role-assignment-membership-id role-assignment))

   (role?
    (assigned-role role-assignment))

   (organization/scope?
    (role-assignment-scope role-assignment))

   (role-assignment-status?
    (role-assignment-status role-assignment))

   (optional-uuid?
    (:role-assignment/assigned-by role-assignment))

   (optional-reason?
    (:role-assignment/assignment-reason role-assignment))

   (let [created-at
         (:role-assignment/created-at role-assignment)

         updated-at
         (:role-assignment/updated-at role-assignment)]
     (optional-time-within?
      created-at
      (:role-assignment/revoked-at role-assignment)
      updated-at))

   (audit-consistent?
    role-assignment
    :role-assignment/revoked-at
    :role-assignment/revoked-by
    :role-assignment/revocation-reason)

   (case
    (role-assignment-status role-assignment)

    :active
    (none-present?
     role-assignment
     [:role-assignment/revoked-at
      :role-assignment/revoked-by
      :role-assignment/revocation-reason])

    :revoked
    (some?
     (:role-assignment/revoked-at role-assignment))

    false)))

(defn- role-assignment-context
  [role-assignment]
  {:role-assignment/id
   (role-assignment-id role-assignment)

   :role-assignment/membership
   (role-assignment-membership-id role-assignment)

   :role-assignment/role
   (assigned-role role-assignment)

   :role-assignment/scope
   (when
    (map? role-assignment)
     (role-assignment-scope role-assignment))

   :role-assignment/status
   (role-assignment-status role-assignment)})

(defn- ensure-role-assignment-document!
  [role-assignment]
  (ensure!
   (role-assignment-document-consistent?
    role-assignment)
   :role-assignment/invalid-document
   "The role-assignment operation is invalid."
   {:role-assignment
    "The role-assignment document is internally inconsistent."}
   (role-assignment-context role-assignment))
  role-assignment)

(defn- ensure-role-assignment-audit-input!
  [role-assignment {:keys [actor-id reason]}]
  (let [context
        (role-assignment-context role-assignment)]
    (ensure!
     (optional-uuid? actor-id)
     :role-assignment/invalid-input
     "The role-assignment operation is invalid."
     {:actor-id
      "The actor must be a UUID when supplied."}
     context)

    (ensure!
     (optional-reason? reason)
     :role-assignment/invalid-input
     "The role-assignment operation is invalid."
     {:reason
      "The reason must be a qualified keyword when supplied."}
     context)))

(defn- update-role-assignment
  [role-assignment now f]
  (ensure-role-assignment-document!
   role-assignment)

  (ensure!
   (valid-change-time?
    role-assignment
    role-assignment-version
    now)
   :role-assignment/invalid-time
   "The role-assignment operation is invalid."
   {:now
    "The change time must not precede the last update."}
   (role-assignment-context role-assignment))

  (let [changed
        (f role-assignment)]
    (ensure!
     (not= role-assignment changed)
     :role-assignment/unchanged
     "The role-assignment operation is invalid."
     {:role-assignment
      "The operation would not change the role assignment."}
     (role-assignment-context role-assignment))

    (ensure-role-assignment-document!
     (command/bump-version
      changed
      role-assignment-version
      now))))

;; =============================================================================
;; RoleAssignment construction
;; =============================================================================

(defn- normalize-role-assignment-create-input
  [input]
  (let [input
        (or input {})]
    {:id
     (:id input)

     :role
     (:role input)

     :scope
     (:scope input)

     :actor-id
     (:actor-id input)

     :reason
     (:reason input)

     :now
     (:now input)}))

(defn- role-assignment-create-input-errors
  [membership
   {:keys
    [id
     role
     scope
     actor-id
     reason
     now]}]
  (cond-> {}
    (not
     (membership-document-consistent?
      membership))
    (assoc
     :membership
     "A valid Membership document is required.")

    (and
     (membership-document-consistent?
      membership)
     (not
      (membership-active? membership)))
    (assoc
     :membership
     "Roles can be granted only to an active Membership.")

    (not
     (uuid? id))
    (assoc
     :id
     "A role-assignment UUID is required.")

    (not
     (role? role))
    (assoc
     :role
     "The role must be helper, supervisor, or admin.")

    (not
     (organization/scope? scope))
    (assoc
     :scope
     "A valid Organization scope is required.")

    (not
     (optional-uuid? actor-id))
    (assoc
     :actor-id
     "The assigning actor must be a UUID when supplied.")

    (not
     (optional-reason? reason))
    (assoc
     :reason
     "The assignment reason must be a qualified keyword when supplied.")

    (not
     (instant? now))
    (assoc
     :now
     "A valid assignment time is required.")))

(defn- new-role-assignment
  [membership input]
  (let [{:keys
         [id
          role
          scope
          actor-id
          reason
          now]
         :as normalized}
        (normalize-role-assignment-create-input input)

        errors
        (role-assignment-create-input-errors
         membership
         normalized)]
    (when
     (seq errors)
      (fail!
       :role-assignment/invalid-create-input
       "A valid role assignment could not be created."
       errors
       {:role-assignment/id id
        :role-assignment/membership
        (membership-id membership)
        :role-assignment/role role
        :role-assignment/scope scope}))

    (ensure-role-assignment-document!
     (cond->
      {:xt/id
       id

       :role-assignment/membership
       (membership-id membership)

       :role-assignment/role
       role

       :role-assignment/scope-type
       (organization/scope-type scope)

       :role-assignment/scope-id
       (organization/scope-id scope)

       :role-assignment/status
       :active

       :role-assignment/revision
       0

       :role-assignment/created-at
       now

       :role-assignment/updated-at
       now}

       actor-id
       (assoc
        :role-assignment/assigned-by
        actor-id)

       reason
       (assoc
        :role-assignment/assignment-reason
        reason)))))

;; =============================================================================
;; RoleAssignment lifecycle transitions
;; =============================================================================

(defn- revoke-role-assignment
  [role-assignment {:keys [now actor-id reason] :as input}]
  (ensure-role-assignment-document!
   role-assignment)

  (ensure-role-assignment-audit-input!
   role-assignment
   input)

  (ensure!
   (role-assignment-active?
    role-assignment)
   :role-assignment/revoked
   "The role-assignment operation is invalid."
   {:status
    "The role assignment is already revoked."}
   (role-assignment-context role-assignment))

  (update-role-assignment
   role-assignment
   now
   #(cond->
     (assoc
      %
      :role-assignment/status
      :revoked

      :role-assignment/revoked-at
      now)

     actor-id
     (assoc
      :role-assignment/revoked-by
      actor-id)

     reason
     (assoc
      :role-assignment/revocation-reason
      reason))))

;; =============================================================================
;; RoleAssignment collection and authorization semantics
;; =============================================================================

(defn active-role-assignments
  [role-assignments]
  (filterv
   role-assignment-active?
   role-assignments))

(defn active-role-assignments-for-membership
  [role-assignments expected-membership-id]
  (filterv
   #(and
     (role-assignment-active? %)
     (role-assignment-for-membership?
      %
      expected-membership-id))
   role-assignments))

(defn active-role-assignments-at-scope
  [role-assignments expected-scope]
  (if
   (organization/scope? expected-scope)
    (filterv
     #(and
       (role-assignment-active? %)
       (role-assignment-at-scope?
        %
        expected-scope))
     role-assignments)

    []))

(defn applicable-role-assignment?
  "Returns true when role-assignment grants authority to membership at the
   target represented by authoritative Organization scope-context.

   This is Membership-side authority only. Callers that also require an active
   User account must establish that separately through User's public API."
  [membership role-assignment scope-context]
  (and
   (membership-document-consistent?
    membership)

   (membership-active?
    membership)

   (role-assignment-document-consistent?
    role-assignment)

   (role-assignment-active?
    role-assignment)

   (role-assignment-for-membership?
    role-assignment
    (membership-id membership))

   (organization/scope-context?
    scope-context)

   (=
    (membership-organization-id membership)
    (organization/scope-context-organization-id
     scope-context))

   (organization/scope-context-operational?
    scope-context)

   (organization/scope-applies?
    scope-context
    (role-assignment-scope role-assignment))))

(defn effective-role-assignments
  [membership role-assignments scope-context]
  (filterv
   #(applicable-role-assignment?
     membership
     %
     scope-context)
   role-assignments))

(defn effective-roles
  "Returns the exact roles Membership grants at an Organization scope.

   There is deliberately no implicit numeric role hierarchy."
  [membership role-assignments scope-context]
  (into
   #{}
   (map assigned-role)
   (effective-role-assignments
    membership
    role-assignments
    scope-context)))

(defn effective-role-assignment
  "Returns one effective assignment for expected-role, or nil."
  [membership role-assignments scope-context expected-role]
  (when
   (role? expected-role)
    (some
     #(when
       (role-assignment-grants-role?
        %
        expected-role)
        %)
     (effective-role-assignments
      membership
      role-assignments
      scope-context))))

(defn has-role?
  [membership role-assignments scope-context expected-role]
  (boolean
   (effective-role-assignment
    membership
    role-assignments
    scope-context
    expected-role)))

(defn helper?
  [membership role-assignments scope-context]
  (has-role?
   membership
   role-assignments
   scope-context
   :helper))

(defn supervisor?
  [membership role-assignments scope-context]
  (has-role?
   membership
   role-assignments
   scope-context
   :supervisor))

(defn admin?
  [membership role-assignments scope-context]
  (has-role?
   membership
   role-assignments
   scope-context
   :admin))

(defn staff?
  [membership role-assignments scope-context]
  (boolean
   (seq
    (effective-roles
     membership
     role-assignments
     scope-context))))

;; =============================================================================
;; Canonical RoleAssignment commands
;; =============================================================================

(defn create-role-assignment-command
  "Creates a role-assignment command for an already-loaded active Membership.

   FX is responsible for proving that the Organization scope belongs to the
   Membership's Organization and for guarding every persisted document used in
   that decision."
  [membership input]
  (command/create
   role-assignment-entity-type
   (new-role-assignment
    membership
    input)
   role-assignment-version))

(defn revoke-role-assignment-command
  [role-assignment input]
  (command/update-command
   role-assignment-entity-type
   :revoke
   role-assignment
   (revoke-role-assignment
    role-assignment
    input)
   role-assignment-version))

(defn revoke-role-assignments-at-scope-commands
  "Returns revocation commands for all active assignments at one exact scope.

   The caller remains responsible for loading the complete relevant assignment
   set and committing the returned commands atomically with any initiating
   operation."
  [role-assignments expected-scope input]
  (ensure!
   (organization/scope? expected-scope)
   :role-assignment/invalid-scope
   "Role assignments cannot be revoked for an invalid scope."
   {:scope
    "A valid Organization scope is required."}
   {:role-assignment/scope expected-scope})

  (mapv
   #(revoke-role-assignment-command
     %
     input)
   (active-role-assignments-at-scope
    role-assignments
    expected-scope)))

(defn revoke-role-assignments-for-membership-commands
  "Returns revocation commands for every active assignment owned by Membership."
  [role-assignments expected-membership-id input]
  (ensure!
   (uuid? expected-membership-id)
   :role-assignment/invalid-membership
   "Role assignments cannot be revoked for an invalid Membership."
   {:membership-id
    "A Membership UUID is required."}
   {:role-assignment/membership expected-membership-id})

  (mapv
   #(revoke-role-assignment-command
     %
     input)
   (active-role-assignments-for-membership
    role-assignments
    expected-membership-id)))
