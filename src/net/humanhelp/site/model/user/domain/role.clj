(ns net.humanhelp.site.model.user.domain.role
  "Pure rules for persisted HumanHelp role-assignment documents.

   A role assignment grants one membership one role at one explicit scope.
   Membership establishes organization affiliation; role assignment establishes
   authority within that organization.

   Role and scope are immutable after creation. Changing either means revoking
   the existing assignment and creating a new assignment. This preserves an
   auditable history and avoids silently rewriting past authority.

   This namespace owns role-assignment values, document invariants, revocation,
   exact-scope collection operations, and command construction. Shared
   structural authorization-scope values come from model.authorization-scope.

   It does not query XTDB, establish that referenced documents exist, determine
   whether a location or group belongs to an organization, authorize actors, or
   interpret effective access across a hierarchy. Effective-access rules belong
   to user.domain.access."
  (:require
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.domain.common :as user.common]))

;; =============================================================================
;; Identity and versioning
;; =============================================================================

(def entity-type
  :role-assignment)

(def version
  {:revision-key :role-assignment/revision
   :created-at-key :role-assignment/created-at
   :updated-at-key :role-assignment/updated-at})

;; =============================================================================
;; Role and scope values
;; =============================================================================

(def organization-scope
  authorization-scope/organization-scope)

(def organization-group-scope
  authorization-scope/organization-group-scope)

(def location-scope
  authorization-scope/location-scope)

(def organization-group-scope?
  authorization-scope/organization-group-scope?)

(def location-scope?
  authorization-scope/location-scope?)

(defn scope
  [role-assignment]
  {:scope/type (:role-assignment/scope-type role-assignment)
   :scope/id (:role-assignment/scope-id role-assignment)})

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def statuses
  #{:active
    :revoked})

(defn status?
  [value]
  (contains? statuses value))

(defn active?
  [role-assignment]
  (= :active (:role-assignment/status role-assignment)))

(defn revoked?
  [role-assignment]
  (= :revoked (:role-assignment/status role-assignment)))

;; =============================================================================
;; Relationship facts
;; =============================================================================

(defn membership-id
  [role-assignment]
  (:role-assignment/membership role-assignment))

(defn organization-id
  [role-assignment]
  (:role-assignment/organization role-assignment))

(defn assigned-role
  [role-assignment]
  (:role-assignment/role role-assignment))

(defn for-membership?
  [role-assignment expected-membership-id]
  (= expected-membership-id
     (membership-id role-assignment)))

(defn for-organization?
  [role-assignment expected-organization-id]
  (= expected-organization-id
     (organization-id role-assignment)))

(defn grants-role?
  [role-assignment expected-role]
  (= expected-role
     (assigned-role role-assignment)))

(defn at-scope?
  [role-assignment expected-scope]
  (authorization-scope/same-scope?
   (scope role-assignment)
   expected-scope))

(defn grants?
  "Returns true when an active assignment exactly matches membership, role,
   and scope. Hierarchical scope implications belong to user.domain.access."
  [role-assignment
   expected-membership-id
   expected-role
   expected-scope]
  (and
   (active? role-assignment)
   (for-membership? role-assignment expected-membership-id)
   (grants-role? role-assignment expected-role)
   (at-scope? role-assignment expected-scope)))

;; =============================================================================
;; Validation
;; =============================================================================

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
  [role-assignment keys]
  (every?
   nil?
   (map role-assignment keys)))

(defn- timestamp-within-document?
  [role-assignment value]
  (model.common/optional-between?
   (:role-assignment/created-at role-assignment)
   value
   (:role-assignment/updated-at role-assignment)))

(defn- scope-consistent?
  [role-assignment]
  (let [assignment-scope
        (scope role-assignment)]
    (and
     (authorization-scope/scope-reference? assignment-scope)

     ;; Organization-wide assignments explicitly reference their organization.
     ;; Group and location ownership requires Organization data and is checked
     ;; by Graph/FX rather than guessed here.
     (or
      (not (authorization-scope/organization-scope? assignment-scope))
      (= (:scope/id assignment-scope)
         (:role-assignment/organization role-assignment))))))

(defn document-consistent?
  "Returns true when role-assignment satisfies its complete local invariants.

   This does not establish that membership, organization, group, or location
   documents exist; that the membership belongs to the organization; or that a
   group/location belongs to the organization."
  [role-assignment]
  (and
   (map? role-assignment)

   (model.common/versioned-document-consistent?
    role-assignment
    version)

   (uuid?
    (:role-assignment/membership role-assignment))

   (uuid?
    (:role-assignment/organization role-assignment))

   (user.common/role?
    (:role-assignment/role role-assignment))

   (scope-consistent?
    role-assignment)

   (status?
    (:role-assignment/status role-assignment))

   (optional-uuid?
    (:role-assignment/assigned-by role-assignment))

   (optional-reason?
    (:role-assignment/assignment-reason role-assignment))

   (timestamp-within-document?
    role-assignment
    (:role-assignment/revoked-at role-assignment))

   (optional-uuid?
    (:role-assignment/revoked-by role-assignment))

   (optional-reason?
    (:role-assignment/revocation-reason role-assignment))

   (case (:role-assignment/status role-assignment)
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

(defn normalize-create-input
  [input]
  (let [input
        (or input {})

        input-scope
        (:scope input)]
    {:id
     (:id input)

     :membership-id
     (:membership-id input)

     :organization-id
     (:organization-id input)

     :role
     (:role input)

     :scope
     input-scope

     :actor-id
     (:actor-id input)

     :reason
     (:reason input)

     :now
     (:now input)}))

(defn create-input-errors
  [{:keys
    [id
     membership-id
     organization-id
     role
     scope
     actor-id
     reason
     now]}]
  (cond-> {}
    (not
     (uuid? id))
    (assoc
     :id
     "A role-assignment UUID is required.")

    (not
     (uuid? membership-id))
    (assoc
     :membership-id
     "A membership UUID is required.")

    (not
     (uuid? organization-id))
    (assoc
     :organization-id
     "An organization UUID is required.")

    (not
     (user.common/role? role))
    (assoc
     :role
     "The role must be helper, supervisor, or admin.")

    (not
     (authorization-scope/scope-reference? scope))
    (assoc
     :scope
     "An organization, organization-group, or location scope is required.")

    (and
     (authorization-scope/organization-scope? scope)
     (not=
      organization-id
      (:scope/id scope)))
    (assoc
     :scope
     "An organization-wide assignment must reference its organization.")

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
     (model.common/timestamp-value? now))
    (assoc
     :now
     "A valid assignment time is required.")))

(defn- context
  [role-assignment]
  {:role-assignment/id
   (:xt/id role-assignment)

   :role-assignment/membership
   (:role-assignment/membership role-assignment)

   :role-assignment/organization
   (:role-assignment/organization role-assignment)

   :role-assignment/role
   (:role-assignment/role role-assignment)

   :role-assignment/scope
   (when
    (map? role-assignment)
     (scope role-assignment))

   :role-assignment/status
   (:role-assignment/status role-assignment)})

(defn- fail!
  [role-assignment error-type errors]
  (model.common/throw-invalid!
   error-type
   "The role-assignment operation is invalid."
   errors
   (context role-assignment)))

(defn- ensure!
  [test role-assignment error-type errors]
  (when-not test
    (fail!
     role-assignment
     error-type
     errors)))

(defn- ensure-document!
  [role-assignment]
  (ensure!
   (document-consistent? role-assignment)
   role-assignment
   :role-assignment/invalid-document
   {:role-assignment
    "The role-assignment document is internally inconsistent."})

  role-assignment)

(defn- ensure-change-time!
  [role-assignment now]
  (ensure!
   (model.common/valid-change-time?
    role-assignment
    version
    now)
   role-assignment
   :role-assignment/invalid-time
   {:now
    "The change time must not precede the last update."}))

(defn- ensure-audit-input!
  [role-assignment {:keys [actor-id reason]}]
  (ensure!
   (optional-uuid? actor-id)
   role-assignment
   :role-assignment/invalid-input
   {:actor-id
    "The actor must be a UUID when supplied."})

  (ensure!
   (optional-reason? reason)
   role-assignment
   :role-assignment/invalid-input
   {:reason
    "The reason must be a qualified keyword when supplied."}))

(defn- update-role-assignment
  [role-assignment now f]
  (ensure-document!
   role-assignment)

  (ensure-change-time!
   role-assignment
   now)

  (let [changed
        (f role-assignment)]
    (ensure!
     (not=
      role-assignment
      changed)
     role-assignment
     :role-assignment/unchanged
     {:role-assignment
      "The operation would not change the role assignment."})

    (-> changed
        (model.common/bump-revision
         version
         now)
        ensure-document!)))

;; =============================================================================
;; Construction
;; =============================================================================

(defn new-role-assignment
  [input]
  (let [{:keys
         [id
          membership-id
          organization-id
          role
          scope
          actor-id
          reason
          now]
         :as normalized}
        (normalize-create-input input)

        errors
        (create-input-errors normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :role-assignment/invalid-create-input
       "A valid role assignment could not be created."
       errors
       {:role-assignment/id
        id

        :role-assignment/membership
        membership-id

        :role-assignment/organization
        organization-id

        :role-assignment/role
        role

        :role-assignment/scope
        scope}))

    (ensure-document!
     (cond->
      {:xt/id
       id

       :role-assignment/membership
       membership-id

       :role-assignment/organization
       organization-id

       :role-assignment/role
       role

       :role-assignment/scope-type
       (:scope/type scope)

       :role-assignment/scope-id
       (:scope/id scope)

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
;; Lifecycle transitions
;; =============================================================================

(defn revoke
  [role-assignment {:keys [now actor-id reason] :as input}]
  (ensure-document!
   role-assignment)

  (ensure-audit-input!
   role-assignment
   input)

  (ensure!
   (active? role-assignment)
   role-assignment
   :role-assignment/revoked
   {:status
    "The role assignment is already revoked."})

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
;; Collection operations
;; =============================================================================

(defn active-assignments
  [role-assignments]
  (filterv
   active?
   role-assignments))

(defn active-for-membership
  [role-assignments expected-membership-id]
  (filterv
   #(and
     (active? %)
     (for-membership? % expected-membership-id))
   role-assignments))

(defn active-for-organization
  [role-assignments expected-organization-id]
  (filterv
   #(and
     (active? %)
     (for-organization? % expected-organization-id))
   role-assignments))

(defn active-at-scope
  [role-assignments expected-scope]
  (filterv
   #(and
     (active? %)
     (at-scope? % expected-scope))
   role-assignments))

;; =============================================================================
;; Commands
;; =============================================================================

(defn create-command
  [input]
  (model.common/create-command
   entity-type
   (new-role-assignment input)
   version))

(defn revoke-command
  [role-assignment input]
  (model.common/update-command
   entity-type
   :revoke
   role-assignment
   (revoke role-assignment input)
   version))

(defn revoke-at-scope-commands
  "Constructs revocation commands for every active assignment at one exact
   scope. Unrelated and already-revoked assignments are ignored.

   This is suitable for consequences such as closing one location. The caller
   remains responsible for loading the complete relevant assignment set and
   committing all returned commands atomically with the initiating operation."
  [role-assignments expected-scope input]
  (when-not
   (authorization-scope/scope-reference? expected-scope)
    (model.common/throw-invalid!
     :role-assignment/invalid-scope
     "Role assignments cannot be revoked for an invalid scope."
     {:scope
      "An organization, organization-group, or location scope is required."}
     {:role-assignment/scope
      expected-scope}))

  (mapv
   #(revoke-command % input)
   (active-at-scope role-assignments expected-scope)))

(defn revoke-for-organization-commands
  "Constructs revocation commands for every active assignment belonging to an
   organization, regardless of its exact scope."
  [role-assignments expected-organization-id input]
  (when-not
   (uuid? expected-organization-id)
    (model.common/throw-invalid!
     :role-assignment/invalid-organization
     "Role assignments cannot be revoked for an invalid organization."
     {:organization-id
      "An organization UUID is required."}))

  (mapv
   #(revoke-command % input)
   (active-for-organization
    role-assignments
    expected-organization-id)))
