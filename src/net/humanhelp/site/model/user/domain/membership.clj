(ns net.humanhelp.site.model.user.domain.membership
  "Pure rules for persisted HumanHelp organization membership documents.

   A membership connects one user identity to one organization. It says that
   the user belongs to the organization; it does not itself grant helper,
   supervisor, or administrator authority. Those grants belong to role
   assignments.

   Membership also owns the organization's simple skill labels for the member.
   HumanHelp treats skills as organization-local canonical strings. It does not
   interpret what a skill means or what qualifications an Organization requires
   before assigning one.

   This namespace owns membership document invariants, lifecycle transitions,
   skill changes, and command construction. It does not query XTDB, enforce one
   membership per user and organization, authorize actors, alter role
   assignments, or inspect organization state."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.domain.common :as user.common]))

;; =============================================================================
;; Identity and versioning
;; =============================================================================

(def entity-type
  :membership)

(def version
  {:revision-key :membership/revision
   :created-at-key :membership/created-at
   :updated-at-key :membership/updated-at})

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def statuses
  #{:active
    :suspended
    :revoked})

(def allowed-transitions
  {[:active :suspend] :suspended
   [:suspended :reactivate] :active
   [:active :revoke] :revoked
   [:suspended :revoke] :revoked})

(defn status?
  [value]
  (contains? statuses value))

(defn active?
  [membership]
  (= :active (:membership/status membership)))

(defn suspended?
  [membership]
  (= :suspended (:membership/status membership)))

(defn revoked?
  [membership]
  (= :revoked (:membership/status membership)))

(defn next-status
  [membership operation]
  (get allowed-transitions
       [(:membership/status membership) operation]))

(defn can-transition?
  [membership operation]
  (some? (next-status membership operation)))

;; =============================================================================
;; Relationship facts
;; =============================================================================

(defn user-id
  [membership]
  (:membership/user membership))

(defn organization-id
  [membership]
  (:membership/organization membership))

(defn for-user?
  [membership expected-user-id]
  (= expected-user-id
     (user-id membership)))

(defn for-organization?
  [membership expected-organization-id]
  (= expected-organization-id
     (organization-id membership)))

(defn relates?
  [membership expected-user-id expected-organization-id]
  (and
   (for-user? membership expected-user-id)
   (for-organization? membership expected-organization-id)))

;; =============================================================================
;; Skill facts
;; =============================================================================

(defn skills
  [membership]
  (:membership/skills membership))

(defn has-skill?
  [membership skill]
  (let [skill'
        (user.common/normalize-skill
         skill)]
    (and
     (some?
      skill')

     (contains?
      (skills membership)
      skill'))))

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
  [membership keys]
  (every?
   nil?
   (map membership keys)))

(defn- timestamp-within-document?
  [membership value]
  (model.common/optional-between?
   (:membership/created-at membership)
   value
   (:membership/updated-at membership)))

(defn document-consistent?
  "Returns true when membership satisfies its complete document invariants.

   This validates only the membership document itself. It does not establish
   that the referenced user or organization exists, that the organization is
   active, or that no duplicate membership exists."
  [membership]
  (and
   (map? membership)

   (model.common/versioned-document-consistent?
    membership
    version)

   (uuid?
    (:membership/user membership))

   (uuid?
    (:membership/organization membership))

   (user.common/skills?
    (:membership/skills membership))

   (status?
    (:membership/status membership))

   (every?
    #(timestamp-within-document? membership %)
    [(:membership/suspended-at membership)
     (:membership/revoked-at membership)])

   (optional-uuid?
    (:membership/suspended-by membership))

   (optional-uuid?
    (:membership/revoked-by membership))

   (optional-reason?
    (:membership/suspension-reason membership))

   (optional-reason?
    (:membership/revocation-reason membership))

   (case (:membership/status membership)
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

(defn normalize-create-input
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
     (user.common/normalize-skills
      (get input :skills #{}))

     :now
     (:now input)}))

(defn create-input-errors
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
     "A user UUID is required.")

    (not
     (uuid? organization-id))
    (assoc
     :organization-id
     "An organization UUID is required.")

    (not
     (user.common/skills?
      skills))
    (assoc
     :skills
     "Skills must be a set of canonical non-blank skill names.")

    (not
     (model.common/timestamp-value? now))
    (assoc
     :now
     "A valid creation time is required.")))

(defn- context
  [membership]
  {:membership/id
   (:xt/id membership)

   :membership/user
   (:membership/user membership)

   :membership/organization
   (:membership/organization membership)

   :membership/status
   (:membership/status membership)})

(defn- fail!
  [membership error-type errors]
  (model.common/throw-invalid!
   error-type
   "The membership operation is invalid."
   errors
   (context membership)))

(defn- ensure!
  [test membership error-type errors]
  (when-not test
    (fail!
     membership
     error-type
     errors)))

(defn- ensure-document!
  [membership]
  (ensure!
   (document-consistent? membership)
   membership
   :membership/invalid-document
   {:membership
    "The membership document is internally inconsistent."})

  membership)

(defn- ensure-change-time!
  [membership now]
  (ensure!
   (model.common/valid-change-time?
    membership
    version
    now)
   membership
   :membership/invalid-time
   {:now
    "The change time must not precede the last update."}))

(defn- ensure-audit-input!
  [membership {:keys [actor-id reason]}]
  (ensure!
   (optional-uuid? actor-id)
   membership
   :membership/invalid-input
   {:actor-id
    "The actor must be a UUID when supplied."})

  (ensure!
   (optional-reason? reason)
   membership
   :membership/invalid-input
   {:reason
    "The reason must be a qualified keyword when supplied."}))

(defn- update-membership
  [membership now f]
  (ensure-document!
   membership)

  (ensure-change-time!
   membership
   now)

  (let [changed
        (f membership)]
    (ensure!
     (not=
      membership
      changed)
     membership
     :membership/unchanged
     {:membership
      "The operation would not change the membership."})

    (-> changed
        (model.common/bump-revision
         version
         now)
        ensure-document!)))

;; =============================================================================
;; Construction
;; =============================================================================

(defn new-membership
  [input]
  (let [{:keys
         [id
          user-id
          organization-id
          skills
          now]
         :as normalized}
        (normalize-create-input input)

        errors
        (create-input-errors normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :membership/invalid-create-input
       "A valid organization membership could not be created."
       errors
       {:membership/id id
        :membership/user user-id
        :membership/organization organization-id}))

    (ensure-document!
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
;; Skill transitions
;; =============================================================================

(defn add-skill
  [membership {:keys [skill now]}]
  (ensure-document!
   membership)

  (ensure!
   (not
    (revoked?
     membership))
   membership
   :membership/revoked
   {:status
    "A revoked membership cannot be changed."})

  (let [skill'
        (user.common/normalize-skill
         skill)]
    (ensure!
     (user.common/skill?
      skill')
     membership
     :membership/invalid-skill
     {:skill
      "The skill must be a non-blank canonical skill name."})

    (ensure!
     (not
      (contains?
       (:membership/skills membership)
       skill'))
     membership
     :membership/skill-already-present
     {:skill
      "The membership already has this skill."})

    (update-membership
     membership
     now
     #(update
       %
       :membership/skills
       conj
       skill'))))

(defn remove-skill
  [membership {:keys [skill now]}]
  (ensure-document!
   membership)

  (ensure!
   (not
    (revoked?
     membership))
   membership
   :membership/revoked
   {:status
    "A revoked membership cannot be changed."})

  (let [skill'
        (user.common/normalize-skill
         skill)]
    (ensure!
     (user.common/skill?
      skill')
     membership
     :membership/invalid-skill
     {:skill
      "The skill must be a non-blank canonical skill name."})

    (ensure!
     (contains?
      (:membership/skills membership)
      skill')
     membership
     :membership/skill-missing
     {:skill
      "The membership does not have this skill."})

    (update-membership
     membership
     now
     #(update
       %
       :membership/skills
       disj
       skill'))))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn suspend
  [membership {:keys [now actor-id reason] :as input}]
  (ensure-document!
   membership)

  (ensure-audit-input!
   membership
   input)

  (ensure!
   (active? membership)
   membership
   (cond
     (revoked? membership)
     :membership/revoked

     (suspended? membership)
     :membership/already-suspended

     :else
     :membership/not-active)
   {:status
    "Only an active membership can be suspended."})

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

(defn reactivate
  [membership {:keys [now]}]
  (ensure-document!
   membership)

  (ensure!
   (suspended? membership)
   membership
   (cond
     (revoked? membership)
     :membership/revoked

     (active? membership)
     :membership/already-active

     :else
     :membership/not-suspended)
   {:status
    "Only a suspended membership can be reactivated."})

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

(defn revoke
  [membership {:keys [now actor-id reason] :as input}]
  (ensure-document!
   membership)

  (ensure-audit-input!
   membership
   input)

  (ensure!
   (can-transition? membership :revoke)
   membership
   :membership/revoked
   {:status
    "The membership is already revoked."})

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
;; Commands
;; =============================================================================

(defn create-command
  [input]
  (model.common/create-command
   entity-type
   (new-membership input)
   version))

(defn- change-command
  [operation before after]
  (model.common/update-command
   entity-type
   operation
   before
   after
   version))

(defn add-skill-command
  [membership input]
  (change-command
   :add-skill
   membership
   (add-skill membership input)))

(defn remove-skill-command
  [membership input]
  (change-command
   :remove-skill
   membership
   (remove-skill membership input)))

(defn suspend-command
  [membership input]
  (change-command
   :suspend
   membership
   (suspend membership input)))

(defn reactivate-command
  [membership input]
  (change-command
   :reactivate
   membership
   (reactivate membership input)))

(defn revoke-command
  [membership input]
  (change-command
   :revoke
   membership
   (revoke membership input)))
