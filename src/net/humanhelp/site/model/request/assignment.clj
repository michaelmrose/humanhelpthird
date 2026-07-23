(ns net.humanhelp.site.model.request.assignment
  "Pure domain rules for persisted Request helper assignments.

   A Request Assignment records one User participating in one Request. The
   assignment role is either :primary or :collaborator.

   Request owns these assignment records because they describe participation in
   a particular Request, not employment or authorization. User owns whether the
   User is an eligible helper at an Organization scope and which
   organization-local skill strings that helper has.

   Assignment role is immutable. Reassignment, takeover, transfer, or promotion
   from collaborator to primary is represented by ending the old assignment and
   creating the appropriate new assignment. This preserves assignment history
   without embedding historical helper state in the Request document.

   Cross-document rules such as 'at most one active primary assignment per
   Request' and 'do not create two active assignments for the same helper and
   Request' require current persistence facts and therefore belong in Request
   FX/Graph rather than this namespace.

   This namespace does not query XTDB, authorize actors, inspect User roles or
   skills, change Request lifecycle state, execute transactions, or publish
   Gesso Live changes."
  (:require
   [net.humanhelp.site.model.common :as model.common]))

;; =============================================================================
;; Identity, versioning, and vocabulary
;; =============================================================================

(def entity-type
  :request-assignment)

(def version
  {:revision-key :request-assignment/revision
   :created-at-key :request-assignment/created-at
   :updated-at-key :request-assignment/updated-at})

(def roles
  #{:primary
    :collaborator})

(def statuses
  #{:active
    :ended})

(defn role?
  [value]
  (contains?
   roles
   value))

(defn status?
  [value]
  (contains?
   statuses
   value))

;; =============================================================================
;; Projections and facts
;; =============================================================================

(defn assignment-id
  [assignment]
  (:xt/id assignment))

(defn request-id
  [assignment]
  (:request-assignment/request assignment))

(defn helper-id
  [assignment]
  (:request-assignment/helper assignment))

(defn role
  [assignment]
  (:request-assignment/role assignment))

(defn status
  [assignment]
  (:request-assignment/status assignment))

(defn source
  [assignment]
  (:request-assignment/source assignment))

(defn assigned-by
  [assignment]
  (:request-assignment/assigned-by assignment))

(defn assigned-at
  [assignment]
  (:request-assignment/assigned-at assignment))

(defn ended-at
  [assignment]
  (:request-assignment/ended-at assignment))

(defn ended-by
  [assignment]
  (:request-assignment/ended-by assignment))

(defn end-reason
  [assignment]
  (:request-assignment/end-reason assignment))

(defn revision
  [assignment]
  (:request-assignment/revision assignment))

(defn created-at
  [assignment]
  (:request-assignment/created-at assignment))

(defn updated-at
  [assignment]
  (:request-assignment/updated-at assignment))

(defn active?
  [assignment]
  (=
   :active
   (status assignment)))

(defn ended?
  [assignment]
  (=
   :ended
   (status assignment)))

(defn primary?
  [assignment]
  (=
   :primary
   (role assignment)))

(defn collaborator?
  [assignment]
  (=
   :collaborator
   (role assignment)))

(defn active-primary?
  [assignment]
  (and
   (active?
    assignment)

   (primary?
    assignment)))

(defn active-collaborator?
  [assignment]
  (and
   (active?
    assignment)

   (collaborator?
    assignment)))

(defn for-request?
  [assignment expected-request-id]
  (and
   (uuid?
    expected-request-id)

   (=
    expected-request-id
    (request-id assignment))))

(defn for-helper?
  [assignment expected-helper-id]
  (and
   (uuid?
    expected-helper-id)

   (=
    expected-helper-id
    (helper-id assignment))))

(defn active-for-helper?
  [assignment expected-helper-id]
  (and
   (active?
    assignment)

   (for-helper?
    assignment
    expected-helper-id)))

(defn active-for-request?
  [assignment expected-request-id]
  (and
   (active?
    assignment)

   (for-request?
    assignment
    expected-request-id)))

;; =============================================================================
;; Collection facts
;; =============================================================================

(defn active-assignments
  [assignments]
  (filterv
   active?
   assignments))

(defn ended-assignments
  [assignments]
  (filterv
   ended?
   assignments))

(defn primary-assignments
  [assignments]
  (filterv
   primary?
   assignments))

(defn collaborator-assignments
  [assignments]
  (filterv
   collaborator?
   assignments))

(defn active-primary-assignments
  [assignments]
  (filterv
   active-primary?
   assignments))

(defn active-collaborator-assignments
  [assignments]
  (filterv
   active-collaborator?
   assignments))

(defn active-assignment-for-helper
  "Returns the active assignment for helper-id, nil when absent, and throws
   when the supplied collection contains more than one.

   Persistence workflows should prevent this ambiguity. Keeping the pure
   collection helper strict makes corrupted or incorrectly-composed assignment
   collections fail loudly instead of selecting an arbitrary record."
  [assignments expected-helper-id]
  (let [matches
        (filterv
         #(active-for-helper?
           %
           expected-helper-id)
         assignments)]
    (case
     (count matches)
      0
      nil

      1
      (first matches)

      (model.common/throw-invalid!
       :request-assignment/ambiguous-helper
       "More than one active Request assignment exists for the helper."
       {:helper-id
        "A helper may have at most one active assignment on a Request."}
       {:request-assignment/helper expected-helper-id
        :request-assignment/count (count matches)}))))

(defn active-primary-assignment
  "Returns the one active primary assignment, nil when absent, and throws when
   more than one exists."
  [assignments]
  (let [matches
        (active-primary-assignments
         assignments)]
    (case
     (count matches)
      0
      nil

      1
      (first matches)

      (model.common/throw-invalid!
       :request-assignment/ambiguous-primary
       "More than one active primary Request assignment exists."
       {:role
        "A Request may have at most one active primary assignment."}
       {:request-assignment/count
        (count matches)}))))

(defn active-helper-ids
  [assignments]
  (into
   #{}
   (map helper-id)
   (active-assignments
    assignments)))

(defn active-collaborator-helper-ids
  [assignments]
  (into
   #{}
   (map helper-id)
   (active-collaborator-assignments
    assignments)))

;; =============================================================================
;; Complete document consistency
;; =============================================================================

(defn- optional-uuid?
  [value]
  (or
   (nil?
    value)

   (uuid?
    value)))

(defn- optional-reason?
  [value]
  (or
   (nil?
    value)

   (qualified-keyword?
    value)))

(defn- source?
  [value]
  (qualified-keyword?
   value))

(defn- timestamp-within-document?
  [assignment value]
  (model.common/optional-between?
   (created-at assignment)
   value
   (updated-at assignment)))

(defn- active-state-consistent?
  [assignment]
  (and
   (nil?
    (ended-at assignment))

   (nil?
    (ended-by assignment))

   (nil?
    (end-reason assignment))))

(defn- ended-state-consistent?
  [assignment]
  (and
   (model.common/timestamp-value?
    (ended-at assignment))

   (qualified-keyword?
    (end-reason assignment))))

(defn document-consistent?
  "Returns true when assignment is a complete valid persisted Request
   Assignment document.

   This validates only the record itself. It does not establish that the
   Request exists, that the helper is eligible, or that the Request's other
   assignments satisfy collection-level uniqueness rules."
  [assignment]
  (and
   (map?
    assignment)

   (model.common/versioned-document-consistent?
    assignment
    version)

   (uuid?
    (request-id assignment))

   (uuid?
    (helper-id assignment))

   (role?
    (role assignment))

   (status?
    (status assignment))

   (source?
    (source assignment))

   (optional-uuid?
    (assigned-by assignment))

   (model.common/timestamp-value?
    (assigned-at assignment))

   (timestamp-within-document?
    assignment
    (assigned-at assignment))

   (optional-uuid?
    (ended-by assignment))

   (optional-reason?
    (end-reason assignment))

   (timestamp-within-document?
    assignment
    (ended-at assignment))

   (model.common/timestamp<=
    (assigned-at assignment)
    (updated-at assignment))

   (case
    (status assignment)

    :active
    (active-state-consistent?
     assignment)

    :ended
    (and
     (ended-state-consistent?
      assignment)

     (model.common/timestamp<=
      (assigned-at assignment)
      (ended-at assignment)))

    false)))

(defn- context
  [assignment]
  {:request-assignment/id
   (assignment-id assignment)

   :request-assignment/request
   (request-id assignment)

   :request-assignment/helper
   (helper-id assignment)

   :request-assignment/role
   (role assignment)

   :request-assignment/status
   (status assignment)

   :request-assignment/revision
   (revision assignment)})

(defn require-document
  [assignment]
  (when-not
   (document-consistent?
    assignment)
    (model.common/throw-invalid!
     :request-assignment/invalid-document
     "The Request assignment document is invalid."
     {:request-assignment
      "The assignment identity, role, lifecycle, or version fields are inconsistent."}
     (context assignment)))
  assignment)

;; =============================================================================
;; Construction
;; =============================================================================

(defn normalize-create-input
  [input]
  (let [input
        (or
         input
         {})]
    {:id
     (:id input)

     :request-id
     (:request-id input)

     :helper-id
     (:helper-id input)

     :role
     (:role input)

     :source
     (:source input)

     :actor-id
     (:actor-id input)

     :now
     (:now input)}))

(defn create-input-errors
  [input]
  (let [{:keys
         [id
          request-id
          helper-id
          role
          source
          actor-id
          now]}
        (normalize-create-input
         input)]
    (cond-> {}
      (not
       (uuid?
        id))
      (assoc
       :id
       "A Request assignment UUID is required.")

      (not
       (uuid?
        request-id))
      (assoc
       :request-id
       "A Request UUID is required.")

      (not
       (uuid?
        helper-id))
      (assoc
       :helper-id
       "A helper User UUID is required.")

      (not
       (role?
        role))
      (assoc
       :role
       "The assignment role must be primary or collaborator.")

      (not
       (source?
        source))
      (assoc
       :source
       "The assignment source must be a qualified keyword.")

      (not
       (optional-uuid?
        actor-id))
      (assoc
       :actor-id
       "The assigning actor must be a UUID when supplied.")

      (not
       (model.common/timestamp-value?
        now))
      (assoc
       :now
       "A valid assignment time is required."))))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors
    input)))

(defn new-assignment
  [input]
  (let [{:keys
         [id
          request-id
          helper-id
          role
          source
          actor-id
          now]
         :as normalized}
        (normalize-create-input
         input)

        errors
        (create-input-errors
         normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :request-assignment/invalid-create-input
       "A valid Request assignment could not be created."
       errors
       {:request-assignment/id id
        :request-assignment/request request-id
        :request-assignment/helper helper-id}))

    (require-document
     (cond->
      {:xt/id
       id

       :request-assignment/request
       request-id

       :request-assignment/helper
       helper-id

       :request-assignment/role
       role

       :request-assignment/status
       :active

       :request-assignment/source
       source

       :request-assignment/assigned-at
       now

       :request-assignment/revision
       0

       :request-assignment/created-at
       now

       :request-assignment/updated-at
       now}

       actor-id
       (assoc
        :request-assignment/assigned-by
        actor-id)))))

;; =============================================================================
;; Guarded revision
;; =============================================================================

(defn- immutable-identity
  [assignment]
  {:xt/id
   (assignment-id assignment)

   :request-assignment/request
   (request-id assignment)

   :request-assignment/helper
   (helper-id assignment)

   :request-assignment/role
   (role assignment)

   :request-assignment/source
   (source assignment)

   :request-assignment/assigned-by
   (assigned-by assignment)

   :request-assignment/assigned-at
   (assigned-at assignment)

   :request-assignment/created-at
   (created-at assignment)})

(defn- version-state
  [assignment]
  {:request-assignment/revision
   (revision assignment)

   :request-assignment/updated-at
   (updated-at assignment)})

(defn- revise-assignment
  [assignment now mutation-fn]
  (require-document
   assignment)

  (when-not
   (model.common/valid-change-time?
    assignment
    version
    now)
    (model.common/throw-invalid!
     :request-assignment/invalid-change-time
     "The Request assignment change time is invalid."
     {:now
      "The change time must be an Instant at or after the current update time."}
     (context assignment)))

  (when-not
   (fn?
    mutation-fn)
    (model.common/throw-invalid!
     :request-assignment/invalid-mutation
     "The Request assignment mutation is invalid."
     {:mutation
      "The mutation must be callable."}
     (context assignment)))

  (let [changed
        (mutation-fn
         assignment)]
    (when-not
     (map?
      changed)
      (model.common/throw-invalid!
       :request-assignment/invalid-mutation
       "The Request assignment mutation is invalid."
       {:mutation
        "The mutation must return a Request assignment map."}
       (context assignment)))

    (when-not
     (=
      (immutable-identity assignment)
      (immutable-identity changed))
      (model.common/throw-invalid!
       :request-assignment/immutable-identity
       "The Request assignment mutation is invalid."
       {:request-assignment
        "Request, helper, role, source, assignment actor, assignment time, and creation time are immutable."}
       (context assignment)))

    (when-not
     (=
      (version-state assignment)
      (version-state changed))
      (model.common/throw-invalid!
       :request-assignment/invalid-version-mutation
       "The Request assignment mutation is invalid."
       {:request-assignment
        "The mutation must not directly change revision or updated-at."}
       (context assignment)))

    (when
     (=
      assignment
      changed)
      (model.common/throw-invalid!
       :request-assignment/unchanged
       "The Request assignment mutation is invalid."
       {:request-assignment
        "The mutation would not change the assignment."}
       (context assignment)))

    (-> changed
        (model.common/bump-revision
         version
         now)
        require-document)))

;; =============================================================================
;; Lifecycle transition
;; =============================================================================

(defn normalize-end-input
  [input]
  (let [input
        (or
         input
         {})]
    {:actor-id
     (:actor-id input)

     :reason
     (:reason input)

     :now
     (:now input)}))

(defn end-input-errors
  [input]
  (let [{:keys
         [actor-id
          reason
          now]}
        (normalize-end-input
         input)]
    (cond-> {}
      (not
       (optional-uuid?
        actor-id))
      (assoc
       :actor-id
       "The ending actor must be a UUID when supplied.")

      (not
       (qualified-keyword?
        reason))
      (assoc
       :reason
       "An assignment end reason must be a qualified keyword.")

      (not
       (model.common/timestamp-value?
        now))
      (assoc
       :now
       "A valid assignment end time is required."))))

(defn end-assignment
  [assignment input]
  (require-document
   assignment)

  (when-not
   (active?
    assignment)
    (model.common/throw-invalid!
     :request-assignment/already-ended
     "The Request assignment has already ended."
     {:status
      "Only an active Request assignment can be ended."}
     (context assignment)))

  (let [{:keys
         [actor-id
          reason
          now]
         :as normalized}
        (normalize-end-input
         input)

        errors
        (end-input-errors
         normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :request-assignment/invalid-end-input
       "The Request assignment end is invalid."
       errors
       (context assignment)))

    (revise-assignment
     assignment
     now
     #(cond->
       (assoc
        %
        :request-assignment/status
        :ended

        :request-assignment/ended-at
        now

        :request-assignment/end-reason
        reason)

       actor-id
       (assoc
        :request-assignment/ended-by
        actor-id)))))

;; =============================================================================
;; Model commands
;; =============================================================================

(defn create-command
  [input]
  (model.common/create-command
   entity-type
   (new-assignment
    input)
   version))

(defn end-command
  [assignment input]
  (model.common/update-command
   entity-type
   :end
   assignment
   (end-assignment
    assignment
    input)
   version))
