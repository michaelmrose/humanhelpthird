(ns net.humanhelp.site.model.organization.domain
  "Pure rules for HumanHelp organizations, organization groups, locations, and
   authoritative scope contexts.

   Organization owns the tenant hierarchy:

     organization
       -> zero or more nested organization groups
       -> locations

   Persisted parent relationships use explicit parent-type and parent-id fields
   so they remain straightforward to query. Structural authorization-scope
   values are shared with User through model.authorization-scope.

   This namespace owns document invariants, lifecycle transitions, hierarchy
   composition from already-loaded documents, and model command construction.
   It does not query XTDB, prove that referenced documents exist, authorize
   users, inspect User memberships or roles, revoke User assignments, or alter
   Requests.

   Graph and FX are responsible for loading complete ancestry, preventing
   duplicate entities, proving parent ownership, checking cycle freedom before
   moves, and composing cross-model consequences atomically."
  (:require
   [clojure.string :as str]
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
   [net.humanhelp.site.model.common :as model.common]))

;; =============================================================================
;; Entity identity and versioning
;; =============================================================================

(def organization-entity-type :organization)

(def organization-group-entity-type :organization-group)

(def location-entity-type :location)

(def organization-version {:revision-key :organization/revision
   :created-at-key :organization/created-at :updated-at-key :organization/updated-at})

(def organization-group-version {:revision-key :organization-group/revision
   :created-at-key :organization-group/created-at :updated-at-key :organization-group/updated-at})

(def location-version {:revision-key :location/revision :created-at-key :location/created-at
   :updated-at-key :location/updated-at})

;; =============================================================================
;; Shared values
;; =============================================================================

(def name-max 160)

(def statuses #{:active :suspended :closed})

;; Structural authorization-scope values are shared with User through
;; model.authorization-scope. These aliases preserve Organization's existing
;; public domain vocabulary while keeping one implementation.
(def scope-types
  authorization-scope/scope-types)

(def parent-scope-types
  authorization-scope/parent-scope-types)

(def allowed-transitions {[:active :suspend] :suspended [:suspended :reactivate] :active
   [:active :close] :closed [:suspended :close] :closed})

(defn normalize-name [value] (when (string? value) (str/trim value)))

(defn name? [value] (and (string? value) (= value (normalize-name value)) (not (str/blank? value))
   (<= (count value) name-max)))

(defn status? [value] (contains? statuses value))

(defn active-status? [value] (= :active value))

(defn suspended-status? [value] (= :suspended value))

(defn closed-status? [value] (= :closed value))

(defn next-status [status operation] (get allowed-transitions [status operation]))

(defn can-transition-status? [status operation] (some? (next-status status operation)))

;; =============================================================================
;; Authorization-scope values
;; =============================================================================

(def scope-type?
  authorization-scope/scope-type?)

(def parent-scope-type?
  authorization-scope/parent-scope-type?)

(def scope-reference?
  authorization-scope/scope-reference?)

(def parent-scope-reference?
  authorization-scope/parent-scope-reference?)

(def organization-scope
  authorization-scope/organization-scope)

(def organization-group-scope
  authorization-scope/organization-group-scope)

(def location-scope
  authorization-scope/location-scope)

(def organization-scope?
  authorization-scope/organization-scope?)

(def organization-group-scope?
  authorization-scope/organization-group-scope?)

(def location-scope?
  authorization-scope/location-scope?)

(def same-scope?
  authorization-scope/same-scope?)

;; =============================================================================
;; Organization facts
;; =============================================================================

(defn organization-id [organization] (:xt/id organization))

(defn organization-name [organization] (:organization/name organization))

(defn organization-status [organization] (:organization/status organization))

(defn organization-active? [organization] (active-status? (organization-status organization)))

(defn organization-suspended? [organization] (suspended-status? (organization-status organization)))

(defn organization-closed? [organization] (closed-status? (organization-status organization)))

(defn organization-can-transition? [organization operation] (can-transition-status?
   (organization-status organization) operation))

(defn organization-scope-of [organization] (organization-scope (organization-id organization)))

;; =============================================================================
;; Organization-group facts
;; =============================================================================

(defn organization-group-id [group] (:xt/id group))

(defn organization-group-organization-id [group] (:organization-group/organization group))

(defn organization-group-name [group] (:organization-group/name group))

(defn organization-group-status [group] (:organization-group/status group))

(defn organization-group-active? [group] (active-status? (organization-group-status group)))

(defn organization-group-suspended? [group] (suspended-status? (organization-group-status group)))

(defn organization-group-closed? [group] (closed-status? (organization-group-status group)))

(defn organization-group-can-transition? [group operation] (can-transition-status?
   (organization-group-status group) operation))

(defn organization-group-scope-of [group] (organization-group-scope (organization-group-id group)))

(defn organization-group-parent-scope [group] {:scope/type (:organization-group/parent-type group)

   :scope/id (:organization-group/parent-id group)})

(defn organization-group-for-organization? [group expected-organization-id]
  (= expected-organization-id (organization-group-organization-id group)))

(defn organization-group-direct-child-of? [group expected-parent-scope] (same-scope?
   (organization-group-parent-scope group) expected-parent-scope))

;; =============================================================================
;; Location facts
;; =============================================================================

(defn location-id [location] (:xt/id location))

(defn location-organization-id [location] (:location/organization location))

(defn location-name [location] (:location/name location))

(defn location-status [location] (:location/status location))

(defn location-active? [location] (active-status? (location-status location)))

(defn location-suspended? [location] (suspended-status? (location-status location)))

(defn location-closed? [location] (closed-status? (location-status location)))

(defn location-can-transition? [location operation] (can-transition-status?
   (location-status location) operation))

(defn location-scope-of [location] (location-scope (location-id location)))

(defn location-parent-scope [location] {:scope/type (:location/parent-type location)

   :scope/id (:location/parent-id location)})

(defn location-for-organization? [location expected-organization-id] (= expected-organization-id
     (location-organization-id location)))

(defn location-direct-child-of? [location expected-parent-scope] (same-scope?
   (location-parent-scope location) expected-parent-scope))

;; =============================================================================
;; Shared validation helpers
;; =============================================================================

(defn- optional-uuid? [value] (or (nil? value) (uuid? value)))

(defn- optional-reason? [value] (or (nil? value) (qualified-keyword? value)))

(defn- none-present? [document keys] (every? nil? (map document keys)))

(defn- audit-pair-consistent? [document at-key by-key reason-key] (let [at (get document at-key)

        by (get document by-key)

        reason (get document reason-key)] (and (optional-uuid? by) (optional-reason? reason) (or
      (some? at) (and (nil? by) (nil? reason))))))

(defn- lifecycle-consistent? [document status suspended-at-key suspended-by-key
   suspension-reason-key closed-at-key closed-by-key closure-reason-key] (case status :active
    (none-present? document [suspended-at-key suspended-by-key suspension-reason-key closed-at-key
      closed-by-key closure-reason-key])

    :suspended (and (some? (get document suspended-at-key)) (none-present? document [closed-at-key
       closed-by-key closure-reason-key]))

    :closed (and (some? (get document closed-at-key)) (none-present? document [suspended-at-key
       suspended-by-key suspension-reason-key]))

    false))

(defn- timestamp-within-document? [document version value] (model.common/optional-between?
   (get document (:created-at-key version)) value (get document (:updated-at-key version))))

(defn- timestamps-within-document? [document version values] (every? #(timestamp-within-document?
     document version %) values))

(defn- parent-consistent-with-organization? [organization-id parent-scope] (and
   (parent-scope-reference? parent-scope) (or (organization-group-scope? parent-scope)
    (= parent-scope (organization-scope organization-id)))))

;; =============================================================================
;; Complete document validation
;; =============================================================================

(defn organization-document-consistent? [organization] (and (map? organization)

   (model.common/versioned-document-consistent? organization organization-version)

   (name? (:organization/name organization))

   (status? (:organization/status organization))

   (timestamps-within-document? organization organization-version
    [(:organization/suspended-at organization) (:organization/closed-at organization)])

   (audit-pair-consistent? organization :organization/suspended-at :organization/suspended-by
    :organization/suspension-reason)

   (audit-pair-consistent? organization :organization/closed-at :organization/closed-by
    :organization/closure-reason)

   (lifecycle-consistent? organization (:organization/status organization)
    :organization/suspended-at :organization/suspended-by :organization/suspension-reason
    :organization/closed-at :organization/closed-by :organization/closure-reason)))

(defn organization-group-document-consistent? [group] (let [parent
        (organization-group-parent-scope group)

        organization-id (organization-group-organization-id group)] (and (map? group)

     (model.common/versioned-document-consistent? group organization-group-version)

     (uuid? organization-id)

     (not= (:xt/id group) organization-id)

     (name? (:organization-group/name group))

     (status? (:organization-group/status group))

     (parent-consistent-with-organization? organization-id parent)

     (not= (:xt/id group) (:scope/id parent))

     (timestamps-within-document? group organization-group-version
      [(:organization-group/moved-at group) (:organization-group/suspended-at group)
       (:organization-group/closed-at group)])

     (audit-pair-consistent? group :organization-group/moved-at :organization-group/moved-by
      :organization-group/move-reason)

     (audit-pair-consistent? group :organization-group/suspended-at :organization-group/suspended-by
      :organization-group/suspension-reason)

     (audit-pair-consistent? group :organization-group/closed-at :organization-group/closed-by
      :organization-group/closure-reason)

     (lifecycle-consistent? group (:organization-group/status group)
      :organization-group/suspended-at :organization-group/suspended-by
      :organization-group/suspension-reason :organization-group/closed-at
      :organization-group/closed-by :organization-group/closure-reason))))

(defn location-document-consistent? [location] (let [parent (location-parent-scope location)

        organization-id (location-organization-id location)] (and (map? location)

     (model.common/versioned-document-consistent? location location-version)

     (uuid? organization-id)

     (not= (:xt/id location) organization-id)

     (name? (:location/name location))

     (status? (:location/status location))

     (parent-consistent-with-organization? organization-id parent)

     (not= (:xt/id location) (:scope/id parent))

     (timestamps-within-document? location location-version [(:location/moved-at location)
       (:location/suspended-at location) (:location/closed-at location)])

     (audit-pair-consistent? location :location/moved-at :location/moved-by :location/move-reason)

     (audit-pair-consistent? location :location/suspended-at :location/suspended-by
      :location/suspension-reason)

     (audit-pair-consistent? location :location/closed-at :location/closed-by
      :location/closure-reason)

     (lifecycle-consistent? location (:location/status location) :location/suspended-at
      :location/suspended-by :location/suspension-reason :location/closed-at :location/closed-by
      :location/closure-reason))))

;; =============================================================================
;; Input normalization and validation
;; =============================================================================

(defn normalize-organization-create-input [input] (let [input (or input {})] {:id (:id input)

     :name (normalize-name (:name input))

     :now (:now input)}))

(defn organization-create-input-errors [{:keys [id name now]}] (cond-> {} (not (uuid? id)) (assoc
     :id
     "An organization UUID is required.")

    (not (name? name)) (assoc :name
     "A non-blank organization name of at most 160 characters is required.")

    (not (model.common/timestamp-value? now)) (assoc :now
     "A valid organization creation time is required.")))

(defn normalize-organization-group-create-input [input] (let [input (or input {})] {:id (:id input)

     :organization-id (:organization-id input)

     :parent-scope (:parent-scope input)

     :name (normalize-name (:name input))

     :now (:now input)}))

(defn organization-group-create-input-errors [{:keys [id organization-id parent-scope name now]}]
  (cond-> {} (not (uuid? id)) (assoc :id
     "An organization-group UUID is required.")

    (not (uuid? organization-id)) (assoc :organization-id
     "An organization UUID is required.")

    (and (uuid? id) (uuid? organization-id) (= id organization-id)) (assoc :id
     "The group UUID must differ from the organization UUID.")

    (not (parent-consistent-with-organization? organization-id parent-scope)) (assoc :parent-scope
     "The parent must be this organization or an organization group.")

    (and (uuid? id) (scope-reference? parent-scope) (= id (:scope/id parent-scope))) (assoc
     :parent-scope
     "An organization group cannot be its own parent.")

    (not (name? name)) (assoc :name
     "A non-blank group name of at most 160 characters is required.")

    (not (model.common/timestamp-value? now)) (assoc :now
     "A valid group creation time is required.")))

(defn normalize-location-create-input [input] (let [input (or input {})] {:id (:id input)

     :organization-id (:organization-id input)

     :parent-scope (:parent-scope input)

     :name (normalize-name (:name input))

     :now (:now input)}))

(defn location-create-input-errors [{:keys [id organization-id parent-scope name now]}] (cond-> {}
    (not (uuid? id)) (assoc :id
     "A location UUID is required.")

    (not (uuid? organization-id)) (assoc :organization-id
     "An organization UUID is required.")

    (and (uuid? id) (uuid? organization-id) (= id organization-id)) (assoc :id
     "The location UUID must differ from the organization UUID.")

    (not (parent-consistent-with-organization? organization-id parent-scope)) (assoc :parent-scope
     "The parent must be this organization or an organization group.")

    (and (uuid? id) (scope-reference? parent-scope) (= id (:scope/id parent-scope))) (assoc
     :parent-scope
     "A location cannot use its own UUID as its parent.")

    (not (name? name)) (assoc :name
     "A non-blank location name of at most 160 characters is required.")

    (not (model.common/timestamp-value? now)) (assoc :now
     "A valid location creation time is required.")))

;; =============================================================================
;; Failure and update helpers
;; =============================================================================

(defn- organization-context [organization] {:organization/id (:xt/id organization)

   :organization/name (:organization/name organization)

   :organization/status (:organization/status organization)})

(defn- organization-group-context [group] {:organization-group/id (:xt/id group)

   :organization-group/organization (:organization-group/organization group)

   :organization-group/parent (organization-group-parent-scope group)

   :organization-group/status (:organization-group/status group)})

(defn- location-context [location] {:location/id (:xt/id location)

   :location/organization (:location/organization location)

   :location/parent (location-parent-scope location)

   :location/status (:location/status location)})

(defn- fail! [error-type message errors context] (model.common/throw-invalid! error-type message
   errors context))

(defn- ensure! [test error-type message errors context] (when-not test (fail! error-type message
     errors context)))

(defn- ensure-audit-input! [input error-type context] (let [{:keys [actor-id reason]} input]
    (ensure! (optional-uuid? actor-id) error-type
     "The Organization operation is invalid."
     {:actor-id
      "The actor must be a UUID when supplied."}
     context)

    (ensure! (optional-reason? reason) error-type
     "The Organization operation is invalid."
     {:reason
      "The reason must be a qualified keyword when supplied."}
     context)))

(defn- ensure-organization-document! [organization] (ensure!
   (organization-document-consistent? organization) :organization/invalid-document
   "The organization operation is invalid."
   {:organization
    "The organization document is internally inconsistent."}
   (organization-context organization))

  organization)

(defn- ensure-organization-group-document! [group] (ensure!
   (organization-group-document-consistent? group) :organization-group/invalid-document
   "The organization-group operation is invalid."
   {:organization-group
    "The organization-group document is internally inconsistent."}
   (organization-group-context group))

  group)

(defn- ensure-location-document! [location] (ensure! (location-document-consistent? location)
   :location/invalid-document
   "The location operation is invalid."
   {:location
    "The location document is internally inconsistent."}
   (location-context location))

  location)

(defn- update-organization [organization now f] (ensure-organization-document! organization)

  (ensure! (model.common/valid-change-time? organization organization-version now)
   :organization/invalid-time
   "The organization operation is invalid."
   {:now
    "The change time must not precede the last update."}
   (organization-context organization))

  (let [changed (f organization)] (ensure! (not= organization changed) :organization/unchanged
     "The organization operation is invalid."
     {:organization
      "The operation would not change the organization."}
     (organization-context organization))

    (-> changed (model.common/bump-revision organization-version now)
        ensure-organization-document!)))

(defn- update-organization-group [group now f] (ensure-organization-group-document! group)

  (ensure! (model.common/valid-change-time? group organization-group-version now)
   :organization-group/invalid-time
   "The organization-group operation is invalid."
   {:now
    "The change time must not precede the last update."}
   (organization-group-context group))

  (let [changed (f group)] (ensure! (not= group changed) :organization-group/unchanged
     "The organization-group operation is invalid."
     {:organization-group
      "The operation would not change the organization group."}
     (organization-group-context group))

    (-> changed (model.common/bump-revision organization-group-version now)
        ensure-organization-group-document!)))

(defn- update-location [location now f] (ensure-location-document! location)

  (ensure! (model.common/valid-change-time? location location-version now) :location/invalid-time
   "The location operation is invalid."
   {:now
    "The change time must not precede the last update."}
   (location-context location))

  (let [changed (f location)] (ensure! (not= location changed) :location/unchanged
     "The location operation is invalid."
     {:location
      "The operation would not change the location."}
     (location-context location))

    (-> changed (model.common/bump-revision location-version now) ensure-location-document!)))

;; =============================================================================
;; Construction
;; =============================================================================

(defn new-organization [input] (let [{:keys [id name now] :as normalized}
        (normalize-organization-create-input input)

        errors (organization-create-input-errors normalized)] (when (seq errors) (fail!
       :organization/invalid-create-input
       "A valid organization could not be created."
       errors {:organization/id id}))

    (ensure-organization-document! {:xt/id id :organization/name name :organization/status :active
      :organization/revision 0 :organization/created-at now :organization/updated-at now})))

(defn new-organization-group [input] (let [{:keys [id organization-id parent-scope name now]
         :as normalized} (normalize-organization-group-create-input input)

        errors (organization-group-create-input-errors normalized)] (when (seq errors) (fail!
       :organization-group/invalid-create-input
       "A valid organization group could not be created."
       errors {:organization-group/id id :organization-group/organization organization-id
        :organization-group/parent parent-scope}))

    (ensure-organization-group-document! {:xt/id id :organization-group/organization organization-id
      :organization-group/parent-type (:scope/type parent-scope)
      :organization-group/parent-id (:scope/id parent-scope) :organization-group/name name
      :organization-group/status :active :organization-group/revision 0
      :organization-group/created-at now :organization-group/updated-at now})))

(defn new-location [input] (let [{:keys [id organization-id parent-scope name now] :as normalized}
        (normalize-location-create-input input)

        errors (location-create-input-errors normalized)] (when (seq errors) (fail!
       :location/invalid-create-input
       "A valid location could not be created."
       errors {:location/id id :location/organization organization-id
        :location/parent parent-scope}))

    (ensure-location-document! {:xt/id id :location/organization organization-id
      :location/parent-type (:scope/type parent-scope) :location/parent-id (:scope/id parent-scope)
      :location/name name :location/status :active :location/revision 0 :location/created-at now
      :location/updated-at now})))

;; =============================================================================
;; Rename transitions
;; =============================================================================

(defn rename-organization [organization {:keys [name now]}] (ensure-organization-document!
   organization)

  (ensure! (not (organization-closed? organization)) :organization/closed
   "The organization operation is invalid."
   {:status
    "A closed organization cannot be renamed."}
   (organization-context organization))

  (let [name (normalize-name name)] (ensure! (name? name) :organization/invalid-input
     "The organization operation is invalid."
     {:name
      "A non-blank organization name of at most 160 characters is required."}
     (organization-context organization))

    (update-organization organization now #(assoc % :organization/name name))))

(defn rename-organization-group [group {:keys [name now]}] (ensure-organization-group-document!
   group)

  (ensure! (not (organization-group-closed? group)) :organization-group/closed
   "The organization-group operation is invalid."
   {:status
    "A closed organization group cannot be renamed."}
   (organization-group-context group))

  (let [name (normalize-name name)] (ensure! (name? name) :organization-group/invalid-input
     "The organization-group operation is invalid."
     {:name
      "A non-blank group name of at most 160 characters is required."}
     (organization-group-context group))

    (update-organization-group group now #(assoc % :organization-group/name name))))

(defn rename-location [location {:keys [name now]}] (ensure-location-document! location)

  (ensure! (not (location-closed? location)) :location/closed
   "The location operation is invalid."
   {:status
    "A closed location cannot be renamed."}
   (location-context location))

  (let [name (normalize-name name)] (ensure! (name? name) :location/invalid-input
     "The location operation is invalid."
     {:name
      "A non-blank location name of at most 160 characters is required."}
     (location-context location))

    (update-location location now #(assoc % :location/name name))))

;; =============================================================================
;; Move transitions
;; =============================================================================

(defn move-organization-group
  "Changes the group's direct parent.

   Graph/FX must prove that the new parent exists in the same organization and
   is not the group itself or one of its descendants."
  [group {:keys [parent-scope now actor-id reason] :as input}] (ensure-organization-group-document!
   group)

  (ensure-audit-input! input :organization-group/invalid-input (organization-group-context group))

  (ensure! (not (organization-group-closed? group)) :organization-group/closed
   "The organization-group operation is invalid."
   {:status
    "A closed organization group cannot be moved."}
   (organization-group-context group))

  (ensure! (parent-consistent-with-organization? (organization-group-organization-id group)
    parent-scope) :organization-group/invalid-parent
   "The organization-group operation is invalid."
   {:parent-scope
    "The parent must be this organization or an organization group."}
   (organization-group-context group))

  (ensure! (not= (organization-group-id group) (:scope/id parent-scope)) :organization-group/cycle
   "The organization-group operation is invalid."
   {:parent-scope
    "An organization group cannot be its own parent."}
   (organization-group-context group))

  (ensure! (not (same-scope? (organization-group-parent-scope group) parent-scope))
   :organization-group/parent-unchanged
   "The organization-group operation is invalid."
   {:parent-scope
    "The requested parent is already current."}
   (organization-group-context group))

  (update-organization-group group now #(cond-> (assoc % :organization-group/parent-type
      (:scope/type parent-scope)

      :organization-group/parent-id (:scope/id parent-scope)

      :organization-group/moved-at now)

     actor-id (assoc :organization-group/moved-by actor-id)

     reason (assoc :organization-group/move-reason reason))))

(defn move-location
  "Changes the location's direct parent.

   Graph/FX must prove that an organization-group parent exists in the same
   organization."
  [location {:keys [parent-scope now actor-id reason] :as input}] (ensure-location-document!
   location)

  (ensure-audit-input! input :location/invalid-input (location-context location))

  (ensure! (not (location-closed? location)) :location/closed
   "The location operation is invalid."
   {:status
    "A closed location cannot be moved."}
   (location-context location))

  (ensure! (parent-consistent-with-organization? (location-organization-id location) parent-scope)
   :location/invalid-parent
   "The location operation is invalid."
   {:parent-scope
    "The parent must be this organization or an organization group."}
   (location-context location))

  (ensure! (not= (location-id location) (:scope/id parent-scope)) :location/invalid-parent
   "The location operation is invalid."
   {:parent-scope
    "A location cannot use its own UUID as its parent."}
   (location-context location))

  (ensure! (not (same-scope? (location-parent-scope location) parent-scope))
   :location/parent-unchanged
   "The location operation is invalid."
   {:parent-scope
    "The requested parent is already current."}
   (location-context location))

  (update-location location now #(cond-> (assoc % :location/parent-type (:scope/type parent-scope)

      :location/parent-id (:scope/id parent-scope)

      :location/moved-at now)

     actor-id (assoc :location/moved-by actor-id)

     reason (assoc :location/move-reason reason))))

;; =============================================================================
;; Organization lifecycle transitions
;; =============================================================================

(defn suspend-organization [organization {:keys [now actor-id reason] :as input}]
  (ensure-organization-document! organization)

  (ensure-audit-input! input :organization/invalid-input (organization-context organization))

  (ensure! (organization-active? organization) (cond (organization-closed? organization)
     :organization/closed

     (organization-suspended? organization) :organization/already-suspended

     :else :organization/not-active)
   "The organization operation is invalid."
   {:status
    "Only an active organization can be suspended."}
   (organization-context organization))

  (update-organization organization now #(cond-> (assoc % :organization/status :suspended

      :organization/suspended-at now)

     actor-id (assoc :organization/suspended-by actor-id)

     reason (assoc :organization/suspension-reason reason))))

(defn reactivate-organization [organization {:keys [now]}] (ensure-organization-document!
   organization)

  (ensure! (organization-suspended? organization) (cond (organization-closed? organization)
     :organization/closed

     (organization-active? organization) :organization/already-active

     :else :organization/not-suspended)
   "The organization operation is invalid."
   {:status
    "Only a suspended organization can be reactivated."}
   (organization-context organization))

  (update-organization organization now #(-> % (assoc :organization/status :active) (dissoc
         :organization/suspended-at :organization/suspended-by :organization/suspension-reason))))

(defn close-organization [organization {:keys [now actor-id reason] :as input}]
  (ensure-organization-document! organization)

  (ensure-audit-input! input :organization/invalid-input (organization-context organization))

  (ensure! (organization-can-transition? organization :close) :organization/closed
   "The organization operation is invalid."
   {:status
    "The organization is already closed."}
   (organization-context organization))

  (update-organization organization now #(cond-> (-> % (assoc :organization/status :closed

          :organization/closed-at now) (dissoc :organization/suspended-at :organization/suspended-by
          :organization/suspension-reason))

     actor-id (assoc :organization/closed-by actor-id)

     reason (assoc :organization/closure-reason reason))))

;; =============================================================================
;; Organization-group lifecycle transitions
;; =============================================================================

(defn suspend-organization-group [group {:keys [now actor-id reason] :as input}]
  (ensure-organization-group-document! group)

  (ensure-audit-input! input :organization-group/invalid-input (organization-group-context group))

  (ensure! (organization-group-active? group) (cond (organization-group-closed? group)
     :organization-group/closed

     (organization-group-suspended? group) :organization-group/already-suspended

     :else :organization-group/not-active)
   "The organization-group operation is invalid."
   {:status
    "Only an active organization group can be suspended."}
   (organization-group-context group))

  (update-organization-group group now #(cond-> (assoc % :organization-group/status :suspended

      :organization-group/suspended-at now)

     actor-id (assoc :organization-group/suspended-by actor-id)

     reason (assoc :organization-group/suspension-reason reason))))

(defn reactivate-organization-group [group {:keys [now]}] (ensure-organization-group-document!
   group)

  (ensure! (organization-group-suspended? group) (cond (organization-group-closed? group)
     :organization-group/closed

     (organization-group-active? group) :organization-group/already-active

     :else :organization-group/not-suspended)
   "The organization-group operation is invalid."
   {:status
    "Only a suspended organization group can be reactivated."}
   (organization-group-context group))

  (update-organization-group group now #(-> % (assoc :organization-group/status :active) (dissoc
         :organization-group/suspended-at :organization-group/suspended-by
         :organization-group/suspension-reason))))

(defn close-organization-group [group {:keys [now actor-id reason] :as input}]
  (ensure-organization-group-document! group)

  (ensure-audit-input! input :organization-group/invalid-input (organization-group-context group))

  (ensure! (organization-group-can-transition? group :close) :organization-group/closed
   "The organization-group operation is invalid."
   {:status
    "The organization group is already closed."}
   (organization-group-context group))

  (update-organization-group group now #(cond-> (-> % (assoc :organization-group/status :closed

          :organization-group/closed-at now) (dissoc :organization-group/suspended-at
          :organization-group/suspended-by :organization-group/suspension-reason))

     actor-id (assoc :organization-group/closed-by actor-id)

     reason (assoc :organization-group/closure-reason reason))))

;; =============================================================================
;; Location lifecycle transitions
;; =============================================================================

(defn suspend-location [location {:keys [now actor-id reason] :as input}] (ensure-location-document!
   location)

  (ensure-audit-input! input :location/invalid-input (location-context location))

  (ensure! (location-active? location) (cond (location-closed? location) :location/closed

     (location-suspended? location) :location/already-suspended

     :else :location/not-active)
   "The location operation is invalid."
   {:status
    "Only an active location can be suspended."}
   (location-context location))

  (update-location location now #(cond-> (assoc % :location/status :suspended

      :location/suspended-at now)

     actor-id (assoc :location/suspended-by actor-id)

     reason (assoc :location/suspension-reason reason))))

(defn reactivate-location [location {:keys [now]}] (ensure-location-document! location)

  (ensure! (location-suspended? location) (cond (location-closed? location) :location/closed

     (location-active? location) :location/already-active

     :else :location/not-suspended)
   "The location operation is invalid."
   {:status
    "Only a suspended location can be reactivated."}
   (location-context location))

  (update-location location now #(-> % (assoc :location/status :active) (dissoc
         :location/suspended-at :location/suspended-by :location/suspension-reason))))

(defn close-location [location {:keys [now actor-id reason] :as input}] (ensure-location-document!
   location)

  (ensure-audit-input! input :location/invalid-input (location-context location))

  (ensure! (location-can-transition? location :close) :location/closed
   "The location operation is invalid."
   {:status
    "The location is already closed."}
   (location-context location))

  (update-location location now #(cond-> (-> % (assoc :location/status :closed

          :location/closed-at now) (dissoc :location/suspended-at :location/suspended-by
          :location/suspension-reason))

     actor-id (assoc :location/closed-by actor-id)

     reason (assoc :location/closure-reason reason))))

;; =============================================================================
;; Hierarchy composition
;; =============================================================================

(defn organization-groups-for-organization [groups expected-organization-id] (filterv #(and
     (organization-group-document-consistent? %) (organization-group-for-organization? %
      expected-organization-id)) groups))

(defn locations-for-organization [locations expected-organization-id] (filterv #(and
     (location-document-consistent? %) (location-for-organization? % expected-organization-id))
   locations))

(defn- distinct-document-ids? [documents] (let [ids (mapv :xt/id documents)] (= (count ids)
       (count (set ids)))))

(defn- group-chain-consistent? [organization-id initial-parent groups] (loop [expected-parent
         initial-parent

         remaining (vec groups)] (if-let [group (first remaining)] (and
       (organization-group-document-consistent? group)

       (organization-group-for-organization? group organization-id)

       (same-scope? expected-parent (organization-group-scope-of group))

       (recur (organization-group-parent-scope group) (subvec remaining 1)))

      (same-scope? expected-parent (organization-scope organization-id)))))

(defn organization-group-ancestry-consistent?
  "Validates one group and its already-loaded ancestor groups.

   ancestors must be ordered from the immediate parent toward the root and must
   exclude the target group itself."
  [organization group ancestors] (let [ancestors (vec ancestors)

        organization-id (organization-id organization)] (and (organization-document-consistent?
      organization)

     (organization-group-document-consistent? group)

     (organization-group-for-organization? group organization-id)

     (distinct-document-ids? (into [group] ancestors))

     (group-chain-consistent? organization-id (organization-group-parent-scope group) ancestors))))

(defn location-ancestry-consistent?
  "Validates one location and its already-loaded ancestor groups.

   groups must be ordered from the location's immediate group parent toward the
   organization root. Pass an empty collection when the location is directly
   beneath the organization."
  [organization location groups] (let [groups (vec groups)

        organization-id (organization-id organization)] (and (organization-document-consistent?
      organization)

     (location-document-consistent? location)

     (location-for-organization? location organization-id)

     (distinct-document-ids?
      (into
       [location]
       groups))

     (group-chain-consistent? organization-id (location-parent-scope location) groups))))

(defn organization-group-operational?
  "Returns true when the organization, target group, and every ancestor group
   are individually active."
  [organization group ancestors] (and (organization-group-ancestry-consistent? organization group
    ancestors)

   (organization-active? organization)

   (organization-group-active? group)

   (every? organization-group-active? ancestors)))

(defn location-operational?
  "Returns true when the organization, location, and every ancestor group are
   individually active."
  [organization location groups] (and (location-ancestry-consistent? organization location groups)

   (organization-active? organization)

   (location-active? location)

   (every? organization-group-active? groups)))

(defn scope-context?
  "Returns true when value satisfies the shared structural authorization-scope
   context contract.

   Organization remains responsible for constructing the authoritative
   hierarchy chain and operational value."
  [value]
  (authorization-scope/scope-context?
   value))

(defn organization-scope-context [organization] (ensure-organization-document! organization)

  {:organization/id (organization-id organization)

   :scope/target (organization-scope-of organization)

   :scope/applicable [(organization-scope-of organization)]

   :scope/operational? (organization-active? organization)})

(defn organization-group-scope-context [organization group ancestors] (when-not
   (organization-group-ancestry-consistent? organization group ancestors) (fail!
     :organization-group/invalid-ancestry
     "A scope context cannot be formed from an inconsistent group ancestry."
     {:ancestry
      "The target group and ancestors must form one same-organization chain."}
     (organization-group-context group)))

  (let [applicable (into [(organization-group-scope-of group)] (concat (map
           organization-group-scope-of ancestors) [(organization-scope-of organization)]))]
    {:organization/id (organization-id organization)

     :scope/target (organization-group-scope-of group)

     :scope/applicable applicable

     :scope/operational? (organization-group-operational? organization group ancestors)}))

(defn location-scope-context [organization location groups] (when-not (location-ancestry-consistent?
    organization location groups) (fail! :location/invalid-ancestry
     "A scope context cannot be formed from an inconsistent location ancestry."
     {:ancestry
      "The location and groups must form one same-organization chain."}
     (location-context location)))

  (let [applicable (into [(location-scope-of location)] (concat (map organization-group-scope-of
           groups) [(organization-scope-of organization)]))] {:organization/id
     (organization-id organization)

     :scope/target (location-scope-of location)

     :scope/applicable applicable

     :scope/operational? (location-operational? organization location groups)}))

(defn organization-group-authorization-documents
  "Returns documents whose versions establish the current group scope context."
  [organization group ancestors] (when-not (organization-group-ancestry-consistent? organization
    group ancestors) (fail! :organization-group/invalid-ancestry
     "Authorization documents require a consistent group ancestry."
     {:ancestry
      "The target group and ancestors must form one same-organization chain."}
     (organization-group-context group)))

  (into [group] (concat ancestors [organization])))

(defn location-authorization-documents
  "Returns documents whose versions establish the current location scope
   context. The order matches the target-first applicable scope order."
  [organization location groups] (when-not (location-ancestry-consistent? organization location
    groups) (fail! :location/invalid-ancestry
     "Authorization documents require a consistent location ancestry."
     {:ancestry
      "The location and groups must form one same-organization chain."}
     (location-context location)))

  (into [location] (concat groups [organization])))

;; =============================================================================
;; Model commands
;; =============================================================================

(defn create-organization-command [input] (model.common/create-command organization-entity-type
   (new-organization input) organization-version))

(defn create-organization-group-command [input] (model.common/create-command
   organization-group-entity-type (new-organization-group input) organization-group-version))

(defn create-location-command [input] (model.common/create-command location-entity-type
   (new-location input) location-version))

(defn- organization-change-command [operation before after] (model.common/update-command
   organization-entity-type operation before after organization-version))

(defn- organization-group-change-command [operation before after] (model.common/update-command
   organization-group-entity-type operation before after organization-group-version))

(defn- location-change-command [operation before after] (model.common/update-command
   location-entity-type operation before after location-version))

(defn rename-organization-command [organization input] (organization-change-command :rename
   organization (rename-organization organization input)))

(defn suspend-organization-command [organization input] (organization-change-command :suspend
   organization (suspend-organization organization input)))

(defn reactivate-organization-command [organization input] (organization-change-command :reactivate
   organization (reactivate-organization organization input)))

(defn close-organization-command [organization input] (organization-change-command :close
   organization (close-organization organization input)))

(defn rename-organization-group-command [group input] (organization-group-change-command :rename
   group (rename-organization-group group input)))

(defn move-organization-group-command [group input] (organization-group-change-command :move group
   (move-organization-group group input)))

(defn suspend-organization-group-command [group input] (organization-group-change-command :suspend
   group (suspend-organization-group group input)))

(defn reactivate-organization-group-command [group input] (organization-group-change-command
   :reactivate group (reactivate-organization-group group input)))

(defn close-organization-group-command [group input] (organization-group-change-command :close group
   (close-organization-group group input)))

(defn rename-location-command [location input] (location-change-command :rename location
   (rename-location location input)))

(defn move-location-command [location input] (location-change-command :move location (move-location
    location input)))

(defn suspend-location-command [location input] (location-change-command :suspend location
   (suspend-location location input)))

(defn reactivate-location-command [location input] (location-change-command :reactivate location
   (reactivate-location location input)))

(defn close-location-command [location input] (location-change-command :close location
   (close-location location input)))
