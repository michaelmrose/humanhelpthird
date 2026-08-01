(ns net.humanhelp.site.model.organization.domain
  "Pure Organization rules for organizations, groups, locations, hierarchy
   scopes, lifecycle transitions, scope contexts, and canonical model commands.

   This namespace contains no persistence reads, transaction execution, or
   cross-model authorization."
  (:require
   [clojure.string :as str]
   [gesso.model.command :as command])
  (:import
   [java.time Instant]))

;; =============================================================================
;; Identity, versions, and shared values
;; =============================================================================

(def organization-entity-type :organization)
(def organization-group-entity-type :organization-group)
(def location-entity-type :location)

(def organization-version
  {:revision-key   :organization/revision
   :created-at-key :organization/created-at
   :updated-at-key :organization/updated-at})

(def organization-group-version
  {:revision-key   :organization-group/revision
   :created-at-key :organization-group/created-at
   :updated-at-key :organization-group/updated-at})

(def location-version
  {:revision-key   :location/revision
   :created-at-key :location/created-at
   :updated-at-key :location/updated-at})

(def name-max 160)
(def statuses #{:active :suspended :closed})
(def allowed-transitions
  {[:active :suspend]       :suspended
   [:suspended :reactivate] :active
   [:active :close]         :closed
   [:suspended :close]      :closed})

(defn- normalize-name [value]
  (when (string? value) (str/trim value)))

(defn name? [value]
  (and (string? value)
       (= value (normalize-name value))
       (not (str/blank? value))
       (<= (count value) name-max)))

(defn status? [value] (contains? statuses value))
(defn- active-status? [value] (= :active value))
(defn- suspended-status? [value] (= :suspended value))
(defn- closed-status? [value] (= :closed value))
(defn- next-status [status operation] (get allowed-transitions [status operation]))
(defn- can-transition-status? [status operation] (some? (next-status status operation)))

;; =============================================================================
;; Organization scopes
;; =============================================================================

(def scope-types #{:organization :organization-group :location})
(def parent-scope-types #{:organization :organization-group})

(defn- scope-type? [value] (contains? scope-types value))
(defn- parent-scope-type? [value] (contains? parent-scope-types value))

(defn scope?
  "Returns true when value structurally identifies one supported Organization
   hierarchy scope. This does not establish that the referenced entity exists
   or occupies a valid place in the hierarchy."
  [value]
  (and (map? value)
       (scope-type? (:scope/type value))
       (uuid? (:scope/id value))))

(defn parent-scope?
  "Returns true when value may structurally serve as the parent of an
   Organization Group or Location."
  [value]
  (and (scope? value)
       (parent-scope-type? (:scope/type value))))

(defn scope-type [scope] (:scope/type scope))
(defn scope-id [scope] (:scope/id scope))

(defn organization-scope [organization-id]
  {:scope/type :organization :scope/id organization-id})

(defn organization-group-scope [organization-group-id]
  {:scope/type :organization-group :scope/id organization-group-id})

(defn location-scope [location-id]
  {:scope/type :location :scope/id location-id})

(defn organization-scope? [scope]
  (and (scope? scope) (= :organization (scope-type scope))))

(defn organization-group-scope? [scope]
  (and (scope? scope) (= :organization-group (scope-type scope))))

(defn location-scope? [scope]
  (and (scope? scope) (= :location (scope-type scope))))

(defn same-scope? [left right]
  (and (scope? left) (scope? right) (= left right)))

(defn- applicable-scopes?
  "Returns true for a non-empty target-first vector of distinct structural
   Organization scopes. Hierarchy code is responsible for deriving the vector
   from authoritative persisted data."
  [scopes]
  (and (vector? scopes)
       (seq scopes)
       (every? scope? scopes)
       (= (count scopes) (count (distinct scopes)))))

(defn scope-context?
  "Returns true for the stable Organization scope-context value:

     {:organization/id    uuid
      :scope/target       scope
      :scope/applicable   [target ... organization-scope]
      :scope/operational? boolean}

   The applicable scopes must be target-first and Organization-last. This
   predicate validates structure only; it does not independently prove the
   supplied hierarchy."
  [value]
  (let [organization-id (:organization/id value)
        target          (:scope/target value)
        applicable      (:scope/applicable value)]
    (boolean
     (and (map? value)
          (uuid? organization-id)
          (scope? target)
          (applicable-scopes? applicable)
          (same-scope? target (first applicable))
          (same-scope? (organization-scope organization-id) (peek applicable))
          (boolean? (:scope/operational? value))))))

(defn scope-context-organization-id [context] (:organization/id context))
(defn scope-context-target [context] (:scope/target context))
(defn scope-context-operational? [context] (true? (:scope/operational? context)))

(defn scope-applies?
  "Returns true when scope is in the authoritative applicability chain
   represented by context. Consumers should use this predicate rather than
   depending directly on :scope/applicable."
  [context scope]
  (and (scope-context? context)
       (scope? scope)
       (boolean (some #(same-scope? scope %) (:scope/applicable context)))))

;; =============================================================================
;; Shared entity metadata
;; =============================================================================

(defn- k [entity field]
  (keyword (clojure.core/name entity) (clojure.core/name field)))

(def ^:private entity-specs
  {:organization
   {:entity-type  organization-entity-type
    :version      organization-version
    :label        "organization"
    :document-key :organization
    :movable?     false}

   :organization-group
   {:entity-type      organization-group-entity-type
    :version          organization-group-version
    :label            "organization group"
    :document-key     :organization-group
    :organization-key :organization-group/organization
    :movable?         true}

   :location
   {:entity-type      location-entity-type
    :version          location-version
    :label            "location"
    :document-key     :location
    :organization-key :location/organization
    :movable?         true}})

(defn- spec [entity] (get entity-specs entity))
(defn- version [entity] (:version (spec entity)))
(defn- entity-status [entity document] (get document (k entity :status)))
(defn- entity-name [entity document] (get document (k entity :name)))
(defn- entity-active? [entity document] (active-status? (entity-status entity document)))
(defn- entity-suspended? [entity document] (suspended-status? (entity-status entity document)))
(defn- entity-closed? [entity document] (closed-status? (entity-status entity document)))
(defn- entity-can-transition? [entity document operation]
  (can-transition-status? (entity-status entity document) operation))

;; =============================================================================
;; Public document facts
;; =============================================================================

(defn organization-id [document] (:xt/id document))
(defn organization-name [document] (entity-name :organization document))
(defn organization-status [document] (entity-status :organization document))
(defn organization-active? [document] (entity-active? :organization document))
(defn organization-suspended? [document] (entity-suspended? :organization document))
(defn organization-closed? [document] (entity-closed? :organization document))
(defn organization-scope-of [document] (organization-scope (:xt/id document)))

(defn organization-group-id [document] (:xt/id document))
(defn organization-group-organization-id [document] (:organization-group/organization document))
(defn organization-group-name [document] (entity-name :organization-group document))
(defn organization-group-status [document] (entity-status :organization-group document))
(defn organization-group-active? [document] (entity-active? :organization-group document))
(defn organization-group-suspended? [document] (entity-suspended? :organization-group document))
(defn organization-group-closed? [document] (entity-closed? :organization-group document))
(defn organization-group-scope-of [document] (organization-group-scope (:xt/id document)))
(defn organization-group-parent-scope [document]
  {:scope/type (:organization-group/parent-type document)
   :scope/id   (:organization-group/parent-id document)})
(defn organization-group-for-organization? [document organization-id]
  (= organization-id (:organization-group/organization document)))
(defn location-id [document] (:xt/id document))
(defn location-organization-id [document] (:location/organization document))
(defn location-name [document] (entity-name :location document))
(defn location-status [document] (entity-status :location document))
(defn location-active? [document] (entity-active? :location document))
(defn location-suspended? [document] (entity-suspended? :location document))
(defn location-closed? [document] (entity-closed? :location document))
(defn location-scope-of [document] (location-scope (:xt/id document)))
(defn location-parent-scope [document]
  {:scope/type (:location/parent-type document)
   :scope/id   (:location/parent-id document)})
(defn- location-for-organization? [document organization-id]
  (= organization-id (:location/organization document)))
;; =============================================================================
;; Generic invariant mechanics
;; =============================================================================

(defn- instant? [value] (instance? Instant value))
(defn- at-or-before? [^Instant left ^Instant right] (not (.isAfter left right)))
(defn- within? [^Instant lower ^Instant value ^Instant upper]
  (and (at-or-before? lower value) (at-or-before? value upper)))
(defn- optional-uuid? [value] (or (nil? value) (uuid? value)))
(defn- optional-reason? [value] (or (nil? value) (qualified-keyword? value)))
(defn- none-present? [document keys] (every? #(nil? (get document %)) keys))

(defn- conventional-version-consistent? [entity document]
  (let [{:keys [created-at-key updated-at-key] :as version} (version entity)
        created-at                                          (get document created-at-key)
        updated-at                                          (get document updated-at-key)]
    (and (command/versioned-document? document version)
         (instant? created-at)
         (instant? updated-at)
         (at-or-before? created-at updated-at))))

(defn- optional-time-within? [created-at value updated-at]
  (or (nil? value)
      (and (instant? value) (within? created-at value updated-at))))

(defn- audit-consistent? [document entity audit]
  (let [at           (get document (k entity (keyword (str (clojure.core/name audit) "-at"))))
        by           (get document (k entity (keyword (str (clojure.core/name audit) "-by"))))
        reason-field (case audit
                       :suspended :suspension-reason
                       :closed :closure-reason
                       :moved :move-reason)
        reason       (get document (k entity reason-field))]
    (and (or (nil? at) (instant? at))
         (optional-uuid? by)
         (optional-reason? reason)
         (or (some? at) (and (nil? by) (nil? reason))))))

(defn- lifecycle-consistent? [entity document]
  (let [status    (entity-status entity document)
        suspended [(k entity :suspended-at)
                   (k entity :suspended-by)
                   (k entity :suspension-reason)]
        closed    [(k entity :closed-at)
                   (k entity :closed-by)
                   (k entity :closure-reason)]]
    (case status
      :active (none-present? document (concat suspended closed))
      :suspended (and (some? (get document (first suspended)))
                      (none-present? document closed))
      :closed (and (some? (get document (first closed)))
                   (none-present? document suspended))
      false)))

(defn- timestamps-within-document? [entity document]
  (let [{:keys [created-at-key updated-at-key]} (version entity)
        created-at                              (get document created-at-key)
        updated-at                              (get document updated-at-key)
        fields                                  (cond-> [(k entity :suspended-at) (k entity :closed-at)]
                                                  (:movable? (spec entity)) (conj (k entity :moved-at)))]
    (and (instant? created-at)
         (instant? updated-at)
         (every? #(optional-time-within? created-at (get document %) updated-at)
                 fields))))

(defn- parent-consistent-with-organization? [organization-id parent-scope]
  (and (parent-scope? parent-scope)
       (or (organization-group-scope? parent-scope)
           (same-scope? parent-scope (organization-scope organization-id)))))

(defn- child-invariants? [entity document]
  (let [organization-id (get document (:organization-key (spec entity)))
        parent          {:scope/type (get document (k entity :parent-type))
                         :scope/id   (get document (k entity :parent-id))}]
    (and (uuid? organization-id)
         (not= (:xt/id document) organization-id)
         (parent-consistent-with-organization? organization-id parent)
         (not= (:xt/id document) (:scope/id parent))
         (audit-consistent? document entity :moved))))

(defn- document-consistent? [entity document]
  (and (map? document)
       (conventional-version-consistent? entity document)
       (name? (get document (k entity :name)))
       (status? (get document (k entity :status)))
       (timestamps-within-document? entity document)
       (audit-consistent? document entity :suspended)
       (audit-consistent? document entity :closed)
       (lifecycle-consistent? entity document)
       (or (= entity :organization)
           (child-invariants? entity document))))

(defn organization-document-consistent? [document]
  (document-consistent? :organization document))
(defn organization-group-document-consistent? [document]
  (document-consistent? :organization-group document))
(defn location-document-consistent? [document]
  (document-consistent? :location document))

;; =============================================================================
;; Create input and construction
;; =============================================================================

(defn- normalize-create-input [entity input]
  (let [input (or input {})]
    (cond-> {:id   (:id input)
             :name (normalize-name (:name input))
             :now  (:now input)}
      (not= entity :organization)
      (assoc :organization-id (:organization-id input)
             :parent-scope (:parent-scope input)))))

(defn- create-input-errors [entity {:keys [id organization-id parent-scope name now]}]
  (let [label (:label (spec entity))]
    (cond-> {}
      (not (uuid? id))
      (assoc :id (str "A " label " UUID is required."))

      (and (not= entity :organization) (not (uuid? organization-id)))
      (assoc :organization-id "An organization UUID is required.")

      (and (not= entity :organization) (uuid? id) (uuid? organization-id)
           (= id organization-id))
      (assoc :id (str "The " label " UUID must differ from the organization UUID."))

      (and (not= entity :organization)
           (not (parent-consistent-with-organization? organization-id parent-scope)))
      (assoc :parent-scope "The parent must be this organization or an organization group.")

      (and (not= entity :organization) (uuid? id) (scope? parent-scope)
           (= id (scope-id parent-scope)))
      (assoc :parent-scope
             (if (= entity :organization-group)
               "An organization group cannot be its own parent."
               "A location cannot use its own UUID as its parent."))

      (not (name? name))
      (assoc :name (str "A non-blank " label " name of at most 160 characters is required."))

      (not (instant? now))
      (assoc :now (str "A valid " label " creation time is required.")))))

(defn- context [entity document]
  (cond-> {(k entity :id)     (:xt/id document)
           (k entity :status) (entity-status entity document)}
    (contains? document (k entity :name))
    (assoc (k entity :name) (entity-name entity document))

    (not= entity :organization)
    (assoc (k entity :organization) (get document (:organization-key (spec entity)))
           (k entity :parent) {:scope/type (get document (k entity :parent-type))
                               :scope/id   (get document (k entity :parent-id))})))

(defn- fail! [error-type message errors context]
  (throw
   (ex-info message
            {:error/type    error-type
             :error/details {:errors errors :context context}})))

(defn- ensure! [test error-type message errors context]
  (when-not test (fail! error-type message errors context)))

(defn- ensure-document! [entity document]
  (ensure! (document-consistent? entity document)
           (keyword (clojure.core/name entity) "invalid-document")
           (str "The " (:label (spec entity)) " operation is invalid.")
           {(:document-key (spec entity))
            (str "The " (:label (spec entity)) " document is internally inconsistent.")}
           (context entity document))
  document)

(defn- new-entity [entity input]
  (let [{:keys [id organization-id parent-scope name now] :as normalized}
        (normalize-create-input entity input)
        errors                                                            (create-input-errors entity normalized)]
    (when (seq errors)
      (fail! (keyword (clojure.core/name entity) "invalid-create-input")
             (str "A valid " (:label (spec entity)) " could not be created.")
             errors
             (cond-> {(k entity :id) id}
               (not= entity :organization)
               (assoc (k entity :organization) organization-id
                      (k entity :parent) parent-scope))))
    (ensure-document!
     entity
     (cond-> {:xt/id                             id
              (k entity :name)                   name
              (k entity :status)                 :active
              (:revision-key (version entity))   0
              (:created-at-key (version entity)) now
              (:updated-at-key (version entity)) now}
       (not= entity :organization)
       (assoc (:organization-key (spec entity)) organization-id
              (k entity :parent-type) (scope-type parent-scope)
              (k entity :parent-id) (scope-id parent-scope))))))

;; =============================================================================
;; Shared update mechanics
;; =============================================================================

(defn- ensure-audit-input! [entity document input]
  (let [{:keys [actor-id reason]} input
        ctx                       (context entity document)]
    (ensure! (optional-uuid? actor-id)
             (keyword (clojure.core/name entity) "invalid-input")
             (str "The " (:label (spec entity)) " operation is invalid.")
             {:actor-id "The actor must be a UUID when supplied."}
             ctx)
    (ensure! (optional-reason? reason)
             (keyword (clojure.core/name entity) "invalid-input")
             (str "The " (:label (spec entity)) " operation is invalid.")
             {:reason "The reason must be a qualified keyword when supplied."}
             ctx)))

(defn- valid-change-time? [entity document now]
  (let [updated-at (get document (:updated-at-key (version entity)))]
    (and (instant? now)
         (instant? updated-at)
         (at-or-before? updated-at now))))

(defn- update-entity [entity document now f]
  (ensure-document! entity document)
  (ensure! (valid-change-time? entity document now)
           (keyword (clojure.core/name entity) "invalid-time")
           (str "The " (:label (spec entity)) " operation is invalid.")
           {:now "The change time must not precede the last update."}
           (context entity document))
  (let [changed (f document)]
    (ensure! (not= document changed)
             (keyword (clojure.core/name entity) "unchanged")
             (str "The " (:label (spec entity)) " operation is invalid.")
             {(:document-key (spec entity))
              (str "The operation would not change the " (:label (spec entity)) ".")}
             (context entity document))
    (ensure-document! entity (command/bump-version changed (version entity) now))))

(defn- rename-entity [entity document {:keys [name now]}]
  (ensure-document! entity document)
  (ensure! (not (entity-closed? entity document))
           (keyword (clojure.core/name entity) "closed")
           (str "The " (:label (spec entity)) " operation is invalid.")
           {:status (str "A closed " (:label (spec entity)) " cannot be renamed.")}
           (context entity document))
  (let [normalized-name (normalize-name name)]
    (ensure! (name? normalized-name)
             (keyword (clojure.core/name entity) "invalid-input")
             (str "The " (:label (spec entity)) " operation is invalid.")
             {:name (str "A non-blank " (:label (spec entity))
                         " name of at most 160 characters is required.")}
             (context entity document))
    (update-entity entity document now #(assoc % (k entity :name) normalized-name))))

(defn rename-organization [document input] (rename-entity :organization document input))
(defn rename-organization-group [document input] (rename-entity :organization-group document input))
(defn rename-location [document input] (rename-entity :location document input))

(defn- audit-assoc [document entity audit now actor-id reason]
  (let [at-key     (k entity (keyword (str (clojure.core/name audit) "-at")))
        by-key     (k entity (keyword (str (clojure.core/name audit) "-by")))
        reason-key (k entity (case audit
                               :suspended :suspension-reason
                               :closed :closure-reason
                               :moved :move-reason))]
    (cond-> (assoc document at-key now)
      actor-id (assoc by-key actor-id)
      reason (assoc reason-key reason))))

(defn- clear-audit [document entity audit]
  (apply dissoc document
         [(k entity (keyword (str (clojure.core/name audit) "-at")))
          (k entity (keyword (str (clojure.core/name audit) "-by")))
          (k entity (case audit
                      :suspended :suspension-reason
                      :closed :closure-reason
                      :moved :move-reason))]))

(defn- move-entity [entity document {:keys [parent-scope now actor-id reason] :as input}]
  (ensure-document! entity document)
  (ensure-audit-input! entity document input)
  (ensure! (not (entity-closed? entity document))
           (keyword (clojure.core/name entity) "closed")
           (str "The " (:label (spec entity)) " operation is invalid.")
           {:status (str "A closed " (:label (spec entity)) " cannot be moved.")}
           (context entity document))
  (let [organization-id (get document (:organization-key (spec entity)))
        current-parent  {:scope/type (get document (k entity :parent-type))
                         :scope/id   (get document (k entity :parent-id))}]
    (ensure! (parent-consistent-with-organization? organization-id parent-scope)
             (keyword (clojure.core/name entity) "invalid-parent")
             (str "The " (:label (spec entity)) " operation is invalid.")
             {:parent-scope "The parent must be this organization or an organization group."}
             (context entity document))
    (ensure! (not= (:xt/id document) (scope-id parent-scope))
             (if (= entity :organization-group)
               :organization-group/cycle
               :location/invalid-parent)
             (str "The " (:label (spec entity)) " operation is invalid.")
             {:parent-scope (if (= entity :organization-group)
                              "An organization group cannot be its own parent."
                              "A location cannot use its own UUID as its parent.")}
             (context entity document))
    (ensure! (not (same-scope? current-parent parent-scope))
             (keyword (clojure.core/name entity) "parent-unchanged")
             (str "The " (:label (spec entity)) " operation is invalid.")
             {:parent-scope "The requested parent is already current."}
             (context entity document))
    (update-entity
     entity document now
     #(-> %
          (assoc (k entity :parent-type) (scope-type parent-scope)
                 (k entity :parent-id) (scope-id parent-scope))
          (audit-assoc entity :moved now actor-id reason)))))

(defn move-organization-group [document input]
  (move-entity :organization-group document input))
(defn move-location [document input]
  (move-entity :location document input))

(defn- suspend-entity [entity document {:keys [now actor-id reason] :as input}]
  (ensure-document! entity document)
  (ensure-audit-input! entity document input)
  (ensure! (entity-active? entity document)
           (cond
             (entity-closed? entity document)
             (keyword (clojure.core/name entity) "closed")
             (entity-suspended? entity document)
             (keyword (clojure.core/name entity) "already-suspended")
             :else
             (keyword (clojure.core/name entity) "not-active"))
           (str "The " (:label (spec entity)) " operation is invalid.")
           {:status (str "Only an active " (:label (spec entity)) " can be suspended.")}
           (context entity document))
  (update-entity entity document now
                 #(-> %
                      (assoc (k entity :status) :suspended)
                      (audit-assoc entity :suspended now actor-id reason))))

(defn- reactivate-entity [entity document {:keys [now]}]
  (ensure-document! entity document)
  (ensure! (entity-suspended? entity document)
           (cond
             (entity-closed? entity document)
             (keyword (clojure.core/name entity) "closed")
             (entity-active? entity document)
             (keyword (clojure.core/name entity) "already-active")
             :else
             (keyword (clojure.core/name entity) "not-suspended"))
           (str "The " (:label (spec entity)) " operation is invalid.")
           {:status (str "Only a suspended " (:label (spec entity)) " can be reactivated.")}
           (context entity document))
  (update-entity entity document now
                 #(-> %
                      (assoc (k entity :status) :active)
                      (clear-audit entity :suspended))))

(defn- close-entity [entity document {:keys [now actor-id reason] :as input}]
  (ensure-document! entity document)
  (ensure-audit-input! entity document input)
  (ensure! (entity-can-transition? entity document :close)
           (keyword (clojure.core/name entity) "closed")
           (str "The " (:label (spec entity)) " operation is invalid.")
           {:status (str "The " (:label (spec entity)) " is already closed.")}
           (context entity document))
  (update-entity entity document now
                 #(-> %
                      (assoc (k entity :status) :closed)
                      (clear-audit entity :suspended)
                      (audit-assoc entity :closed now actor-id reason))))

(defn suspend-organization [document input] (suspend-entity :organization document input))
(defn reactivate-organization [document input] (reactivate-entity :organization document input))
(defn close-organization [document input] (close-entity :organization document input))
(defn suspend-organization-group [document input] (suspend-entity :organization-group document input))
(defn reactivate-organization-group [document input] (reactivate-entity :organization-group document input))
(defn close-organization-group [document input] (close-entity :organization-group document input))
(defn suspend-location [document input] (suspend-entity :location document input))
(defn reactivate-location [document input] (reactivate-entity :location document input))
(defn close-location [document input] (close-entity :location document input))

;; =============================================================================
;; Hierarchy consistency and scope contexts
;; =============================================================================

(defn- distinct-document-ids? [documents]
  (let [ids (mapv :xt/id documents)]
    (= (count ids) (count (set ids)))))

(defn- group-chain-consistent? [organization-id initial-parent groups]
  (loop [expected-parent initial-parent
         remaining       (seq groups)]
    (if-let [group (first remaining)]
      (and (organization-group-document-consistent? group)
           (organization-group-for-organization? group organization-id)
           (same-scope? expected-parent (organization-group-scope-of group))
           (recur (organization-group-parent-scope group) (next remaining)))
      (same-scope? expected-parent (organization-scope organization-id)))))

(defn- organization-group-ancestry-consistent? [organization group ancestors]
  (let [ancestors       (vec ancestors)
        organization-id (organization-id organization)]
    (and (organization-document-consistent? organization)
         (organization-group-document-consistent? group)
         (organization-group-for-organization? group organization-id)
         (distinct-document-ids? (into [group] ancestors))
         (group-chain-consistent? organization-id
                                  (organization-group-parent-scope group)
                                  ancestors))))

(defn- location-ancestry-consistent? [organization location groups]
  (let [groups          (vec groups)
        organization-id (organization-id organization)]
    (and (organization-document-consistent? organization)
         (location-document-consistent? location)
         (location-for-organization? location organization-id)
         (distinct-document-ids? (into [location] groups))
         (group-chain-consistent? organization-id
                                  (location-parent-scope location)
                                  groups))))

(defn- organization-group-operational? [organization group ancestors]
  (and (organization-group-ancestry-consistent? organization group ancestors)
       (organization-active? organization)
       (organization-group-active? group)
       (every? organization-group-active? ancestors)))

(defn- location-operational? [organization location groups]
  (and (location-ancestry-consistent? organization location groups)
       (organization-active? organization)
       (location-active? location)
       (every? organization-group-active? groups)))

(defn organization-scope-context [organization]
  (ensure-document! :organization organization)
  {:organization/id    (organization-id organization)
   :scope/target       (organization-scope-of organization)
   :scope/applicable   [(organization-scope-of organization)]
   :scope/operational? (organization-active? organization)})

(defn- hierarchy-scope-context [entity organization target ancestors]
  (let [consistent?  (case entity
                       :organization-group organization-group-ancestry-consistent?
                       :location location-ancestry-consistent?)
        target-scope (case entity
                       :organization-group organization-group-scope-of
                       :location location-scope-of)
        operational? (case entity
                       :organization-group organization-group-operational?
                       :location location-operational?)]
    (when-not (consistent? organization target ancestors)
      (fail! (keyword (clojure.core/name entity) "invalid-ancestry")
             (str "A scope context cannot be formed from an inconsistent "
                  (:label (spec entity)) " ancestry.")
             {:ancestry "The target and ancestors must form one same-organization chain."}
             (context entity target)))
    {:organization/id    (organization-id organization)
     :scope/target       (target-scope target)
     :scope/applicable
     (into [(target-scope target)]
           (concat (map organization-group-scope-of ancestors)
                   [(organization-scope-of organization)]))
     :scope/operational? (operational? organization target ancestors)}))

(defn organization-group-scope-context [organization group ancestors]
  (hierarchy-scope-context :organization-group organization group ancestors))

(defn location-scope-context [organization location groups]
  (hierarchy-scope-context :location organization location groups))

;; =============================================================================
;; Canonical model commands
;; =============================================================================

(defn- create-command* [entity input]
  (command/create (:entity-type (spec entity))
                  (new-entity entity input)
                  (version entity)))

(defn- update-command* [entity operation document transition input]
  (command/update-command (:entity-type (spec entity))
                          operation
                          document
                          (transition document input)
                          (version entity)))

(defn create-organization-command [input] (create-command* :organization input))
(defn create-organization-group-command [input] (create-command* :organization-group input))
(defn create-location-command [input] (create-command* :location input))

(defn rename-organization-command [document input]
  (update-command* :organization :rename document rename-organization input))
(defn suspend-organization-command [document input]
  (update-command* :organization :suspend document suspend-organization input))
(defn reactivate-organization-command [document input]
  (update-command* :organization :reactivate document reactivate-organization input))
(defn close-organization-command [document input]
  (update-command* :organization :close document close-organization input))

(defn rename-organization-group-command [document input]
  (update-command* :organization-group :rename document rename-organization-group input))
(defn move-organization-group-command [document input]
  (update-command* :organization-group :move document move-organization-group input))
(defn suspend-organization-group-command [document input]
  (update-command* :organization-group :suspend document suspend-organization-group input))
(defn reactivate-organization-group-command [document input]
  (update-command* :organization-group :reactivate document reactivate-organization-group input))
(defn close-organization-group-command [document input]
  (update-command* :organization-group :close document close-organization-group input))

(defn rename-location-command [document input]
  (update-command* :location :rename document rename-location input))
(defn move-location-command [document input]
  (update-command* :location :move document move-location input))
(defn suspend-location-command [document input]
  (update-command* :location :suspend document suspend-location input))
(defn reactivate-location-command [document input]
  (update-command* :location :reactivate document reactivate-location input))
(defn close-location-command [document input]
  (update-command* :location :close document close-location input))
