(ns net.humanhelp.site.model.organization.domain
  "Pure rules for HumanHelp organizations, organization groups, and locations.

   The public API is intentionally explicit, while shared mechanics for the
   three closely related entity types are data-driven internally. Organization
   owns lifecycle, hierarchy composition, scope contexts, and command creation;
   Graph/FX own persisted hierarchy loading, authorization, and atomic
   cross-model workflows."
  (:require
   [clojure.string :as str]
   [gesso.model.command :as command]
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
   [net.humanhelp.site.model.common :as model.common]))

;; =============================================================================
;; Identity, versions, and shared values
;; =============================================================================

(def organization-entity-type :organization)
(def organization-group-entity-type :organization-group)
(def location-entity-type :location)

(def organization-version
  {:revision-key :organization/revision
   :created-at-key :organization/created-at
   :updated-at-key :organization/updated-at})

(def organization-group-version
  {:revision-key :organization-group/revision
   :created-at-key :organization-group/created-at
   :updated-at-key :organization-group/updated-at})

(def location-version
  {:revision-key :location/revision
   :created-at-key :location/created-at
   :updated-at-key :location/updated-at})

(def name-max 160)
(def statuses #{:active :suspended :closed})
(def allowed-transitions
  {[:active :suspend] :suspended
   [:suspended :reactivate] :active
   [:active :close] :closed
   [:suspended :close] :closed})

(def scope-types authorization-scope/scope-types)
(def parent-scope-types authorization-scope/parent-scope-types)
(def scope-type? authorization-scope/scope-type?)
(def parent-scope-type? authorization-scope/parent-scope-type?)
(def scope-reference? authorization-scope/scope-reference?)
(def parent-scope-reference? authorization-scope/parent-scope-reference?)
(def organization-scope authorization-scope/organization-scope)
(def organization-group-scope authorization-scope/organization-group-scope)
(def location-scope authorization-scope/location-scope)
(def organization-scope? authorization-scope/organization-scope?)
(def organization-group-scope? authorization-scope/organization-group-scope?)
(def location-scope? authorization-scope/location-scope?)
(def same-scope? authorization-scope/same-scope?)
(defn scope-context? [value]
  (authorization-scope/scope-context? value))

(defn normalize-name [value]
  (when (string? value) (str/trim value)))

(defn name? [value]
  (and (string? value)
       (= value (normalize-name value))
       (not (str/blank? value))
       (<= (count value) name-max)))

(defn status? [value] (contains? statuses value))
(defn active-status? [value] (= :active value))
(defn suspended-status? [value] (= :suspended value))
(defn closed-status? [value] (= :closed value))
(defn next-status [status operation] (get allowed-transitions [status operation]))
(defn can-transition-status? [status operation] (some? (next-status status operation)))

;; =============================================================================
;; Shared entity metadata
;; =============================================================================

(defn- k [entity field]
  (keyword (clojure.core/name entity) (clojure.core/name field)))

(def ^:private entity-specs
  {:organization
   {:entity-type organization-entity-type
    :version organization-version
    :label "organization"
    :document-key :organization
    :movable? false}

   :organization-group
   {:entity-type organization-group-entity-type
    :version organization-group-version
    :label "organization group"
    :document-key :organization-group
    :organization-key :organization-group/organization
    :movable? true}

   :location
   {:entity-type location-entity-type
    :version location-version
    :label "location"
    :document-key :location
    :organization-key :location/organization
    :movable? true}})

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
(defn organization-can-transition? [document operation]
  (entity-can-transition? :organization document operation))
(defn organization-scope-of [document] (organization-scope (:xt/id document)))

(defn organization-group-id [document] (:xt/id document))
(defn organization-group-organization-id [document] (:organization-group/organization document))
(defn organization-group-name [document] (entity-name :organization-group document))
(defn organization-group-status [document] (entity-status :organization-group document))
(defn organization-group-active? [document] (entity-active? :organization-group document))
(defn organization-group-suspended? [document] (entity-suspended? :organization-group document))
(defn organization-group-closed? [document] (entity-closed? :organization-group document))
(defn organization-group-can-transition? [document operation]
  (entity-can-transition? :organization-group document operation))
(defn organization-group-scope-of [document] (organization-group-scope (:xt/id document)))
(defn organization-group-parent-scope [document]
  {:scope/type (:organization-group/parent-type document)
   :scope/id (:organization-group/parent-id document)})
(defn organization-group-for-organization? [document organization-id]
  (= organization-id (:organization-group/organization document)))
(defn organization-group-direct-child-of? [document parent-scope]
  (same-scope? (organization-group-parent-scope document) parent-scope))

(defn location-id [document] (:xt/id document))
(defn location-organization-id [document] (:location/organization document))
(defn location-name [document] (entity-name :location document))
(defn location-status [document] (entity-status :location document))
(defn location-active? [document] (entity-active? :location document))
(defn location-suspended? [document] (entity-suspended? :location document))
(defn location-closed? [document] (entity-closed? :location document))
(defn location-can-transition? [document operation]
  (entity-can-transition? :location document operation))
(defn location-scope-of [document] (location-scope (:xt/id document)))
(defn location-parent-scope [document]
  {:scope/type (:location/parent-type document)
   :scope/id (:location/parent-id document)})
(defn location-for-organization? [document organization-id]
  (= organization-id (:location/organization document)))
(defn location-direct-child-of? [document parent-scope]
  (same-scope? (location-parent-scope document) parent-scope))

;; =============================================================================
;; Generic invariant mechanics
;; =============================================================================

(defn- optional-uuid? [value] (or (nil? value) (uuid? value)))
(defn- optional-reason? [value] (or (nil? value) (qualified-keyword? value)))
(defn- none-present? [document keys] (every? #(nil? (get document %)) keys))

(defn- audit-consistent? [document entity audit]
  (let [at (get document (k entity (keyword (str (clojure.core/name audit) "-at"))))
        by (get document (k entity (keyword (str (clojure.core/name audit) "-by"))))
        reason-field (case audit
                       :suspended :suspension-reason
                       :closed :closure-reason
                       :moved :move-reason)
        reason (get document (k entity reason-field))]
    (and (optional-uuid? by)
         (optional-reason? reason)
         (or (some? at) (and (nil? by) (nil? reason))))))

(defn- lifecycle-consistent? [entity document]
  (let [status (entity-status entity document)
        suspended [(k entity :suspended-at)
                   (k entity :suspended-by)
                   (k entity :suspension-reason)]
        closed [(k entity :closed-at)
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
  (let [v (version entity)
        fields (cond-> [(k entity :suspended-at) (k entity :closed-at)]
                 (:movable? (spec entity)) (conj (k entity :moved-at)))]
    (every?
     #(model.common/optional-between?
       (get document (:created-at-key v))
       (get document %)
       (get document (:updated-at-key v)))
     fields)))

(defn- parent-consistent-with-organization? [organization-id parent-scope]
  (and (parent-scope-reference? parent-scope)
       (or (organization-group-scope? parent-scope)
           (= parent-scope (organization-scope organization-id)))))

(defn- child-invariants? [entity document]
  (let [organization-id (get document (:organization-key (spec entity)))
        parent {:scope/type (get document (k entity :parent-type))
                :scope/id (get document (k entity :parent-id))}]
    (and (uuid? organization-id)
         (not= (:xt/id document) organization-id)
         (parent-consistent-with-organization? organization-id parent)
         (not= (:xt/id document) (:scope/id parent))
         (audit-consistent? document entity :moved))))

(defn- document-consistent? [entity document]
  (and (map? document)
       (model.common/versioned-document-consistent? document (version entity))
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
    (cond-> {:id (:id input)
             :name (normalize-name (:name input))
             :now (:now input)}
      (not= entity :organization)
      (assoc :organization-id (:organization-id input)
             :parent-scope (:parent-scope input)))))

(defn normalize-organization-create-input [input]
  (normalize-create-input :organization input))
(defn normalize-organization-group-create-input [input]
  (normalize-create-input :organization-group input))
(defn normalize-location-create-input [input]
  (normalize-create-input :location input))

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

      (and (not= entity :organization) (uuid? id) (scope-reference? parent-scope)
           (= id (:scope/id parent-scope)))
      (assoc :parent-scope
             (if (= entity :organization-group)
               "An organization group cannot be its own parent."
               "A location cannot use its own UUID as its parent."))

      (not (name? name))
      (assoc :name (str "A non-blank " label " name of at most 160 characters is required."))

      (not (model.common/timestamp-value? now))
      (assoc :now (str "A valid " label " creation time is required.")))))

(defn organization-create-input-errors [input]
  (create-input-errors :organization input))
(defn organization-group-create-input-errors [input]
  (create-input-errors :organization-group input))
(defn location-create-input-errors [input]
  (create-input-errors :location input))

(defn- context [entity document]
  (cond-> {(k entity :id) (:xt/id document)
           (k entity :status) (entity-status entity document)}
    (contains? document (k entity :name))
    (assoc (k entity :name) (entity-name entity document))

    (not= entity :organization)
    (assoc (k entity :organization) (get document (:organization-key (spec entity)))
           (k entity :parent)
           {:scope/type (get document (k entity :parent-type))
            :scope/id (get document (k entity :parent-id))})))

(defn- fail! [error-type message errors context]
  (model.common/throw-invalid! error-type message errors context))

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
        errors (create-input-errors entity normalized)]
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
     (cond-> {:xt/id id
              (k entity :name) name
              (k entity :status) :active
              (:revision-key (version entity)) 0
              (:created-at-key (version entity)) now
              (:updated-at-key (version entity)) now}
       (not= entity :organization)
       (assoc (:organization-key (spec entity)) organization-id
              (k entity :parent-type) (:scope/type parent-scope)
              (k entity :parent-id) (:scope/id parent-scope))))))

(defn new-organization [input] (new-entity :organization input))
(defn new-organization-group [input] (new-entity :organization-group input))
(defn new-location [input] (new-entity :location input))

;; =============================================================================
;; Shared update mechanics
;; =============================================================================

(defn- ensure-audit-input! [entity document input]
  (let [{:keys [actor-id reason]} input
        ctx (context entity document)]
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

(defn- update-entity [entity document now f]
  (ensure-document! entity document)
  (ensure! (model.common/valid-change-time? document (version entity) now)
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
    (update-entity entity document now
                   #(assoc % (k entity :name) normalized-name))))

(defn rename-organization [document input] (rename-entity :organization document input))
(defn rename-organization-group [document input] (rename-entity :organization-group document input))
(defn rename-location [document input] (rename-entity :location document input))

(defn- audit-assoc [document entity audit now actor-id reason]
  (let [at-key (k entity (keyword (str (clojure.core/name audit) "-at")))
        by-key (k entity (keyword (str (clojure.core/name audit) "-by")))
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
        current-parent {:scope/type (get document (k entity :parent-type))
                        :scope/id (get document (k entity :parent-id))}]
    (ensure! (parent-consistent-with-organization? organization-id parent-scope)
             (keyword (clojure.core/name entity) "invalid-parent")
             (str "The " (:label (spec entity)) " operation is invalid.")
             {:parent-scope "The parent must be this organization or an organization group."}
             (context entity document))
    (ensure! (not= (:xt/id document) (:scope/id parent-scope))
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
          (assoc (k entity :parent-type) (:scope/type parent-scope)
                 (k entity :parent-id) (:scope/id parent-scope))
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
             (entity-closed? entity document) (keyword (clojure.core/name entity) "closed")
             (entity-suspended? entity document) (keyword (clojure.core/name entity) "already-suspended")
             :else (keyword (clojure.core/name entity) "not-active"))
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
             (entity-closed? entity document) (keyword (clojure.core/name entity) "closed")
             (entity-active? entity document) (keyword (clojure.core/name entity) "already-active")
             :else (keyword (clojure.core/name entity) "not-suspended"))
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
;; Hierarchy composition
;; =============================================================================

(defn organization-groups-for-organization [groups organization-id]
  (filterv #(and (organization-group-document-consistent? %)
                 (organization-group-for-organization? % organization-id))
           groups))

(defn locations-for-organization [locations organization-id]
  (filterv #(and (location-document-consistent? %)
                 (location-for-organization? % organization-id))
           locations))

(defn- distinct-document-ids? [documents]
  (let [ids (mapv :xt/id documents)] (= (count ids) (count (set ids)))))

(defn- group-chain-consistent? [organization-id initial-parent groups]
  (loop [expected-parent initial-parent
         remaining (seq groups)]
    (if-let [group (first remaining)]
      (and (organization-group-document-consistent? group)
           (organization-group-for-organization? group organization-id)
           (same-scope? expected-parent (organization-group-scope-of group))
           (recur (organization-group-parent-scope group) (next remaining)))
      (same-scope? expected-parent (organization-scope organization-id)))))

(defn organization-group-ancestry-consistent? [organization group ancestors]
  (let [ancestors (vec ancestors)
        organization-id (organization-id organization)]
    (and (organization-document-consistent? organization)
         (organization-group-document-consistent? group)
         (organization-group-for-organization? group organization-id)
         (distinct-document-ids? (into [group] ancestors))
         (group-chain-consistent? organization-id
                                  (organization-group-parent-scope group)
                                  ancestors))))

(defn location-ancestry-consistent? [organization location groups]
  (let [groups (vec groups)
        organization-id (organization-id organization)]
    (and (organization-document-consistent? organization)
         (location-document-consistent? location)
         (location-for-organization? location organization-id)
         (distinct-document-ids? (into [location] groups))
         (group-chain-consistent? organization-id (location-parent-scope location) groups))))

(defn organization-group-operational? [organization group ancestors]
  (and (organization-group-ancestry-consistent? organization group ancestors)
       (organization-active? organization)
       (organization-group-active? group)
       (every? organization-group-active? ancestors)))

(defn location-operational? [organization location groups]
  (and (location-ancestry-consistent? organization location groups)
       (organization-active? organization)
       (location-active? location)
       (every? organization-group-active? groups)))

(defn organization-scope-context [organization]
  (ensure-document! :organization organization)
  {:organization/id (organization-id organization)
   :scope/target (organization-scope-of organization)
   :scope/applicable [(organization-scope-of organization)]
   :scope/operational? (organization-active? organization)})

(defn- hierarchy-scope-context [entity organization target ancestors]
  (let [consistent? (case entity
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
    {:organization/id (organization-id organization)
     :scope/target (target-scope target)
     :scope/applicable (into [(target-scope target)]
                             (concat (map organization-group-scope-of ancestors)
                                     [(organization-scope-of organization)]))
     :scope/operational? (operational? organization target ancestors)}))

(defn organization-group-scope-context [organization group ancestors]
  (hierarchy-scope-context :organization-group organization group ancestors))

(defn location-scope-context [organization location groups]
  (hierarchy-scope-context :location organization location groups))

(defn- authorization-documents [entity organization target ancestors]
  (let [consistent? (case entity
                      :organization-group organization-group-ancestry-consistent?
                      :location location-ancestry-consistent?)]
    (when-not (consistent? organization target ancestors)
      (fail! (keyword (clojure.core/name entity) "invalid-ancestry")
             (str "Authorization documents require a consistent "
                  (:label (spec entity)) " ancestry.")
             {:ancestry "The target and ancestors must form one same-organization chain."}
             (context entity target)))
    (into [target] (concat ancestors [organization]))))

(defn organization-group-authorization-documents [organization group ancestors]
  (authorization-documents :organization-group organization group ancestors))
(defn location-authorization-documents [organization location groups]
  (authorization-documents :location organization location groups))

;; =============================================================================
;; Canonical model commands
;; =============================================================================

(defn- create-command* [entity input]
  (command/create (:entity-type (spec entity)) (new-entity entity input) (version entity)))

(defn- update-command* [entity operation document transition input]
  (command/update-command (:entity-type (spec entity)) operation document
                          (transition document input) (version entity)))

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
