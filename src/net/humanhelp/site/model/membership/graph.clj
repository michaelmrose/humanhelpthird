(ns net.humanhelp.site.model.membership.graph
  "Read-only relationship queries for the HumanHelp Membership model.

   gesso.model owns ordinary persisted-document loading and Graph projection for
   Membership and RoleAssignment entities.

   This namespace owns only reads that cannot be represented as singular
   descriptor lookups:

   - Memberships for a User;
   - Memberships for an Organization;
   - the current Membership for one User and Organization;
   - RoleAssignments belonging to a Membership;
   - active RoleAssignments at an exact Organization scope.

   Authorization policy remains in membership.domain. Organization hierarchy
   remains in organization.core. Transaction guards and mutation planning remain
   in membership.fx."
  (:require
   [com.biffweb.xtdb :as biff.xtdb]
   [gesso.model.core :as model]
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.membership.domain :as membership]
   [net.humanhelp.site.model.membership.schema :as membership.schema]
   [net.humanhelp.site.model.organization.core :as organization]))

;; =============================================================================
;; Conventional model surfaces
;; =============================================================================

(def membership-document-query
  (model/document-query
   membership.schema/membership-descriptor))

(def role-assignment-document-query
  (model/document-query
   membership.schema/role-assignment-descriptor))

;; =============================================================================
;; Errors and persistence boundary
;; =============================================================================

(defn- fail!
  [type message details]
  (throw
   (ex-info
    message
    {:error/type type
     :error/details details})))

(defn- require-uuid!
  [value type label]
  (when-not
   (uuid? value)
    (fail!
     type
     (str label " must be a UUID.")
     {:value value}))
  value)

(defn- query-context!
  [ctx]
  (if
   (and
    (map? ctx)
    (or
     (:biff.xtdb/connection-pool ctx)
     (:biff.xtdb/node ctx)))
    ctx

    (fail!
     :membership.graph/missing-biff-connection
     "Membership reads require Biff 2 XTDB context with :biff.xtdb/connection-pool or :biff.xtdb/node."
     {:ctx-keys
      (when
       (map? ctx)
        (set
         (keys ctx)))})))

(defn- deref-if-needed
  [value]
  (if
   (instance?
    clojure.lang.IDeref
    value)
    @value
    value))

(defn- malli-options
  [ctx]
  (some->
   (:biff/malli-opts ctx)
   deref-if-needed))

(defn- q
  [ctx query]
  (biff.xtdb/q
   (query-context! ctx)
   query))

(defn- normalize-row
  [descriptor ctx document]
  (model.schema/normalize-and-validate
   (:document-schema descriptor)
   document
   {:codec-overrides
    (get-in
     descriptor
     [:persistence
      :codec-overrides])

    :malli-options
    (malli-options ctx)}))

(defn- rows
  [descriptor ctx where]
  (mapv
   #(normalize-row
     descriptor
     ctx
     %)
   (q
    ctx
    {:select
     (model/document-columns
      descriptor)

     :from
     [(:entity-type descriptor)]

     :where
     where})))

(defn- sort-documents
  [created-at-key documents]
  (->>
   documents
   (sort-by
    (juxt
     created-at-key
     :xt/id))
   vec))

(defn- exactly-one-or-nil!
  [documents error-type message details]
  (case
   (count documents)

    0
    nil

    1
    (first documents)

    (fail!
     error-type
     message
     (assoc
      details
      :result-count
      (count documents)))))

;; =============================================================================
;; Membership relationship reads
;; =============================================================================

(defn memberships-for-user
  "Returns every Membership for User, including historical revoked Memberships,
   in stable creation order."
  [ctx user-id]
  (require-uuid!
   user-id
   :membership.graph/invalid-user-id
   "User ID")

  (sort-documents
   :membership/created-at
   (rows
    membership.schema/membership-descriptor
    ctx
    [:=
     :membership/user
     user-id])))

(defn memberships-for-organization
  "Returns every Membership for Organization, including historical revoked
   Memberships, in stable creation order."
  [ctx organization-id]
  (require-uuid!
   organization-id
   :membership.graph/invalid-organization-id
   "Organization ID")

  (sort-documents
   :membership/created-at
   (rows
    membership.schema/membership-descriptor
    ctx
    [:=
     :membership/organization
     organization-id])))

(defn memberships-for-user-and-organization
  "Returns every historical Membership linking User to Organization."
  [ctx user-id organization-id]
  (require-uuid!
   user-id
   :membership.graph/invalid-user-id
   "User ID")

  (require-uuid!
   organization-id
   :membership.graph/invalid-organization-id
   "Organization ID")

  (sort-documents
   :membership/created-at
   (rows
    membership.schema/membership-descriptor
    ctx
    [:and
     [:=
      :membership/user
      user-id]

     [:=
      :membership/organization
      organization-id]])))

(defn current-membership
  "Returns the one non-revoked Membership connecting User and Organization.

   Suspended Memberships remain current relationships; they simply confer no
   current authority.

   More than one non-revoked Membership for the same User and Organization is
   treated as persisted corruption rather than arbitrarily selecting one."
  [ctx user-id organization-id]
  (exactly-one-or-nil!
   (filterv
    #(not
      (membership/membership-revoked?
       %))
    (memberships-for-user-and-organization
     ctx
     user-id
     organization-id))

   :membership.graph/non-unique-current-membership

   "More than one current Membership exists for the same User and Organization."

   {:user/id
    user-id

    :organization/id
    organization-id}))

(defn active-memberships-for-organization
  "Returns active Memberships for Organization."
  [ctx organization-id]
  (filterv
   membership/membership-active?
   (memberships-for-organization
    ctx
    organization-id)))

;; =============================================================================
;; RoleAssignment relationship reads
;; =============================================================================

(defn role-assignments-for-membership
  "Returns every RoleAssignment owned by Membership, including revoked history,
   in stable creation order."
  [ctx membership-id]
  (require-uuid!
   membership-id
   :membership.graph/invalid-membership-id
   "Membership ID")

  (sort-documents
   :role-assignment/created-at
   (rows
    membership.schema/role-assignment-descriptor
    ctx
    [:=
     :role-assignment/membership
     membership-id])))

(defn active-role-assignments-for-membership
  "Returns Membership's currently active RoleAssignments."
  [ctx membership-id]
  (filterv
   membership/role-assignment-active?
   (role-assignments-for-membership
    ctx
    membership-id)))

(defn role-assignments-at-scope
  "Returns every RoleAssignment at one exact Organization scope, including
   revoked history.

   Organization ownership is intentionally not duplicated onto RoleAssignment.
   The structural scope itself identifies the assignment target."
  [ctx scope]
  (when-not
   (organization/scope? scope)
    (fail!
     :membership.graph/invalid-scope
     "RoleAssignment scope must be a valid Organization scope."
     {:scope scope}))

  (sort-documents
   :role-assignment/created-at
   (rows
    membership.schema/role-assignment-descriptor
    ctx
    [:and
     [:=
      :role-assignment/scope-type
      (organization/scope-type scope)]

     [:=
      :role-assignment/scope-id
      (organization/scope-id scope)]])))

(defn active-role-assignments-at-scope
  "Returns every active RoleAssignment at one exact Organization scope."
  [ctx scope]
  (filterv
   membership/role-assignment-active?
   (role-assignments-at-scope
    ctx
    scope)))

(defn active-role-assignments-for-membership-at-scope
  "Returns Membership's active assignments at one exact Organization scope."
  [ctx membership-id scope]
  (when-not
   (organization/scope? scope)
    (fail!
     :membership.graph/invalid-scope
     "RoleAssignment scope must be a valid Organization scope."
     {:scope scope}))

  (require-uuid!
   membership-id
   :membership.graph/invalid-membership-id
   "Membership ID")

  (sort-documents
   :role-assignment/created-at
   (rows
    membership.schema/role-assignment-descriptor
    ctx
    [:and
     [:=
      :role-assignment/membership
      membership-id]

     [:=
      :role-assignment/scope-type
      (organization/scope-type scope)]

     [:=
      :role-assignment/scope-id
      (organization/scope-id scope)]

     [:=
      :role-assignment/status
      :active]])))

;; =============================================================================
;; Membership authorization read snapshot
;; =============================================================================

(defn membership-access-snapshot
  "Returns the current Membership relationship and all of its RoleAssignments.

   Returns nil when User has no current Membership in Organization.

   This function deliberately does not calculate effective authorization.
   Callers obtain an authoritative Organization scope-context separately and
   pass these documents to membership.domain."
  [ctx user-id organization-id]
  (when-let [membership-document
             (current-membership
              ctx
              user-id
              organization-id)]
    {:membership
     membership-document

     :role-assignments
     (role-assignments-for-membership
      ctx
      (membership/membership-id
       membership-document))}))

;; =============================================================================
;; Custom Graph contribution
;; =============================================================================

(def custom-resolvers
  "Membership currently needs no hand-written public Graph resolvers.

   Its ordinary entity Graph surfaces are descriptor-generated. The relational
   reads above are internal model reads consumed by Membership core and FX."
  [])
