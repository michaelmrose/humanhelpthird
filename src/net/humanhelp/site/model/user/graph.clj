(ns net.humanhelp.site.model.user.graph
  "Read-only Gesso Graph resolvers for the HumanHelp User model.

   This namespace loads User-model documents from XTDB, traverses their
   relationships, and derives customer and scoped-access facts. It performs no
   mutations, mutation authorization, messaging, session handling, or
   Organization hierarchy resolution.

   :user/applicable-scopes must be supplied by Organization. For a location it
   normally contains that location, every containing organization group, and
   the organization itself."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [net.humanhelp.site.model.user.domain.access :as access]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.model.user.domain.invitation :as invitation]
   [net.humanhelp.site.model.user.domain.membership :as membership]
   [net.humanhelp.site.model.user.domain.role :as role]))

;; =============================================================================
;; Stored documents
;; =============================================================================

(def user-document-columns
  [:xt/id
   :user/phone
   :user/email
   :user/display-name
   :user/status
   :user/revision
   :user/created-at
   :user/updated-at
   :user/phone-verified-at
   :user/email-verified-at
   :user/suspended-at
   :user/suspended-by
   :user/suspension-reason
   :user/deleted-at
   :user/deleted-by
   :user/deletion-reason])

(def membership-document-columns
  [:xt/id
   :membership/user
   :membership/organization
   :membership/status
   :membership/revision
   :membership/created-at
   :membership/updated-at
   :membership/suspended-at
   :membership/suspended-by
   :membership/suspension-reason
   :membership/revoked-at
   :membership/revoked-by
   :membership/revocation-reason])

(def role-assignment-document-columns
  [:xt/id
   :role-assignment/membership
   :role-assignment/organization
   :role-assignment/role
   :role-assignment/scope-type
   :role-assignment/scope-id
   :role-assignment/status
   :role-assignment/revision
   :role-assignment/created-at
   :role-assignment/updated-at
   :role-assignment/assigned-by
   :role-assignment/assignment-reason
   :role-assignment/revoked-at
   :role-assignment/revoked-by
   :role-assignment/revocation-reason])

(def invitation-document-columns
  [:xt/id
   :invitation/organization
   :invitation/invited-by
   :invitation/phone
   :invitation/email
   :invitation/role
   :invitation/scope-type
   :invitation/scope-id
   :invitation/token-hash
   :invitation/status
   :invitation/revision
   :invitation/created-at
   :invitation/updated-at
   :invitation/expires-at
   :invitation/accepted-at
   :invitation/accepted-by
   :invitation/membership
   :invitation/role-assignment
   :invitation/declined-at
   :invitation/declined-by
   :invitation/revoked-at
   :invitation/revoked-by
   :invitation/revocation-reason])

(def user-document-query [:*])
(def membership-document-query [:*])
(def role-assignment-document-query [:*])
(def invitation-document-query [:*])

;; =============================================================================
;; Graph field projections
;; =============================================================================

(def user-field-pairs
  [[:xt/id :user/id]
   [:user/phone :user/phone]
   [:user/email :user/email]
   [:user/display-name :user/display-name]
   [:user/status :user/status]
   [:user/revision :user/revision]
   [:user/created-at :user/created-at]
   [:user/updated-at :user/updated-at]
   [:user/phone-verified-at :user/phone-verified-at]
   [:user/email-verified-at :user/email-verified-at]
   [:user/suspended-at :user/suspended-at]
   [:user/suspended-by :user/suspended-by]
   [:user/suspension-reason :user/suspension-reason]
   [:user/deleted-at :user/deleted-at]
   [:user/deleted-by :user/deleted-by]
   [:user/deletion-reason :user/deletion-reason]])

(def membership-field-pairs
  [[:xt/id :membership/id]
   [:membership/user :membership/user-id]
   [:membership/organization :membership/organization-id]
   [:membership/status :membership/status]
   [:membership/revision :membership/revision]
   [:membership/created-at :membership/created-at]
   [:membership/updated-at :membership/updated-at]
   [:membership/suspended-at :membership/suspended-at]
   [:membership/suspended-by :membership/suspended-by]
   [:membership/suspension-reason :membership/suspension-reason]
   [:membership/revoked-at :membership/revoked-at]
   [:membership/revoked-by :membership/revoked-by]
   [:membership/revocation-reason :membership/revocation-reason]])

(def role-assignment-field-pairs
  [[:xt/id :role-assignment/id]
   [:role-assignment/membership :role-assignment/membership-id]
   [:role-assignment/organization :role-assignment/organization-id]
   [:role-assignment/role :role-assignment/role]
   [:role-assignment/scope-type :role-assignment/scope-type]
   [:role-assignment/scope-id :role-assignment/scope-id]
   [:role-assignment/status :role-assignment/status]
   [:role-assignment/revision :role-assignment/revision]
   [:role-assignment/created-at :role-assignment/created-at]
   [:role-assignment/updated-at :role-assignment/updated-at]
   [:role-assignment/assigned-by :role-assignment/assigned-by]
   [:role-assignment/assignment-reason :role-assignment/assignment-reason]
   [:role-assignment/revoked-at :role-assignment/revoked-at]
   [:role-assignment/revoked-by :role-assignment/revoked-by]
   [:role-assignment/revocation-reason
    :role-assignment/revocation-reason]])

(def invitation-field-pairs
  [[:xt/id :invitation/id]
   [:invitation/organization :invitation/organization-id]
   [:invitation/invited-by :invitation/invited-by-id]
   [:invitation/phone :invitation/phone]
   [:invitation/email :invitation/email]
   [:invitation/role :invitation/role]
   [:invitation/scope-type :invitation/scope-type]
   [:invitation/scope-id :invitation/scope-id]
   [:invitation/status :invitation/status]
   [:invitation/revision :invitation/revision]
   [:invitation/created-at :invitation/created-at]
   [:invitation/updated-at :invitation/updated-at]
   [:invitation/expires-at :invitation/expires-at]
   [:invitation/accepted-at :invitation/accepted-at]
   [:invitation/accepted-by :invitation/accepted-by-id]
   [:invitation/membership :invitation/membership-id]
   [:invitation/role-assignment :invitation/role-assignment-id]
   [:invitation/declined-at :invitation/declined-at]
   [:invitation/declined-by :invitation/declined-by-id]
   [:invitation/revoked-at :invitation/revoked-at]
   [:invitation/revoked-by :invitation/revoked-by-id]
   [:invitation/revocation-reason :invitation/revocation-reason]])

(def user-field-query
  (mapv second user-field-pairs))

(def membership-field-query
  (mapv second membership-field-pairs))

(def role-assignment-field-query
  (conj
   (mapv second role-assignment-field-pairs)
   {:role-assignment/scope [:scope/type :scope/id]}))

(def invitation-field-query
  (into
   (mapv second invitation-field-pairs)
   [:invitation/recipient-type
    :invitation/recipient-value
    {:invitation/scope [:scope/type :scope/id]}]))

(defn- project-document
  [document field-pairs]
  (reduce
   (fn [result [document-key graph-key]]
     (if-some [value (get document document-key)]
       (assoc result graph-key value)
       result))
   {}
   field-pairs))

;; =============================================================================
;; Query inputs
;; =============================================================================

(defn- without-nils
  [m]
  (into {} (remove (comp nil? val)) m))

(defn user-query-input
  [{:keys [user-id phone email]}]
  (without-nils
   {:user/id user-id
    :user/phone phone
    :user/email email}))

(defn membership-query-input
  [{:keys [membership-id]}]
  (without-nils {:membership/id membership-id}))

(defn role-assignment-query-input
  [{:keys [role-assignment-id]}]
  (without-nils {:role-assignment/id role-assignment-id}))

(defn invitation-query-input
  [{:keys [invitation-id token-hash]}]
  (without-nils
   {:invitation/id invitation-id
    :invitation/token-hash token-hash}))

(defn customer-query-input
  [{:keys [user-id]}]
  (without-nils {:user/id user-id}))

(defn access-query-input
  [{:keys [user-id organization-id applicable-scopes]}]
  (without-nils
   {:user/id user-id
    :membership/organization-id organization-id
    :user/applicable-scopes
    (when (some? applicable-scopes)
      (vec applicable-scopes))}))

(defn scoped-role-assignment-query-input
  [{:keys [organization-id scope]}]
  (without-nils
   {:role-assignment/organization-id organization-id
    :role-assignment/scope-type (:scope/type scope)
    :role-assignment/scope-id (:scope/id scope)}))

;; =============================================================================
;; XTDB reads
;; =============================================================================

(defn- queryable-from-ctx
  [ctx]
  (or
   (:biff/conn ctx)
   (:biff/db ctx)
   (:biff/node ctx)
   (:xtdb/node ctx)
   (throw
    (ex-info
     "User Graph requires :biff/conn, :biff/db, :biff/node, or :xtdb/node."
     {:error/type :user.graph/missing-queryable
      :ctx-keys (when (map? ctx) (set (keys ctx)))}))))

(defn- q
  [ctx query]
  (biffx/q (queryable-from-ctx ctx) query))

(defn- load-by-id
  [ctx table columns id]
  (when
   (uuid? id)
    (first
     (q ctx
        {:select columns
         :from table
         :where [:= :xt/id id]}))))

(defn- exactly-one-or-nil!
  [documents error-data]
  (case (count documents)
    0 nil
    1 (first documents)
    (throw
     (ex-info
      "A User-model lookup expected at most one current document."
      (merge
       {:error/type :user.graph/non-unique-result
        :result-count (count documents)}
       error-data)))))

(defn- sort-documents
  [created-at-key documents]
  (->> documents
       (sort-by (juxt created-at-key :xt/id))
       vec))

(defn- rows
  [ctx table columns where]
  (q ctx
     {:select columns
      :from table
      :where where}))

(defn- load-user
  [ctx user-id]
  (load-by-id ctx identity/entity-type user-document-columns user-id))

(defn- user-by-phone-doc
  [ctx phone]
  (when
   (string? phone)
    (exactly-one-or-nil!
     (rows ctx
           identity/entity-type
           user-document-columns
           [:= :user/phone phone])
     {:entity-type identity/entity-type
      :lookup :phone})))

(defn- user-by-email-doc
  [ctx email]
  (when
   (string? email)
    (exactly-one-or-nil!
     (rows ctx
           identity/entity-type
           user-document-columns
           [:= :user/email email])
     {:entity-type identity/entity-type
      :lookup :email})))

(defn- load-membership
  [ctx membership-id]
  (load-by-id
   ctx
   membership/entity-type
   membership-document-columns
   membership-id))

(defn- memberships-for-user
  [ctx user-id]
  (if
   (uuid? user-id)
    (sort-documents
     :membership/created-at
     (rows ctx
           membership/entity-type
           membership-document-columns
           [:= :membership/user user-id]))
    []))

(defn- current-membership
  [ctx user-id organization-id]
  (if
   (and (uuid? user-id) (uuid? organization-id))
    (->>
     (rows
      ctx
      membership/entity-type
      membership-document-columns
      [:and
       [:= :membership/user user-id]
       [:= :membership/organization organization-id]])
     (filterv access/current-membership?)
     (exactly-one-or-nil!
      {:entity-type membership/entity-type
       :lookup :current-user-and-organization
       :user/id user-id
       :membership/organization-id organization-id}))
    nil))

(defn- load-role-assignment
  [ctx role-assignment-id]
  (load-by-id
   ctx
   role/entity-type
   role-assignment-document-columns
   role-assignment-id))

(defn- role-assignments-for-membership
  [ctx membership-id]
  (if
   (uuid? membership-id)
    (sort-documents
     :role-assignment/created-at
     (rows
      ctx
      role/entity-type
      role-assignment-document-columns
      [:= :role-assignment/membership membership-id]))
    []))

(defn- active-role-assignments-at-scope
  [ctx organization-id scope-type scope-id]
  (if
   (and
    (uuid? organization-id)
    (keyword? scope-type)
    (uuid? scope-id))
    (sort-documents
     :role-assignment/created-at
     (rows
      ctx
      role/entity-type
      role-assignment-document-columns
      [:and
       [:= :role-assignment/organization organization-id]
       [:= :role-assignment/scope-type scope-type]
       [:= :role-assignment/scope-id scope-id]
       [:= :role-assignment/status :active]]))
    []))

(defn- load-invitation
  [ctx invitation-id]
  (load-by-id
   ctx
   invitation/entity-type
   invitation-document-columns
   invitation-id))

(defn- invitation-by-token-hash-doc
  [ctx token-hash]
  (when
   (string? token-hash)
    (exactly-one-or-nil!
     (rows
      ctx
      invitation/entity-type
      invitation-document-columns
      [:= :invitation/token-hash token-hash])
     {:entity-type invitation/entity-type
      :lookup :token-hash})))

(defn- lookup-result
  [found-key document-key document]
  (if document
    {found-key true
     document-key document}
    {found-key false}))

(defn- id-seeds
  [id-key documents]
  (mapv
   (fn [document]
     {id-key (:xt/id document)})
   documents))

;; =============================================================================
;; User identity
;; =============================================================================

(graph/defresolver user-by-id
  {:input [:user/id]
   :output
   [:user/found?
    {[:? :user/doc] user-document-query}]}
  [ctx {:user/keys [id]}]
  (lookup-result
   :user/found?
   :user/doc
   (load-user ctx id)))

(graph/defresolver user-by-phone
  {:input [:user/phone]
   :output
   [:user/found?
    {[:? :user/doc] user-document-query}]}
  [ctx {:user/keys [phone]}]
  (lookup-result
   :user/found?
   :user/doc
   (user-by-phone-doc ctx phone)))

(graph/defresolver user-by-email
  {:input [:user/email]
   :output
   [:user/found?
    {[:? :user/doc] user-document-query}]}
  [ctx {:user/keys [email]}]
  (lookup-result
   :user/found?
   :user/doc
   (user-by-email-doc ctx email)))

(graph/defresolver user-fields
  {:input [{:user/doc user-document-query}]
   :output user-field-query}
  [_ctx {:user/keys [doc]}]
  (project-document doc user-field-pairs))

;; =============================================================================
;; Memberships
;; =============================================================================

(graph/defresolver membership-by-id
  {:input [:membership/id]
   :output
   [:membership/found?
    {[:? :membership/doc] membership-document-query}]}
  [ctx {:membership/keys [id]}]
  (lookup-result
   :membership/found?
   :membership/doc
   (load-membership ctx id)))

(graph/defresolver memberships-for-user-resolver
  {:input [:user/id]
   :output [{:user/memberships [:membership/id]}]}
  [ctx {:user/keys [id]}]
  {:user/memberships
   (id-seeds
    :membership/id
    (memberships-for-user ctx id))})

(graph/defresolver current-membership-for-organization
  {:input
   [:user/id
    :membership/organization-id]

   :output
   [:user/current-membership-found?
    {[:? :user/current-membership]
     [:membership/id]}]}
  [ctx
   {:user/keys [id]
    :membership/keys [organization-id]}]
  (if-some [document
            (current-membership ctx id organization-id)]
    {:user/current-membership-found? true
     :user/current-membership
     {:membership/id (:xt/id document)}}
    {:user/current-membership-found? false}))

(graph/defresolver membership-fields
  {:input [{:membership/doc membership-document-query}]
   :output membership-field-query}
  [_ctx {:membership/keys [doc]}]
  (project-document doc membership-field-pairs))

;; =============================================================================
;; Role assignments
;; =============================================================================

(graph/defresolver role-assignment-by-id
  {:input [:role-assignment/id]
   :output
   [:role-assignment/found?
    {[:? :role-assignment/doc]
     role-assignment-document-query}]}
  [ctx {:role-assignment/keys [id]}]
  (lookup-result
   :role-assignment/found?
   :role-assignment/doc
   (load-role-assignment ctx id)))

(graph/defresolver role-assignments-for-membership-resolver
  {:input [:membership/id]
   :output
   [{:membership/role-assignments
     [:role-assignment/id]}]}
  [ctx {:membership/keys [id]}]
  {:membership/role-assignments
   (id-seeds
    :role-assignment/id
    (role-assignments-for-membership ctx id))})

(graph/defresolver active-role-assignments-at-scope-resolver
  {:input
   [:role-assignment/organization-id
    :role-assignment/scope-type
    :role-assignment/scope-id]

   :output
   [{:user/active-role-assignments-at-scope
     [:role-assignment/id]}]}
  [ctx
   {:role-assignment/keys
    [organization-id scope-type scope-id]}]
  {:user/active-role-assignments-at-scope
   (id-seeds
    :role-assignment/id
    (active-role-assignments-at-scope
     ctx
     organization-id
     scope-type
     scope-id))})

(graph/defresolver role-assignment-fields
  {:input
   [{:role-assignment/doc role-assignment-document-query}]
   :output role-assignment-field-query}
  [_ctx {:role-assignment/keys [doc]}]
  (assoc
   (project-document doc role-assignment-field-pairs)
   :role-assignment/scope
   (role/scope doc)))

;; =============================================================================
;; Invitations
;; =============================================================================

(graph/defresolver invitation-by-id
  {:input [:invitation/id]
   :output
   [:invitation/found?
    {[:? :invitation/doc] invitation-document-query}]}
  [ctx {:invitation/keys [id]}]
  (lookup-result
   :invitation/found?
   :invitation/doc
   (load-invitation ctx id)))

(graph/defresolver invitation-by-token-hash
  {:input [:invitation/token-hash]
   :output
   [:invitation/found?
    {[:? :invitation/doc] invitation-document-query}]}
  [ctx {:invitation/keys [token-hash]}]
  (lookup-result
   :invitation/found?
   :invitation/doc
   (invitation-by-token-hash-doc ctx token-hash)))

(graph/defresolver invitation-fields
  {:input [{:invitation/doc invitation-document-query}]
   :output invitation-field-query}
  [_ctx {:invitation/keys [doc]}]
  (merge
   (project-document doc invitation-field-pairs)
   {:invitation/recipient-type
    (invitation/recipient-type doc)

    :invitation/recipient-value
    (invitation/recipient-value doc)

    :invitation/scope
    (invitation/scope doc)}))

;; =============================================================================
;; Customer and access facts
;; =============================================================================

(graph/defresolver user-customer-facts
  {:input
   [{:user/doc user-document-query}
    {:user/memberships
     [{:membership/doc membership-document-query}]}]

   :output
   [:user/organization-affiliated?
    :user/customer?]}
  [_ctx input]
  (let [user (:user/doc input)
        memberships
        (mapv :membership/doc (:user/memberships input))]
    {:user/organization-affiliated?
     (access/organization-affiliated? user memberships)

     :user/customer?
     (access/customer? user memberships)}))

(graph/defresolver user-access-facts
  {:input
   [{:user/doc user-document-query}

    {:user/current-membership
     [{:membership/doc membership-document-query}
      {:membership/role-assignments
       [{:role-assignment/doc
         role-assignment-document-query}]}]}

    {:user/applicable-scopes
     [:scope/type :scope/id]}]

   :output
   [:user/effective-roles
    :user/helper?
    :user/supervisor?
    :user/admin?
    :user/staff?]}
  [_ctx input]
  (let [user (:user/doc input)
        membership-node (:user/current-membership input)
        membership (:membership/doc membership-node)
        role-assignments
        (mapv
         :role-assignment/doc
         (:membership/role-assignments membership-node))
        applicable-scopes (:user/applicable-scopes input)]
    {:user/effective-roles
     (access/effective-roles
      user membership role-assignments applicable-scopes)

     :user/helper?
     (access/helper?
      user membership role-assignments applicable-scopes)

     :user/supervisor?
     (access/supervisor?
      user membership role-assignments applicable-scopes)

     :user/admin?
     (access/admin?
      user membership role-assignments applicable-scopes)

     :user/staff?
     (access/staff?
      user membership role-assignments applicable-scopes)}))

;; =============================================================================
;; Public query contracts
;; =============================================================================

(def user-command-query
  [:user/found?
   {[:? :user/doc] user-document-query}])

(def membership-command-query
  [:membership/found?
   {[:? :membership/doc] membership-document-query}])

(def role-assignment-command-query
  [:role-assignment/found?
   {[:? :role-assignment/doc]
    role-assignment-document-query}])

(def invitation-command-query
  [:invitation/found?
   {[:? :invitation/doc] invitation-document-query}])

(def customer-query
  [:user/found?
   {[:? :user/doc] user-document-query}
   {[:? :user/memberships]
    [{:membership/doc membership-document-query}]}
   [:? :user/organization-affiliated?]
   [:? :user/customer?]])

(def access-query
  [:user/found?
   {[:? :user/doc] user-document-query}

   :user/current-membership-found?

   {[:? :user/current-membership]
    [{:membership/doc membership-document-query}
     {:membership/role-assignments
      [{:role-assignment/doc
        role-assignment-document-query}]}]}

   [:? :user/effective-roles]
   [:? :user/helper?]
   [:? :user/supervisor?]
   [:? :user/admin?]
   [:? :user/staff?]])

(def active-role-assignments-at-scope-query
  [{:user/active-role-assignments-at-scope
    [{:role-assignment/doc
      role-assignment-document-query}]}])

;; =============================================================================
;; Resolver collection
;; =============================================================================

(def resolvers
  [user-by-id
   user-by-phone
   user-by-email
   user-fields

   membership-by-id
   memberships-for-user-resolver
   current-membership-for-organization
   membership-fields

   role-assignment-by-id
   role-assignments-for-membership-resolver
   active-role-assignments-at-scope-resolver
   role-assignment-fields

   invitation-by-id
   invitation-by-token-hash
   invitation-fields

   user-customer-facts
   user-access-facts])
