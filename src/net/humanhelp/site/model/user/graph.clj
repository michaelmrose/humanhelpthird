(ns net.humanhelp.site.model.user.graph
  "Graph resolvers for HumanHelp users and their organization relationships.

   This namespace owns XTDB reads for:

   - users
   - memberships
   - role assignments
   - staff invitations
   - request capabilities

   Resolvers expose stored documents and pure facts derived by the user domain.
   Authorization decisions and mutations belong to user.fx.

   Lookup resolvers accept exactly one lookup value. They deliberately reject
   ambiguous lookups instead of silently choosing an identifier."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.graph :as graph]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.capability :as capability]
   [net.humanhelp.site.model.user.domain :as user.domain]
   [net.humanhelp.site.model.user.identity :as identity]
   [net.humanhelp.site.model.user.invitation :as invitation]
   [net.humanhelp.site.model.user.membership :as membership]
   [net.humanhelp.site.model.user.role :as role]))

;; =============================================================================
;; Stored document columns
;; =============================================================================

(def user-document-columns
  [:xt/id
   :user/email
   :user/phone
   :user/phone-display
   :user/display-name
   :user/phone-verified-at
   :user/status
   :user/revision
   :user/joined-at
   :user/updated-at
   :user/suspended-at
   :user/deleted-at])

(def membership-document-columns
  [:xt/id
   :membership/user
   :membership/organization
   :membership/status
   :membership/revision
   :membership/created-at
   :membership/updated-at
   :membership/ended-at])

(def role-assignment-document-columns
  [:xt/id
   :role-assignment/membership
   :role-assignment/role
   :role-assignment/location
   :role-assignment/status
   :role-assignment/revision
   :role-assignment/created-at
   :role-assignment/updated-at
   :role-assignment/ended-at])

(def invitation-document-columns
  [:xt/id
   :invitation/organization
   :invitation/location
   :invitation/phone
   :invitation/email
   :invitation/role
   :invitation/token-hash
   :invitation/status
   :invitation/created-by
   :invitation/accepted-by
   :invitation/revision
   :invitation/created-at
   :invitation/updated-at
   :invitation/expires-at
   :invitation/accepted-at
   :invitation/revoked-at])

(def request-capability-document-columns
  [:xt/id
   :request-capability/request
   :request-capability/user
   :request-capability/token-hash
   :request-capability/status
   :request-capability/revision
   :request-capability/created-at
   :request-capability/updated-at
   :request-capability/expires-at
   :request-capability/last-used-at
   :request-capability/revoked-at])

(def user-document-query
  [:*])

(def membership-document-query
  [:*])

(def role-assignment-document-query
  [:*])

(def invitation-document-query
  [:*])

(def request-capability-document-query
  [:*])

;; =============================================================================
;; Query-input construction
;; =============================================================================

(defn user-query-input
  [{:keys
    [user-id
     phone
     email]}]
  (cond->
   {}

    user-id
    (assoc
     :user/id
     user-id)

    phone
    (assoc
     :user/phone
     (identity/normalize-phone phone))

    email
    (assoc
     :user/email
     (identity/normalize-email email))))

(defn membership-query-input
  [{:keys [membership-id]}]
  (cond->
   {}

    membership-id
    (assoc
     :membership/id
     membership-id)))

(defn role-assignment-query-input
  [{:keys [role-assignment-id]}]
  (cond->
   {}

    role-assignment-id
    (assoc
     :role-assignment/id
     role-assignment-id)))

(defn invitation-query-input
  [{:keys
    [invitation-id
     token-hash]}]
  (cond->
   {}

    invitation-id
    (assoc
     :invitation/id
     invitation-id)

    token-hash
    (assoc
     :invitation/token-hash
     token-hash)))

(defn request-capability-query-input
  [{:keys
    [request-capability-id
     token-hash]}]
  (cond->
   {}

    request-capability-id
    (assoc
     :request-capability/id
     request-capability-id)

    token-hash
    (assoc
     :request-capability/token-hash
     token-hash)))

;; =============================================================================
;; XTDB access
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
     {:error/type
      :user.graph/missing-queryable

      :ctx-keys
      (when (map? ctx)
        (set
         (keys ctx)))}))))

(defn- q
  [ctx query]
  (biffx/q
   (queryable-from-ctx ctx)
   query))

(defn- one-or-none
  "Returns the only row, nil for no rows, and throws for duplicate rows."
  [rows error-data]
  (let [rows
        (vec rows)]
    (case
     (count rows)

      0
      nil

      1
      (first rows)

      (throw
       (ex-info
        "Expected at most one document for a unique lookup."
        (merge
         {:error/type
          :user.graph/non-unique-result

          :result-count
          (count rows)}
         error-data))))))

;; =============================================================================
;; User reads
;; =============================================================================

(defn- load-user
  [ctx
   {:user/keys
    [id
     phone
     email]}]
  (when-not
   (model.common/exactly-one-present?
    id
    phone
    email)
    (throw
     (ex-info
      "A user lookup requires exactly one of user ID, phone, or email."
      {:error/type
       :user.graph/invalid-user-lookup

       :user/id
       id

       :user/phone
       phone

       :user/email
       email})))

  (let [[attribute value]
        (cond
          id
          [:xt/id id]

          phone
          [:user/phone phone]

          :else
          [:user/email email])]
    (one-or-none
     (q
      ctx
      {:select
       user-document-columns

       :from
       identity/entity-type

       :where
       [:= attribute value]})

     {:entity-type
      identity/entity-type

      :lookup-attribute
      attribute

      :lookup-value
      value})))

(defn- load-memberships-for-user
  [ctx user-id]
  (if-not
   (uuid? user-id)
    []

    (vec
     (q
      ctx
      {:select
       membership-document-columns

       :from
       membership/entity-type

       :where
       [:= :membership/user user-id]}))))

;; =============================================================================
;; Membership reads
;; =============================================================================

(defn- load-membership
  [ctx membership-id]
  (when (uuid? membership-id)
    (one-or-none
     (q
      ctx
      {:select
       membership-document-columns

       :from
       membership/entity-type

       :where
       [:= :xt/id membership-id]})

     {:entity-type
      membership/entity-type

      :membership/id
      membership-id})))

(defn- load-role-assignments-for-membership
  [ctx membership-id]
  (if-not
   (uuid? membership-id)
    []

    (vec
     (q
      ctx
      {:select
       role-assignment-document-columns

       :from
       role/entity-type

       :where
       [:=
        :role-assignment/membership
        membership-id]}))))

;; =============================================================================
;; Role-assignment reads
;; =============================================================================

(defn- load-role-assignment
  [ctx assignment-id]
  (when (uuid? assignment-id)
    (one-or-none
     (q
      ctx
      {:select
       role-assignment-document-columns

       :from
       role/entity-type

       :where
       [:= :xt/id assignment-id]})

     {:entity-type
      role/entity-type

      :role-assignment/id
      assignment-id})))

;; =============================================================================
;; Invitation reads
;; =============================================================================

(defn- load-invitation
  [ctx
   {:invitation/keys
    [id
     token-hash]}]
  (when-not
   (model.common/exactly-one-present?
    id
    token-hash)
    (throw
     (ex-info
      "An invitation lookup requires exactly one of invitation ID or token hash."
      {:error/type
       :user.graph/invalid-invitation-lookup

       :invitation/id
       id})))

  (let [[attribute value]
        (if id
          [:xt/id id]
          [:invitation/token-hash token-hash])]
    (one-or-none
     (q
      ctx
      {:select
       invitation-document-columns

       :from
       invitation/entity-type

       :where
       [:= attribute value]})

     {:entity-type
      invitation/entity-type

      :lookup-attribute
      attribute

      :lookup-value
      value})))

;; =============================================================================
;; Request-capability reads
;; =============================================================================

(defn- load-request-capability
  [ctx
   {:request-capability/keys
    [id
     token-hash]}]
  (when-not
   (model.common/exactly-one-present?
    id
    token-hash)
    (throw
     (ex-info
      "A request-capability lookup requires exactly one of capability ID or token hash."
      {:error/type
       :user.graph/invalid-capability-lookup

       :request-capability/id
       id})))

  (let [[attribute value]
        (if id
          [:xt/id id]
          [:request-capability/token-hash token-hash])]
    (one-or-none
     (q
      ctx
      {:select
       request-capability-document-columns

       :from
       capability/entity-type

       :where
       [:= attribute value]})

     {:entity-type
      capability/entity-type

      :lookup-attribute
      attribute

      :lookup-value
      value})))

;; =============================================================================
;; User lookup
;; =============================================================================

(graph/defresolver user-lookup
  {:input
   [[:? :user/id]
    [:? :user/phone]
    [:? :user/email]]

   :output
   [:user/found?

    {[:? :user/doc]
     user-document-query}]}

  [ctx input]
  (if-some [doc
            (load-user ctx input)]
    {:user/found?
     true

     :user/doc
     doc}

    {:user/found?
     false}))

;; =============================================================================
;; User stored fields
;; =============================================================================

(graph/defresolver user-fields
  {:input
   [{:user/doc
     user-document-query}]

   :output
   [:user/id
    [:? :user/email]
    [:? :user/phone]
    [:? :user/phone-display]
    [:? :user/display-name]
    [:? :user/phone-verified-at]
    :user/status
    :user/revision
    :user/joined-at
    :user/updated-at
    [:? :user/suspended-at]
    [:? :user/deleted-at]]}

  [_ctx
   {:user/keys [doc]}]
  (cond->
   {:user/id
    (:xt/id doc)

    :user/status
    (:user/status doc)

    :user/revision
    (:user/revision doc)

    :user/joined-at
    (:user/joined-at doc)

    :user/updated-at
    (:user/updated-at doc)}

    (:user/email doc)
    (assoc
     :user/email
     (:user/email doc))

    (:user/phone doc)
    (assoc
     :user/phone
     (:user/phone doc))

    (:user/phone-display doc)
    (assoc
     :user/phone-display
     (:user/phone-display doc))

    (:user/display-name doc)
    (assoc
     :user/display-name
     (:user/display-name doc))

    (:user/phone-verified-at doc)
    (assoc
     :user/phone-verified-at
     (:user/phone-verified-at doc))

    (:user/suspended-at doc)
    (assoc
     :user/suspended-at
     (:user/suspended-at doc))

    (:user/deleted-at doc)
    (assoc
     :user/deleted-at
     (:user/deleted-at doc))))

(graph/defresolver user-lifecycle-facts
  {:input
   [{:user/doc
     user-document-query}]

   :output
   [:user/active?]}

  [_ctx
   {:user/keys [doc]}]
  {:user/active?
   (identity/active? doc)})

;; =============================================================================
;; User memberships
;; =============================================================================

(graph/defresolver user-memberships
  {:input
   [:user/id]

   :output
   [{:user/memberships
     membership-document-query}]}

  [ctx
   {:user/keys [id]}]
  {:user/memberships
   (load-memberships-for-user
    ctx
    id)})

(graph/defresolver user-active-memberships
  {:input
   [{:user/memberships
     membership-document-query}]

   :output
   [{:user/active-memberships
     membership-document-query}]}

  [_ctx
   {:user/keys [memberships]}]
  {:user/active-memberships
   (user.domain/active-memberships
    memberships)})

(graph/defresolver user-customer-fact
  {:input
   [{:user/memberships
     membership-document-query}]

   :output
   [:user/customer?]}

  [_ctx
   {:user/keys [memberships]}]
  {:user/customer?
   (empty? memberships)})

;; =============================================================================
;; Membership lookup and fields
;; =============================================================================

(graph/defresolver membership-by-id
  {:input
   [:membership/id]

   :output
   [:membership/found?

    {[:? :membership/doc]
     membership-document-query}]}

  [ctx
   {:membership/keys [id]}]
  (if-some [doc
            (load-membership ctx id)]
    {:membership/found?
     true

     :membership/doc
     doc}

    {:membership/found?
     false}))

(graph/defresolver membership-fields
  {:input
   [{:membership/doc
     membership-document-query}]

   :output
   [:membership/id
    :membership/user-id
    :membership/organization-id
    :membership/status
    :membership/revision
    :membership/created-at
    :membership/updated-at
    [:? :membership/ended-at]]}

  [_ctx
   {:membership/keys [doc]}]
  (cond->
   {:membership/id
    (:xt/id doc)

    :membership/user-id
    (:membership/user doc)

    :membership/organization-id
    (:membership/organization doc)

    :membership/status
    (:membership/status doc)

    :membership/revision
    (:membership/revision doc)

    :membership/created-at
    (:membership/created-at doc)

    :membership/updated-at
    (:membership/updated-at doc)}

    (:membership/ended-at doc)
    (assoc
     :membership/ended-at
     (:membership/ended-at doc))))

(graph/defresolver membership-lifecycle-facts
  {:input
   [{:membership/doc
     membership-document-query}]

   :output
   [:membership/active?]}

  [_ctx
   {:membership/keys [doc]}]
  {:membership/active?
   (membership/active? doc)})

;; =============================================================================
;; Membership role assignments
;; =============================================================================

(graph/defresolver membership-role-assignments
  {:input
   [:membership/id]

   :output
   [{:membership/role-assignments
     role-assignment-document-query}]}

  [ctx
   {:membership/keys [id]}]
  {:membership/role-assignments
   (load-role-assignments-for-membership
    ctx
    id)})

(graph/defresolver membership-active-role-assignments
  {:input
   [{:membership/role-assignments
     role-assignment-document-query}]

   :output
   [{:membership/active-role-assignments
     role-assignment-document-query}]}

  [_ctx
   {:membership/keys [role-assignments]}]
  {:membership/active-role-assignments
   (user.domain/active-role-assignments
    role-assignments)})

(graph/defresolver membership-role-facts
  {:input
   [:membership/id

    {:membership/role-assignments
     role-assignment-document-query}]

   :output
   [:membership/roles
    :membership/location-ids]}

  [_ctx
   {:membership/keys
    [id
     role-assignments]}]
  {:membership/roles
   (user.domain/roles-for-membership
    role-assignments
    id)

   :membership/location-ids
   (user.domain/location-ids-for-membership
    role-assignments
    id)})

;; =============================================================================
;; Role-assignment lookup and fields
;; =============================================================================

(graph/defresolver role-assignment-by-id
  {:input
   [:role-assignment/id]

   :output
   [:role-assignment/found?

    {[:? :role-assignment/doc]
     role-assignment-document-query}]}

  [ctx
   {:role-assignment/keys [id]}]
  (if-some [doc
            (load-role-assignment ctx id)]
    {:role-assignment/found?
     true

     :role-assignment/doc
     doc}

    {:role-assignment/found?
     false}))

(graph/defresolver role-assignment-fields
  {:input
   [{:role-assignment/doc
     role-assignment-document-query}]

   :output
   [:role-assignment/id
    :role-assignment/membership-id
    :role-assignment/role
    [:? :role-assignment/location-id]
    :role-assignment/status
    :role-assignment/revision
    :role-assignment/created-at
    :role-assignment/updated-at
    [:? :role-assignment/ended-at]]}

  [_ctx
   {:role-assignment/keys [doc]}]
  (cond->
   {:role-assignment/id
    (:xt/id doc)

    :role-assignment/membership-id
    (:role-assignment/membership doc)

    :role-assignment/role
    (:role-assignment/role doc)

    :role-assignment/status
    (:role-assignment/status doc)

    :role-assignment/revision
    (:role-assignment/revision doc)

    :role-assignment/created-at
    (:role-assignment/created-at doc)

    :role-assignment/updated-at
    (:role-assignment/updated-at doc)}

    (:role-assignment/location doc)
    (assoc
     :role-assignment/location-id
     (:role-assignment/location doc))

    (:role-assignment/ended-at doc)
    (assoc
     :role-assignment/ended-at
     (:role-assignment/ended-at doc))))

(graph/defresolver role-assignment-lifecycle-facts
  {:input
   [{:role-assignment/doc
     role-assignment-document-query}]

   :output
   [:role-assignment/active?
    :role-assignment/organization-wide?]}

  [_ctx
   {:role-assignment/keys [doc]}]
  {:role-assignment/active?
   (role/active? doc)

   :role-assignment/organization-wide?
   (role/organization-wide? doc)})

;; =============================================================================
;; Invitation lookup and fields
;; =============================================================================

(graph/defresolver invitation-lookup
  {:input
   [[:? :invitation/id]
    [:? :invitation/token-hash]]

   :output
   [:invitation/found?

    {[:? :invitation/doc]
     invitation-document-query}]}

  [ctx input]
  (if-some [doc
            (load-invitation ctx input)]
    {:invitation/found?
     true

     :invitation/doc
     doc}

    {:invitation/found?
     false}))

(graph/defresolver invitation-fields
  {:input
   [{:invitation/doc
     invitation-document-query}]

   :output
   [:invitation/id
    :invitation/organization-id
    [:? :invitation/location-id]
    [:? :invitation/phone]
    [:? :invitation/email]
    :invitation/role
    :invitation/token-hash
    :invitation/status
    :invitation/created-by
    [:? :invitation/accepted-by]
    :invitation/revision
    :invitation/created-at
    :invitation/updated-at
    :invitation/expires-at
    [:? :invitation/accepted-at]
    [:? :invitation/revoked-at]]}

  [_ctx
   {:invitation/keys [doc]}]
  (cond->
   {:invitation/id
    (:xt/id doc)

    :invitation/organization-id
    (:invitation/organization doc)

    :invitation/role
    (:invitation/role doc)

    :invitation/token-hash
    (:invitation/token-hash doc)

    :invitation/status
    (:invitation/status doc)

    :invitation/created-by
    (:invitation/created-by doc)

    :invitation/revision
    (:invitation/revision doc)

    :invitation/created-at
    (:invitation/created-at doc)

    :invitation/updated-at
    (:invitation/updated-at doc)

    :invitation/expires-at
    (:invitation/expires-at doc)}

    (:invitation/location doc)
    (assoc
     :invitation/location-id
     (:invitation/location doc))

    (:invitation/phone doc)
    (assoc
     :invitation/phone
     (:invitation/phone doc))

    (:invitation/email doc)
    (assoc
     :invitation/email
     (:invitation/email doc))

    (:invitation/accepted-by doc)
    (assoc
     :invitation/accepted-by
     (:invitation/accepted-by doc))

    (:invitation/accepted-at doc)
    (assoc
     :invitation/accepted-at
     (:invitation/accepted-at doc))

    (:invitation/revoked-at doc)
    (assoc
     :invitation/revoked-at
     (:invitation/revoked-at doc))))

(graph/defresolver invitation-lifecycle-facts
  {:input
   [{:invitation/doc
     invitation-document-query}]

   :output
   [:invitation/pending?
    :invitation/expired?]}

  [_ctx
   {:invitation/keys [doc]}]
  {:invitation/pending?
   (invitation/pending? doc)

   ;; This describes the persisted lifecycle state. Whether a still-pending
   ;; invitation has passed expires-at requires an explicit current time and is
   ;; handled by FX.
   :invitation/expired?
   (invitation/expired-status? doc)})

;; =============================================================================
;; Request-capability lookup and fields
;; =============================================================================

(graph/defresolver request-capability-lookup
  {:input
   [[:? :request-capability/id]
    [:? :request-capability/token-hash]]

   :output
   [:request-capability/found?

    {[:? :request-capability/doc]
     request-capability-document-query}]}

  [ctx input]
  (if-some [doc
            (load-request-capability ctx input)]
    {:request-capability/found?
     true

     :request-capability/doc
     doc}

    {:request-capability/found?
     false}))

(graph/defresolver request-capability-fields
  {:input
   [{:request-capability/doc
     request-capability-document-query}]

   :output
   [:request-capability/id
    :request-capability/request-id
    [:? :request-capability/user-id]
    :request-capability/token-hash
    :request-capability/status
    :request-capability/revision
    :request-capability/created-at
    :request-capability/updated-at
    :request-capability/expires-at
    [:? :request-capability/last-used-at]
    [:? :request-capability/revoked-at]]}

  [_ctx
   {:request-capability/keys [doc]}]
  (cond->
   {:request-capability/id
    (:xt/id doc)

    :request-capability/request-id
    (:request-capability/request doc)

    :request-capability/token-hash
    (:request-capability/token-hash doc)

    :request-capability/status
    (:request-capability/status doc)

    :request-capability/revision
    (:request-capability/revision doc)

    :request-capability/created-at
    (:request-capability/created-at doc)

    :request-capability/updated-at
    (:request-capability/updated-at doc)

    :request-capability/expires-at
    (:request-capability/expires-at doc)}

    (:request-capability/user doc)
    (assoc
     :request-capability/user-id
     (:request-capability/user doc))

    (:request-capability/last-used-at doc)
    (assoc
     :request-capability/last-used-at
     (:request-capability/last-used-at doc))

    (:request-capability/revoked-at doc)
    (assoc
     :request-capability/revoked-at
     (:request-capability/revoked-at doc))))

(graph/defresolver request-capability-lifecycle-facts
  {:input
   [{:request-capability/doc
     request-capability-document-query}]

   :output
   [:request-capability/active?]}

  [_ctx
   {:request-capability/keys [doc]}]
  {:request-capability/active?
   (capability/active? doc)})

;; =============================================================================
;; Shared Graph queries
;; =============================================================================

(def user-summary-query
  [:user/found?

   {[:? :user/doc]
    user-document-query}

   [:? :user/active?]])

(def user-access-query
  [:user/found?

   {[:? :user/doc]
    user-document-query}

   [:? :user/active?]
   [:? :user/customer?]

   {:user/memberships
    [:membership/id
     :membership/user-id
     :membership/organization-id
     :membership/status
     :membership/active?

     {:membership/role-assignments
      [:role-assignment/id
       :role-assignment/membership-id
       :role-assignment/role
       [:? :role-assignment/location-id]
       :role-assignment/status
       :role-assignment/active?
       :role-assignment/organization-wide?]}]}])

(def membership-command-query
  [:membership/found?

   {[:? :membership/doc]
    membership-document-query}

   [:? :membership/active?]

   {[:? :membership/role-assignments]
    role-assignment-document-query}])

(def role-assignment-command-query
  [:role-assignment/found?

   {[:? :role-assignment/doc]
    role-assignment-document-query}

   [:? :role-assignment/active?]
   [:? :role-assignment/organization-wide?]])

(def invitation-command-query
  [:invitation/found?

   {[:? :invitation/doc]
    invitation-document-query}

   [:? :invitation/pending?]
   [:? :invitation/expired?]])

(def request-capability-command-query
  [:request-capability/found?

   {[:? :request-capability/doc]
    request-capability-document-query}

   [:? :request-capability/active?]])

;; =============================================================================
;; Resolver collection
;; =============================================================================

(def resolvers
  [user-lookup
   user-fields
   user-lifecycle-facts
   user-memberships
   user-active-memberships
   user-customer-fact

   membership-by-id
   membership-fields
   membership-lifecycle-facts
   membership-role-assignments
   membership-active-role-assignments
   membership-role-facts

   role-assignment-by-id
   role-assignment-fields
   role-assignment-lifecycle-facts

   invitation-lookup
   invitation-fields
   invitation-lifecycle-facts

   request-capability-lookup
   request-capability-fields
   request-capability-lifecycle-facts])
