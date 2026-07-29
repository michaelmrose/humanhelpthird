(ns net.humanhelp.site.model.invitation.graph
  "Invitation-specific persistence reads.

   gesso.model owns ordinary Invitation loading by :xt/id and generated Graph
   field projection from invitation.schema.

   This namespace owns Invitation reads that are not conventional singular
   descriptor lookups:

   - lookup by opaque token hash;
   - Invitation collections by Organization and inviter;
   - Invitation collections by recipient;
   - pending Invitation collections;
   - exact pending-offer lookup.

   Token hashes are deliberately NOT exposed through Gesso Graph resolvers.
   Token-based lookup is an internal Invitation-model operation used by FX.

   Every XTDB document crosses the same persistence boundary as generated
   gesso.model reads:

     query
       -> schema-derived persistence normalization
       -> Malli/domain validation

   No mutation, authorization, token generation, token hashing, or transaction
   composition belongs here."
  (:require
   [com.biffweb.experimental :as biffx]
   [gesso.model.core :as model]
   [gesso.model.schema :as model.schema]
   [net.humanhelp.site.model.invitation.domain :as invitation]
   [net.humanhelp.site.model.invitation.schema :as invitation.schema]
   [net.humanhelp.site.model.organization.core :as organization]))

;; =============================================================================
;; Conventional model metadata
;; =============================================================================

(def invitation-descriptor
  invitation.schema/invitation-descriptor)

(def invitation-document-columns
  (model/document-columns
   invitation-descriptor))

(def invitation-document-query
  (model/document-query
   invitation-descriptor))

;; =============================================================================
;; Errors
;; =============================================================================

(defn- fail!
  [type message details]
  (throw
   (ex-info
    message
    {:error/type
     type

     :error/details
     details})))

;; =============================================================================
;; Persistence boundary
;; =============================================================================

(defn- connection!
  [ctx]
  (or
   (:biff/conn
    ctx)

   (fail!
    :invitation.graph/missing-biff-connection
    "Invitation reads require :biff/conn."
    {:ctx-keys
     (when
      (map?
       ctx)
      (set
       (keys
        ctx)))})))

(defn- malli-options
  [ctx]
  (let [value
        (:biff/malli-opts
         ctx)]
    (if
     (instance?
      clojure.lang.IDeref
      value)
      @value
      value)))

(defn- normalize-loaded-document
  [ctx document]
  (model.schema/normalize-and-validate
   invitation.schema/invitation-document-schema
   document
   {:codec-overrides
    (get-in
     invitation-descriptor
     [:persistence
      :codec-overrides])

    :malli-options
    (malli-options
     ctx)}))

(defn- q
  [ctx query]
  (biffx/q
   (connection!
    ctx)
   query))

(defn- rows
  [ctx where]
  (mapv
   #(normalize-loaded-document
     ctx
     %)
   (q
    ctx
    {:select
     invitation-document-columns

     :from
     invitation/entity-type

     :where
     where})))

;; =============================================================================
;; Deterministic collections
;; =============================================================================

(defn- sort-invitations
  [invitations]
  (->>
   invitations
   (sort-by
    (juxt
     :invitation/created-at
     :xt/id))
   vec))

(defn- sorted-rows
  [ctx where]
  (sort-invitations
   (rows
    ctx
    where)))

;; =============================================================================
;; Singular-result validation
;; =============================================================================

(defn- exactly-one-or-nil!
  [documents error-type message details]
  (case
   (count
    documents)

   0
   nil

   1
   (first
    documents)

   (fail!
    error-type
    message
    (assoc
     details
     :result-count
     (count
      documents)))))

;; =============================================================================
;; Token lookup
;; =============================================================================

(defn invitation-by-token-hash
  "Returns the Invitation identified by token-hash, or nil.

   More than one matching document is persisted corruption because an opaque
   bearer token must identify at most one Invitation.

   This function is intentionally not represented by a custom Graph resolver.
   Raw token hashes remain internal to the Invitation model."
  [ctx token-hash]
  (when
   (invitation/token-hash?
    token-hash)

    (exactly-one-or-nil!
     (rows
      ctx
      [:=
       :invitation/token-hash
       token-hash])

     :invitation.graph/non-unique-token-hash
     "An Invitation token hash identifies more than one Invitation."
     {:invitation/token-hash
      token-hash})))

;; =============================================================================
;; Organization collections
;; =============================================================================

(defn invitations-for-organization
  "Returns every historical Invitation belonging to Organization.

   Results are ordered by creation time and then Invitation UUID."
  [ctx organization-id]
  (if
   (uuid?
    organization-id)

    (sorted-rows
     ctx
     [:=
      :invitation/organization
      organization-id])

    []))

(defn pending-invitations-for-organization
  "Returns Organization's currently persisted pending Invitations.

   This is a persisted-status query. It does not reinterpret a pending
   Invitation whose expires-at has passed as :expired; explicit expiration
   materialization belongs to Invitation FX."
  [ctx organization-id]
  (if
   (uuid?
    organization-id)

    (sorted-rows
     ctx
     [:and
      [:=
       :invitation/organization
       organization-id]

      [:=
       :invitation/status
       :pending]])

    []))

;; =============================================================================
;; Inviter collections
;; =============================================================================

(defn invitations-by-inviter
  "Returns every historical Invitation created by User."
  [ctx user-id]
  (if
   (uuid?
    user-id)

    (sorted-rows
     ctx
     [:=
      :invitation/invited-by
      user-id])

    []))

(defn pending-invitations-by-inviter
  "Returns pending Invitations created by User."
  [ctx user-id]
  (if
   (uuid?
    user-id)

    (sorted-rows
     ctx
     [:and
      [:=
       :invitation/invited-by
       user-id]

      [:=
       :invitation/status
       :pending]])

    []))

;; =============================================================================
;; Recipient collections
;; =============================================================================

(defn invitations-for-phone
  "Returns every historical Invitation addressed to canonical phone."
  [ctx phone]
  (if
   (string?
    phone)

    (sorted-rows
     ctx
     [:=
      :invitation/phone
      phone])

    []))

(defn invitations-for-email
  "Returns every historical Invitation addressed to canonical email."
  [ctx email]
  (if
   (string?
    email)

    (sorted-rows
     ctx
     [:=
      :invitation/email
      email])

    []))

(defn pending-invitations-for-phone
  "Returns pending Invitations addressed to canonical phone."
  [ctx phone]
  (if
   (string?
    phone)

    (sorted-rows
     ctx
     [:and
      [:=
       :invitation/phone
       phone]

      [:=
       :invitation/status
       :pending]])

    []))

(defn pending-invitations-for-email
  "Returns pending Invitations addressed to canonical email."
  [ctx email]
  (if
   (string?
    email)

    (sorted-rows
     ctx
     [:and
      [:=
       :invitation/email
       email]

      [:=
       :invitation/status
       :pending]])

    []))

(defn pending-invitations-for-recipient
  "Returns pending Invitations for exactly one canonical recipient.

   recipient must contain exactly one of:

     {:phone canonical-phone}
     {:email canonical-email}

   Invalid or ambiguous recipient values return an empty collection. Input
   normalization belongs to Invitation FX or the caller."
  [ctx {:keys
        [phone
         email]}]
  (cond
    (and
     (string?
      phone)
     (nil?
      email))
    (pending-invitations-for-phone
     ctx
     phone)

    (and
     (string?
      email)
     (nil?
      phone))
    (pending-invitations-for-email
     ctx
     email)

    :else
    []))

;; =============================================================================
;; Exact pending offer
;; =============================================================================

(defn- recipient-predicate
  [{:keys
    [phone
     email]}]
  (cond
    (and
     (string?
      phone)
     (nil?
      email))
    [:=
     :invitation/phone
     phone]

    (and
     (string?
      email)
     (nil?
      phone))
    [:=
     :invitation/email
     email]

    :else
    nil))

(defn pending-offer-predicate
  "Returns the XTDB predicate for one exact pending Invitation offer, or nil.

   input:

     {:organization-id uuid
      :phone           canonical-phone | nil
      :email           canonical-email | nil
      :role            :helper | :supervisor | :admin
      :scope           Organization scope}

   Exactly one recipient value is required.

   FX may reuse this predicate for both an ordinary preflight read and an
   atomic assert-none transaction requirement."
  [{:keys
    [organization-id
     phone
     email
     role
     scope]}]
  (let [recipient
        (recipient-predicate
         {:phone
          phone

          :email
          email})]
    (when
     (and
      (uuid?
       organization-id)

      recipient

      (invitation/status?
       :pending)

      (some?
       role)

      (organization/scope?
       scope))

      [:and
       [:=
        :invitation/organization
        organization-id]

       recipient

       [:=
        :invitation/role
        role]

       [:=
        :invitation/scope-type
        (organization/scope-type
         scope)]

       [:=
        :invitation/scope-id
        (organization/scope-id
         scope)]

       [:=
        :invitation/status
        :pending]])))

(defn pending-invitations-for-offer
  "Returns persisted pending Invitations representing one exact proposed grant.

   Expired-but-not-yet-materialized Invitations still have persisted status
   :pending and therefore appear here. FX decides whether such Invitations must
   first be materialized as expired before a replacement offer is created."
  [ctx input]
  (if-let [predicate
           (pending-offer-predicate
            input)]

    (sorted-rows
     ctx
     predicate)

    []))

(defn pending-invitation-for-offer
  "Returns the one persisted pending Invitation for an exact offer, or nil.

   Multiple exact pending offers are considered persisted corruption. Creation
   FX should also protect this invariant atomically."
  [ctx input]
  (exactly-one-or-nil!
   (pending-invitations-for-offer
    ctx
    input)

   :invitation.graph/non-unique-pending-offer
   "More than one pending Invitation exists for the same exact offer."
   {:offer
    (select-keys
     input
     [:organization-id
      :phone
      :email
      :role
      :scope])}))

;; =============================================================================
;; Scope collections
;; =============================================================================

(defn invitations-at-scope
  "Returns every historical Invitation at one exact Organization scope."
  [ctx scope]
  (if
   (organization/scope?
    scope)

    (sorted-rows
     ctx
     [:and
      [:=
       :invitation/scope-type
       (organization/scope-type
        scope)]

      [:=
       :invitation/scope-id
       (organization/scope-id
        scope)]])

    []))

(defn pending-invitations-at-scope
  "Returns persisted pending Invitations at one exact Organization scope."
  [ctx scope]
  (if
   (organization/scope?
    scope)

    (sorted-rows
     ctx
     [:and
      [:=
       :invitation/scope-type
       (organization/scope-type
        scope)]

      [:=
       :invitation/scope-id
       (organization/scope-id
        scope)]

      [:=
       :invitation/status
       :pending]])

    []))

;; =============================================================================
;; Custom Graph contribution
;; =============================================================================

(def custom-resolvers
  "Invitation currently contributes no custom public Graph resolvers.

   Conventional by-ID loading and persisted-field projection are generated by
   gesso.model. Relationship collections remain explicit model reads, and the
   token-hash lookup intentionally remains internal."
  [])
