(ns net.humanhelp.site.model.invitation.fx
  "Invitation-specific read dependencies and transaction planning.

   Invitation FX owns the effectful concerns surrounding canonical Invitation
   domain commands:

   - Invitation loading and guarded dependencies;
   - opaque bearer-token generation and hashing;
   - pending-offer and token uniqueness assertions;
   - inviter authorization through Membership;
   - recipient/User validation;
   - semantic Invitation changes for Gesso Live;
   - retryable Invitation acceptance planning.

   Invitation acceptance is deliberately convergent rather than artificially
   forced into one transaction.

   Repeated calls to next-acceptance-step progress through valid persisted
   states:

     1. create the Membership when no current Membership exists;
     2. create the exact RoleAssignment when it does not already exist;
     3. mark the Invitation accepted;
     4. report :complete after acceptance has already committed.

   Each successful intermediate commit is independently valid. A caller may
   therefore commit the returned :plan and invoke next-acceptance-step again.
   If a later operation fails, retry observes the previously committed work and
   continues instead of duplicating it.

   This namespace does not commit transactions. Callers commit returned plans
   through gesso.model.tx and then re-read before advancing a multi-transaction
   acceptance workflow."
  (:require
   [gesso.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.invitation.domain :as invitation]
   [net.humanhelp.site.model.invitation.graph :as invitation.graph]
   [net.humanhelp.site.model.invitation.schema :as invitation.schema]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest SecureRandom]
   [java.time Duration Instant]
   [java.util Base64]))

;; =============================================================================
;; Invitation defaults
;; =============================================================================

(def invitation-valid-for
  "Default validity period for a newly created Invitation."
  (Duration/ofDays
   7))

(def token-byte-count
  "Entropy bytes used by the default bearer-token generator."
  32)

(def token-generator-key
  "Optional ctx key containing a zero-argument bearer-token generator.

   Production falls back to a SecureRandom-backed URL-safe token generator.
   Tests may install a deterministic generator."
  ::token-generator)

;; =============================================================================
;; Errors and FX context
;; =============================================================================

(defn- fail!
  ([type message]
   (fail!
    type
    message
    nil))

  ([type message details]
   (throw
    (ex-info
     message
     (cond->
      {:error/type
       type}

       (some?
        details)
       (assoc
        :error/details
        details))))))

(defn- now!
  [ctx]
  (or
   (:biff.fx/now
    ctx)

   (fail!
    :invitation.fx/missing-now
    "Invitation planning requires :biff.fx/now.")))

(defn- seed!
  [ctx]
  (or
   (:biff.fx/seed
    ctx)

   (fail!
    :invitation.fx/missing-seed
    "Invitation creation requires :biff.fx/seed.")))

(defn- generated-id
  [ctx]
  (first
   (fx/uuid7
    (seed!
     ctx)
    (now!
     ctx))))

(defn- require-invitation-id!
  [invitation-id]
  (when-not
   (uuid?
    invitation-id)
    (fail!
     :invitation/invalid-invitation-id
     "Invitation ID must be a UUID."
     {:invitation/id
      invitation-id}))

  invitation-id)

(defn- require-user-id!
  [user-id]
  (when-not
   (uuid?
    user-id)
    (fail!
     :invitation/invalid-user-id
     "User ID must be a UUID."
     {:user/id
      user-id}))

  user-id)

(defn- instant?
  [value]
  (instance?
   Instant
   value))

;; =============================================================================
;; Bearer tokens
;; =============================================================================

(defn- random-token
  []
  (let [bytes
        (byte-array
         token-byte-count)

        random
        (SecureRandom.)]
    (.nextBytes
     random
     bytes)

    (.encodeToString
     (.withoutPadding
      (Base64/getUrlEncoder))
     bytes)))

(defn- token-generator
  [ctx]
  (or
   (get
    ctx
    token-generator-key)

   random-token))

(defn- generated-token
  [ctx]
  (let [generator
        (token-generator
         ctx)

        token
        (generator)]
    (when-not
     (and
      (string?
       token)

      (not
       (clojure.string/blank?
        token)))
      (fail!
       :invitation.fx/invalid-generated-token
       "The Invitation token generator returned an invalid token."))

    token))

(defn hash-token
  "Returns the persisted SHA-256 representation of a raw bearer token.

   The raw token must never be persisted."
  [token]
  (when-not
   (and
    (string?
     token)

    (not
     (clojure.string/blank?
      token)))
    (fail!
     :invitation/invalid-token
     "A nonblank Invitation token is required."))

  (let [digest
        (MessageDigest/getInstance
         "SHA-256")

        bytes
        (.digest
         digest
         (.getBytes
          ^String token
          StandardCharsets/UTF_8))]
    (.encodeToString
     (.withoutPadding
      (Base64/getUrlEncoder))
     bytes)))

;; =============================================================================
;; Conventional Invitation reads
;; =============================================================================

(defn- load-invitation
  [ctx invitation-id]
  (model/load-by-id
   invitation.schema/invitation-descriptor
   ctx
   (require-invitation-id!
    invitation-id)))

(defn- require-invitation!
  [ctx invitation-id]
  (or
   (load-invitation
    ctx
    invitation-id)

   (fail!
    :invitation/not-found
    "The Invitation does not exist."
    {:invitation/id
     invitation-id})))

;; =============================================================================
;; Guarded Invitation dependencies
;; =============================================================================

(defn- invitation-guard-fragment
  [invitation-document]
  (model.tx/guards-fragment
   (command/guard
    invitation/entity-type
    invitation-document
    invitation/version)))

(defn invitation-dependency
  "Returns Invitation plus a guard-only transaction fragment.

   Returns nil when Invitation does not exist."
  [ctx invitation-id]
  (when-let [invitation-document
             (load-invitation
              ctx
              invitation-id)]
    {:invitation
     invitation-document

     :transaction-fragment
     (invitation-guard-fragment
      invitation-document)}))

(defn require-invitation-dependency
  "Returns invitation-dependency or throws when Invitation does not exist."
  [ctx invitation-id]
  (or
   (invitation-dependency
    ctx
    invitation-id)

   (fail!
    :invitation/not-found
    "The Invitation does not exist."
    {:invitation/id
     invitation-id})))

(defn invitation-by-token-dependency
  "Returns the Invitation identified by raw bearer token plus a guard-only
   transaction fragment.

   Returns nil when token identifies no Invitation."
  [ctx token]
  (when-let [invitation-document
             (invitation.graph/invitation-by-token-hash
              ctx
              (hash-token
               token))]
    {:invitation
     invitation-document

     :transaction-fragment
     (invitation-guard-fragment
      invitation-document)}))

(defn require-invitation-by-token-dependency
  "Returns invitation-by-token-dependency or throws when token identifies no
   Invitation."
  [ctx token]
  (or
   (invitation-by-token-dependency
    ctx
    token)

   (fail!
    :invitation/not-found
    "The Invitation does not exist.")))

;; =============================================================================
;; Semantic Live changes
;; =============================================================================

(defn- change-kind
  [operation]
  (if
   (=
    :create
    operation)
    :created
    :updated))

(defn- invitation-change
  [operation model-command]
  (let [document
        (command/after
         model-command)]
    {:topic
     :invitation

     :id
     (:xt/id
      document)

     :change/kind
     (change-kind
      operation)

     :invitation/operation
     operation

     :invitation/id
     (:xt/id
      document)

     :invitation/organization-id
     (invitation/organization-id
      document)

     :invitation/status
     (invitation/invitation-status
      document)

     :invitation/role
     (invitation/offered-role
      document)

     :invitation/scope
     (invitation/scope
      document)

     :invitation/revision
     (:invitation/revision
      document)}))

(defn- change-entry
  [change]
  {:coalesce-key
   [:invitation
    (:invitation/id
     change)]})

(def transaction-options
  {:entry-fn
   change-entry})

;; =============================================================================
;; Plan construction
;; =============================================================================

(defn- mutation-fragment
  [operation model-command]
  (model.tx/fragment
   {:commands
    [model-command]

    :changes
    [(invitation-change
      operation
      model-command)]}))

(defn- planned
  ([operation model-command]
   (planned
    operation
    model-command
    model.tx/empty-fragment
    nil))

  ([operation model-command dependency-fragment]
   (planned
    operation
    model-command
    dependency-fragment
    nil))

  ([operation model-command dependency-fragment result-extra]
   {:result
    (merge
     {:invitation
      (command/after
       model-command)}
     result-extra)

    :transaction-fragment
    (model.tx/compose
     dependency-fragment
     (mutation-fragment
      operation
      model-command))

    :transaction-options
    transaction-options}))

;; =============================================================================
;; Recipient normalization
;; =============================================================================

(defn- normalized-recipient
  [{:keys
    [phone
     email]}]
  {:phone
   (user/normalize-phone
    phone)

   :email
   (user/normalize-email
    email)})

(defn- recipient-count
  [{:keys
    [phone
     email]}]
  (count
   (filter
    some?
    [phone
     email])))

(defn- require-recipient!
  [input]
  (let [{:keys
         [phone
          email]
         :as recipient}
        (normalized-recipient
         input)]
    (when-not
     (=
      1
      (recipient-count
       recipient))
      (fail!
       :invitation/invalid-recipient
       "Exactly one Invitation phone number or email address is required."
       {:phone
        phone

        :email
        email}))

    (when
     (and
      phone
      (not
       (user/phone?
        phone)))
      (fail!
       :invitation/invalid-recipient
       "The Invitation phone number is invalid."
       {:phone
        phone}))

    (when
     (and
      email
      (not
       (user/email?
        email)))
      (fail!
       :invitation/invalid-recipient
       "The Invitation email address is invalid."
       {:email
        email}))

    recipient))

;; =============================================================================
;; Invitation creation requirements
;; =============================================================================

(defn- require-create-scope!
  [scope-dependency organization-id scope]
  (let [scope-context
        (:scope-context
         scope-dependency)

        actual-organization-id
        (organization/scope-context-organization-id
         scope-context)]
    (when-not
     (=
      organization-id
      actual-organization-id)
      (fail!
       :invitation/scope-ownership-mismatch
       "The Invitation scope does not belong to the supplied Organization."
       {:organization/id
        organization-id

        :scope
        scope

        :scope/organization-id
        actual-organization-id}))

    (when-not
     (organization/scope-context-operational?
      scope-context)
      (fail!
       :invitation/scope-not-operational
       "An Invitation cannot be created for a non-operational scope."
       {:scope
        scope}))

    scope-context))

(defn- duplicate-pending-offer!
  [ctx offer now]
  (when-let [existing
             (invitation.graph/pending-invitation-for-offer
              ctx
              offer)]
    (if
     (invitation/past-expiration?
      existing
      now)

      (fail!
       :invitation/pending-offer-expired
       "An expired persisted pending Invitation must be expired before this offer can be recreated."
       {:invitation/id
        (invitation/invitation-id
         existing)})

      (fail!
       :invitation/pending-offer-exists
       "An equivalent pending Invitation already exists."
       {:invitation/id
        (invitation/invitation-id
         existing)}))))

(defn- invitation-create-assertions
  [token-hash offer]
  [(model.tx/assert-none
    invitation/entity-type
    [:=
     :invitation/token-hash
     token-hash])

   (model.tx/assert-none
    invitation/entity-type
    (invitation.graph/pending-offer-predicate
     offer))])

;; =============================================================================
;; Invitation creation
;; =============================================================================

(defn plan-create-invitation
  "Plans creation of one authorized staff Invitation.

   input:

     {:organization-id uuid
      :invited-by      uuid
      :phone           canonicalizable-phone | nil
      :email           canonicalizable-email | nil
      :role            :helper | :supervisor | :admin
      :scope           Organization scope
      :expires-at      optional Instant}

   The inviter must currently hold :admin at the target scope.

   Returns the raw bearer token only in :result. The persisted Invitation
   contains only its hash."
  [ctx input]
  (let [now
        (now!
         ctx)

        organization-id
        (:organization-id
         input)

        invited-by
        (:invited-by
         input)

        role
        (:role
         input)

        scope
        (:scope
         input)

        {:keys
         [phone
          email]}
        (require-recipient!
         input)

        expires-at
        (or
         (:expires-at
          input)

         (.plus
          ^Instant now
          invitation-valid-for))

        token
        (generated-token
         ctx)

        token-hash
        (hash-token
         token)

        admin-dependency
        (membership/require-admin-dependency
         ctx
         invited-by
         scope)

        scope-context
        (require-create-scope!
         admin-dependency
         organization-id
         scope)

        offer
        {:organization-id
         organization-id

         :phone
         phone

         :email
         email

         :role
         role

         :scope
         scope}

        _
        (duplicate-pending-offer!
         ctx
         offer
         now)

        model-command
        (invitation/create-invitation-command
         {:id
          (generated-id
           ctx)

          :organization-id
          organization-id

          :invited-by
          invited-by

          :phone
          phone

          :email
          email

          :role
          role

          :scope
          scope

          :token-hash
          token-hash

          :now
          now

          :expires-at
          expires-at})

        assertion-fragment
        (model.tx/fragment
         {:assertions
          (invitation-create-assertions
           token-hash
           offer)})

        dependency-fragment
        (model.tx/compose
         (:transaction-fragment
          admin-dependency)

         assertion-fragment)]
    (planned
     :create
     model-command
     dependency-fragment
     {:token
      token

      :scope-context
      scope-context})))

;; =============================================================================
;; Recipient Invitation requirements
;; =============================================================================

(defn- require-usable-pending!
  [invitation-document now]
  (cond
    (invitation/accepted?
     invitation-document)
    (fail!
     :invitation/accepted
     "The Invitation has already been accepted."
     {:invitation/id
      (invitation/invitation-id
       invitation-document)})

    (invitation/declined?
     invitation-document)
    (fail!
     :invitation/declined
     "The Invitation has already been declined."
     {:invitation/id
      (invitation/invitation-id
       invitation-document)})

    (invitation/revoked?
     invitation-document)
    (fail!
     :invitation/revoked
     "The Invitation has been revoked."
     {:invitation/id
      (invitation/invitation-id
       invitation-document)})

    (invitation/expired?
     invitation-document)
    (fail!
     :invitation/expired
     "The Invitation has expired."
     {:invitation/id
      (invitation/invitation-id
       invitation-document)})

    (invitation/past-expiration?
     invitation-document
     now)
    (fail!
     :invitation/expired
     "The Invitation has expired."
     {:invitation/id
      (invitation/invitation-id
       invitation-document)

      :invitation/expires-at
      (:invitation/expires-at
       invitation-document)})

    (not
     (invitation/pending?
      invitation-document))
    (fail!
     :invitation/not-pending
     "The Invitation is not pending."
     {:invitation/id
      (invitation/invitation-id
       invitation-document)

      :invitation/status
      (invitation/invitation-status
       invitation-document)}))

  invitation-document)

(defn- require-recipient-user-dependency!
  [ctx invitation-document user-id]
  (let [{user-document :user
         :as dependency}
        (user/require-user-dependency
         ctx
         (require-user-id!
          user-id))]
    (when-not
     (user/active?
      user-document)
      (fail!
       :invitation/user-not-active
       "Only an active User may act on this Invitation."
       {:user/id
        user-id

        :user/status
        (user/user-status
         user-document)}))

    (when-not
     (invitation/addressed-to-user?
      invitation-document
      user-document)
      (fail!
       :invitation/recipient-mismatch
       "The Invitation does not belong to this User."
       {:invitation/id
        (invitation/invitation-id
         invitation-document)

        :user/id
        user-id}))

    dependency))

;; =============================================================================
;; Decline
;; =============================================================================

(defn plan-decline-invitation
  "Plans recipient decline of an Invitation identified by raw bearer token."
  [ctx {:keys
        [token
         user-id]}]
  (let [now
        (now!
         ctx)

        {invitation-document :invitation
         invitation-fragment :transaction-fragment}
        (require-invitation-by-token-dependency
         ctx
         token)

        _
        (require-usable-pending!
         invitation-document
         now)

        {user-document :user
         user-fragment :transaction-fragment}
        (require-recipient-user-dependency!
         ctx
         invitation-document
         user-id)

        model-command
        (invitation/decline-invitation-command
         invitation-document
         {:now
          now

          :actor-id
          (user/user-id
           user-document)})]
    (planned
     :decline
     model-command
     (model.tx/compose
      invitation-fragment
      user-fragment))))

;; =============================================================================
;; Revocation
;; =============================================================================

(defn plan-revoke-invitation
  "Plans administrative revocation of one pending Invitation.

   actor-id must currently hold :admin at the Invitation's scope."
  [ctx {:keys
        [invitation-id
         actor-id
         reason]}]
  (let [now
        (now!
         ctx)

        {invitation-document :invitation
         invitation-fragment :transaction-fragment}
        (require-invitation-dependency
         ctx
         invitation-id)

        _
        (require-usable-pending!
         invitation-document
         now)

        invitation-scope
        (invitation/scope
         invitation-document)

        admin-dependency
        (membership/require-admin-dependency
         ctx
         actor-id
         invitation-scope)

        scope-context
        (:scope-context
         admin-dependency)

        actual-organization-id
        (organization/scope-context-organization-id
         scope-context)

        _
        (when-not
         (=
          (invitation/organization-id
           invitation-document)
          actual-organization-id)
          (fail!
           :invitation/scope-ownership-mismatch
           "The Invitation scope no longer belongs to its recorded Organization."
           {:invitation/id
            invitation-id

            :invitation/organization-id
            (invitation/organization-id
             invitation-document)

            :scope/organization-id
            actual-organization-id}))

        model-command
        (invitation/revoke-invitation-command
         invitation-document
         {:now
          now

          :actor-id
          actor-id

          :reason
          reason})]
    (planned
     :revoke
     model-command
     (model.tx/compose
      invitation-fragment
      (:transaction-fragment
       admin-dependency)))))

;; =============================================================================
;; Expiration
;; =============================================================================

(defn plan-expire-invitation
  "Plans explicit materialization of Invitation expiration.

   No actor authorization is required. The domain permits expiration only when
   now has reached or passed expires-at."
  [ctx {:keys
        [invitation-id]}]
  (let [now
        (now!
         ctx)

        {invitation-document :invitation
         invitation-fragment :transaction-fragment}
        (require-invitation-dependency
         ctx
         invitation-id)

        model-command
        (invitation/expire-invitation-command
         invitation-document
         {:now
          now})]
    (planned
     :expire
     model-command
     invitation-fragment)))

;; =============================================================================
;; Acceptance workflow helpers
;; =============================================================================

(defn- invitation-accepted-by-user?
  [invitation-document user-id]
  (and
   (invitation/accepted?
    invitation-document)

   (=
    user-id
    (invitation/accepted-by-id
     invitation-document))))

(defn- require-acceptance-invitation!
  [invitation-document user-id now]
  (cond
    (invitation-accepted-by-user?
     invitation-document
     user-id)
    :complete

    (invitation/accepted?
     invitation-document)
    (fail!
     :invitation/accepted
     "The Invitation has already been accepted."
     {:invitation/id
      (invitation/invitation-id
       invitation-document)

      :invitation/accepted-by
      (invitation/accepted-by-id
       invitation-document)})

    :else
    (do
      (require-usable-pending!
       invitation-document
       now)
      :pending)))

(defn- require-active-current-membership!
  [membership-document]
  (when
   (and
    membership-document

    (not
     (membership/membership-active?
      membership-document)))
    (fail!
     :invitation/membership-not-active
     "An existing current Membership must be active before this Invitation can be accepted."
     {:membership/id
      (membership/membership-id
       membership-document)

      :membership/status
      (membership/membership-status
       membership-document)}))

  membership-document)

(defn- exact-active-role-assignment
  [ctx membership-document invitation-document]
  (let [invitation-scope
        (invitation/scope
         invitation-document)

        invitation-role
        (invitation/offered-role
         invitation-document)

        assignments
        (->> (membership/active-role-assignments-for-membership-at-scope
              ctx
              (membership/membership-id
               membership-document)
              invitation-scope)

             (filter
              #(=
                invitation-role
                (membership/assigned-role
                 %)))

             vec)]
    (case
     (count
      assignments)

     0
     nil

     1
     (first
      assignments)

     (fail!
      :invitation/non-unique-role-assignment
      "More than one active RoleAssignment satisfies the exact Invitation offer."
      {:invitation/id
       (invitation/invitation-id
        invitation-document)

       :membership/id
       (membership/membership-id
        membership-document)

       :role
       invitation-role

       :scope
       invitation-scope

       :role-assignment/ids
       (mapv
        membership/role-assignment-id
        assignments)}))))

;; =============================================================================
;; Final acceptance plan
;; =============================================================================

(defn plan-accept-invitation
  "Plans only the final Invitation transition to :accepted.

   The required Membership and exact active RoleAssignment must already exist
   in persistence. This function does not create them.

   For the retryable end-to-end workflow use next-acceptance-step."
  [ctx {:keys
        [token
         user-id]}]
  (let [now
        (now!
         ctx)

        {invitation-document :invitation
         invitation-fragment :transaction-fragment}
        (require-invitation-by-token-dependency
         ctx
         token)

        acceptance-state
        (require-acceptance-invitation!
         invitation-document
         user-id
         now)]

    (when
     (=
      :complete
      acceptance-state)
      (fail!
       :invitation/already-complete
       "The Invitation has already been accepted by this User."
       {:invitation/id
        (invitation/invitation-id
         invitation-document)

        :user/id
        user-id}))

    (let [{user-document :user
           user-fragment :transaction-fragment}
          (require-recipient-user-dependency!
           ctx
           invitation-document
           user-id)

          organization-id
          (invitation/organization-id
           invitation-document)

          {membership-document :membership
           membership-fragment :transaction-fragment}
          (or
           (membership/current-membership-dependency
            ctx
            user-id
            organization-id)

           (fail!
            :invitation/membership-missing
            "The accepting User does not yet have a current Membership in the invited Organization."
            {:invitation/id
             (invitation/invitation-id
              invitation-document)

             :user/id
             user-id

             :organization/id
             organization-id}))

          _
          (require-active-current-membership!
           membership-document)

          role-assignment
          (or
           (exact-active-role-assignment
            ctx
            membership-document
            invitation-document)

           (fail!
            :invitation/role-assignment-missing
            "The exact RoleAssignment offered by the Invitation does not yet exist."
            {:invitation/id
             (invitation/invitation-id
              invitation-document)

             :membership/id
             (membership/membership-id
              membership-document)

             :role
             (invitation/offered-role
              invitation-document)

             :scope
             (invitation/scope
              invitation-document)}))

          model-command
          (invitation/accept-invitation-command
           invitation-document
           {:user
            user-document

            :membership
            membership-document

            :role-assignment
            role-assignment

            :now
            now})]
      (planned
       :accept
       model-command
       (model.tx/compose
        invitation-fragment
        user-fragment
        membership-fragment)
       {:user
        user-document

        :membership
        membership-document

        :role-assignment
        role-assignment}))))

;; =============================================================================
;; Retryable acceptance progression
;; =============================================================================

(defn next-acceptance-step
  "Returns the next independently valid step for accepting an Invitation.

   input:

     {:token   raw-bearer-token
      :user-id accepting-user-uuid}

   Return shapes:

     {:step :create-membership
      :invitation ...
      :user ...
      :plan <Membership plan>}

     {:step :create-role-assignment
      :invitation ...
      :user ...
      :membership ...
      :plan <Membership plan>}

     {:step :accept-invitation
      :invitation ...
      :user ...
      :membership ...
      :role-assignment ...
      :plan <Invitation plan>}

     {:step :complete
      :result {...}}

   The caller commits :plan, then invokes this function again against the
   resulting consistency-aware context.

   This structure deliberately makes retries normal:

   - a Membership committed before a later failure is reused;
   - a RoleAssignment committed before a later failure is reused;
   - an already accepted Invitation reports :complete.

   No committed intermediate state exists solely as rollback scaffolding."
  [ctx {:keys
        [token
         user-id]
        :as input}]
  (let [now
        (now!
         ctx)

        {invitation-document :invitation}
        (require-invitation-by-token-dependency
         ctx
         token)

        acceptance-state
        (require-acceptance-invitation!
         invitation-document
         user-id
         now)]

    (if
     (=
      :complete
      acceptance-state)

      {:step
       :complete

       :result
       {:invitation
        invitation-document

        :invitation-id
        (invitation/invitation-id
         invitation-document)

        :user-id
        user-id

        :membership-id
        (invitation/accepted-membership-id
         invitation-document)

        :role-assignment-id
        (invitation/accepted-role-assignment-id
         invitation-document)}}

      (let [{user-document :user}
            (require-recipient-user-dependency!
             ctx
             invitation-document
             user-id)

            organization-id
            (invitation/organization-id
             invitation-document)

            membership-document
            (membership/current-membership
             ctx
             user-id
             organization-id)]

        (if
         (nil?
          membership-document)

          {:step
           :create-membership

           :invitation
           invitation-document

           :user
           user-document

           :plan
           (membership/plan-create-membership
            ctx
            {:user-id
             user-id

             :organization-id
             organization-id

             :skills
             #{}})}

          (do
            (require-active-current-membership!
             membership-document)

            (if-let [role-assignment
                     (exact-active-role-assignment
                      ctx
                      membership-document
                      invitation-document)]

              {:step
               :accept-invitation

               :invitation
               invitation-document

               :user
               user-document

               :membership
               membership-document

               :role-assignment
               role-assignment

               :plan
               (plan-accept-invitation
                ctx
                input)}

              {:step
               :create-role-assignment

               :invitation
               invitation-document

               :user
               user-document

               :membership
               membership-document

               :plan
               (membership/plan-create-role-assignment
                ctx
                {:membership-id
                 (membership/membership-id
                  membership-document)

                 :role
                 (invitation/offered-role
                  invitation-document)

                 :scope
                 (invitation/scope
                  invitation-document)

                 :actor-id
                 user-id

                 :reason
                 :invitation/accepted})})))))))

;; =============================================================================
;; Acceptance workflow observations
;; =============================================================================

(defn acceptance-state
  "Returns a read-only summary of progress toward realizing Invitation.

   This is useful for UI/status display. It does not provide transaction guards
   and is not an authorization proof."
  [ctx {:keys
        [token
         user-id]}]
  (let [invitation-document
        (some->
         (invitation.graph/invitation-by-token-hash
          ctx
          (hash-token
           token)))

        user-document
        (when
         invitation-document
         (user/user
          ctx
          user-id))

        recipient?
        (and
         user-document
         (user/active?
          user-document)
         (invitation/addressed-to-user?
          invitation-document
          user-document))

        membership-document
        (when
         recipient?
         (membership/current-membership
          ctx
          user-id
          (invitation/organization-id
           invitation-document)))

        role-assignment
        (when
         (and
          membership-document
          (membership/membership-active?
           membership-document))
         (exact-active-role-assignment
          ctx
          membership-document
          invitation-document))]
    {:invitation
     invitation-document

     :user
     user-document

     :recipient?
     (boolean
      recipient?)

     :membership
     membership-document

     :role-assignment
     role-assignment

     :accepted?
     (boolean
      (and
       invitation-document
       (invitation-accepted-by-user?
        invitation-document
        user-id)))

     :ready-to-accept?
     (boolean
      (and
       invitation-document
       recipient?
       (invitation/pending?
        invitation-document)
       (not
        (invitation/past-expiration?
         invitation-document
         (now!
          ctx)))
       membership-document
       (membership/membership-active?
        membership-document)
       role-assignment))}))
