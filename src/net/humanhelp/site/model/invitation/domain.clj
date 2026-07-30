(ns net.humanhelp.site.model.invitation.domain
  "Pure rules for persisted HumanHelp staff Invitations.

   An Invitation proposes admission to one Organization by offering one exact
   Membership role at one explicit Organization-owned scope.

   An Invitation addresses exactly one canonical User contact:

   - phone; or
   - email.

   Only an opaque token hash is persisted. Raw bearer tokens, token generation,
   hashing, delivery, and lookup belong outside this namespace.

   Acceptance is deliberately modeled against already-loaded User, Membership,
   and RoleAssignment documents. The domain therefore proves that the accepted
   Membership and RoleAssignment actually satisfy the Invitation before
   recording their IDs.

   FX is responsible for:

   - generating IDs and bearer tokens;
   - hashing raw tokens;
   - loading Invitations;
   - loading and guarding Users;
   - authorizing inviters/revokers through Membership;
   - resolving and guarding Organization scopes;
   - finding or creating Memberships and RoleAssignments;
   - uniqueness assertions;
   - composing all resulting commands atomically;
   - semantic Gesso Live changes;
   - transaction execution.

   Cross-model semantics are consumed only through the stable User,
   Membership, and Organization core APIs."
  (:require
   [gesso.model.command :as command]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.time Instant]))

;; =============================================================================
;; Identity and versioning
;; =============================================================================

(def entity-type
  :invitation)

(def version
  {:revision-key
   :invitation/revision

   :created-at-key
   :invitation/created-at

   :updated-at-key
   :invitation/updated-at})

;; =============================================================================
;; Token values
;; =============================================================================

(def token-hash-min
  32)

(def token-hash-max
  256)

(defn token-hash?
  "Validates the persisted opaque token representation.

   This predicate deliberately says nothing about the cryptographic strength of
   the hashing scheme. Invitation FX owns token generation and hashing."
  [value]
  (and
   (string?
    value)

   (<=
    token-hash-min
    (count value)
    token-hash-max)

   (boolean
    (re-matches
     #"\S+"
     value))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def ^:private statuses
  #{:pending
    :accepted
    :declined
    :revoked
    :expired})

(def ^:private terminal-statuses
  #{:accepted
    :declined
    :revoked
    :expired})

(def ^:private allowed-transitions
  {[:pending :accept]
   :accepted

   [:pending :decline]
   :declined

   [:pending :revoke]
   :revoked

   [:pending :expire]
   :expired})

(defn status?
  [value]
  (contains?
   statuses
   value))

(defn invitation-status
  [invitation]
  (:invitation/status
   invitation))

(defn pending?
  [invitation]
  (=
   :pending
   (invitation-status
    invitation)))

(defn accepted?
  [invitation]
  (=
   :accepted
   (invitation-status
    invitation)))

(defn declined?
  [invitation]
  (=
   :declined
   (invitation-status
    invitation)))

(defn revoked?
  [invitation]
  (=
   :revoked
   (invitation-status
    invitation)))

(defn expired?
  [invitation]
  (=
   :expired
   (invitation-status
    invitation)))

(defn terminal?
  [invitation]
  (contains?
   terminal-statuses
   (invitation-status
    invitation)))

(defn next-status
  [invitation operation]
  (get
   allowed-transitions
   [(invitation-status invitation)
    operation]))

(defn can-transition?
  [invitation operation]
  (some?
   (next-status
    invitation
    operation)))

;; =============================================================================
;; Persisted Invitation facts
;; =============================================================================

(defn invitation-id
  [invitation]
  (:xt/id
   invitation))

(defn organization-id
  [invitation]
  (:invitation/organization
   invitation))

(defn invited-by-id
  [invitation]
  (:invitation/invited-by
   invitation))

(defn offered-role
  [invitation]
  (:invitation/role
   invitation))

(defn scope
  [invitation]
  {:scope/type
   (:invitation/scope-type
    invitation)

   :scope/id
   (:invitation/scope-id
    invitation)})

(defn accepted-by-id
  [invitation]
  (:invitation/accepted-by
   invitation))

(defn accepted-membership-id
  [invitation]
  (:invitation/membership
   invitation))

(defn accepted-role-assignment-id
  [invitation]
  (:invitation/role-assignment
   invitation))

(defn for-organization?
  [invitation expected-organization-id]
  (=
   expected-organization-id
   (organization-id
    invitation)))

(defn invited-by?
  [invitation expected-user-id]
  (=
   expected-user-id
   (invited-by-id
    invitation)))

(defn offers-role?
  [invitation expected-role]
  (=
   expected-role
   (offered-role
    invitation)))

(defn at-scope?
  [invitation expected-scope]
  (organization/same-scope?
   (scope invitation)
   expected-scope))

;; =============================================================================
;; Recipient values
;; =============================================================================

(defn recipient-type
  [invitation]
  (cond
    (some?
     (:invitation/phone
      invitation))
    :phone

    (some?
     (:invitation/email
      invitation))
    :email

    :else
    nil))

(defn recipient-value
  [invitation]
  (case
   (recipient-type
    invitation)

    :phone
    (:invitation/phone
     invitation)

    :email
    (:invitation/email
     invitation)

    nil))

(defn addressed-to?
  "Returns true when supplied contact values canonically match the Invitation.

   This checks value equality only. It does not establish that a person owns or
   has verified the supplied contact."
  [invitation {:keys
               [phone
                email]}]
  (case
   (recipient-type
    invitation)

    :phone
    (=
     (:invitation/phone
      invitation)
     (user/normalize-phone
      phone))

    :email
    (=
     (:invitation/email
      invitation)
     (user/normalize-email
      email))

    false))

(defn addressed-to-user?
  "Returns true when Invitation addresses a currently verified contact on User.

   Verification is intentional: merely storing the same unverified phone or
   email must not establish ownership of an Invitation."
  [invitation user-document]
  (case
   (recipient-type
    invitation)

    :phone
    (and
     (user/phone-verified?
      user-document)

     (=
      (:invitation/phone
       invitation)
      (user/user-phone
       user-document)))

    :email
    (and
     (user/email-verified?
      user-document)

     (=
      (:invitation/email
       invitation)
      (user/user-email
       user-document)))

    false))

;; =============================================================================
;; Expiration
;; =============================================================================

(defn- instant?
  [value]
  (instance?
   Instant
   value))

(defn- before?
  [^Instant left ^Instant right]
  (.isBefore
   left
   right))

(defn- at-or-before?
  [^Instant left ^Instant right]
  (not
   (.isAfter
    left
    right)))

(defn- at-or-after?
  [^Instant left ^Instant right]
  (not
   (.isBefore
    left
    right)))

(defn past-expiration?
  "Returns true at or after Invitation expiration."
  [invitation now]
  (and
   (instant?
    now)

   (instant?
    (:invitation/expires-at
     invitation))

   (at-or-after?
    now
    (:invitation/expires-at
     invitation))))

(defn usable-at?
  "Returns true when Invitation is pending and may still be acted upon at now.

   Expiration is exclusive: an Invitation is no longer usable exactly at its
   expires-at Instant."
  [invitation now]
  (and
   (pending?
    invitation)

   (instant?
    now)

   (instant?
    (:invitation/created-at
     invitation))

   (instant?
    (:invitation/expires-at
     invitation))

   (at-or-before?
    (:invitation/created-at
     invitation)
    now)

   (before?
    now
    (:invitation/expires-at
     invitation))))

;; =============================================================================
;; Document invariants
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
   (qualified-keyword?
    value)))

(defn- none-present?
  [document keys]
  (every?
   #(nil?
     (get
      document
      %))
   keys))

(def ^:private acceptance-keys
  [:invitation/accepted-at
   :invitation/accepted-by
   :invitation/membership
   :invitation/role-assignment])

(def ^:private decline-keys
  [:invitation/declined-at
   :invitation/declined-by])

(def ^:private revocation-keys
  [:invitation/revoked-at
   :invitation/revoked-by
   :invitation/revocation-reason])

(def ^:private expiration-keys
  [:invitation/expired-at])

(def ^:private all-terminal-keys
  (vec
   (concat
    acceptance-keys
    decline-keys
    revocation-keys
    expiration-keys)))

(defn- recipient-consistent?
  [invitation]
  (let [phone
        (:invitation/phone
         invitation)

        email
        (:invitation/email
         invitation)]
    (and
     (=
      1
      (count
       (filter
        some?
        [phone
         email])))

     (or
      (nil?
       phone)
      (user/phone?
       phone))

     (or
      (nil?
       email)
      (user/email?
       email)))))

(defn- scope-consistent?
  [invitation]
  (let [organization-id
        (organization-id
         invitation)

        invitation-scope
        (scope
         invitation)]
    (and
     (uuid?
      organization-id)

     (organization/scope?
      invitation-scope)

     (or
      (not
       (organization/organization-scope?
        invitation-scope))

      (=
       organization-id
       (organization/scope-id
        invitation-scope))))))

(defn- version-consistent?
  [invitation]
  (let [created-at
        (:invitation/created-at
         invitation)

        updated-at
        (:invitation/updated-at
         invitation)

        expires-at
        (:invitation/expires-at
         invitation)]
    (and
     (command/versioned-document?
      invitation
      version)

     (instant?
      created-at)

     (instant?
      updated-at)

     (instant?
      expires-at)

     (at-or-before?
      created-at
      updated-at)

     (before?
      created-at
      expires-at))))

(defn- pending-consistent?
  [invitation]
  (and
   (=
    0
    (:invitation/revision
     invitation))

   (=
    (:invitation/created-at
     invitation)
    (:invitation/updated-at
     invitation))

   (none-present?
    invitation
    all-terminal-keys)))

(defn- accepted-consistent?
  [invitation]
  (let [accepted-at
        (:invitation/accepted-at
         invitation)]
    (and
     (=
      1
      (:invitation/revision
       invitation))

     (instant?
      accepted-at)

     (=
      accepted-at
      (:invitation/updated-at
       invitation))

     (before?
      accepted-at
      (:invitation/expires-at
       invitation))

     (uuid?
      (:invitation/accepted-by
       invitation))

     (uuid?
      (:invitation/membership
       invitation))

     (uuid?
      (:invitation/role-assignment
       invitation))

     (none-present?
      invitation
      (concat
       decline-keys
       revocation-keys
       expiration-keys)))))

(defn- declined-consistent?
  [invitation]
  (let [declined-at
        (:invitation/declined-at
         invitation)]
    (and
     (=
      1
      (:invitation/revision
       invitation))

     (instant?
      declined-at)

     (=
      declined-at
      (:invitation/updated-at
       invitation))

     (before?
      declined-at
      (:invitation/expires-at
       invitation))

     (optional-uuid?
      (:invitation/declined-by
       invitation))

     (none-present?
      invitation
      (concat
       acceptance-keys
       revocation-keys
       expiration-keys)))))

(defn- revoked-consistent?
  [invitation]
  (let [revoked-at
        (:invitation/revoked-at
         invitation)]
    (and
     (=
      1
      (:invitation/revision
       invitation))

     (instant?
      revoked-at)

     (=
      revoked-at
      (:invitation/updated-at
       invitation))

     (before?
      revoked-at
      (:invitation/expires-at
       invitation))

     (optional-uuid?
      (:invitation/revoked-by
       invitation))

     (optional-reason?
      (:invitation/revocation-reason
       invitation))

     (or
      (some?
       revoked-at)

      (and
       (nil?
        (:invitation/revoked-by
         invitation))

       (nil?
        (:invitation/revocation-reason
         invitation))))

     (none-present?
      invitation
      (concat
       acceptance-keys
       decline-keys
       expiration-keys)))))

(defn- expired-consistent?
  [invitation]
  (let [expired-at
        (:invitation/expired-at
         invitation)]
    (and
     (=
      1
      (:invitation/revision
       invitation))

     (instant?
      expired-at)

     (=
      expired-at
      (:invitation/updated-at
       invitation))

     (at-or-after?
      expired-at
      (:invitation/expires-at
       invitation))

     (none-present?
      invitation
      (concat
       acceptance-keys
       decline-keys
       revocation-keys)))))

(defn- lifecycle-consistent?
  [invitation]
  (case
   (invitation-status
    invitation)

    :pending
    (pending-consistent?
     invitation)

    :accepted
    (accepted-consistent?
     invitation)

    :declined
    (declined-consistent?
     invitation)

    :revoked
    (revoked-consistent?
     invitation)

    :expired
    (expired-consistent?
     invitation)

    false))

(defn document-consistent?
  "Returns true when Invitation satisfies every local persisted invariant.

   This proves structural consistency only. It deliberately does not establish:

   - that invited-by currently exists or is authorized;
   - that the Organization scope currently exists;
   - that a Location/Group belongs to invitation/organization;
   - token-hash uniqueness;
   - that an accepted User still exists;
   - that recorded Membership/RoleAssignment documents still exist.

   Those are persistence or cross-model facts owned by Invitation FX."
  [invitation]
  (and
   (map?
    invitation)

   (uuid?
    (invitation-id
     invitation))

   (uuid?
    (organization-id
     invitation))

   (uuid?
    (invited-by-id
     invitation))

   (recipient-consistent?
    invitation)

   (membership/role?
    (offered-role
     invitation))

   (scope-consistent?
    invitation)

   (token-hash?
    (:invitation/token-hash
     invitation))

   (status?
    (invitation-status
     invitation))

   (version-consistent?
    invitation)

   (lifecycle-consistent?
    invitation)))

;; =============================================================================
;; Errors and guarded mutation mechanics
;; =============================================================================

(defn- context
  [invitation]
  {:invitation/id
   (invitation-id
    invitation)

   :invitation/organization
   (organization-id
    invitation)

   :invitation/recipient-type
   (recipient-type
    invitation)

   :invitation/role
   (offered-role
    invitation)

   :invitation/scope
   (when
    (map?
     invitation)
     (scope
      invitation))

   :invitation/status
   (invitation-status
    invitation)})

(defn- fail!
  [error-type message errors invitation]
  (throw
   (ex-info
    message
    {:error/type
     error-type

     :error/details
     {:errors
      errors

      :context
      (context
        invitation)}})))

(defn- ensure!
  [test error-type message errors invitation]
  (when-not
   test
    (fail!
     error-type
     message
     errors
     invitation)))

(defn- ensure-document!
  [invitation]
  (ensure!
   (document-consistent?
    invitation)

   :invitation/invalid-document
   "The Invitation operation is invalid."
   {:invitation
    "The Invitation document is internally inconsistent."}
   invitation)

  invitation)

(defn- valid-change-time?
  [invitation now]
  (and
   (instant?
    now)

   (instant?
    (:invitation/updated-at
     invitation))

   (at-or-before?
    (:invitation/updated-at
     invitation)
    now)))

(defn- terminal-error
  [invitation]
  (case
   (invitation-status
    invitation)

    :accepted
    :invitation/accepted

    :declined
    :invitation/declined

    :revoked
    :invitation/revoked

    :expired
    :invitation/expired

    :invitation/not-pending))

(defn- ensure-pending!
  [invitation]
  (ensure-document!
   invitation)

  (ensure!
   (pending?
    invitation)

   (terminal-error
    invitation)

   "The Invitation operation is invalid."
   {:status
    "Only a pending Invitation can be changed."}
   invitation)

  invitation)

(defn- ensure-change-time!
  [invitation now]
  (ensure!
   (valid-change-time?
    invitation
    now)

   :invitation/invalid-time
   "The Invitation operation is invalid."
   {:now
    "The change time must not precede the current Invitation update time."}
   invitation)

  now)

(defn- ensure-usable!
  [invitation now]
  (ensure-change-time!
   invitation
   now)

  (ensure!
   (not
    (past-expiration?
     invitation
     now))

   :invitation/expired
   "The Invitation can no longer be used."
   {:expires-at
    "The Invitation has reached its expiration time."}
   invitation)

  invitation)

(defn- ensure-optional-actor!
  [invitation actor-id]
  (ensure!
   (optional-uuid?
    actor-id)

   :invitation/invalid-input
   "The Invitation operation is invalid."
   {:actor-id
    "The actor must be a UUID when supplied."}
   invitation)

  actor-id)

(defn- ensure-revocation-input!
  [invitation {:keys
               [actor-id
                reason]}]
  (ensure-optional-actor!
   invitation
   actor-id)

  (ensure!
   (optional-reason?
    reason)

   :invitation/invalid-input
   "The Invitation operation is invalid."
   {:reason
    "The revocation reason must be a qualified keyword when supplied."}
   invitation))

(defn- immutable-identity
  [invitation]
  (select-keys
   invitation
   [:xt/id
    :invitation/organization
    :invitation/invited-by
    :invitation/phone
    :invitation/email
    :invitation/role
    :invitation/scope-type
    :invitation/scope-id
    :invitation/token-hash
    :invitation/created-at
    :invitation/expires-at]))

(defn- version-state
  [invitation]
  (select-keys
   invitation
   [:invitation/revision
    :invitation/updated-at]))

(defn- update-invitation
  [invitation now mutation-fn]
  (ensure-document!
   invitation)

  (ensure-change-time!
   invitation
   now)

  (let [changed
        (mutation-fn
         invitation)]
    (ensure!
     (map?
      changed)

     :invitation/invalid-mutation
     "The Invitation operation is invalid."
     {:mutation
      "The Invitation mutation must return a map."}
     invitation)

    (ensure!
     (=
      (immutable-identity
       invitation)
      (immutable-identity
       changed))

     :invitation/immutable-identity
     "The Invitation operation is invalid."
     {:invitation
      "Recipient, offer, token, organization, creator, and expiration are immutable."}
     invitation)

    (ensure!
     (=
      (version-state
       invitation)
      (version-state
       changed))

     :invitation/invalid-version-mutation
     "The Invitation operation is invalid."
     {:invitation
      "The mutation must not directly change revision or updated-at."}
     invitation)

    (ensure!
     (not=
      invitation
      changed)

     :invitation/unchanged
     "The Invitation operation is invalid."
     {:invitation
      "The operation would not change the Invitation."}
     invitation)

    (ensure-document!
     (command/bump-version
      changed
      version
      now))))

;; =============================================================================
;; Creation
;; =============================================================================

(defn- normalize-create-input
  [input]
  (let [input
        (or
         input
         {})]
    {:id
     (:id
      input)

     :organization-id
     (:organization-id
      input)

     :invited-by
     (:invited-by
      input)

     :phone
     (user/normalize-phone
      (:phone
       input))

     :email
     (user/normalize-email
      (:email
       input))

     :role
     (:role
      input)

     :scope
     (:scope
      input)

     :token-hash
     (:token-hash
      input)

     :now
     (:now
      input)

     :expires-at
     (:expires-at
      input)}))

(defn- create-input-errors
  [{:keys
    [id
     organization-id
     invited-by
     phone
     email
     role
     scope
     token-hash
     now
     expires-at]}]
  (cond-> {}
    (not
     (uuid?
      id))
    (assoc
     :id
     "An Invitation UUID is required.")

    (not
     (uuid?
      organization-id))
    (assoc
     :organization-id
     "An Organization UUID is required.")

    (not
     (uuid?
      invited-by))
    (assoc
     :invited-by
     "The inviting User UUID is required.")

    (not=
     1
     (count
      (filter
       some?
       [phone
        email])))
    (assoc
     :recipient
     "Exactly one phone number or email address is required.")

    (and
     (some?
      phone)
     (not
      (user/phone?
       phone)))
    (assoc
     :phone
     "The phone number must be canonical E.164.")

    (and
     (some?
      email)
     (not
      (user/email?
       email)))
    (assoc
     :email
     "The email address is invalid.")

    (not
     (membership/role?
      role))
    (assoc
     :role
     "The offered role must be helper, supervisor, or admin.")

    (not
     (organization/scope?
      scope))
    (assoc
     :scope
     "An Organization, Organization Group, or Location scope is required.")

    (and
     (uuid?
      organization-id)

     (organization/organization-scope?
      scope)

     (not=
      organization-id
      (organization/scope-id
       scope)))
    (assoc
     :scope
     "An Organization-wide Invitation must reference its own Organization.")

    (not
     (token-hash?
      token-hash))
    (assoc
     :token-hash
     "A valid opaque Invitation token hash is required.")

    (not
     (instant?
      now))
    (assoc
     :now
     "A valid Invitation creation time is required.")

    (not
     (instant?
      expires-at))
    (assoc
     :expires-at
     "A valid Invitation expiration time is required.")

    (and
     (instant?
      now)

     (instant?
      expires-at)

     (not
      (before?
       now
       expires-at)))
    (assoc
     :expires-at
     "The Invitation expiration time must be after its creation time.")))

(defn- new-invitation
  [input]
  (let [{:keys
         [id
          organization-id
          invited-by
          phone
          email
          role
          scope
          token-hash
          now
          expires-at]
         :as normalized}
        (normalize-create-input
         input)

        errors
        (create-input-errors
         normalized)]
    (when
     (seq
      errors)
      (fail!
       :invitation/invalid-create-input
       "A valid Invitation could not be created."
       errors
       {:xt/id
        id

        :invitation/organization
        organization-id

        :invitation/invited-by
        invited-by

        :invitation/role
        role

        :invitation/scope-type
        (:scope/type
         scope)

        :invitation/scope-id
        (:scope/id
         scope)}))

    (ensure-document!
     (cond->
      {:xt/id
       id

       :invitation/organization
       organization-id

       :invitation/invited-by
       invited-by

       :invitation/role
       role

       :invitation/scope-type
       (organization/scope-type
        scope)

       :invitation/scope-id
       (organization/scope-id
        scope)

       :invitation/token-hash
       token-hash

       :invitation/status
       :pending

       :invitation/revision
       0

       :invitation/created-at
       now

       :invitation/updated-at
       now

       :invitation/expires-at
       expires-at}

       phone
       (assoc
        :invitation/phone
        phone)

       email
       (assoc
        :invitation/email
        email)))))

;; =============================================================================
;; Acceptance relationship validation
;; =============================================================================

(defn- acceptable-user?
  [invitation user-document]
  (and
   (user/active?
    user-document)

   (addressed-to-user?
    invitation
    user-document)))

(defn- acceptance-membership-consistent?
  [invitation user-document membership-document]
  (and
   (membership/membership-active?
    membership-document)

   (=
    (user/user-id
     user-document)
    (membership/membership-user-id
     membership-document))

   (=
    (organization-id
     invitation)
    (membership/membership-organization-id
     membership-document))))

(defn- acceptance-role-assignment-consistent?
  [invitation membership-document role-assignment]
  (and
   (membership/role-assignment-active?
    role-assignment)

   (=
    (membership/membership-id
     membership-document)
    (membership/role-assignment-membership-id
     role-assignment))

   (=
    (offered-role
     invitation)
    (membership/assigned-role
     role-assignment))

   (organization/same-scope?
    (scope
     invitation)
    (membership/role-assignment-scope
     role-assignment))))

;; =============================================================================
;; Acceptance
;; =============================================================================

(defn- accept-invitation
  [invitation {:keys
               [user
                membership
                role-assignment
                now]}]
  (ensure-pending!
   invitation)

  (ensure-usable!
   invitation
   now)

  (ensure!
   (map?
    user)

   :invitation/invalid-input
   "The Invitation acceptance is invalid."
   {:user
    "A loaded User document is required."}
   invitation)

  (ensure!
   (user/active?
    user)

   :invitation/user-not-active
   "The Invitation cannot be accepted by this User."
   {:user
    "Only an active User may accept an Invitation."}
   invitation)

  (ensure!
   (addressed-to-user?
    invitation
    user)

   :invitation/recipient-mismatch
   "The Invitation does not belong to this User."
   {:recipient
    "The Invitation must match a verified contact on the accepting User."}
   invitation)

  (ensure!
   (map?
    membership)

   :invitation/invalid-input
   "The Invitation acceptance is invalid."
   {:membership
    "A loaded Membership document is required."}
   invitation)

  (ensure!
   (acceptance-membership-consistent?
    invitation
    user
    membership)

   :invitation/membership-mismatch
   "The Membership does not satisfy the Invitation."
   {:membership
    "The Membership must be active and connect the accepting User to the invited Organization."}
   invitation)

  (ensure!
   (map?
    role-assignment)

   :invitation/invalid-input
   "The Invitation acceptance is invalid."
   {:role-assignment
    "A loaded RoleAssignment document is required."}
   invitation)

  (ensure!
   (acceptance-role-assignment-consistent?
    invitation
    membership
    role-assignment)

   :invitation/role-assignment-mismatch
   "The RoleAssignment does not satisfy the Invitation."
   {:role-assignment
    "The RoleAssignment must be active and grant the invited role at the invited scope through the accepted Membership."}
   invitation)

  (update-invitation
   invitation
   now
   #(assoc
     %
     :invitation/status
     :accepted

     :invitation/accepted-at
     now

     :invitation/accepted-by
     (user/user-id
      user)

     :invitation/membership
     (membership/membership-id
      membership)

     :invitation/role-assignment
     (membership/role-assignment-id
      role-assignment))))

;; =============================================================================
;; Decline
;; =============================================================================

(defn- decline-invitation
  [invitation {:keys
               [now
                actor-id]}]
  (ensure-pending!
   invitation)

  (ensure-optional-actor!
   invitation
   actor-id)

  (ensure-usable!
   invitation
   now)

  (update-invitation
   invitation
   now
   #(cond->
     (assoc
      %
      :invitation/status
      :declined

      :invitation/declined-at
      now)

      actor-id
      (assoc
       :invitation/declined-by
       actor-id))))

;; =============================================================================
;; Revocation
;; =============================================================================

(defn- revoke-invitation
  [invitation {:keys
               [now
                actor-id
                reason]
               :as input}]
  (ensure-pending!
   invitation)

  (ensure-revocation-input!
   invitation
   input)

  (ensure-usable!
   invitation
   now)

  (update-invitation
   invitation
   now
   #(cond->
     (assoc
      %
      :invitation/status
      :revoked

      :invitation/revoked-at
      now)

      actor-id
      (assoc
       :invitation/revoked-by
       actor-id)

      reason
      (assoc
       :invitation/revocation-reason
       reason))))

;; =============================================================================
;; Expiration
;; =============================================================================

(defn- expire-invitation
  [invitation {:keys
               [now]}]
  (ensure-pending!
   invitation)

  (ensure-change-time!
   invitation
   now)

  (ensure!
   (past-expiration?
    invitation
    now)

   :invitation/not-expired
   "The Invitation cannot yet be expired."
   {:expires-at
    "The Invitation has not reached its expiration time."}
   invitation)

  (update-invitation
   invitation
   now
   #(assoc
     %
     :invitation/status
     :expired

     :invitation/expired-at
     now)))

;; =============================================================================
;; Canonical model commands
;; =============================================================================

(defn create-invitation-command
  [input]
  (command/create
   entity-type
   (new-invitation
    input)
   version))

(defn- update-command
  [operation invitation transition input]
  (command/update-command
   entity-type
   operation
   invitation
   (transition
    invitation
    input)
   version))

(defn accept-invitation-command
  "Constructs the terminal acceptance update.

   input must contain already-loaded:

     :user
     :membership
     :role-assignment

   plus :now.

   The supplied documents must prove that acceptance realizes exactly the
   recipient, Organization, role, and scope represented by Invitation."
  [invitation input]
  (update-command
   :accept
   invitation
   accept-invitation
   input))

(defn decline-invitation-command
  [invitation input]
  (update-command
   :decline
   invitation
   decline-invitation
   input))

(defn revoke-invitation-command
  [invitation input]
  (update-command
   :revoke
   invitation
   revoke-invitation
   input))

(defn expire-invitation-command
  [invitation input]
  (update-command
   :expire
   invitation
   expire-invitation
   input))
