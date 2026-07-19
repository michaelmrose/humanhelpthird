(ns net.humanhelp.site.model.user.domain.invitation
  "Pure rules for persisted HumanHelp staff invitations.

   An invitation addresses exactly one phone number or email address and offers
   one role at one organization scope. Only the bearer-token hash is persisted.

   Acceptance records the User, Membership, and role-assignment IDs produced by
   the surrounding atomic workflow. Structural authorization-scope values come
   from model.authorization-scope.

   This namespace does not create those documents, query XTDB, compare raw
   tokens, deliver invitations, establish recipient ownership, validate
   Organization hierarchy, or authorize actors."
  (:require
   [net.humanhelp.site.model.authorization-scope :as authorization-scope]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.domain.common :as user.common]))

(def entity-type :invitation)

(def version
  {:revision-key :invitation/revision
   :created-at-key :invitation/created-at
   :updated-at-key :invitation/updated-at})

(def token-hash-min 32)
(def token-hash-max 256)

;; Lifecycle -------------------------------------------------------------------

(def statuses
  #{:pending :accepted :declined :revoked :expired})

(def terminal-statuses
  #{:accepted :declined :revoked :expired})

(def allowed-transitions
  {[:pending :accept] :accepted
   [:pending :decline] :declined
   [:pending :revoke] :revoked
   [:pending :expire] :expired})

(defn status? [value]
  (contains? statuses value))

(defn pending? [invitation]
  (= :pending (:invitation/status invitation)))

(defn accepted? [invitation]
  (= :accepted (:invitation/status invitation)))

(defn declined? [invitation]
  (= :declined (:invitation/status invitation)))

(defn revoked? [invitation]
  (= :revoked (:invitation/status invitation)))

(defn expired? [invitation]
  (= :expired (:invitation/status invitation)))

(defn terminal? [invitation]
  (contains? terminal-statuses (:invitation/status invitation)))

(defn next-status [invitation operation]
  (get allowed-transitions [(:invitation/status invitation) operation]))

(defn can-transition? [invitation operation]
  (some? (next-status invitation operation)))

;; Facts -----------------------------------------------------------------------

(defn organization-id [invitation]
  (:invitation/organization invitation))

(defn offered-role [invitation]
  (:invitation/role invitation))

(defn scope [invitation]
  {:scope/type (:invitation/scope-type invitation)
   :scope/id (:invitation/scope-id invitation)})

(defn for-organization? [invitation expected-organization-id]
  (= expected-organization-id (organization-id invitation)))

(defn offers-role? [invitation expected-role]
  (= expected-role (offered-role invitation)))

(defn at-scope? [invitation expected-scope]
  (authorization-scope/same-scope? (scope invitation) expected-scope))

(defn recipient-type [invitation]
  (cond
    (some? (:invitation/phone invitation)) :phone
    (some? (:invitation/email invitation)) :email
    :else nil))

(defn recipient-value [invitation]
  (case (recipient-type invitation)
    :phone (:invitation/phone invitation)
    :email (:invitation/email invitation)
    nil))

(defn addressed-to?
  "Checks canonical value equality only; it does not establish ownership."
  [invitation {:keys [phone email]}]
  (case (recipient-type invitation)
    :phone (= (:invitation/phone invitation)
              (user.common/normalize-phone phone))
    :email (= (:invitation/email invitation)
              (user.common/normalize-email email))
    false))

(defn past-expiration?
  "Returns true at or after the expiration instant."
  [invitation now]
  (and
   (model.common/timestamp-value? now)
   (model.common/timestamp-value? (:invitation/expires-at invitation))
   (not (model.common/timestamp< now (:invitation/expires-at invitation)))))

(defn usable-at? [invitation now]
  (and
   (pending? invitation)
   (model.common/timestamp-value? now)
   (model.common/timestamp<= (:invitation/created-at invitation) now)
   (not (past-expiration? invitation now))))

;; Validation ------------------------------------------------------------------

(defn token-hash?
  "Validates the opaque persisted representation, not cryptographic strength."
  [value]
  (and
   (string? value)
   (<= token-hash-min (count value) token-hash-max)
   (boolean (re-matches #"\S+" value))))

(defn- optional-uuid? [value]
  (or (nil? value) (uuid? value)))

(defn- optional-reason? [value]
  (or (nil? value) (qualified-keyword? value)))

(defn- none-present? [invitation keys]
  (every? nil? (map invitation keys)))

(defn- timestamp-within-document? [invitation value]
  (model.common/optional-between?
   (:invitation/created-at invitation)
   value
   (:invitation/updated-at invitation)))

(defn- terminal-before-expiration? [invitation value]
  (or
   (nil? value)
   (model.common/timestamp< value (:invitation/expires-at invitation))))

(defn- scope-consistent? [invitation]
  (let [invitation-scope (scope invitation)]
    (and
     (authorization-scope/scope-reference? invitation-scope)
     (or
      (not (authorization-scope/organization-scope? invitation-scope))
      (= (:scope/id invitation-scope)
         (:invitation/organization invitation))))))

(def accepted-fields
  [:invitation/accepted-at
   :invitation/accepted-by
   :invitation/membership
   :invitation/role-assignment])

(def declined-fields
  [:invitation/declined-at
   :invitation/declined-by])

(def revoked-fields
  [:invitation/revoked-at
   :invitation/revoked-by
   :invitation/revocation-reason])

(def terminal-fields
  (into [] cat [accepted-fields declined-fields revoked-fields]))

(defn- pending-consistent? [invitation]
  (and
   (none-present? invitation terminal-fields)
   (model.common/timestamp<
    (:invitation/updated-at invitation)
    (:invitation/expires-at invitation))))

(defn- accepted-consistent? [invitation]
  (and
   (some? (:invitation/accepted-at invitation))
   (uuid? (:invitation/accepted-by invitation))
   (uuid? (:invitation/membership invitation))
   (uuid? (:invitation/role-assignment invitation))
   (none-present? invitation (into [] cat [declined-fields revoked-fields]))))

(defn- declined-consistent? [invitation]
  (and
   (some? (:invitation/declined-at invitation))
   (none-present? invitation (into [] cat [accepted-fields revoked-fields]))))

(defn- revoked-consistent? [invitation]
  (and
   (some? (:invitation/revoked-at invitation))
   (none-present? invitation (into [] cat [accepted-fields declined-fields]))))

(defn- expired-consistent? [invitation]
  (and
   (none-present? invitation terminal-fields)
   (model.common/timestamp<=
    (:invitation/expires-at invitation)
    (:invitation/updated-at invitation))))

(defn document-consistent?
  "Validates only the invitation document's local invariants."
  [invitation]
  (and
   (map? invitation)
   (model.common/versioned-document-consistent? invitation version)
   (uuid? (:invitation/organization invitation))
   (uuid? (:invitation/invited-by invitation))
   (model.common/exactly-one-present?
    (:invitation/phone invitation)
    (:invitation/email invitation))
   (or (nil? (:invitation/phone invitation))
       (user.common/phone? (:invitation/phone invitation)))
   (or (nil? (:invitation/email invitation))
       (user.common/email? (:invitation/email invitation)))
   (user.common/role? (:invitation/role invitation))
   (scope-consistent? invitation)
   (token-hash? (:invitation/token-hash invitation))
   (status? (:invitation/status invitation))
   (model.common/timestamp-value? (:invitation/expires-at invitation))
   (model.common/timestamp<
    (:invitation/created-at invitation)
    (:invitation/expires-at invitation))
   (every?
    #(timestamp-within-document? invitation %)
    [(:invitation/accepted-at invitation)
     (:invitation/declined-at invitation)
     (:invitation/revoked-at invitation)])
   (every?
    #(terminal-before-expiration? invitation %)
    [(:invitation/accepted-at invitation)
     (:invitation/declined-at invitation)
     (:invitation/revoked-at invitation)])
   (optional-uuid? (:invitation/accepted-by invitation))
   (optional-uuid? (:invitation/declined-by invitation))
   (optional-uuid? (:invitation/revoked-by invitation))
   (optional-reason? (:invitation/revocation-reason invitation))
   (optional-uuid? (:invitation/membership invitation))
   (optional-uuid? (:invitation/role-assignment invitation))
   (case (:invitation/status invitation)
     :pending (pending-consistent? invitation)
     :accepted (accepted-consistent? invitation)
     :declined (declined-consistent? invitation)
     :revoked (revoked-consistent? invitation)
     :expired (expired-consistent? invitation)
     false)))

(defn normalize-create-input [input]
  (let [input (or input {})]
    {:id (:id input)
     :organization-id (:organization-id input)
     :invited-by (:invited-by input)
     :phone (user.common/normalize-phone (:phone input))
     :email (user.common/normalize-email (:email input))
     :role (:role input)
     :scope (:scope input)
     :token-hash (:token-hash input)
     :now (:now input)
     :expires-at (:expires-at input)}))

(defn create-input-errors
  [{:keys [id organization-id invited-by phone email role scope token-hash
           now expires-at]}]
  (cond-> {}
    (not (uuid? id))
    (assoc :id "An invitation UUID is required.")

    (not (uuid? organization-id))
    (assoc :organization-id "An organization UUID is required.")

    (not (uuid? invited-by))
    (assoc :invited-by "The inviting user UUID is required.")

    (not (model.common/exactly-one-present? phone email))
    (assoc :recipient "Exactly one phone number or email address is required.")

    (and (some? phone) (not (user.common/phone? phone)))
    (assoc :phone "The phone number must be canonical E.164.")

    (and (some? email) (not (user.common/email? email)))
    (assoc :email "The email address is invalid.")

    (not (user.common/role? role))
    (assoc :role "The role must be helper, supervisor, or admin.")

    (not (authorization-scope/scope-reference? scope))
    (assoc :scope
           "An organization, organization-group, or location scope is required.")

    (and
     (authorization-scope/organization-scope? scope)
     (not= organization-id (:scope/id scope)))
    (assoc :scope
           "An organization-wide invitation must reference its organization.")

    (not (token-hash? token-hash))
    (assoc :token-hash "A valid invitation token hash is required.")

    (not (model.common/timestamp-value? now))
    (assoc :now "A valid invitation creation time is required.")

    (not (model.common/timestamp-value? expires-at))
    (assoc :expires-at "A valid invitation expiration time is required.")

    (and
     (model.common/timestamp-value? now)
     (model.common/timestamp-value? expires-at)
     (not (model.common/timestamp< now expires-at)))
    (assoc :expires-at
           "The expiration time must be after the creation time.")))

(defn- context [invitation]
  {:invitation/id (:xt/id invitation)
   :invitation/organization (:invitation/organization invitation)
   :invitation/recipient-type (recipient-type invitation)
   :invitation/role (:invitation/role invitation)
   :invitation/scope (when (map? invitation) (scope invitation))
   :invitation/status (:invitation/status invitation)})

(defn- fail! [invitation error-type errors]
  (model.common/throw-invalid!
   error-type
   "The invitation operation is invalid."
   errors
   (context invitation)))

(defn- ensure! [test invitation error-type errors]
  (when-not test
    (fail! invitation error-type errors)))

(defn- ensure-document! [invitation]
  (ensure!
   (document-consistent? invitation)
   invitation
   :invitation/invalid-document
   {:invitation "The invitation document is internally inconsistent."})
  invitation)

(defn- ensure-change-time! [invitation now]
  (ensure!
   (model.common/valid-change-time? invitation version now)
   invitation
   :invitation/invalid-time
   {:now "The change time must not precede the last update."}))

(defn- ensure-optional-actor! [invitation actor-id]
  (ensure!
   (optional-uuid? actor-id)
   invitation
   :invitation/invalid-input
   {:actor-id "The actor must be a UUID when supplied."}))

(defn- ensure-revocation-input! [invitation {:keys [actor-id reason]}]
  (ensure-optional-actor! invitation actor-id)
  (ensure!
   (optional-reason? reason)
   invitation
   :invitation/invalid-input
   {:reason "The revocation reason must be a qualified keyword when supplied."}))

(defn- terminal-error [invitation]
  (case (:invitation/status invitation)
    :accepted :invitation/accepted
    :declined :invitation/declined
    :revoked :invitation/revoked
    :expired :invitation/expired
    :invitation/not-pending))

(defn- ensure-pending! [invitation]
  (ensure!
   (pending? invitation)
   invitation
   (terminal-error invitation)
   {:status "Only a pending invitation can be changed."}))

(defn- ensure-usable! [invitation now]
  (ensure-change-time! invitation now)
  (ensure!
   (not (past-expiration? invitation now))
   invitation
   :invitation/expired
   {:expires-at "The invitation has expired."}))

(defn- update-invitation [invitation now f]
  (ensure-document! invitation)
  (ensure-change-time! invitation now)
  (let [changed (f invitation)]
    (ensure!
     (not= invitation changed)
     invitation
     :invitation/unchanged
     {:invitation "The operation would not change the invitation."})
    (-> changed
        (model.common/bump-revision version now)
        ensure-document!)))

;; Construction ----------------------------------------------------------------

(defn new-invitation [input]
  (let [{:keys [id organization-id invited-by phone email role scope token-hash
                now expires-at]
         :as normalized}
        (normalize-create-input input)
        errors (create-input-errors normalized)]
    (when (seq errors)
      (model.common/throw-invalid!
       :invitation/invalid-create-input
       "A valid staff invitation could not be created."
       errors
       {:invitation/id id
        :invitation/organization organization-id
        :invitation/invited-by invited-by
        :invitation/role role
        :invitation/scope scope}))
    (ensure-document!
     (cond->
      {:xt/id id
       :invitation/organization organization-id
       :invitation/invited-by invited-by
       :invitation/role role
       :invitation/scope-type (:scope/type scope)
       :invitation/scope-id (:scope/id scope)
       :invitation/token-hash token-hash
       :invitation/status :pending
       :invitation/revision 0
       :invitation/created-at now
       :invitation/updated-at now
       :invitation/expires-at expires-at}
       phone (assoc :invitation/phone phone)
       email (assoc :invitation/email email)))))

;; Lifecycle transitions --------------------------------------------------------

(defn accept
  [invitation {:keys [now user-id membership-id role-assignment-id]}]
  (ensure-document! invitation)
  (ensure-pending! invitation)
  (ensure! (uuid? user-id) invitation :invitation/invalid-input
           {:user-id "The accepting user UUID is required."})
  (ensure! (uuid? membership-id) invitation :invitation/invalid-input
           {:membership-id "The accepted membership UUID is required."})
  (ensure! (uuid? role-assignment-id) invitation :invitation/invalid-input
           {:role-assignment-id "The accepted role-assignment UUID is required."})
  (ensure-usable! invitation now)
  (update-invitation
   invitation
   now
   #(assoc %
           :invitation/status :accepted
           :invitation/accepted-at now
           :invitation/accepted-by user-id
           :invitation/membership membership-id
           :invitation/role-assignment role-assignment-id)))

(defn decline
  [invitation {:keys [now actor-id]}]
  (ensure-document! invitation)
  (ensure-pending! invitation)
  (ensure-optional-actor! invitation actor-id)
  (ensure-usable! invitation now)
  (update-invitation
   invitation
   now
   #(cond->
     (assoc %
            :invitation/status :declined
            :invitation/declined-at now)
     actor-id (assoc :invitation/declined-by actor-id))))

(defn revoke
  [invitation {:keys [now actor-id reason] :as input}]
  (ensure-document! invitation)
  (ensure-pending! invitation)
  (ensure-revocation-input! invitation input)
  (ensure-usable! invitation now)
  (update-invitation
   invitation
   now
   #(cond->
     (assoc %
            :invitation/status :revoked
            :invitation/revoked-at now)
     actor-id (assoc :invitation/revoked-by actor-id)
     reason (assoc :invitation/revocation-reason reason))))

(defn expire
  [invitation {:keys [now]}]
  (ensure-document! invitation)
  (ensure-pending! invitation)
  (ensure-change-time! invitation now)
  (ensure!
   (past-expiration? invitation now)
   invitation
   :invitation/not-expired
   {:expires-at "The invitation has not reached its expiration time."})
  (update-invitation
   invitation
   now
   #(assoc % :invitation/status :expired)))

;; Commands --------------------------------------------------------------------

(defn create-command [input]
  (model.common/create-command entity-type (new-invitation input) version))

(defn- change-command [operation before after]
  (model.common/update-command entity-type operation before after version))

(defn accept-command [invitation input]
  (change-command :accept invitation (accept invitation input)))

(defn decline-command [invitation input]
  (change-command :decline invitation (decline invitation input)))

(defn revoke-command [invitation input]
  (change-command :revoke invitation (revoke invitation input)))

(defn expire-command [invitation input]
  (change-command :expire invitation (expire invitation input)))
