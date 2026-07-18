(ns net.humanhelp.site.model.user.fx
  "User-owned effectful workflows.

   This first vertical slice provides two operations:

   - invite a helper to an active location;
   - accept a location-scoped helper invitation.

   The state machines load facts and authorize the requested business action.
   Pure planning functions then construct domain commands, generic
   authorization-version guards, model-specific uniqueness assertions,
   semantic Gesso Live changes, and the application result.

   All transaction preparation and execution is delegated to
   net.humanhelp.site.model.fx. User FX decides which documents established an
   authorization decision, while model.fx validates, normalizes, and enforces
   those versions atomically. This namespace contains no XTDB execution, Biff
   transaction formatting, SQL count construction, or Live dispatcher
   implementation."
  (:require
   [clojure.string :as str]
   [gesso.fx :as fx]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.user.domain.access :as access]
   [net.humanhelp.site.model.user.domain.common :as user.common]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.model.user.domain.invitation :as invitation]
   [net.humanhelp.site.model.user.domain.membership :as membership]
   [net.humanhelp.site.model.user.domain.role :as role]
   [net.humanhelp.site.model.user.graph :as user.graph])
  (:import
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest SecureRandom]
   [java.time Duration]
   [java.util Base64]))

;; =============================================================================
;; Public operation contracts
;; =============================================================================

(def invitation-valid-for
  (Duration/ofDays 7))

(def token-byte-count
  32)

(def token-generator-key
  "Optional ctx key containing a zero-argument raw invitation-token generator.

   Production uses SecureRandom. Tests may supply a deterministic generator."
  ::token-generator)

(def location-context-query
  "Organization Graph contract required by the User invitation workflows.

   Given :organization/id and :location/id, Organization supplies:

   - the current location document;
   - whether it is active;
   - its owning organization;
   - the trusted scope chain applicable at the location;
   - authorization-version guards for every Organization document whose change
     could alter that scope chain or the location's active/ownership facts.

   Each guard has this generic shape:

     {:model/entity-type keyword
      :model/expected    model.common/expected-version-map}

   At least one guard must cover the requested location document. Organization
   owns the persistence details and must include relationship/group documents
   when changing them could invalidate the authorization decision."
  [:location/found?
   {[:? :location/doc] [:*]}
   [:? :location/active?]
   [:? :location/organization-id]
   {[:? :location/applicable-scopes]
    [:scope/type :scope/id]}
   [:? :location/authorization-versions]])

(def membership-with-roles-query
  [:user/found?
   {[:? :user/doc] user.graph/user-document-query}
   :user/current-membership-found?
   {[:? :user/current-membership]
    [{:membership/doc user.graph/membership-document-query}
     {:membership/role-assignments
      [{:role-assignment/doc
        user.graph/role-assignment-document-query}]}]}])

;; =============================================================================
;; Errors and common values
;; =============================================================================

(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type error-type}
       (some? details) (assoc :error/details details))))))

(defn- require-authenticated-user-id!
  [ctx]
  (or (:current-user/id ctx)
      (fail! :user/not-authenticated
             "A signed-in user is required.")))

(defn- require-found!
  [facts found-key document-key error-type message]
  (if (true? (get facts found-key))
    (or (get facts document-key)
        (fail! :user.fx/incomplete-graph-result
               "Graph reported a found entity without its document."
               {:found-key found-key
                :document-key document-key}))
    (fail! error-type message)))

(defn- command-document
  [command]
  (model.common/command-document command))

(defn- document-authorization-version
  [entity-type version document]
  {:model/entity-type entity-type
   :model/expected
   (model.common/expected-version
    document
    version)})

(defn- change-entry
  [{:keys [topic id]}]
  {:coalesce-key [topic id]})

;; =============================================================================
;; Secure invitation tokens
;; =============================================================================

(defonce ^:private secure-random
  (SecureRandom.))

(def ^:private base64url-encoder
  (.withoutPadding (Base64/getUrlEncoder)))

(defn- encode-bytes
  [bytes]
  (.encodeToString base64url-encoder bytes))

(defn generate-token
  "Generates a cryptographically random URL-safe bearer token."
  []
  (let [bytes (byte-array token-byte-count)]
    (.nextBytes secure-random bytes)
    (encode-bytes bytes)))

(defn hash-token
  "Returns the canonical persisted SHA-256 hash for a raw bearer token."
  [token]
  (when-not (and (string? token)
                 (not (str/blank? token)))
    (fail! :invitation/invalid-token
           "A nonblank invitation token is required."))
  (-> (MessageDigest/getInstance "SHA-256")
      (.digest (.getBytes ^String token StandardCharsets/UTF_8))
      encode-bytes))

(defn- token-from
  [ctx]
  (let [generator (or (get ctx token-generator-key) generate-token)]
    (when-not (ifn? generator)
      (fail! :user.fx/invalid-token-generator
             "The invitation token generator must be callable."
             {:value generator}))
    (generator)))

;; =============================================================================
;; Organization location facts
;; =============================================================================

(defn- location-query-input
  [organization-id location-id]
  {:organization/id organization-id
   :location/id location-id})

(defn- location-scope
  [location-id]
  (role/location-scope location-id))

(defn- organization-scope
  [organization-id]
  (role/organization-scope organization-id))

(defn- require-location-authorization-versions!
  [authorization-versions location-id]
  (when-not
   (sequential? authorization-versions)
    (fail! :location/invalid-authorization-versions
           "Organization authorization versions must be sequential."
           {:location/id location-id
            :authorization-versions authorization-versions}))

  (let [authorization-versions
        (vec authorization-versions)

        targets
        (mapv
         model.fx/authorization-version-target
         authorization-versions)

        expected-location-target
        [:location location-id]]
    (when
     (empty? authorization-versions)
      (fail! :location/missing-authorization-versions
             "Organization must supply authorization-version guards."
             {:location/id location-id}))

    (when-not
     (some
      #(= expected-location-target %)
      targets)
      (fail! :location/missing-location-version
             "Organization authorization guards must include the requested location document."
             {:location/id location-id
              :targets targets}))

    authorization-versions))

(defn- require-location-context!
  [facts organization-id location-id]
  (let [location
        (require-found!
         facts
         :location/found?
         :location/doc
         :location/not-found
         "The location no longer exists.")

        scopes
        (vec (:location/applicable-scopes facts))

        authorization-versions
        (require-location-authorization-versions!
         (:location/authorization-versions facts)
         location-id)

        expected-location-scope
        (location-scope location-id)

        expected-organization-scope
        (organization-scope organization-id)]
    (when-not (:location/active? facts)
      (fail! :location/not-active
             "The location is not active."
             {:location/id location-id}))

    (when-not (= organization-id (:location/organization-id facts))
      (fail! :location/organization-mismatch
             "The location does not belong to the supplied organization."
             {:organization/id organization-id
              :location/id location-id
              :actual-organization-id
              (:location/organization-id facts)}))

    (when-not (= location-id (:xt/id location))
      (fail! :location/inconsistent-facts
             "The location document ID does not match the requested location."
             {:location/id location-id
              :document-id (:xt/id location)}))

    (when-not (and (access/applicable-scopes? scopes)
                   (some #(user.common/same-scope?
                           expected-location-scope
                           %)
                         scopes)
                   (some #(user.common/same-scope?
                           expected-organization-scope
                           %)
                         scopes))
      (fail! :location/invalid-scope-context
             "Organization returned an invalid location scope chain."
             {:organization/id organization-id
              :location/id location-id
              :scopes scopes}))

    {:location location
     :authorization-versions authorization-versions
     :scopes scopes}))

;; =============================================================================
;; User access facts
;; =============================================================================

(defn- current-membership-document
  [facts]
  (get-in facts [:user/current-membership :membership/doc]))

(defn- current-role-documents
  [facts]
  (mapv :role-assignment/doc
        (get-in facts
                [:user/current-membership
                 :membership/role-assignments])))

(defn- require-location-admin!
  [facts scopes organization-id location-id]
  (let [user (:user/doc facts)
        membership-document (current-membership-document facts)
        assignments (current-role-documents facts)]
    (when-not (and user membership-document)
      (fail! :user.fx/incomplete-access-proof
             "The access result did not include the documents required to prove administrator authority."
             {:organization/id organization-id
              :location/id location-id}))

    (if-let [admin-assignment
             (access/administrator-assignment
              user
              membership-document
              assignments
              scopes)]
      {:user user
       :membership membership-document
       :role-assignment admin-assignment}

      (fail! :user/not-authorized
             "Administrator authority at this location is required."
             {:organization/id organization-id
              :location/id location-id}))))

(defn- access-proof-authorization-versions
  [{:keys [user membership role-assignment]}]
  [(document-authorization-version
    identity/entity-type
    identity/version
    user)
   (document-authorization-version
    membership/entity-type
    membership/version
    membership)
   (document-authorization-version
    role/entity-type
    role/version
    role-assignment)])

;; =============================================================================
;; Invitation recipient ownership
;; =============================================================================

(defn- verified-recipient?
  [invitation-document user]
  (case (invitation/recipient-type invitation-document)
    :phone
    (and (identity/phone-verified? user)
         (= (:invitation/phone invitation-document)
            (:user/phone user)))

    :email
    (and (identity/email-verified? user)
         (= (:invitation/email invitation-document)
            (:user/email user)))

    false))

(defn- require-acceptable-invitation!
  [invitation-document user now]
  (when-not (identity/active? user)
    (fail! :user/not-active
           "Only an active user can accept a staff invitation."
           {:user/id (:xt/id user)}))

  (when-not (and (invitation/offers-role? invitation-document :helper)
                 (= :location
                    (:scope/type
                     (invitation/scope invitation-document))))
    (fail! :invitation/not-helper-location-offer
           "This operation accepts only location-scoped helper invitations."
           {:invitation/id (:xt/id invitation-document)
            :role (invitation/offered-role invitation-document)
            :scope (invitation/scope invitation-document)}))

  (when-not (invitation/usable-at? invitation-document now)
    (fail! (if (invitation/past-expiration? invitation-document now)
             :invitation/expired
             :invitation/not-pending)
           "The invitation can no longer be accepted."
           {:invitation/id (:xt/id invitation-document)}))

  (when-not (verified-recipient? invitation-document user)
    (fail! :invitation/recipient-mismatch
           "The invitation does not match a verified contact on the signed-in user."
           {:invitation/id (:xt/id invitation-document)
            :user/id (:xt/id user)
            :recipient-type
            (invitation/recipient-type invitation-document)})))

;; =============================================================================
;; User-specific commit guards
;; =============================================================================

(defn- current-membership-predicate
  [user-id organization-id]
  [:and
   [:= :membership/user user-id]
   [:= :membership/organization organization-id]
   [:<> :membership/status :revoked]])

(defn- active-role-predicate
  [membership-id assigned-role scope]
  [:and
   [:= :role-assignment/membership membership-id]
   [:= :role-assignment/role assigned-role]
   [:= :role-assignment/scope-type (:scope/type scope)]
   [:= :role-assignment/scope-id (:scope/id scope)]
   [:= :role-assignment/status :active]])

;; =============================================================================
;; Semantic primary changes
;; =============================================================================

(defn- invitation-change
  [invitation-document change-kind operation]
  (let [scope (invitation/scope invitation-document)]
    {:topic :invitation
     :id (:xt/id invitation-document)
     :change/kind change-kind
     :invitation/operation operation
     :organization/id (invitation/organization-id invitation-document)
     :scope/type (:scope/type scope)
     :scope/id (:scope/id scope)}))

(defn- membership-change
  [membership-document]
  {:topic :membership
   :id (:xt/id membership-document)
   :change/kind :created
   :user/id (:membership/user membership-document)
   :organization/id (:membership/organization membership-document)})

(defn- role-assignment-change
  [role-assignment]
  {:topic :role-assignment
   :id (:xt/id role-assignment)
   :change/kind :created
   :membership/id (:role-assignment/membership role-assignment)
   :organization/id (:role-assignment/organization role-assignment)
   :role (:role-assignment/role role-assignment)
   :scope/type (:role-assignment/scope-type role-assignment)
   :scope/id (:role-assignment/scope-id role-assignment)})

;; =============================================================================
;; Invite helper planning
;; =============================================================================

(defn plan-helper-invitation
  "Purely constructs the transaction plan and public result for a validated,
   authorized location-helper invitation."
  [{:keys
    [command
     raw-token
     location-authorization-versions
     access-proof]}]
  (let [invitation-document
        (command-document command)]
    {:transaction-plan
     {:commands
      [command]

      :authorization-versions
      (into
       (vec location-authorization-versions)
       (access-proof-authorization-versions
        access-proof))

      :assertions
      [(model.fx/assert-none
        invitation/entity-type
        [:= :invitation/token-hash
         (:invitation/token-hash invitation-document)])]

      :changes
      [(invitation-change
        invitation-document
        :created
        :create)]

      :entry-fn
      change-entry}

     :result
     {:invitation invitation-document
      :token raw-token}}))

;; =============================================================================
;; Invite helper machine
;; =============================================================================

(fx/defmachine invite-helper-to-location-machine
  :start
  (fn [ctx]
    (let [now (:biff.fx/now ctx)
          seed (:biff.fx/seed ctx)
          [invitation-id _] (fx/uuid7 seed now)
          input (:user.fx/input ctx)
          organization-id (:organization-id input)
          location-id (:location-id input)
          raw-token (token-from ctx)
          invitation-input
          (invitation/normalize-create-input
           {:id invitation-id
            :organization-id organization-id
            :invited-by (require-authenticated-user-id! ctx)
            :phone (:phone input)
            :email (:email input)
            :role :helper
            :scope (location-scope location-id)
            :token-hash (hash-token raw-token)
            :now now
            :expires-at (.plus now invitation-valid-for)})
          errors (invitation/create-input-errors invitation-input)]
      (when (seq errors)
        (model.common/throw-invalid!
         :invitation/invalid-create-input
         "A valid location helper invitation could not be created."
         errors
         {:organization/id organization-id
          :location/id location-id}))

      {:user.fx/raw-token raw-token
       :user.fx/invitation-input invitation-input
       :user.fx/location-facts
       [:biff.graph.fx/query
        (location-query-input organization-id location-id)
        location-context-query]
       :biff.fx/next :authorize}))

  :authorize
  (fn [ctx]
    (let [input (:user.fx/input ctx)
          organization-id (:organization-id input)
          location-id (:location-id input)
          location-context
          (require-location-context!
           (:user.fx/location-facts ctx)
           organization-id
           location-id)]
      {:user.fx/raw-token (:user.fx/raw-token ctx)
       :user.fx/invitation-input (:user.fx/invitation-input ctx)
       :user.fx/location-context location-context
       :user.fx/access-facts
       [:biff.graph.fx/query
        (user.graph/access-query-input
         {:user-id (require-authenticated-user-id! ctx)
          :organization-id organization-id
          :applicable-scopes (:scopes location-context)})
        user.graph/access-query]
       :biff.fx/next :plan}))

  :plan
  (fn [ctx]
    (let [input (:user.fx/input ctx)
          organization-id (:organization-id input)
          location-id (:location-id input)
          location-context (:user.fx/location-context ctx)
          access-proof
          (require-location-admin!
           (:user.fx/access-facts ctx)
           (:scopes location-context)
           organization-id
           location-id)
          command
          (invitation/create-command
           (:user.fx/invitation-input ctx))
          plan
          (plan-helper-invitation
           {:command command
            :raw-token (:user.fx/raw-token ctx)
            :location-authorization-versions
            (:authorization-versions location-context)
            :access-proof access-proof})]
      {:user.fx/result (:result plan)
       :user.fx/transaction-plan (:transaction-plan plan)
       :biff.fx/next :commit}))

  :commit
  (fn [{:user.fx/keys [result transaction-plan]}]
    {:user.fx/result result
     :user.fx/transaction
     [model.fx/transact-effect transaction-plan]
     :biff.fx/next :finish})

  :finish
  (fn [{:user.fx/keys [result transaction]}]
    {:biff.fx/return
     (assoc result :transaction transaction)}))

(defn invite-helper-to-location
  "Creates one pending helper invitation for an active location.

   Input contains :organization-id, :location-id, and exactly one of :phone or
   :email. The returned raw token is available exactly once for the delivery
   layer; only its hash is persisted."
  [ctx input]
  (invite-helper-to-location-machine
   (assoc ctx :user.fx/input input)))

;; =============================================================================
;; Accept invitation planning
;; =============================================================================

(defn- matching-helper-assignments
  [assignments membership-id invitation-document]
  (filterv
   #(role/grants?
     %
     membership-id
     :helper
     (invitation/scope invitation-document))
   assignments))

(defn- require-reusable-membership!
  [membership-document]
  (when (membership/suspended? membership-document)
    (fail! :membership/suspended
           "A suspended membership must be reactivated before accepting this invitation."
           {:membership/id (:xt/id membership-document)}))

  (when-not (membership/active? membership-document)
    (fail! :membership/not-active
           "The existing organization membership is not active."
           {:membership/id (:xt/id membership-document)}))

  membership-document)

(defn plan-invitation-acceptance
  "Purely plans acceptance of one validated location-helper invitation.

   The result reuses an active current membership and matching active helper
   assignment when present. Otherwise it creates the missing documents. The
   returned transaction plan atomically commits every command and rechecks all
   facts on which the decision depended."
  [{:keys [now
           user
           invitation-document
           location-authorization-versions
           existing-membership
           existing-role-assignments
           generated-membership-id
           generated-role-assignment-id]}]
  (require-acceptable-invitation! invitation-document user now)

  (let [organization-id
        (invitation/organization-id invitation-document)

        invitation-scope
        (invitation/scope invitation-document)

        existing-membership
        (some-> existing-membership require-reusable-membership!)

        membership-command
        (when-not existing-membership
          (membership/create-command
           {:id generated-membership-id
            :user-id (:xt/id user)
            :organization-id organization-id
            :now now}))

        membership-document
        (or existing-membership
            (command-document membership-command))

        matching-assignments
        (if existing-membership
          (matching-helper-assignments
           existing-role-assignments
           (:xt/id membership-document)
           invitation-document)
          [])

        _
        (when (< 1 (count matching-assignments))
          (fail! :role-assignment/ambiguous
                 "More than one active helper assignment matches this invitation."
                 {:membership/id (:xt/id membership-document)
                  :scope invitation-scope
                  :role-assignment/ids
                  (mapv :xt/id matching-assignments)}))

        existing-role-assignment
        (first matching-assignments)

        role-command
        (when-not existing-role-assignment
          (role/create-command
           {:id generated-role-assignment-id
            :membership-id (:xt/id membership-document)
            :organization-id organization-id
            :role :helper
            :scope invitation-scope
            :actor-id (:invitation/invited-by invitation-document)
            :reason :invitation/accepted
            :now now}))

        role-assignment
        (or existing-role-assignment
            (command-document role-command))

        invitation-command
        (invitation/accept-command
         invitation-document
         {:now now
          :user-id (:xt/id user)
          :membership-id (:xt/id membership-document)
          :role-assignment-id (:xt/id role-assignment)})

        accepted-invitation
        (command-document invitation-command)

        commands
        (cond-> []
          membership-command (conj membership-command)
          role-command (conj role-command)
          true (conj invitation-command))

        membership-predicate
        (current-membership-predicate
         (:xt/id user)
         organization-id)

        role-predicate
        (active-role-predicate
         (:xt/id membership-document)
         :helper
         invitation-scope)

        authorization-versions
        (cond->
         (into
          [(document-authorization-version
            identity/entity-type
            identity/version
            user)]
          location-authorization-versions)

          existing-membership
          (conj
           (document-authorization-version
            membership/entity-type
            membership/version
            existing-membership))

          existing-role-assignment
          (conj
           (document-authorization-version
            role/entity-type
            role/version
            existing-role-assignment)))

        assertions
        (cond-> []
          existing-membership
          (conj
           (model.fx/assert-one
            membership/entity-type
            membership-predicate))

          membership-command
          (conj
           (model.fx/assert-none
            membership/entity-type
            membership-predicate))

          existing-role-assignment
          (conj
           (model.fx/assert-one
            role/entity-type
            role-predicate))

          role-command
          (conj
           (model.fx/assert-none
            role/entity-type
            role-predicate)))

        changes
        (cond-> []
          membership-command
          (conj (membership-change membership-document))

          role-command
          (conj (role-assignment-change role-assignment))

          true
          (conj
           (invitation-change accepted-invitation :updated :accept)))]

    {:transaction-plan
     {:commands commands
      :authorization-versions authorization-versions
      :assertions assertions
      :changes changes
      :entry-fn change-entry}

     :result
     {:user user
      :membership membership-document
      :role-assignment role-assignment
      :invitation accepted-invitation}}))

;; =============================================================================
;; Accept invitation machine
;; =============================================================================

(fx/defmachine accept-invitation-machine
  :start
  (fn [ctx]
    (let [now (:biff.fx/now ctx)
          seed (:biff.fx/seed ctx)
          [membership-id seed] (fx/uuid7 seed now)
          [role-assignment-id _] (fx/uuid7 seed now)
          token-hash (hash-token (get-in ctx [:user.fx/input :token]))]
      {:user.fx/generated-membership-id membership-id
       :user.fx/generated-role-assignment-id role-assignment-id
       :user.fx/invitation-facts
       [:biff.graph.fx/query
        (user.graph/invitation-query-input {:token-hash token-hash})
        user.graph/invitation-command-query]
       :user.fx/user-facts
       [:biff.graph.fx/query
        (user.graph/user-query-input
         {:user-id (require-authenticated-user-id! ctx)})
        user.graph/user-command-query]
       :biff.fx/next :load-context}))

  :load-context
  (fn [ctx]
    (let [invitation-document
          (require-found!
           (:user.fx/invitation-facts ctx)
           :invitation/found?
           :invitation/doc
           :invitation/not-found
           "The invitation no longer exists.")

          user
          (require-found!
           (:user.fx/user-facts ctx)
           :user/found?
           :user/doc
           :user/not-found
           "The signed-in user no longer exists.")

          organization-id
          (invitation/organization-id invitation-document)

          location-id
          (:scope/id (invitation/scope invitation-document))]
      (require-acceptable-invitation!
       invitation-document
       user
       (:biff.fx/now ctx))

      {:user.fx/generated-membership-id
       (:user.fx/generated-membership-id ctx)
       :user.fx/generated-role-assignment-id
       (:user.fx/generated-role-assignment-id ctx)
       :user.fx/invitation invitation-document
       :user.fx/user user
       :user.fx/location-facts
       [:biff.graph.fx/query
        (location-query-input organization-id location-id)
        location-context-query]
       :user.fx/membership-facts
       [:biff.graph.fx/query
        {:user/id (:xt/id user)
         :membership/organization-id organization-id}
        membership-with-roles-query]
       :biff.fx/next :plan}))

  :plan
  (fn [ctx]
    (let [invitation-document (:user.fx/invitation ctx)
          user (:user.fx/user ctx)
          organization-id
          (invitation/organization-id invitation-document)
          location-id
          (:scope/id (invitation/scope invitation-document))
          location-context
          (require-location-context!
           (:user.fx/location-facts ctx)
           organization-id
           location-id)
          membership-facts (:user.fx/membership-facts ctx)
          existing-membership
          (when (:user/current-membership-found? membership-facts)
            (current-membership-document membership-facts))
          plan
          (plan-invitation-acceptance
           {:now (:biff.fx/now ctx)
            :user user
            :invitation-document invitation-document
            :location-authorization-versions
            (:authorization-versions location-context)
            :existing-membership existing-membership
            :existing-role-assignments
            (if existing-membership
              (current-role-documents membership-facts)
              [])
            :generated-membership-id
            (:user.fx/generated-membership-id ctx)
            :generated-role-assignment-id
            (:user.fx/generated-role-assignment-id ctx)})]
      {:user.fx/result (:result plan)
       :user.fx/transaction-plan (:transaction-plan plan)
       :biff.fx/next :commit}))

  :commit
  (fn [{:user.fx/keys [result transaction-plan]}]
    {:user.fx/result result
     :user.fx/transaction
     [model.fx/transact-effect transaction-plan]
     :biff.fx/next :finish})

  :finish
  (fn [{:user.fx/keys [result transaction]}]
    {:biff.fx/return
     (assoc result :transaction transaction)}))

(defn accept-invitation
  "Accepts a pending location-scoped helper invitation.

   Input is {:token raw-bearer-token}. The signed-in user must own the
   invitation's verified phone or email. Acceptance atomically creates or
   reuses the organization membership, creates or reuses the location helper
   assignment, and marks the invitation accepted."
  [ctx input]
  (accept-invitation-machine
   (assoc ctx :user.fx/input input)))

;; =============================================================================
;; Public operation contribution
;; =============================================================================

(def operations
  {:user/invite-helper-to-location #'invite-helper-to-location
   :user/accept-invitation #'accept-invitation})
