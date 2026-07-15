(ns net.humanhelp.site.model.user.fx
  "User-owned effectful workflows.

   This first vertical slice provides exactly two operations:

   - invite an authenticated organization administrator to offer the helper
     role at one active location;
   - let an authenticated user with the invitation's verified contact accept
     that offer atomically.

   Organization contributes the location Graph facts described by
   `location-context-query`. User FX owns the invitation, membership, and role
   assignment workflow, but it does not own location hierarchy or lifecycle.

   Persistence deliberately composes existing infrastructure:

   - Gesso FX executes state machines and resolves effects;
   - Biff validates persisted documents and formats HoneySQL transaction
     assertions;
   - Gesso's narrow XTDB2 consistency adapter executes the transaction and
     returns an explicit read-after-write consistency basis.

   This namespace contains no HTTP, Hiccup, HTMX, SSE, live invalidation, email,
   or SMS delivery code."
  (:require
   [clojure.string :as str]
   [com.biffweb.experimental :as biffx]
   [gesso.fx :as fx]
   [gesso.live.consistency.xtdb :as live.xtdb]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.user.domain.access :as access]
   [net.humanhelp.site.model.user.domain.common :as user.common]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.model.user.domain.invitation :as invitation]
   [net.humanhelp.site.model.user.domain.membership :as membership]
   [net.humanhelp.site.model.user.domain.role :as role]
   [net.humanhelp.site.model.user.graph :as user.graph])
  (:import
   [java.security MessageDigest SecureRandom]
   [java.time Duration]
   [java.util Base64]))

;; =============================================================================
;; Public operation and effect contracts
;; =============================================================================

(def commit-effect
  "Gesso FX effect used to validate, format, and execute one XTDB2 transaction."
  ::commit)

(def invitation-valid-for
  (Duration/ofDays 7))

(def token-byte-count
  32)

(def location-version
  "Version metadata expected from Organization location documents.

   Organization should use the same revision conventions as the other models."
  {:revision-key :location/revision
   :created-at-key :location/created-at
   :updated-at-key :location/updated-at})

(def location-context-query
  "Organization Graph contract required by these User workflows.

   Given :organization/id and :location/id, Organization must provide:

   - whether the location exists and is active;
   - the current persisted location document;
   - its owning organization;
   - the trusted scopes that apply at the location.

   The applicable scope collection normally contains the location, every
   containing organization group, and the organization."
  [:location/found?
   {[:? :location/doc] [:*]}
   [:? :location/active?]
   [:? :location/organization-id]
   {[:? :location/applicable-scopes]
    [:scope/type :scope/id]}])

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
;; Errors
;; =============================================================================

(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info message
             {:error/type error-type
              :error/details details}))))

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
      (.digest (.getBytes ^String token java.nio.charset.StandardCharsets/UTF_8))
      encode-bytes))

;; =============================================================================
;; Biff + Gesso XTDB2 transaction effect
;; =============================================================================

(defn- deref-if-needed
  [value]
  (if (instance? clojure.lang.IDeref value)
    @value
    value))

(defn- malli-opts!
  [ctx]
  (or (some-> (:biff/malli-opts ctx) deref-if-needed)
      (fail! :user.fx/missing-malli-options
             "User FX requires Biff Malli options in :biff/malli-opts.")))

(defn- prepare-xtdb2-transaction
  [ctx tx]
  (biffx/validate-tx tx (malli-opts! ctx))
  (mapv biffx/format-query tx))

(defn- handle-commit
  [ctx tx]
  (let [result (live.xtdb/execute-tx-from!
                ctx
                (prepare-xtdb2-transaction ctx tx))]
    (when-let [poll-now (:biff.xtdb.listener/poll-now ctx)]
      (poll-now))
    result))

(def handlers
  {commit-effect handle-commit})

;; =============================================================================
;; XTDB2 transaction assertions and writes
;; =============================================================================

(defn- table-symbol
  [entity-type]
  (symbol (name entity-type)))

(defn- count-query
  [entity-type where]
  {:select [[[:count '*]]]
   :from (table-symbol entity-type)
   :where where})

(defn- assert-count
  [operator expected entity-type where]
  {:assert [operator expected (count-query entity-type where)]})

(defn- assert-none
  [entity-type where]
  (assert-count := 0 entity-type where))

(defn- assert-at-most-one
  [entity-type where]
  (assert-count :>= 1 entity-type where))

(defn- assert-id-absent
  [entity-type id]
  (assert-none entity-type [:= :xt/id id]))

(defn- assert-current-document
  [entity-type version document]
  (let [{:keys [revision-key updated-at-key]} version]
    (assert-count
     :=
     1
     entity-type
     [:and
      [:= :xt/id (:xt/id document)]
      [:= revision-key (get document revision-key)]
      [:= updated-at-key (get document updated-at-key)]])))

(defn- command-document
  [command]
  (model.common/command-document command))

(defn- put-command
  [entity-type command]
  [:put-docs entity-type (command-document command)])

(defn- current-membership-unique
  [membership-document]
  (assert-at-most-one
   membership/entity-type
   [:and
    [:= :membership/user (:membership/user membership-document)]
    [:= :membership/organization
     (:membership/organization membership-document)]
    [:<> :membership/status :revoked]]))

(defn- active-role-unique
  [role-assignment]
  (assert-at-most-one
   role/entity-type
   [:and
    [:= :role-assignment/membership
     (:role-assignment/membership role-assignment)]
    [:= :role-assignment/role
     (:role-assignment/role role-assignment)]
    [:= :role-assignment/scope-type
     (:role-assignment/scope-type role-assignment)]
    [:= :role-assignment/scope-id
     (:role-assignment/scope-id role-assignment)]
    [:= :role-assignment/status :active]]))

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

(defn- require-location-context!
  [facts organization-id location-id]
  (let [location (require-found!
                  facts
                  :location/found?
                  :location/doc
                  :location/not-found
                  "The location no longer exists.")
        scopes (vec (:location/applicable-scopes facts))
        expected-location-scope (location-scope location-id)
        expected-organization-scope (organization-scope organization-id)]
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
    (when-not (and (nat-int? (:location/revision location))
                   (model.common/timestamp-value?
                    (:location/updated-at location)))
      (fail! :location/inconsistent-facts
             "The location document lacks valid revision metadata."
             {:location/id location-id
              :revision (:location/revision location)
              :updated-at (:location/updated-at location)}))
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
     :scopes scopes}))

;; =============================================================================
;; Access facts and commit guards
;; =============================================================================

(defn- current-membership-node
  [facts]
  (:user/current-membership facts))

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
  (when-not (:user/admin? facts)
    (fail! :user/not-authorized
           "Administrator authority at this location is required."
           {:organization/id organization-id
            :location/id location-id}))
  (let [user (:user/doc facts)
        membership (current-membership-document facts)
        assignments (current-role-documents facts)
        admin-assignment
        (some (fn [assignment]
                (when (and (role/grants-role? assignment :admin)
                           (access/effective-assignment?
                            membership
                            assignment
                            scopes))
                  assignment))
              assignments)]
    (when-not (and user membership admin-assignment)
      (fail! :user.fx/incomplete-access-proof
             "The access result did not include the documents proving administrator authority."
             {:organization/id organization-id
              :location/id location-id}))
    {:user user
     :membership membership
     :role-assignment admin-assignment}))

(defn- access-guard-assertions
  [{:keys [user membership role-assignment]}]
  [(assert-current-document identity/entity-type identity/version user)
   (assert-current-document membership/entity-type
                            membership/version
                            membership)
   (assert-current-document role/entity-type role/version role-assignment)])

;; =============================================================================
;; Invitation recipient ownership
;; =============================================================================

(defn- verified-recipient?
  [invitation user]
  (case (invitation/recipient-type invitation)
    :phone
    (and (identity/phone-verified? user)
         (= (:invitation/phone invitation)
            (:user/phone user)))

    :email
    (and (identity/email-verified? user)
         (= (:invitation/email invitation)
            (:user/email user)))

    false))

(defn- require-acceptable-invitation!
  [invitation user now]
  (when-not (identity/active? user)
    (fail! :user/not-active
           "Only an active user can accept a staff invitation."
           {:user/id (:xt/id user)}))
  (when-not (and (invitation/offers-role? invitation :helper)
                 (= :location
                    (:scope/type (invitation/scope invitation))))
    (fail! :invitation/not-helper-location-offer
           "This operation accepts only location-scoped helper invitations."
           {:invitation/id (:xt/id invitation)
            :role (invitation/offered-role invitation)
            :scope (invitation/scope invitation)}))
  (when-not (invitation/usable-at? invitation now)
    (fail! (if (invitation/past-expiration? invitation now)
             :invitation/expired
             :invitation/not-pending)
           "The invitation can no longer be accepted."
           {:invitation/id (:xt/id invitation)}))
  (when-not (verified-recipient? invitation user)
    (fail! :invitation/recipient-mismatch
           "The invitation does not match a verified contact on the signed-in user."
           {:invitation/id (:xt/id invitation)
            :user/id (:xt/id user)
            :recipient-type (invitation/recipient-type invitation)})))

;; =============================================================================
;; Invite helper to location
;; =============================================================================

(def ^:private invite-helper-machine
  (fx/machine
   ::invite-helper-to-location

   :start
   (fn [ctx]
     (let [now (:biff.fx/now ctx)
           seed (:biff.fx/seed ctx)
           [invitation-id _] (fx/uuid7 seed now)
           input (:user.fx/input ctx)
           organization-id (:organization-id input)
           location-id (:location-id input)
           raw-token (generate-token)
           token-hash (hash-token raw-token)
           invitation-input
           (invitation/normalize-create-input
            {:id invitation-id
             :organization-id organization-id
             :invited-by (require-authenticated-user-id! ctx)
             :phone (:phone input)
             :email (:email input)
             :role :helper
             :scope (location-scope location-id)
             :token-hash token-hash
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
        :user.fx/token-facts
        [:biff.graph.fx/query
         (user.graph/invitation-query-input {:token-hash token-hash})
         user.graph/invitation-command-query]
        :biff.fx/next :authorize}))

   :authorize
   (fn [ctx]
     (let [input (:user.fx/input ctx)
           invitation-input (:user.fx/invitation-input ctx)
           organization-id (:organization-id input)
           location-id (:location-id input)
           {:keys [location scopes]}
           (require-location-context!
            (:user.fx/location-facts ctx)
            organization-id
            location-id)]
       (when (:invitation/found? (:user.fx/token-facts ctx))
         (fail! :invitation/token-collision
                "The generated invitation token hash already exists."))
       {:user.fx/raw-token (:user.fx/raw-token ctx)
        :user.fx/invitation-input invitation-input
        :user.fx/location location
        :user.fx/scopes scopes
        :user.fx/access-facts
        [:biff.graph.fx/query
         (user.graph/access-query-input
          {:user-id (require-authenticated-user-id! ctx)
           :organization-id organization-id
           :applicable-scopes scopes})
         user.graph/access-query]
        :biff.fx/next :build}))

   :build
   (fn [ctx]
     (let [input (:user.fx/input ctx)
           organization-id (:organization-id input)
           location-id (:location-id input)
           access-proof
           (require-location-admin!
            (:user.fx/access-facts ctx)
            (:user.fx/scopes ctx)
            organization-id
            location-id)
           command
           (invitation/create-command
            (:user.fx/invitation-input ctx))
           invitation-document (command-document command)
           tx
           (vec
            (concat
             [(assert-current-document
               :location
               location-version
               (:user.fx/location ctx))]
             (access-guard-assertions access-proof)
             [(assert-id-absent invitation/entity-type
                                (:xt/id invitation-document))
              (put-command invitation/entity-type command)
              (biffx/assert-unique
               invitation/entity-type
               {:invitation/token-hash
                (:invitation/token-hash invitation-document)})]))]
       {:user.fx/result
        {:invitation invitation-document
         :token (:user.fx/raw-token ctx)}
        :user.fx/tx [commit-effect tx]
        :biff.fx/next :finish}))

   :finish
   (fn [ctx]
     {:biff.fx/return
      (assoc (:user.fx/result ctx)
             :tx (:user.fx/tx ctx))})))

(defn invite-helper-to-location
  "Creates one pending helper invitation for an active location.

   Input:

     {:organization-id UUID
      :location-id UUID
      :phone canonical-E.164} ; exactly one of phone/email

   or:

     {:organization-id UUID
      :location-id UUID
      :email canonical-email}

   Returns the persisted invitation and the raw token exactly once. Delivery is
   owned by the application/notification layer."
  [ctx input]
  (invite-helper-machine
   (assoc ctx :user.fx/input input)))

;; =============================================================================
;; Accept helper invitation
;; =============================================================================

(defn- matching-helper-assignment
  [assignments membership-id invitation]
  (some (fn [assignment]
          (when (role/grants?
                 assignment
                 membership-id
                 :helper
                 (invitation/scope invitation))
            assignment))
        assignments))

(def ^:private accept-invitation-machine
  (fx/machine
   ::accept-invitation

   :start
   (fn [ctx]
     (let [now (:biff.fx/now ctx)
           seed (:biff.fx/seed ctx)
           [membership-id seed] (fx/uuid7 seed now)
           [role-assignment-id _] (fx/uuid7 seed now)
           input (:user.fx/input ctx)
           token-hash (hash-token (:token input))]
       {:user.fx/token-hash token-hash
        :user.fx/generated-membership-id membership-id
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
           organization-id (invitation/organization-id invitation-document)
           location-id (:scope/id (invitation/scope invitation-document))]
       (require-acceptable-invitation!
        invitation-document
        user
        (:biff.fx/now ctx))
       {:user.fx/invitation invitation-document
        :user.fx/user user
        :user.fx/generated-membership-id
        (:user.fx/generated-membership-id ctx)
        :user.fx/generated-role-assignment-id
        (:user.fx/generated-role-assignment-id ctx)
        :user.fx/location-facts
        [:biff.graph.fx/query
         (location-query-input organization-id location-id)
         location-context-query]
        :user.fx/membership-facts
        [:biff.graph.fx/query
         {:user/id (:xt/id user)
          :membership/organization-id organization-id}
         membership-with-roles-query]
        :biff.fx/next :build}))

   :build
   (fn [ctx]
     (let [now (:biff.fx/now ctx)
           invitation-document (:user.fx/invitation ctx)
           user (:user.fx/user ctx)
           organization-id (invitation/organization-id invitation-document)
           invitation-scope (invitation/scope invitation-document)
           location-id (:scope/id invitation-scope)
           {:keys [location]}
           (require-location-context!
            (:user.fx/location-facts ctx)
            organization-id
            location-id)
           membership-facts (:user.fx/membership-facts ctx)
           existing-membership
           (when (:user/current-membership-found? membership-facts)
             (current-membership-document membership-facts))
           _
           (when (and existing-membership
                      (membership/suspended? existing-membership))
             (fail! :membership/suspended
                    "A suspended membership must be reactivated before accepting this invitation."
                    {:membership/id (:xt/id existing-membership)}))
           _
           (when (and existing-membership
                      (not (membership/active? existing-membership)))
             (fail! :membership/not-active
                    "The existing organization membership is not active."
                    {:membership/id (:xt/id existing-membership)}))
           membership-command
           (when-not existing-membership
             (membership/create-command
              {:id (:user.fx/generated-membership-id ctx)
               :user-id (:xt/id user)
               :organization-id organization-id
               :now now}))
           membership-document
           (or existing-membership
               (command-document membership-command))
           existing-role-assignment
           (when existing-membership
             (matching-helper-assignment
              (current-role-documents membership-facts)
              (:xt/id membership-document)
              invitation-document))
           role-command
           (when-not existing-role-assignment
             (role/create-command
              {:id (:user.fx/generated-role-assignment-id ctx)
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
           preconditions
           (vec
            (concat
             [(assert-current-document
               invitation/entity-type
               invitation/version
               invitation-document)
              (assert-current-document
               identity/entity-type
               identity/version
               user)
              (assert-current-document
               :location
               location-version
               location)]
             (when existing-membership
               [(assert-current-document
                 membership/entity-type
                 membership/version
                 existing-membership)])
             (when existing-role-assignment
               [(assert-current-document
                 role/entity-type
                 role/version
                 existing-role-assignment)])
             (when membership-command
               [(assert-id-absent
                 membership/entity-type
                 (:xt/id membership-document))])
             (when role-command
               [(assert-id-absent
                 role/entity-type
                 (:xt/id role-assignment))])))
           writes
           (vec
            (concat
             (when membership-command
               [(put-command membership/entity-type membership-command)])
             (when role-command
               [(put-command role/entity-type role-command)])
             [(put-command invitation/entity-type invitation-command)]))
           postconditions
           (vec
            (concat
             (when membership-command
               [(current-membership-unique membership-document)])
             (when role-command
               [(active-role-unique role-assignment)])))
           tx (into preconditions (concat writes postconditions))]
       {:user.fx/result
        {:user user
         :membership membership-document
         :role-assignment role-assignment
         :invitation (command-document invitation-command)}
        :user.fx/tx [commit-effect tx]
        :biff.fx/next :finish}))

   :finish
   (fn [ctx]
     {:biff.fx/return
      (assoc (:user.fx/result ctx)
             :tx (:user.fx/tx ctx))})))

(defn accept-invitation
  "Accepts a pending location-scoped helper invitation.

   Input:

     {:token raw-bearer-token}

   The signed-in user must own the invitation's verified phone or email. The
   transaction atomically creates or reuses the organization membership,
   creates or reuses the location helper assignment, and marks the invitation
   accepted."
  [ctx input]
  (accept-invitation-machine
   (assoc ctx :user.fx/input input)))

;; =============================================================================
;; Public operation contribution
;; =============================================================================

(def operations
  {:user/invite-helper-to-location #'invite-helper-to-location
   :user/accept-invitation #'accept-invitation})
