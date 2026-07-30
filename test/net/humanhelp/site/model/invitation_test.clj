(ns net.humanhelp.site.model.invitation-test
  "Coverage for the rewritten HumanHelp Invitation model.

   This suite exercises all five Invitation layers:

   - pure Invitation lifecycle and invariants;
   - persisted Malli schema and descriptor plumbing;
   - Invitation-specific Graph reads;
   - guarded dependencies and mutation planners;
   - retryable cross-model acceptance progression;
   - the stable Invitation core boundary.

   Cross-model behavior is isolated only at the public User, Organization, and
   Membership core seams. The tests do not depend on those models' internal
   domain/schema/graph/fx namespaces."
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.fx :as fx]
   [com.biffweb.xtdb :as biff.xtdb]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [net.humanhelp.site.model.invitation.core :as invitation]
   [net.humanhelp.site.model.invitation.domain :as domain]
   [net.humanhelp.site.model.invitation.fx :as invitation.fx]
   [net.humanhelp.site.model.invitation.graph :as invitation.graph]
   [net.humanhelp.site.model.invitation.schema :as invitation.schema]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixed identities
;; =============================================================================

(def organization-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def other-organization-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000002"))

(def location-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000001"))

(def other-location-id
  (UUID/fromString
   "20000000-0000-0000-0000-000000000002"))

(def invitation-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000001"))

(def other-invitation-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000002"))

(def generated-invitation-id
  (UUID/fromString
   "30000000-0000-7000-8000-000000000003"))

(def missing-invitation-id
  (UUID/fromString
   "30000000-0000-0000-0000-000000000099"))

(def user-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000001"))

(def other-user-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000002"))

(def admin-user-id
  (UUID/fromString
   "40000000-0000-0000-0000-000000000003"))

(def membership-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000001"))

(def other-membership-id
  (UUID/fromString
   "50000000-0000-0000-0000-000000000002"))

(def role-assignment-id
  (UUID/fromString
   "60000000-0000-0000-0000-000000000001"))

(def other-role-assignment-id
  (UUID/fromString
   "60000000-0000-0000-0000-000000000002"))

;; =============================================================================
;; Fixed values
;; =============================================================================

(def canonical-phone
  "+12065550123")

(def other-phone
  "+12065550124")

(def canonical-email
  "person@example.com")

(def other-email
  "other@example.com")

(def raw-token
  "correct-horse-battery-staple-invitation-token")

(def other-raw-token
  "some-other-invitation-token")

(def token-hash
  (invitation.fx/hash-token
   raw-token))

(def other-token-hash
  (invitation.fx/hash-token
   other-raw-token))

(def t-before
  (Instant/parse
   "2026-06-30T23:59:00Z"))

(def t0
  (Instant/parse
   "2026-07-01T00:00:00Z"))

(def t1
  (Instant/parse
   "2026-07-01T00:01:00Z"))

(def t2
  (Instant/parse
   "2026-07-01T00:02:00Z"))

(def t3
  (Instant/parse
   "2026-07-01T00:03:00Z"))

(def expires-at
  (Instant/parse
   "2026-07-08T00:00:00Z"))

(def after-expiration
  (Instant/parse
   "2026-07-08T00:00:01Z"))

(def location-scope
  (organization/location-scope
   location-id))

(def other-location-scope
  (organization/location-scope
   other-location-id))

(def organization-scope
  (organization/organization-scope
   organization-id))

(def location-context
  {:organization/id
   organization-id

   :scope/target
   location-scope

   :scope/applicable
   [location-scope
    organization-scope]

   :scope/operational?
   true})

;; =============================================================================
;; Generic helpers
;; =============================================================================

(defn- after
  [model-command]
  (command/after
   model-command))

(defn- before
  [model-command]
  (command/before
   model-command))

(defn- error-type
  [f]
  (try
    (f)
    ::did-not-throw
    (catch Throwable error
      (loop [error error]
        (when error
          (or
           (:error/type
            (ex-data error))
           (recur
            (ex-cause error))))))))

(defn- plan-fragment
  [plan]
  (:transaction-fragment
   plan))

(defn- plan-command
  [plan]
  (first
   (:commands
    (plan-fragment
     plan))))

(defn- plan-change
  [plan]
  (first
   (:changes
    (plan-fragment
     plan))))

(defn- plan-assertions
  [plan]
  (:assertions
   (plan-fragment
    plan)))

(defn- guard-targets
  [fragment]
  (mapv
   command/guard-target
   (:guards
    fragment)))

;; =============================================================================
;; Foreign-model fixture documents
;; =============================================================================

(defn- active-user
  ([]
   (active-user
    {}))

  ([overrides]
   (merge
    {:xt/id
     user-id

     :user/status
     :active

     :user/email
     canonical-email

     :user/email-verified-at
     t0

     :user/revision
     0

     :user/created-at
     t0

     :user/updated-at
     t0}

    overrides)))

(defn- active-phone-user
  []
  {:xt/id
   user-id

   :user/status
   :active

   :user/phone
   canonical-phone

   :user/phone-verified-at
   t0

   :user/revision
   0

   :user/created-at
   t0

   :user/updated-at
   t0})

(defn- active-membership
  ([]
   (active-membership
    {}))

  ([overrides]
   (merge
    {:xt/id
     membership-id

     :membership/user
     user-id

     :membership/organization
     organization-id

     :membership/skills
     #{}

     :membership/status
     :active

     :membership/revision
     0

     :membership/created-at
     t0

     :membership/updated-at
     t0}

    overrides)))

(defn- active-role-assignment
  ([]
   (active-role-assignment
    {}))

  ([overrides]
   (merge
    {:xt/id
     role-assignment-id

     :role-assignment/membership
     membership-id

     :role-assignment/role
     :helper

     :role-assignment/scope-type
     :location

     :role-assignment/scope-id
     location-id

     :role-assignment/status
     :active

     :role-assignment/revision
     0

     :role-assignment/created-at
     t0

     :role-assignment/updated-at
     t0}

    overrides)))

;; =============================================================================
;; Invitation fixture construction
;; =============================================================================

(defn- invitation-command
  ([]
   (invitation-command
    {}))

  ([overrides]
   (domain/create-invitation-command
    (merge
     {:id
      invitation-id

      :organization-id
      organization-id

      :invited-by
      admin-user-id

      :email
      canonical-email

      :role
      :helper

      :scope
      location-scope

      :token-hash
      token-hash

      :now
      t0

      :expires-at
      expires-at}

     overrides))))

(defn- pending-invitation
  ([]
   (pending-invitation
    {}))

  ([overrides]
   (after
    (invitation-command
     overrides))))

(defn- accepted-invitation
  []
  (after
   (domain/accept-invitation-command
    (pending-invitation)
    {:user
     (active-user)

     :membership
     (active-membership)

     :role-assignment
     (active-role-assignment)

     :now
     t1})))

(defn- declined-invitation
  []
  (after
   (domain/decline-invitation-command
    (pending-invitation)
    {:actor-id
     user-id

     :now
     t1})))

(defn- revoked-invitation
  []
  (after
   (domain/revoke-invitation-command
    (pending-invitation)
    {:actor-id
     admin-user-id

     :reason
     :test/revoked

     :now
     t1})))

(defn- expired-invitation
  []
  (after
   (domain/expire-invitation-command
    (pending-invitation)
    {:now
     after-expiration})))

;; =============================================================================
;; Token values
;; =============================================================================

(deftest token-hash-test
  (testing "hashing is deterministic and suitable for persisted token values"
    (is
     (=
      token-hash
      (invitation.fx/hash-token
       raw-token)))

    (is
     (not=
      token-hash
      (invitation.fx/hash-token
       other-raw-token)))

    (is
     (domain/token-hash?
      token-hash))

    (is
     (not=
      raw-token
      token-hash)))

  (testing "bad bearer tokens are rejected"
    (is
     (=
      :invitation/invalid-token
      (error-type
       #(invitation.fx/hash-token
         nil))))

    (is
     (=
      :invitation/invalid-token
      (error-type
       #(invitation.fx/hash-token
         "   "))))))

;; =============================================================================
;; Creation and persisted invariants
;; =============================================================================

(deftest create-email-invitation-test
  (let [model-command
        (invitation-command
         {:email
          "  PERSON@EXAMPLE.COM  "})

        document
        (after
         model-command)]

    (is
     (command/create?
      model-command))

    (is
     (=
      :invitation
      (:model/entity-type
       model-command)))

    (is
     (=
      invitation-id
      (invitation/invitation-id
       document)))

    (is
     (=
      organization-id
      (invitation/organization-id
       document)))

    (is
     (=
      admin-user-id
      (invitation/invited-by-id
       document)))

    (is
     (=
      :helper
      (invitation/offered-role
       document)))

    (is
     (=
      location-scope
      (invitation/scope
       document)))

    (is
     (=
      :email
      (invitation/recipient-type
       document)))

    (is
     (=
      canonical-email
      (invitation/recipient-value
       document)))

    (is
     (invitation/pending?
      document))

    (is
     (=
      0
      (:invitation/revision
       document)))

    (is
     (=
      t0
      (:invitation/created-at
       document)))

    (is
     (=
      t0
      (:invitation/updated-at
       document)))

    (is
     (domain/document-consistent?
      document))

    (is
     (m/validate
      invitation.schema/invitation-document-schema
      document))))

(deftest create-phone-invitation-test
  (let [document
        (pending-invitation
         {:email
          nil

          :phone
          "  +12065550123  "})]

    (is
     (=
      :phone
      (invitation/recipient-type
       document)))

    (is
     (=
      canonical-phone
      (invitation/recipient-value
       document)))

    (is
     (domain/document-consistent?
      document))

    (is
     (m/validate
      invitation.schema/invitation-document-schema
      document))))

(deftest create-validation-test
  (doseq [input
          [{:email nil
            :phone nil}

           {:email canonical-email
            :phone canonical-phone}

           {:email "not-an-email"}

           {:email nil
            :phone "2065550123"}

           {:role :owner}

           {:scope {:scope/type :planet
                    :scope/id location-id}}

           {:token-hash "short"}

           {:expires-at t0}

           {:expires-at t-before}]]

    (is
     (=
      :invitation/invalid-create-input
      (error-type
       #(invitation-command
         input)))))

  (testing "organization-wide Invitation must target its recorded Organization"
    (is
     (=
      :invitation/invalid-create-input
      (error-type
       #(invitation-command
         {:scope
          (organization/organization-scope
           other-organization-id)}))))))

(deftest closed-schema-test
  (let [document
        (pending-invitation)]

    (is
     (m/validate
      invitation.schema/invitation-document-schema
      document))

    (is
     (false?
      (m/validate
       invitation.schema/invitation-document-schema
       (assoc
        document
        :unexpected/value
        true))))))

;; =============================================================================
;; Recipient semantics
;; =============================================================================

(deftest recipient-value-match-test
  (let [email-invitation
        (pending-invitation)

        phone-invitation
        (pending-invitation
         {:email nil
          :phone canonical-phone})]

    (is
     (invitation/addressed-to?
      email-invitation
      {:email
       " PERSON@EXAMPLE.COM "}))

    (is
     (false?
      (invitation/addressed-to?
       email-invitation
       {:email other-email})))

    (is
     (invitation/addressed-to?
      phone-invitation
      {:phone
       " +12065550123 "}))

    (is
     (false?
      (invitation/addressed-to?
       phone-invitation
       {:phone other-phone})))))

(deftest recipient-user-verification-test
  (let [email-invitation
        (pending-invitation)

        phone-invitation
        (pending-invitation
         {:email nil
          :phone canonical-phone})]

    (testing "verified matching contacts own the Invitation"
      (is
       (invitation/addressed-to-user?
        email-invitation
        (active-user)))

      (is
       (invitation/addressed-to-user?
        phone-invitation
        (active-phone-user))))

    (testing "matching but unverified contact is insufficient"
      (is
       (false?
        (invitation/addressed-to-user?
         email-invitation
         (dissoc
          (active-user)
          :user/email-verified-at))))

      (is
       (false?
        (invitation/addressed-to-user?
         phone-invitation
         (dissoc
          (active-phone-user)
          :user/phone-verified-at)))))

    (testing "verified wrong contact is insufficient"
      (is
       (false?
        (invitation/addressed-to-user?
         email-invitation
         (active-user
          {:user/email
           other-email})))))))

;; =============================================================================
;; Expiration semantics
;; =============================================================================

(deftest expiration-boundary-test
  (let [document
        (pending-invitation)]

    (is
     (invitation/usable-at?
      document
      t0))

    (is
     (invitation/usable-at?
      document
      t1))

    (is
     (false?
      (invitation/past-expiration?
       document
       t1)))

    (testing "expires-at itself is no longer usable"
      (is
       (invitation/past-expiration?
        document
        expires-at))

      (is
       (false?
        (invitation/usable-at?
         document
         expires-at))))

    (is
     (invitation/past-expiration?
      document
      after-expiration))))

;; =============================================================================
;; Acceptance domain rules
;; =============================================================================

(deftest accept-invitation-command-test
  (let [original
        (pending-invitation)

        user-document
        (active-user)

        membership-document
        (active-membership)

        role-assignment
        (active-role-assignment)

        model-command
        (domain/accept-invitation-command
         original
         {:user
          user-document

          :membership
          membership-document

          :role-assignment
          role-assignment

          :now
          t1})

        accepted
        (after
         model-command)]

    (is
     (command/update?
      model-command))

    (is
     (=
      :accept
      (command/operation
       model-command)))

    (is
     (=
      original
      (before
       model-command)))

    (is
     (invitation/accepted?
      accepted))

    (is
     (invitation/terminal?
      accepted))

    (is
     (=
      user-id
      (invitation/accepted-by-id
       accepted)))

    (is
     (=
      membership-id
      (invitation/accepted-membership-id
       accepted)))

    (is
     (=
      role-assignment-id
      (invitation/accepted-role-assignment-id
       accepted)))

    (is
     (=
      t1
      (:invitation/accepted-at
       accepted)))

    (is
     (=
      1
      (:invitation/revision
       accepted)))

    (is
     (=
      t1
      (:invitation/updated-at
       accepted)))

    (is
     (domain/document-consistent?
      accepted))

    (is
     (m/validate
      invitation.schema/invitation-document-schema
      accepted))))

(deftest acceptance-requires-correct-user-test
  (let [document
        (pending-invitation)

        membership-document
        (active-membership)

        role-assignment
        (active-role-assignment)]

    (testing "User must be active"
      (is
       (=
        :invitation/user-not-active
        (error-type
         #(domain/accept-invitation-command
           document
           {:user
            (assoc
             (active-user)
             :user/status
             :suspended)

            :membership
            membership-document

            :role-assignment
            role-assignment

            :now
            t1})))))

    (testing "Invitation must belong to a verified User contact"
      (is
       (=
        :invitation/recipient-mismatch
        (error-type
         #(domain/accept-invitation-command
           document
           {:user
            (active-user
             {:user/email
              other-email})

            :membership
            membership-document

            :role-assignment
            role-assignment

            :now
            t1})))))

    (testing "unverified matching contact is rejected"
      (is
       (=
        :invitation/recipient-mismatch
        (error-type
         #(domain/accept-invitation-command
           document
           {:user
            (dissoc
             (active-user)
             :user/email-verified-at)

            :membership
            membership-document

            :role-assignment
            role-assignment

            :now
            t1})))))))

(deftest acceptance-requires-correct-membership-test
  (let [document
        (pending-invitation)

        user-document
        (active-user)

        role-assignment
        (active-role-assignment)]

    (doseq [bad-membership
            [(assoc
              (active-membership)
              :membership/status
              :suspended)

             (assoc
              (active-membership)
              :membership/user
              other-user-id)

             (assoc
              (active-membership)
              :membership/organization
              other-organization-id)]]

      (is
       (=
        :invitation/membership-mismatch
        (error-type
         #(domain/accept-invitation-command
           document
           {:user
            user-document

            :membership
            bad-membership

            :role-assignment
            role-assignment

            :now
            t1})))))))

(deftest acceptance-requires-exact-role-assignment-test
  (let [document
        (pending-invitation)

        user-document
        (active-user)

        membership-document
        (active-membership)]

    (doseq [bad-assignment
            [(assoc
              (active-role-assignment)
              :role-assignment/status
              :revoked)

             (assoc
              (active-role-assignment)
              :role-assignment/membership
              other-membership-id)

             (assoc
              (active-role-assignment)
              :role-assignment/role
              :admin)

             (assoc
              (active-role-assignment)
              :role-assignment/scope-id
              other-location-id)]]

      (is
       (=
        :invitation/role-assignment-mismatch
        (error-type
         #(domain/accept-invitation-command
           document
           {:user
            user-document

            :membership
            membership-document

            :role-assignment
            bad-assignment

            :now
            t1})))))))

(deftest acceptance-rejects-expired-and-terminal-test
  (let [input
        {:user
         (active-user)

         :membership
         (active-membership)

         :role-assignment
         (active-role-assignment)

         :now
         after-expiration}]

    (is
     (=
      :invitation/expired
      (error-type
       #(domain/accept-invitation-command
         (pending-invitation)
         input))))

    (is
     (=
      :invitation/accepted
      (error-type
       #(domain/accept-invitation-command
         (accepted-invitation)
         (assoc
          input
          :now
          t2)))))))

;; =============================================================================
;; Other lifecycle transitions
;; =============================================================================

(deftest decline-invitation-command-test
  (let [model-command
        (domain/decline-invitation-command
         (pending-invitation)
         {:actor-id
          user-id

          :now
          t1})

        document
        (after
         model-command)]

    (is
     (=
      :decline
      (command/operation
       model-command)))

    (is
     (invitation/declined?
      document))

    (is
     (invitation/terminal?
      document))

    (is
     (=
      user-id
      (:invitation/declined-by
       document)))

    (is
     (=
      t1
      (:invitation/declined-at
       document)))

    (is
     (domain/document-consistent?
      document))))

(deftest revoke-invitation-command-test
  (let [model-command
        (domain/revoke-invitation-command
         (pending-invitation)
         {:actor-id
          admin-user-id

          :reason
          :test/revoked

          :now
          t1})

        document
        (after
         model-command)]

    (is
     (=
      :revoke
      (command/operation
       model-command)))

    (is
     (invitation/revoked?
      document))

    (is
     (=
      admin-user-id
      (:invitation/revoked-by
       document)))

    (is
     (=
      :test/revoked
      (:invitation/revocation-reason
       document)))

    (is
     (domain/document-consistent?
      document))))

(deftest expire-invitation-command-test
  (let [model-command
        (domain/expire-invitation-command
         (pending-invitation)
         {:now
          after-expiration})

        document
        (after
         model-command)]

    (is
     (=
      :expire
      (command/operation
       model-command)))

    (is
     (invitation/expired?
      document))

    (is
     (=
      after-expiration
      (:invitation/expired-at
       document)))

    (is
     (=
      after-expiration
      (:invitation/updated-at
       document)))

    (is
     (domain/document-consistent?
      document)))

  (is
   (=
    :invitation/not-expired
    (error-type
     #(domain/expire-invitation-command
       (pending-invitation)
       {:now
        t1})))))

(deftest terminal-invitations-are-immutable-test
  (doseq [document
          [(accepted-invitation)
           (declined-invitation)
           (revoked-invitation)
           (expired-invitation)]]

    (is
     (invitation/terminal?
      document))

    (is
     (not
      (domain/can-transition?
       document
       :accept)))

    (is
     (not
      (domain/can-transition?
       document
       :decline)))

    (is
     (not
      (domain/can-transition?
       document
       :revoke)))

    (is
     (not
      (domain/can-transition?
       document
       :expire)))))

(deftest update-time-test
  (is
   (=
    :invitation/invalid-time
    (error-type
     #(domain/decline-invitation-command
       (pending-invitation)
       {:actor-id
        user-id

        :now
        t-before})))))

;; =============================================================================
;; Descriptor and module
;; =============================================================================

(deftest descriptor-test
  (is
   (model/descriptor?
    invitation.schema/invitation-descriptor))

  (is
   (=
    :invitation
    (:entity-type
     invitation.schema/invitation-descriptor)))

  (is
   (=
    {:graph-key
     :invitation/id}
    (:identity
     invitation.schema/invitation-descriptor)))

  (is
   (=
    domain/version
    (:version
     invitation.schema/invitation-descriptor)))

  (testing "Invitation deliberately generates no equality lookup resolver"
    (is
     (nil?
      (:lookups
       invitation.schema/invitation-descriptor))))

  (testing "token hash is not in the public generated field projection"
    (let [query
          (model/field-query
           invitation.schema/invitation-descriptor)

          values
          (set
           (filter
            keyword?
            (tree-seq
             coll?
             seq
             query)))]

      (is
       (not
        (contains?
         values
         :invitation/token-hash)))))

  (testing "module contains only generated Invitation resolvers"
    (let [expected
          #{(model/by-id-resolver-id
             invitation.schema/invitation-descriptor)

            (model/fields-resolver-id
             invitation.schema/invitation-descriptor)}

          actual
          (set
           (map
            :biff.graph/id
            invitation/resolvers))]

      (is
       (=
        expected
        actual))

      (is
       (=
        2
        (count
         invitation/resolvers))))))

;; =============================================================================
;; Stable core facts
;; =============================================================================

(deftest core-document-facts-test
  (let [document
        (pending-invitation)]

    (is
     (=
      invitation-id
      (invitation/invitation-id
       document)))

    (is
     (=
      organization-id
      (invitation/organization-id
       document)))

    (is
     (=
      admin-user-id
      (invitation/invited-by-id
       document)))

    (is
     (=
      :helper
      (invitation/offered-role
       document)))

    (is
     (=
      location-scope
      (invitation/scope
       document)))

    (is
     (=
      :pending
      (invitation/status
       document)))

    (is
     (invitation/for-organization?
      document
      organization-id))

    (is
     (invitation/invited-by?
      document
      admin-user-id))

    (is
     (invitation/offers-role?
      document
      :helper))

    (is
     (invitation/at-scope?
      document
      location-scope))

    (is
     (false?
      (invitation/at-scope?
       document
       other-location-scope)))))

;; =============================================================================
;; Core by-ID reads
;; =============================================================================

(deftest core-read-test
  (let [document
        (pending-invitation)]

    (with-redefs
     [model/load-by-id
      (fn [descriptor _ctx id]
        (when
         (and
          (=
           :invitation
           (:entity-type
            descriptor))

          (=
           invitation-id
           id))
          document))]

      (is
       (=
        document
        (invitation/invitation
         {}
         invitation-id)))

      (is
       (=
        document
        (invitation/require-invitation
         {}
         invitation-id)))

      (is
       (nil?
        (invitation/invitation
         {}
         missing-invitation-id)))

      (is
       (=
        :invitation/not-found
        (error-type
         #(invitation/require-invitation
           {}
           missing-invitation-id))))

      (is
       (=
        :invitation.core/invalid-invitation-id
        (error-type
         #(invitation/invitation
           {}
           "bad-id")))))))

;; =============================================================================
;; In-memory Graph query seam
;; =============================================================================

(defn- predicate-match?
  [document predicate]
  (let [[op
         & args]
        predicate]

    (case
     op

      :=
      (let [[key value]
            args]
        (=
         value
         (get
          document
          key)))

      :and
      (every?
       #(predicate-match?
         document
         %)
       args)

      (throw
       (ex-info
        "Unsupported test query predicate."
        {:predicate
         predicate})))))

(defn- with-graph-documents*
  [documents f]
  (let [documents
        (vec
         documents)]

    (with-redefs
     [biff.xtdb/q
      (fn [_ctx query]
        (let [entity-type
              (first
               (:from
                query))

              where
              (:where
               query)]
          (->>
           documents

           (filter
            (fn [document]
              (and
               (=
                entity-type
                domain/entity-type)

               (or
                (nil?
                 where)

                (predicate-match?
                 document
                 where)))))

           vec)))]

      (f))))

(defmacro with-graph-documents
  [documents & body]
  `(with-graph-documents*
     ~documents
     (fn []
       ~@body)))

(def graph-ctx
  {:biff.xtdb/node
   ::test-node})

;; =============================================================================
;; Graph reads
;; =============================================================================

(deftest token-hash-graph-lookup-test
  (let [document
        (pending-invitation)]

    (with-graph-documents
      [document]

      (is
       (=
        document
        (invitation.graph/invitation-by-token-hash
         graph-ctx
         token-hash)))

      (is
       (nil?
        (invitation.graph/invitation-by-token-hash
         graph-ctx
         other-token-hash)))

      (is
       (nil?
        (invitation.graph/invitation-by-token-hash
         graph-ctx
         "short"))))))

(deftest duplicate-token-is-corruption-test
  (let [first-document
        (pending-invitation)

        second-document
        (pending-invitation
         {:id
          other-invitation-id})]

    (with-graph-documents
      [first-document
       second-document]

      (is
       (=
        :invitation.graph/non-unique-token-hash
        (error-type
         #(invitation.graph/invitation-by-token-hash
           graph-ctx
           token-hash)))))))

(deftest graph-collection-test
  (let [first-document
        (pending-invitation)

        second-document
        (pending-invitation
         {:id
          other-invitation-id

          :email
          other-email

          :now
          t1

          :expires-at
          (Instant/parse
           "2026-07-09T00:00:00Z")})

        declined
        (after
         (domain/decline-invitation-command
          second-document
          {:actor-id
           other-user-id

           :now
           t2}))]

    (with-graph-documents
      [declined
       first-document]

      (is
       (=
        [first-document
         declined]
        (invitation/invitations-for-organization
         graph-ctx
         organization-id)))

      (is
       (=
        [first-document]
        (invitation/pending-invitations-for-organization
         graph-ctx
         organization-id)))

      (is
       (=
        [first-document]
        (invitation/pending-invitations-for-email
         graph-ctx
         canonical-email)))

      (is
       (=
        [first-document]
        (invitation/pending-invitations-at-scope
         graph-ctx
         location-scope))))))

(deftest exact-pending-offer-query-test
  (let [document
        (pending-invitation)

        offer
        {:organization-id
         organization-id

         :email
         canonical-email

         :role
         :helper

         :scope
         location-scope}]

    (with-graph-documents
      [document]

      (is
       (=
        [document]
        (invitation/pending-invitations-for-offer
         graph-ctx
         offer)))

      (is
       (=
        document
        (invitation/pending-invitation-for-offer
         graph-ctx
         offer)))

      (is
       (empty?
        (invitation/pending-invitations-for-offer
         graph-ctx
         (assoc
          offer
          :role
          :admin))))

      (is
       (empty?
        (invitation/pending-invitations-for-offer
         graph-ctx
         (assoc
          offer
          :scope
          other-location-scope)))))))

(deftest duplicate-pending-offer-is-corruption-test
  (let [first-document
        (pending-invitation)

        second-document
        (pending-invitation
         {:id
          other-invitation-id

          :token-hash
          other-token-hash})

        offer
        {:organization-id
         organization-id

         :email
         canonical-email

         :role
         :helper

         :scope
         location-scope}]

    (with-graph-documents
      [first-document
       second-document]

      (is
       (=
        :invitation.graph/non-unique-pending-offer
        (error-type
         #(invitation/pending-invitation-for-offer
           graph-ctx
           offer)))))))

;; =============================================================================
;; Guarded Invitation dependency
;; =============================================================================

(deftest invitation-dependency-test
  (let [document
        (pending-invitation)]

    (with-redefs
     [model/load-by-id
      (fn [_descriptor _ctx id]
        (when
         (=
          invitation-id
          id)
          document))]

      (let [{loaded :invitation
             fragment :transaction-fragment}
            (invitation/require-invitation-dependency
             {}
             invitation-id)]

        (is
         (=
          document
          loaded))

        (is
         (=
          [[:invitation
            invitation-id]]
          (guard-targets
           fragment))))

      (is
       (nil?
        (invitation/invitation-dependency
         {}
         missing-invitation-id)))

      (is
       (=
        :invitation/not-found
        (error-type
         #(invitation/require-invitation-dependency
           {}
           missing-invitation-id)))))))

(deftest token-dependency-test
  (let [document
        (pending-invitation)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (fn [_ctx supplied-hash]
        (when
         (=
          token-hash
          supplied-hash)
          document))]

      (let [{loaded :invitation
             fragment :transaction-fragment}
            (invitation/require-invitation-by-token-dependency
             {}
             raw-token)]

        (is
         (=
          document
          loaded))

        (is
         (=
          [[:invitation
            invitation-id]]
          (guard-targets
           fragment))))

      (is
       (nil?
        (invitation/invitation-by-token-dependency
         {}
         other-raw-token)))

      (is
       (=
        :invitation/not-found
        (error-type
         #(invitation/require-invitation-by-token-dependency
           {}
           other-raw-token)))))))

;; =============================================================================
;; Creation planning
;; =============================================================================

(deftest create-plan-test
  (let [admin-call
        (atom nil)]

    (with-redefs
     [fx/uuid7
      (fn [_seed _now]
        [generated-invitation-id])

      membership/require-admin-dependency
      (fn [ctx actor-id scope]
        (reset!
         admin-call
         [ctx
          actor-id
          scope])

        {:scope-context
         location-context

         :transaction-fragment
         model.tx/empty-fragment})

      invitation.graph/pending-invitation-for-offer
      (fn [_ctx _offer]
        nil)]

      (let [ctx
            {:biff.fx/seed
             7

             :biff.fx/now
             t1

             invitation.fx/token-generator-key
             (constantly
              raw-token)}

            plan
            (invitation/plan-create-invitation
             ctx
             {:organization-id
              organization-id

              :invited-by
              admin-user-id

              :email
              " PERSON@EXAMPLE.COM "

              :role
              :helper

              :scope
              location-scope

              :expires-at
              expires-at})

            model-command
            (plan-command
             plan)

            document
            (after
             model-command)]

        (is
         (=
          [ctx
           admin-user-id
           location-scope]
          @admin-call))

        (is
         (command/create?
          model-command))

        (is
         (=
          generated-invitation-id
          (:xt/id
           document)))

        (is
         (=
          canonical-email
          (:invitation/email
           document)))

        (is
         (=
          token-hash
          (:invitation/token-hash
           document)))

        (is
         (not=
          raw-token
          (:invitation/token-hash
           document)))

        (is
         (=
          raw-token
          (get-in
           plan
           [:result
            :token])))

        (is
         (=
          document
          (get-in
           plan
           [:result
            :invitation])))

        (is
         (=
          2
          (count
           (plan-assertions
            plan))))

        (is
         (=
          #{(model.tx/assert-none
             :invitation
             [:=
              :invitation/token-hash
              token-hash])

            (model.tx/assert-none
             :invitation
             (invitation.graph/pending-offer-predicate
              {:organization-id
               organization-id

               :email
               canonical-email

               :phone
               nil

               :role
               :helper

               :scope
               location-scope}))}
          (set
           (plan-assertions
            plan))))

        (is
         (=
          {:topic
           :invitation

           :id
           generated-invitation-id

           :change/kind
           :created

           :invitation/operation
           :create

           :invitation/id
           generated-invitation-id

           :invitation/organization-id
           organization-id

           :invitation/status
           :pending

           :invitation/role
           :helper

           :invitation/scope
           location-scope

           :invitation/revision
           0}
          (plan-change
           plan)))

        (is
         (=
          {:coalesce-key
           [:invitation
            generated-invitation-id]}
          ((:entry-fn
            (:transaction-options
             plan))
           (plan-change
            plan))))))))

(deftest create-plan-rejects-duplicate-offer-test
  (with-redefs
   [membership/require-admin-dependency
    (fn [_ctx _actor-id _scope]
      {:scope-context
       location-context

       :transaction-fragment
       model.tx/empty-fragment})

    invitation.graph/pending-invitation-for-offer
    (fn [_ctx _offer]
      (pending-invitation))]

    (is
     (=
      :invitation/pending-offer-exists
      (error-type
       #(invitation/plan-create-invitation
         {:biff.fx/seed
          1

          :biff.fx/now
          t1

          invitation.fx/token-generator-key
          (constantly
           raw-token)}

         {:organization-id
          organization-id

          :invited-by
          admin-user-id

          :email
          canonical-email

          :role
          :helper

          :scope
          location-scope}))))))

(deftest create-plan-rejects-expired-unmaterialized-duplicate-test
  (let [expired-pending
        (pending-invitation
         {:expires-at
          t1})]

    (with-redefs
     [membership/require-admin-dependency
      (fn [_ctx _actor-id _scope]
        {:scope-context
         location-context

         :transaction-fragment
         model.tx/empty-fragment})

      invitation.graph/pending-invitation-for-offer
      (fn [_ctx _offer]
        expired-pending)]

      (is
       (=
        :invitation/pending-offer-expired
        (error-type
         #(invitation/plan-create-invitation
           {:biff.fx/seed
            1

            :biff.fx/now
            t2

            invitation.fx/token-generator-key
            (constantly
             raw-token)}

           {:organization-id
            organization-id

            :invited-by
            admin-user-id

            :email
            canonical-email

            :role
            :helper

            :scope
            location-scope})))))))

;; =============================================================================
;; Decline planning
;; =============================================================================

(deftest decline-plan-test
  (let [document
        (pending-invitation)

        user-document
        (active-user)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (fn [_ctx supplied-hash]
        (when
         (=
          token-hash
          supplied-hash)
          document))

      user/require-user-dependency
      (fn [_ctx supplied-user-id]
        (is
         (=
          user-id
          supplied-user-id))

        {:user
         user-document

         :transaction-fragment
         model.tx/empty-fragment})]

      (let [plan
            (invitation/plan-decline-invitation
             {:biff.fx/now
              t1}
             {:token
              raw-token

              :user-id
              user-id})

            model-command
            (plan-command
             plan)

            changed
            (after
             model-command)]

        (is
         (=
          :decline
          (command/operation
           model-command)))

        (is
         (invitation/declined?
          changed))

        (is
         (=
          user-id
          (:invitation/declined-by
           changed)))

        (is
         (=
          [[:invitation
            invitation-id]]
          (guard-targets
           (plan-fragment
            plan))))))))

(deftest decline-plan-rejects-wrong-recipient-test
  (let [document
        (pending-invitation)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (fn [_ctx _hash]
        document)

      user/require-user-dependency
      (fn [_ctx _user-id]
        {:user
         (active-user
          {:user/email
           other-email})

         :transaction-fragment
         model.tx/empty-fragment})]

      (is
       (=
        :invitation/recipient-mismatch
        (error-type
         #(invitation/plan-decline-invitation
           {:biff.fx/now
            t1}
           {:token
            raw-token

            :user-id
            user-id})))))))

;; =============================================================================
;; Revoke planning
;; =============================================================================

(deftest revoke-plan-requires-admin-test
  (let [document
        (pending-invitation)

        admin-call
        (atom nil)]

    (with-redefs
     [model/load-by-id
      (fn [_descriptor _ctx id]
        (when
         (=
          invitation-id
          id)
          document))

      membership/require-admin-dependency
      (fn [ctx actor-id scope]
        (reset!
         admin-call
         [ctx
          actor-id
          scope])

        {:scope-context
         location-context

         :transaction-fragment
         model.tx/empty-fragment})]

      (let [ctx
            {:biff.fx/now
             t1}

            plan
            (invitation/plan-revoke-invitation
             ctx
             {:invitation-id
              invitation-id

              :actor-id
              admin-user-id

              :reason
              :test/revoked})

            changed
            (after
             (plan-command
              plan))]

        (is
         (=
          [ctx
           admin-user-id
           location-scope]
          @admin-call))

        (is
         (invitation/revoked?
          changed))

        (is
         (=
          admin-user-id
          (:invitation/revoked-by
           changed)))

        (is
         (=
          :test/revoked
          (:invitation/revocation-reason
           changed)))))))

;; =============================================================================
;; Expiration planning
;; =============================================================================

(deftest expiration-plan-test
  (let [document
        (pending-invitation)]

    (with-redefs
     [model/load-by-id
      (fn [_descriptor _ctx id]
        (when
         (=
          invitation-id
          id)
          document))]

      (let [plan
            (invitation/plan-expire-invitation
             {:biff.fx/now
              after-expiration}
             {:invitation-id
              invitation-id})

            model-command
            (plan-command
             plan)]

        (is
         (=
          :expire
          (command/operation
           model-command)))

        (is
         (invitation/expired?
          (after
           model-command)))

        (is
         (=
          [[:invitation
            invitation-id]]
          (guard-targets
           (plan-fragment
            plan))))))))

;; =============================================================================
;; Acceptance progression
;; =============================================================================

(defn- acceptance-user-dependency
  [_ctx supplied-user-id]
  (when-not
   (=
    user-id
    supplied-user-id)
    (throw
     (AssertionError.
      "Unexpected User ID.")))

  {:user
   (active-user)

   :transaction-fragment
   model.tx/empty-fragment})

(defn- acceptance-invitation-lookup
  [document]
  (fn [_ctx supplied-hash]
    (when
     (=
      token-hash
      supplied-hash)
      document)))

(deftest acceptance-step-creates-membership-first-test
  (let [document
        (pending-invitation)

        expected-plan
        {:result
         {:membership
          ::new-membership}}

        planner-input
        (atom nil)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)

      user/require-user-dependency
      acceptance-user-dependency

      membership/current-membership
      (fn [_ctx supplied-user-id supplied-organization-id]
        (is
         (=
          user-id
          supplied-user-id))

        (is
         (=
          organization-id
          supplied-organization-id))

        nil)

      membership/plan-create-membership
      (fn [ctx input]
        (reset!
         planner-input
         [ctx
          input])

        expected-plan)]

      (let [ctx
            {:biff.fx/now
             t1}

            result
            (invitation/next-acceptance-step
             ctx
             {:token
              raw-token

              :user-id
              user-id})]

        (is
         (=
          :create-membership
          (:step
           result)))

        (is
         (=
          document
          (:invitation
           result)))

        (is
         (=
          expected-plan
          (:plan
           result)))

        (is
         (=
          [ctx
           {:user-id
            user-id

            :organization-id
            organization-id

            :skills
            #{}}]
          @planner-input))))))

(deftest acceptance-step-creates-role-second-test
  (let [document
        (pending-invitation)

        membership-document
        (active-membership)

        expected-plan
        {:result
         {:role-assignment
          ::new-role-assignment}}

        planner-input
        (atom nil)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)

      user/require-user-dependency
      acceptance-user-dependency

      membership/current-membership
      (fn [_ctx _user-id _organization-id]
        membership-document)

      membership/active-role-assignments-for-membership-at-scope
      (fn [_ctx supplied-membership-id supplied-scope]
        (is
         (=
          membership-id
          supplied-membership-id))

        (is
         (=
          location-scope
          supplied-scope))

        [])

      membership/plan-create-role-assignment
      (fn [ctx input]
        (reset!
         planner-input
         [ctx
          input])

        expected-plan)]

      (let [ctx
            {:biff.fx/now
             t1}

            result
            (invitation/next-acceptance-step
             ctx
             {:token
              raw-token

              :user-id
              user-id})]

        (is
         (=
          :create-role-assignment
          (:step
           result)))

        (is
         (=
          membership-document
          (:membership
           result)))

        (is
         (=
          expected-plan
          (:plan
           result)))

        (is
         (=
          [ctx
           {:membership-id
            membership-id

            :role
            :helper

            :scope
            location-scope

            :actor-id
            user-id

            :reason
            :invitation/accepted}]
          @planner-input))))))

(deftest acceptance-step-accepts-after-role-exists-test
  (let [document
        (pending-invitation)

        membership-document
        (active-membership)

        role-assignment
        (active-role-assignment)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)

      user/require-user-dependency
      acceptance-user-dependency

      membership/current-membership
      (fn [_ctx _user-id _organization-id]
        membership-document)

      membership/current-membership-dependency
      (fn [_ctx _user-id _organization-id]
        {:membership
         membership-document

         :transaction-fragment
         model.tx/empty-fragment})

      membership/active-role-assignments-for-membership-at-scope
      (fn [_ctx _membership-id _scope]
        [role-assignment])]

      (let [result
            (invitation/next-acceptance-step
             {:biff.fx/now
              t1}
             {:token
              raw-token

              :user-id
              user-id})

            plan
            (:plan
             result)

            model-command
            (plan-command
             plan)

            accepted
            (after
             model-command)]

        (is
         (=
          :accept-invitation
          (:step
           result)))

        (is
         (=
          role-assignment
          (:role-assignment
           result)))

        (is
         (=
          :accept
          (command/operation
           model-command)))

        (is
         (invitation/accepted?
          accepted))

        (is
         (=
          membership-id
          (invitation/accepted-membership-id
           accepted)))

        (is
         (=
          role-assignment-id
          (invitation/accepted-role-assignment-id
           accepted)))))))

(deftest acceptance-step-is-complete-after-acceptance-test
  (let [document
        (accepted-invitation)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)]

      (let [result
            (invitation/next-acceptance-step
             {:biff.fx/now
              t2}
             {:token
              raw-token

              :user-id
              user-id})]

        (is
         (=
          :complete
          (:step
           result)))

        (is
         (=
          invitation-id
          (get-in
           result
           [:result
            :invitation-id])))

        (is
         (=
          user-id
          (get-in
           result
           [:result
            :user-id])))

        (is
         (=
          membership-id
          (get-in
           result
           [:result
            :membership-id])))

        (is
         (=
          role-assignment-id
          (get-in
           result
           [:result
            :role-assignment-id])))))))

(deftest acceptance-retry-does-not-recreate-existing-work-test
  (let [document
        (pending-invitation)

        membership-document
        (active-membership)

        role-assignment
        (active-role-assignment)

        create-membership-calls
        (atom 0)

        create-role-calls
        (atom 0)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)

      user/require-user-dependency
      acceptance-user-dependency

      membership/current-membership
      (fn [_ctx _user-id _organization-id]
        membership-document)

      membership/current-membership-dependency
      (fn [_ctx _user-id _organization-id]
        {:membership
         membership-document

         :transaction-fragment
         model.tx/empty-fragment})

      membership/active-role-assignments-for-membership-at-scope
      (fn [_ctx _membership-id _scope]
        [role-assignment])

      membership/plan-create-membership
      (fn [& _]
        (swap!
         create-membership-calls
         inc)

        ::unexpected-membership-plan)

      membership/plan-create-role-assignment
      (fn [& _]
        (swap!
         create-role-calls
         inc)

        ::unexpected-role-plan)]

      (is
       (=
        :accept-invitation
        (:step
         (invitation/next-acceptance-step
          {:biff.fx/now
           t1}
          {:token
           raw-token

           :user-id
           user-id}))))

      (is
       (zero?
        @create-membership-calls))

      (is
       (zero?
        @create-role-calls)))))

(deftest acceptance-rejects-suspended-existing-membership-test
  (let [document
        (pending-invitation)

        suspended-membership
        (assoc
         (active-membership)
         :membership/status
         :suspended)]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)

      user/require-user-dependency
      acceptance-user-dependency

      membership/current-membership
      (fn [_ctx _user-id _organization-id]
        suspended-membership)]

      (is
       (=
        :invitation/membership-not-active
        (error-type
         #(invitation/next-acceptance-step
           {:biff.fx/now
            t1}
           {:token
            raw-token

            :user-id
            user-id})))))))

(deftest acceptance-rejects-expired-invitation-test
  (let [document
        (pending-invitation
         {:expires-at
          t1})]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)]

      (is
       (=
        :invitation/expired
        (error-type
         #(invitation/next-acceptance-step
           {:biff.fx/now
            t2}
           {:token
            raw-token

            :user-id
            user-id})))))))

(deftest acceptance-rejects-other-users-completed-invitation-test
  (let [document
        (accepted-invitation)

        other-user
        other-user-id]

    (with-redefs
     [invitation.graph/invitation-by-token-hash
      (acceptance-invitation-lookup
       document)]

      (is
       (=
        :invitation/accepted
        (error-type
         #(invitation/next-acceptance-step
           {:biff.fx/now
            t2}
           {:token
            raw-token

            :user-id
            other-user})))))))

;; =============================================================================
;; Acceptance state
;; =============================================================================

(deftest acceptance-state-progress-test
  (let [document
        (pending-invitation)

        membership-document
        (active-membership)

        role-assignment
        (active-role-assignment)]

    (testing "recipient with no Membership"
      (with-redefs
       [invitation.graph/invitation-by-token-hash
        (acceptance-invitation-lookup
         document)

        user/user
        (fn [_ctx _user-id]
          (active-user))

        membership/current-membership
        (fn [_ctx _user-id _organization-id]
          nil)]

        (let [state
              (invitation/acceptance-state
               {:biff.fx/now
                t1}
               {:token
                raw-token

                :user-id
                user-id})]

          (is
           (:recipient?
            state))

          (is
           (nil?
            (:membership
             state)))

          (is
           (nil?
            (:role-assignment
             state)))

          (is
           (false?
            (:accepted?
             state)))

          (is
           (false?
            (:ready-to-accept?
             state))))))

    (testing "exact Membership and RoleAssignment make acceptance ready"
      (with-redefs
       [invitation.graph/invitation-by-token-hash
        (acceptance-invitation-lookup
         document)

        user/user
        (fn [_ctx _user-id]
          (active-user))

        membership/current-membership
        (fn [_ctx _user-id _organization-id]
          membership-document)

        membership/active-role-assignments-for-membership-at-scope
        (fn [_ctx _membership-id _scope]
          [role-assignment])]

        (let [state
              (invitation/acceptance-state
               {:biff.fx/now
                t1}
               {:token
                raw-token

                :user-id
                user-id})]

          (is
           (:recipient?
            state))

          (is
           (=
            membership-document
            (:membership
             state)))

          (is
           (=
            role-assignment
            (:role-assignment
             state)))

          (is
           (:ready-to-accept?
            state)))))))

;; =============================================================================
;; Planner context errors
;; =============================================================================

(deftest planner-context-errors-test
  (testing "now is required before planning begins"
    (is
     (=
      :invitation.fx/missing-now
      (error-type
       #(invitation/plan-create-invitation
         {:biff.fx/seed
          1}
         {:organization-id
          organization-id

          :invited-by
          admin-user-id

          :email
          canonical-email

          :role
          :helper

          :scope
          location-scope})))))

  (testing "seed is required when the Invitation ID is generated"
    (with-redefs
     [membership/require-admin-dependency
      (fn [_ctx _actor-id _scope]
        {:scope-context
         location-context

         :transaction-fragment
         model.tx/empty-fragment})

      invitation.graph/pending-invitation-for-offer
      (fn [_ctx _offer]
        nil)]

      (is
       (=
        :invitation.fx/missing-seed
        (error-type
         #(invitation/plan-create-invitation
           {:biff.fx/now
            t1

            invitation.fx/token-generator-key
            (constantly
             raw-token)}

           {:organization-id
            organization-id

            :invited-by
            admin-user-id

            :email
            canonical-email

            :role
            :helper

            :scope
            location-scope})))))))

;; =============================================================================
;; Core forwarding boundaries
;; =============================================================================

(deftest core-graph-forwarding-test
  (let [document
        (pending-invitation)]

    (with-redefs
     [invitation.graph/invitations-for-organization
      (fn [ctx id]
        [:organization
         ctx
         id])

      invitation.graph/pending-invitations-for-recipient
      (fn [ctx recipient]
        [:recipient
         ctx
         recipient])

      invitation.graph/invitations-at-scope
      (fn [ctx scope]
        [:scope
         ctx
         scope])]

      (is
       (=
        [:organization
         ::ctx
         organization-id]
        (invitation/invitations-for-organization
         ::ctx
         organization-id)))

      (is
       (=
        [:recipient
         ::ctx
         {:email canonical-email}]
        (invitation/pending-invitations-for-recipient
         ::ctx
         {:email
          canonical-email})))

      (is
       (=
        [:scope
         ::ctx
         location-scope]
        (invitation/invitations-at-scope
         ::ctx
         location-scope)))

      ;; Keep document referenced so accidental fixture corruption is visible.
      (is
       (domain/document-consistent?
        document)))))

;; =============================================================================
;; Lifecycle summary
;; =============================================================================

(deftest lifecycle-summary-test
  (let [pending
        (pending-invitation)

        accepted
        (accepted-invitation)

        declined
        (declined-invitation)

        revoked
        (revoked-invitation)

        expired
        (expired-invitation)]

    (is
     (invitation/pending?
      pending))

    (is
     (invitation/accepted?
      accepted))

    (is
     (invitation/declined?
      declined))

    (is
     (invitation/revoked?
      revoked))

    (is
     (invitation/expired?
      expired))

    (is
     (false?
      (invitation/terminal?
       pending)))

    (doseq [document
            [accepted
             declined
             revoked
             expired]]

      (is
       (invitation/terminal?
        document))

      (is
       (domain/document-consistent?
        document))

      (is
       (m/validate
        invitation.schema/invitation-document-schema
        document)))))
