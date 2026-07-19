(ns net.humanhelp.site.model.user-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.graph :as graph]
   [malli.core :as m]
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.fx :as model.fx]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.model.user.domain.access :as access]
   [net.humanhelp.site.model.user.domain.common :as user.common]
   [net.humanhelp.site.model.user.domain.identity :as identity]
   [net.humanhelp.site.model.user.domain.invitation :as invitation]
   [net.humanhelp.site.model.user.domain.membership :as membership]
   [net.humanhelp.site.model.user.domain.role :as role]
   [net.humanhelp.site.model.user.fx :as user.fx]
   [net.humanhelp.site.model.user.graph :as user.graph]
   [net.humanhelp.site.model.user.schema :as user.schema])
  (:import
   [java.time Instant]
   [java.util UUID]))
(defn uuid [value] (UUID/fromString value))
(def user-id (uuid "00000000-0000-0000-0000-000000000001"))
(def admin-user-id (uuid "00000000-0000-0000-0000-000000000003"))
(def organization-id (uuid "10000000-0000-0000-0000-000000000001"))
(def other-organization-id (uuid "10000000-0000-0000-0000-000000000002"))
(def group-id (uuid "20000000-0000-0000-0000-000000000001"))
(def location-id (uuid "30000000-0000-0000-0000-000000000001"))
(def other-location-id (uuid "30000000-0000-0000-0000-000000000002"))
(def membership-id (uuid "40000000-0000-0000-0000-000000000001"))
(def other-membership-id (uuid "40000000-0000-0000-0000-000000000002"))
(def helper-role-id (uuid "50000000-0000-0000-0000-000000000001"))
(def admin-role-id (uuid "50000000-0000-0000-0000-000000000002"))
(def duplicate-helper-role-id (uuid "50000000-0000-0000-0000-000000000003"))
(def invitation-id (uuid "60000000-0000-0000-0000-000000000001"))
(def generated-membership-id (uuid "70000000-0000-0000-0000-000000000001"))
(def generated-role-id (uuid "70000000-0000-0000-0000-000000000002"))
(def t0 (Instant/parse "2026-07-01T12:00:00Z"))
(def t1 (Instant/parse "2026-07-01T12:01:00Z"))
(def t2 (Instant/parse "2026-07-01T12:02:00Z"))
(def t7 (Instant/parse "2026-07-08T12:00:00Z"))
(def t8 (Instant/parse "2026-07-09T12:00:00Z"))
(def far-future (Instant/parse "2099-01-01T00:00:00Z"))
(def raw-token "test-invitation-token")
(def canonical-email "person@example.com")
(def canonical-phone "+12065550123")
(def location-scope (role/location-scope location-id))
(def group-scope (role/organization-group-scope group-id))
(def organization-scope (role/organization-scope organization-id))
(def applicable-scopes [location-scope group-scope organization-scope])
(def scope-context {:organization/id organization-id
                    :scope/target location-scope
                    :scope/applicable applicable-scopes
                    :scope/operational? true})
(def location-expected-version
  {:model/id location-id :model/revision-key :location/revision
   :model/revision 4 :model/updated-at-key :location/updated-at
   :model/updated-at t0})
(def location-authorization-versions
  [{:model/entity-type :location :model/expected location-expected-version}])
(defn active-user
  ([]
   (active-user {}))
  ([overrides]
   (identity/new-user
    (merge
     {:id user-id
      :email canonical-email
      :email-verified? true
      :display-name "Person"
      :now t0}
     overrides))))
(defn admin-user []
  (active-user {:id admin-user-id :email "admin@example.com"
                :display-name "Admin"}))
(defn active-membership
  ([]
   (active-membership {}))
  ([overrides]
   (membership/new-membership
    (merge
     {:id membership-id
      :user-id user-id
      :organization-id organization-id
      :now t0}
     overrides))))
(defn role-assignment
  ([]
   (role-assignment {}))
  ([overrides]
   (role/new-role-assignment
    (merge
     {:id helper-role-id
      :membership-id membership-id
      :organization-id organization-id
      :role :helper
      :scope location-scope
      :actor-id admin-user-id
      :reason :test/seed
      :now t0}
     overrides))))
(defn admin-assignment
  []
  (role-assignment
   {:id admin-role-id
    :membership-id other-membership-id
    :role :admin
    :scope organization-scope}))
(defn pending-invitation
  ([]
   (pending-invitation {}))
  ([overrides]
   (invitation/new-invitation
    (merge
     {:id invitation-id
      :organization-id organization-id
      :invited-by admin-user-id
      :email canonical-email
      :role :helper
      :scope location-scope
      :token-hash (user.fx/hash-token raw-token)
      :now t0
      :expires-at t7}
     overrides))))
(defn suspended-membership
  []
  (membership/suspend
   (active-membership)
   {:now t1
    :actor-id admin-user-id
    :reason :test/suspended}))
(defn revoked-role
  ([]
   (revoked-role (role-assignment)))
  ([assignment]
   (role/revoke
    assignment
    {:now t1
     :actor-id admin-user-id
     :reason :test/revoked})))
(defn error-type
  [f]
  (try
    (f)
    ::did-not-throw
    (catch Throwable error
      (loop [error error]
        (when error
          (or (:error/type (ex-data error))
              (recur (ex-cause error))))))))
(defn command-document [command]
  (model.common/command-document command))
(defn topic-set [changes] (set (map :topic changes)))
(deftest identity-normalization-and-validation-test
  (testing "contacts and display names normalize canonically"
    (is (= canonical-email
           (user.common/normalize-email "  PERSON@EXAMPLE.COM  ")))
    (is (= canonical-phone
           (user.common/normalize-phone "  +12065550123  ")))
    (is (= "Person Name"
           (identity/normalize-display-name "  Person Name  ")))
    (is (user.common/email? canonical-email))
    (is (user.common/phone? canonical-phone))
    (is (identity/display-name? "Person")))
  (testing "invalid values fail validation"
    (is (false? (user.common/email? "not-an-email")))
    (is (false? (user.common/phone? "2065550123")))
    (is (false? (identity/display-name? "   ")))))
(deftest identity-create-and-schema-test
  (let [document
        (active-user
         {:phone canonical-phone
          :phone-verified? true})]
    (is (= user-id (:xt/id document)))
    (is (= :active (:user/status document)))
    (is (= 0 (:user/revision document)))
    (is (= t0 (:user/created-at document)))
    (is (= t0 (:user/updated-at document)))
    (is (identity/document-consistent? document))
    (is (identity/phone-verified? document))
    (is (identity/email-verified? document))
    (is (identity/has-verified-contact? document))
    (is (m/validate user.schema/user-document-schema document))
    (is (false?
         (m/validate
          user.schema/user-document-schema
          (assoc document :unexpected/value true))))))
  (is (= :user/invalid-create-input
         (error-type
          #(identity/new-user
            {:id user-id
             :display-name "Missing contact"
             :now t0}))))
(deftest identity-contact-lifecycle-test
  (let [original
        (active-user
         {:phone canonical-phone
          :phone-verified? true})
        replaced
        (identity/replace-email
         original
         {:email "new@example.com"
          :now t1})
        verified
        (identity/verify-email
         replaced
         {:email "new@example.com"
          :now t2})]
    (is (= "new@example.com" (:user/email replaced)))
    (is (nil? (:user/email-verified-at replaced)))
    (is (= 1 (:user/revision replaced)))
    (is (= t1 (:user/updated-at replaced)))
    (is (identity/email-verified? verified))
    (is (= 2 (:user/revision verified)))
    (is (= :user/verification-target-mismatch
           (error-type
            #(identity/verify-email
              replaced
              {:email canonical-email
               :now t2}))))
    (is (= :user/contact-unchanged
           (error-type
            #(identity/replace-email
              original
              {:email canonical-email
               :now t1}))))))
(deftest identity-status-lifecycle-test
  (let [original (active-user)
        suspended
        (identity/suspend
         original
         {:now t1
          :actor-id admin-user-id
          :reason :test/suspended})
        reactivated
        (identity/reactivate suspended {:now t2})
        deleted
        (identity/delete-user
         suspended
         {:now t2
          :actor-id admin-user-id
          :reason :test/deleted})]
    (is (identity/suspended? suspended))
    (is (= 1 (:user/revision suspended)))
    (is (= admin-user-id (:user/suspended-by suspended)))
    (is (identity/active? reactivated))
    (is (nil? (:user/suspended-at reactivated)))
    (is (identity/deleted? deleted))
    (is (nil? (:user/suspended-at deleted)))
    (is (= :user/deleted
           (error-type
            #(identity/reactivate deleted {:now t8}))))
    (is (= :user/not-active
           (error-type
            #(identity/suspend suspended {:now t2}))))))
(deftest membership-create-and-schema-test
  (let [document (active-membership)]
    (is (= membership-id (:xt/id document)))
    (is (= user-id (membership/user-id document)))
    (is (= organization-id (membership/organization-id document)))
    (is (membership/active? document))
    (is (membership/for-user? document user-id))
    (is (membership/for-organization? document organization-id))
    (is (membership/document-consistent? document))
    (is (m/validate user.schema/membership-document-schema document))))
(deftest membership-lifecycle-test
  (let [original (active-membership)
        suspended
        (membership/suspend
         original
         {:now t1
          :actor-id admin-user-id
          :reason :test/suspended})
        reactivated
        (membership/reactivate suspended {:now t2})
        revoked
        (membership/revoke
         suspended
         {:now t2
          :actor-id admin-user-id
          :reason :test/revoked})]
    (is (membership/suspended? suspended))
    (is (= 1 (:membership/revision suspended)))
    (is (membership/active? reactivated))
    (is (nil? (:membership/suspended-at reactivated)))
    (is (membership/revoked? revoked))
    (is (nil? (:membership/suspended-at revoked)))
    (is (= :membership/already-suspended
           (error-type
            #(membership/suspend suspended {:now t2}))))
    (is (= :membership/revoked
           (error-type
            #(membership/reactivate revoked {:now t8}))))))
(deftest role-scope-test
  (is (= {:scope/type :organization
          :scope/id organization-id}
         organization-scope))
  (is (= {:scope/type :organization-group
          :scope/id group-id}
         group-scope))
  (is (= {:scope/type :location
          :scope/id location-id}
         location-scope))
  (is (role/organization-group-scope? group-scope))
  (is (role/location-scope? location-scope))
  (is (user.common/scope-reference? organization-scope))
  (is (user.common/same-scope? location-scope
                               (role/location-scope location-id))))
(deftest role-assignment-create-grant-and-schema-test
  (let [assignment (role-assignment)]
    (is (role/active? assignment))
    (is (= :helper (role/assigned-role assignment)))
    (is (= location-scope (role/scope assignment)))
    (is (role/grants-role? assignment :helper))
    (is (role/at-scope? assignment location-scope))
    (is (role/grants?
         assignment
         membership-id
         :helper
         location-scope))
    (is (false?
         (role/grants?
          assignment
          membership-id
          :admin
          location-scope)))
    (is (role/document-consistent? assignment))
    (is (m/validate user.schema/role-assignment-document-schema assignment))))
(deftest role-assignment-lifecycle-and-collections-test
  (let [location-helper (role-assignment)
        organization-admin
        (role-assignment
         {:id admin-role-id
          :role :admin
          :scope organization-scope})
        revoked-helper (revoked-role location-helper)
        assignments [location-helper organization-admin revoked-helper]]
    (is (role/revoked? revoked-helper))
    (is (= [location-helper organization-admin]
           (role/active-assignments assignments)))
    (is (= [location-helper revoked-helper]
           (filterv #(role/at-scope? % location-scope) assignments)))
    (is (= [location-helper]
           (role/active-at-scope assignments location-scope)))
    (is (= :role-assignment/revoked
           (error-type
            #(role/revoke revoked-helper {:now t2}))))
    (let [commands
          (role/revoke-at-scope-commands
           assignments
           location-scope
           {:now t2
            :actor-id admin-user-id
            :reason :location/closed})]
      (is (= 1 (count commands)))
      (is (= :revoke (:model/operation (first commands))))
      (is (role/revoked? (command-document (first commands)))))))
(deftest invitation-create-recipient-and-schema-test
  (let [document (pending-invitation)]
    (is (invitation/pending? document))
    (is (= organization-id (invitation/organization-id document)))
    (is (= :helper (invitation/offered-role document)))
    (is (= location-scope (invitation/scope document)))
    (is (= :email (invitation/recipient-type document)))
    (is (= canonical-email (invitation/recipient-value document)))
    (is (invitation/addressed-to? document {:email canonical-email}))
    (is (invitation/usable-at? document t1))
    (is (false? (invitation/past-expiration? document t1)))
    (is (invitation/document-consistent? document))
    (is (m/validate user.schema/invitation-document-schema document)))
  (is (= :invitation/invalid-create-input
         (error-type
          #(pending-invitation
            {:phone canonical-phone}))))
  (is (= :invitation/invalid-create-input
         (error-type
          #(pending-invitation
            {:expires-at t0})))))
(deftest invitation-terminal-lifecycle-test
  (let [pending (pending-invitation)
        accepted
        (invitation/accept
         pending
         {:now t1
          :user-id user-id
          :membership-id membership-id
          :role-assignment-id helper-role-id})
        declined
        (invitation/decline
         pending
         {:now t1
          :actor-id user-id})
        revoked
        (invitation/revoke
         pending
         {:now t1
          :actor-id admin-user-id
          :reason :test/revoked})
        expired
        (invitation/expire pending {:now t8})]
    (is (invitation/accepted? accepted))
    (is (= membership-id (:invitation/membership accepted)))
    (is (= helper-role-id (:invitation/role-assignment accepted)))
    (is (invitation/declined? declined))
    (is (invitation/revoked? revoked))
    (is (invitation/expired? expired))
    (doseq [document [accepted declined revoked expired]]
      (is (invitation/terminal? document))
      (is (invitation/document-consistent? document)))
    (is (= :invitation/accepted
           (error-type
            #(invitation/decline accepted {:now t2}))))
    (is (= :invitation/not-expired
           (error-type
            #(invitation/expire pending {:now t1}))))))
(deftest membership-access-composition-test
  (let [user-document (active-user)
        membership-document (active-membership)
        suspended (suspended-membership)]
    (is (access/membership-for-user? user-document membership-document))
    (is (access/current-membership? membership-document))
    (is (access/current-membership? suspended))
    (is (access/access-enabled-membership? user-document membership-document))
    (is (false?
         (access/access-enabled-membership? user-document suspended)))
    (is (access/organization-affiliated?
         user-document
         [membership-document]))
    (is (false?
         (access/customer? user-document [membership-document])))
    (is (access/customer? user-document []))))
(deftest scoped-role-access-test
  (let [user-document (active-user)
        membership-document (active-membership)
        helper (role-assignment)
        admin
        (role-assignment
         {:id admin-role-id
          :role :admin
          :scope organization-scope})
        assignments [helper admin]]
    (is (access/applicable-scopes? applicable-scopes))
    (is (access/assignment-applies-at? helper applicable-scopes))
    (is (access/assignment-applies-at? admin applicable-scopes))
    (is (= assignments
           (access/effective-assignments
            user-document
            membership-document
            assignments
            applicable-scopes)))
    (is (= #{:helper :admin}
           (access/effective-roles
            user-document
            membership-document
            assignments
            applicable-scopes)))
    (is (= admin
           (access/administrator-assignment
            user-document
            membership-document
            assignments
            applicable-scopes)))
    (is (access/helper?
         user-document membership-document assignments applicable-scopes))
    (is (access/admin?
         user-document membership-document assignments applicable-scopes))
    (is (access/staff?
         user-document membership-document assignments applicable-scopes))
    (is (false?
         (access/supervisor?
          user-document membership-document assignments applicable-scopes))))
  (testing "roles do not imply a hierarchy"
    (let [user-document (active-user)
          membership-document (active-membership)
          admin
          (role-assignment
           {:id admin-role-id
            :role :admin
            :scope organization-scope})]
      (is (= #{:admin}
             (access/effective-roles
              user-document
              membership-document
              [admin]
              applicable-scopes)))
      (is (access/admin?
           user-document membership-document [admin] applicable-scopes))
      (is (false?
           (access/helper?
            user-document membership-document [admin] applicable-scopes))))))
(deftest access-fails-closed-test
  (let [user-document (active-user)
        membership-document (active-membership)
        helper (role-assignment)]
    (is (= []
           (access/effective-assignments
            user-document
            (suspended-membership)
            [helper]
            applicable-scopes)))
    (is (= []
           (access/effective-assignments
            user-document
            membership-document
            [(revoked-role helper)]
            applicable-scopes)))
    (is (= []
           (access/effective-assignments
            user-document
            membership-document
            [helper]
            [organization-scope])))
    (is (= []
           (access/effective-assignments
            user-document
            (active-membership {:organization-id other-organization-id})
            [helper]
            applicable-scopes)))))
(deftest public-access-context-test
  (let [user-document (active-user)
        membership-document (active-membership)
        helper (role-assignment)
        admin
        (role-assignment
         {:id admin-role-id
          :role :admin
          :scope organization-scope})
        context
        (access/access-context
         user-document
         membership-document
         [helper admin]
         applicable-scopes
         organization-id)]
    (is (access/access-context? context))
    (is (= user-id (:user/id context)))
    (is (= membership-id (:membership/id context)))
    (is (= #{:helper :admin} (:user/effective-roles context)))
    (is (= #{:user/invite-helper-to-location}
           (:user/capabilities context)))
    (is (access/can-invite-helper? context))
    (is (not-any?
         #(contains? context %)
         [:user/doc :membership/doc :role-assignment/doc]))
    (is (false?
         (access/access-context?
          (assoc context
                 :user/capabilities #{}))))))
(deftest public-access-context-without-membership-test
  (let [context
        (access/access-context
         (active-user)
         nil
         []
         applicable-scopes
         organization-id)]
    (is (access/access-context? context))
    (is (nil? (:membership/id context)))
    (is (false? (:membership/active? context)))
    (is (= #{} (:user/effective-roles context)))
    (is (= #{} (:user/capabilities context)))
    (is (false? (access/can-invite-helper? context)))))
(deftest schema-registry-test
  (is (= user.schema/user-document-schema
         (:user user.schema/schema)))
  (is (= user.schema/membership-document-schema
         (:membership user.schema/schema)))
  (is (= user.schema/role-assignment-document-schema
         (:role-assignment user.schema/schema)))
  (is (= user.schema/invitation-document-schema
         (:invitation user.schema/schema)))
  (is (m/validate (:user/doc user.schema/schema) (active-user)))
  (is (m/validate (:membership/doc user.schema/schema) (active-membership)))
  (is (m/validate (:role-assignment/doc user.schema/schema) (role-assignment)))
  (is (m/validate (:invitation/doc user.schema/schema) (pending-invitation))))
(deftest graph-input-builder-test
  (is (= {:user/id user-id}
         (user.graph/user-query-input {:user-id user-id})))
  (is (= {:user/phone canonical-phone}
         (user.graph/user-query-input {:phone canonical-phone})))
  (is (= {:user/email canonical-email}
         (user.graph/user-query-input {:email canonical-email})))
  (is (= {:membership/id membership-id}
         (user.graph/membership-query-input
          {:membership-id membership-id})))
  (is (= {:role-assignment/id helper-role-id}
         (user.graph/role-assignment-query-input
          {:role-assignment-id helper-role-id})))
  (is (= {:invitation/id invitation-id}
         (user.graph/invitation-query-input
          {:invitation-id invitation-id})))
  (is (= {:invitation/token-hash "hash"}
         (user.graph/invitation-query-input
          {:token-hash "hash"})))
  (is (= {:user/id user-id
          :membership/organization-id organization-id
          :user/applicable-scopes applicable-scopes}
         (user.graph/access-query-input
          {:user-id user-id
           :organization-id organization-id
           :applicable-scopes applicable-scopes})))
  (is (= {:role-assignment/organization-id organization-id
          :role-assignment/scope-type :location
          :role-assignment/scope-id location-id}
         (user.graph/scoped-role-assignment-query-input
          {:organization-id organization-id
           :scope location-scope}))))
(deftest core-registration-and-operation-contract-test
  (is (= user.schema/schema user/schema))
  (is (= user.graph/resolvers user/resolvers))
  (is (= {:schema user/schema
          :biff.graph/resolvers user/resolvers}
         user/module))
  (is (= identity/entity-type user/user-entity-type))
  (is (= membership/entity-type user/membership-entity-type))
  (is (= role/entity-type user/role-assignment-entity-type))
  (is (= invitation/entity-type user/invitation-entity-type))
  (is (identical?
       #'user/invite-helper-to-location
       (:user/invite-helper-to-location user/operations)))
  (is (identical?
       #'user/accept-invitation
       (:user/accept-invitation user/operations))))
(deftest core-scope-context-validation-test
  (is (user/scope-context? scope-context))
  (is (false?
       (user/scope-context?
        (update scope-context :scope/applicable conj location-scope))))
  (is (false?
       (user/scope-context?
        (assoc scope-context
               :scope/applicable [location-scope]))))
  (is (false?
       (user/scope-context?
        (assoc scope-context
               :scope/target
               (role/location-scope other-location-id))))))
(deftest core-access-context-read-test
  (let [seen (atom nil)
        facts
        {:user/found? true
         :user/doc (active-user)
         :user/current-membership-found? true
         :user/current-membership
         {:membership/doc (active-membership)
          :membership/role-assignments
          [{:role-assignment/doc (role-assignment)}
           {:role-assignment/doc
            (role-assignment
             {:id admin-role-id
              :role :admin
              :scope organization-scope})}]}}
        context
        (with-redefs
         [graph/query
          (fn [_ctx input query]
            (reset! seen {:input input :query query})
            facts)]
          (user/access-context
           {:request/id :test}
           {:user-id user-id
            :scope-context scope-context}))]
    (is (= (user.graph/access-query-input
            {:user-id user-id
             :organization-id organization-id
             :applicable-scopes applicable-scopes})
           (:input @seen)))
    (is (= user.graph/access-query (:query @seen)))
    (is (user/access-context? context))
    (is (= location-scope (:scope/target context)))
    (is (= "Person" (:user/display-name context)))
    (is (= #{:helper :admin} (:user/effective-roles context)))
    (is (user/can-invite-helper? context))
    (is (not-any?
         #(contains? context %)
         [:user/doc :membership/doc :role-assignment/doc]))))
(deftest core-access-context-no-membership-test
  (let [facts
        {:user/found? true
         :user/doc (active-user)
         :user/current-membership-found? false}
        context
        (with-redefs
         [graph/query (fn [& _] facts)]
          (user/access-context
           {}
           {:user-id user-id
            :scope-context scope-context}))]
    (is (user/access-context? context))
    (is (nil? (:membership/id context)))
    (is (= #{} (:user/effective-roles context)))
    (is (false? (user/can-invite-helper? context)))))
(deftest core-access-context-errors-test
  (is (= :user/invalid-user-id
         (error-type
          #(user/access-context
            {}
            {:user-id "not-a-uuid"
             :scope-context scope-context}))))
  (is (= :user/invalid-scope-context
         (error-type
          #(user/access-context
            {}
            {:user-id user-id
             :scope-context {}}))))
  (is (= :user/not-found
         (with-redefs
          [graph/query
           (fn [& _]
             {:user/found? false})]
           (error-type
            #(user/access-context
              {}
              {:user-id user-id
               :scope-context scope-context}))))))
(deftest core-named-read-delegation-test
  (let [calls (atom [])]
    (with-redefs
     [graph/query
      (fn [_ctx input query]
        (swap! calls conj [input query])
        {:ok true})]
      (is (= {:ok true} (user/user-facts {} {:user-id user-id})))
      (is (= {:ok true} (user/membership-facts {} membership-id)))
      (is (= {:ok true}
             (user/role-assignment-facts {} helper-role-id)))
      (is (= {:ok true} (user/invitation-facts {} invitation-id)))
      (is (= {:ok true} (user/customer-facts {} user-id)))
      (is (= {:ok true}
             (user/active-role-assignments-at-scope
              {}
              organization-id
              location-scope))))
    (is (= 6 (count @calls)))
    (is (= user/user-query (second (nth @calls 0))))
    (is (= user/membership-query (second (nth @calls 1))))
    (is (= user/role-assignment-query (second (nth @calls 2))))
    (is (= user/invitation-query (second (nth @calls 3))))
    (is (= user/customer-query (second (nth @calls 4))))
    (is (= user/active-role-assignments-at-scope-query
           (second (nth @calls 5))))))
(deftest core-write-delegation-test
  (let [invite-call (atom nil)
        accept-call (atom nil)]
    (with-redefs
     [user.fx/invite-helper-to-location
      (fn [ctx input]
        (reset! invite-call [ctx input])
        :invited)
      user.fx/accept-invitation
      (fn [ctx input]
        (reset! accept-call [ctx input])
        :accepted)]
      (is (= :invited
             (user/invite-helper-to-location
              {:ctx true}
              {:organization-id organization-id})))
      (is (= :accepted
             (user/accept-invitation
              {:ctx true}
              {:token raw-token}))))
    (is (= [{:ctx true}
            {:organization-id organization-id}]
           @invite-call))
    (is (= [{:ctx true}
            {:token raw-token}]
           @accept-call))))
(deftest invitation-token-test
  (let [hash-a (user.fx/hash-token raw-token)
        hash-b (user.fx/hash-token raw-token)
        generated (user.fx/generate-token)]
    (is (= hash-a hash-b))
    (is (not= raw-token hash-a))
    (is (invitation/token-hash? hash-a))
    (is (= 43 (count generated)))
    (is (re-matches #"[A-Za-z0-9_-]+" generated))
    (is (= :invitation/invalid-token
           (error-type #(user.fx/hash-token "   "))))))
(deftest model-fx-assertion-helper-test
  (is (= {:assert
          [:= 0
           {:select [[[:count '*']]]
            :from 'invitation
            :where [:= :xt/id invitation-id]}]}
         (model.fx/assert-document-absent
          :invitation
          invitation-id)))
  (is (= {:assert
          [:= 1
           {:select [[[:count '*']]]
            :from 'location
            :where
            [:and
             [:= :xt/id location-id]
             [:= :location/revision 4]
             [:= :location/updated-at t0]]}]}
         (model.fx/assert-document-current
          :location
          location-expected-version)))
  (is (= :model.fx/invalid-document-id
         (error-type
          #(model.fx/assert-document-absent :user "bad")))))
(deftest model-fx-command-translation-test
  (let [create-command
        (invitation/create-command
         {:id invitation-id
          :organization-id organization-id
          :invited-by admin-user-id
          :email canonical-email
          :role :helper
          :scope location-scope
          :token-hash (user.fx/hash-token raw-token)
          :now t0
          :expires-at t7})
        create-document (command-document create-command)
        accept-command
        (invitation/accept-command
         create-document
         {:now t1
          :user-id user-id
          :membership-id membership-id
          :role-assignment-id helper-role-id})]
    (is (= (model.fx/assert-document-absent
            :invitation
            invitation-id)
           (model.fx/command-precondition create-command)))
    (is (= [:put-docs :invitation create-document]
           (model.fx/command->tx-op create-command)))
    (is (= (model.fx/assert-document-current
            :invitation
            (:model/expected accept-command))
           (model.fx/command-precondition accept-command)))
    (let [custom (model.fx/assert-none
                  :invitation
                  [:= :invitation/token-hash "hash"])
          ops
          (model.fx/transaction-ops
           {:assertions [custom]
            :commands [create-command]})]
      (is (= custom (nth ops 0)))
      (is (= (model.fx/command-precondition create-command)
             (nth ops 1)))
      (is (= (model.fx/command->tx-op create-command)
             (nth ops 2))))
    (is (= :model.fx/duplicate-command-targets
           (error-type
            #(model.fx/transaction-ops
              {:commands [create-command create-command]}))))))
(deftest plan-helper-invitation-test
  (let [inviter (admin-user)
        inviter-membership
        (active-membership
         {:id other-membership-id
          :user-id admin-user-id})
        admin-role (admin-assignment)
        command
        (invitation/create-command
         {:id invitation-id
          :organization-id organization-id
          :invited-by admin-user-id
          :email canonical-email
          :role :helper
          :scope location-scope
          :token-hash (user.fx/hash-token raw-token)
          :now t0
          :expires-at t7})
        plan
        (user.fx/plan-helper-invitation
         {:command command
          :raw-token raw-token
          :location-authorization-versions location-authorization-versions
          :access-proof
          {:user inviter
           :membership inviter-membership
           :role-assignment admin-role}})
        transaction-plan (:transaction-plan plan)
        result (:result plan)]
    (is (= [command] (:commands transaction-plan)))
    (is (= 4 (count (:authorization-versions transaction-plan))))
    (is (= 1 (count (:assertions transaction-plan))))
    (is (= #{:invitation}
           (topic-set (:changes transaction-plan))))
    (is (ifn? (:entry-fn transaction-plan)))
    (is (= raw-token (:token result)))
    (is (= (command-document command) (:invitation result)))
    (is (not= raw-token
              (:invitation/token-hash (:invitation result))))))
(deftest plan-invitation-acceptance-creates-missing-documents-test
  (let [plan
        (user.fx/plan-invitation-acceptance
         {:now t1
          :user (active-user)
          :invitation-document (pending-invitation)
          :location-authorization-versions location-authorization-versions
          :existing-membership nil
          :existing-role-assignments []
          :generated-membership-id generated-membership-id
          :generated-role-assignment-id generated-role-id})
        transaction-plan (:transaction-plan plan)
        result (:result plan)]
    (is (= 3 (count (:commands transaction-plan))))
    (is (= [:create :create :accept]
           (mapv :model/operation (:commands transaction-plan))))
    (is (= 2 (count (:authorization-versions transaction-plan))))
    (is (= 2 (count (:assertions transaction-plan))))
    (is (= #{:membership :role-assignment :invitation}
           (topic-set (:changes transaction-plan))))
    (is (= generated-membership-id
           (get-in result [:membership :xt/id])))
    (is (= generated-role-id
           (get-in result [:role-assignment :xt/id])))
    (is (invitation/accepted? (:invitation result)))
    (is (= generated-membership-id
           (get-in result [:invitation :invitation/membership])))
    (is (= generated-role-id
           (get-in result [:invitation :invitation/role-assignment])))))
(deftest plan-invitation-acceptance-reuses-existing-documents-test
  (let [membership-document (active-membership)
        assignment (role-assignment)
        plan
        (user.fx/plan-invitation-acceptance
         {:now t1
          :user (active-user)
          :invitation-document (pending-invitation)
          :location-authorization-versions location-authorization-versions
          :existing-membership membership-document
          :existing-role-assignments [assignment]
          :generated-membership-id generated-membership-id
          :generated-role-assignment-id generated-role-id})
        transaction-plan (:transaction-plan plan)
        result (:result plan)]
    (is (= 1 (count (:commands transaction-plan))))
    (is (= :accept
           (:model/operation (first (:commands transaction-plan)))))
    (is (= 4 (count (:authorization-versions transaction-plan))))
    (is (= 2 (count (:assertions transaction-plan))))
    (is (= #{:invitation}
           (topic-set (:changes transaction-plan))))
    (is (= membership-document (:membership result)))
    (is (= assignment (:role-assignment result)))))
(deftest plan-invitation-acceptance-creates-only-missing-role-test
  (let [membership-document (active-membership)
        plan
        (user.fx/plan-invitation-acceptance
         {:now t1
          :user (active-user)
          :invitation-document (pending-invitation)
          :location-authorization-versions location-authorization-versions
          :existing-membership membership-document
          :existing-role-assignments []
          :generated-membership-id generated-membership-id
          :generated-role-assignment-id generated-role-id})
        transaction-plan (:transaction-plan plan)]
    (is (= [:create :accept]
           (mapv :model/operation (:commands transaction-plan))))
    (is (= 3 (count (:authorization-versions transaction-plan))))
    (is (= 2 (count (:assertions transaction-plan))))
    (is (= #{:role-assignment :invitation}
           (topic-set (:changes transaction-plan))))))
(deftest plan-invitation-acceptance-rejects-invalid-state-test
  (is (= :membership/suspended
         (error-type
          #(user.fx/plan-invitation-acceptance
            {:now t1
             :user (active-user)
             :invitation-document (pending-invitation)
             :location-authorization-versions location-authorization-versions
             :existing-membership (suspended-membership)
             :existing-role-assignments []
             :generated-membership-id generated-membership-id
             :generated-role-assignment-id generated-role-id}))))
  (is (= :role-assignment/ambiguous
         (error-type
          #(user.fx/plan-invitation-acceptance
            {:now t1
             :user (active-user)
             :invitation-document (pending-invitation)
             :location-authorization-versions location-authorization-versions
             :existing-membership (active-membership)
             :existing-role-assignments
             [(role-assignment)
              (role-assignment {:id duplicate-helper-role-id})]
             :generated-membership-id generated-membership-id
             :generated-role-assignment-id generated-role-id}))))
  (is (= :invitation/recipient-mismatch
         (error-type
          #(user.fx/plan-invitation-acceptance
            {:now t1
             :user (active-user {:email-verified? false})
             :invitation-document (pending-invitation)
             :location-authorization-versions location-authorization-versions
             :existing-membership nil
             :existing-role-assignments []
             :generated-membership-id generated-membership-id
             :generated-role-assignment-id generated-role-id}))))
  (is (= :invitation/expired
         (error-type
          #(user.fx/plan-invitation-acceptance
            {:now t8
             :user (active-user)
             :invitation-document (pending-invitation)
             :location-authorization-versions location-authorization-versions
             :existing-membership nil
             :existing-role-assignments []
             :generated-membership-id generated-membership-id
             :generated-role-assignment-id generated-role-id})))))
(defn location-facts
  ([]
   (location-facts true))
  ([active?]
   {:location/found? true
    :location/doc {:xt/id location-id
                   :location/revision 4
                   :location/updated-at t0}
    :location/active? active?
    :location/organization-id organization-id
    :location/applicable-scopes applicable-scopes
    :location/authorization-versions location-authorization-versions}))
(defn admin-access-facts
  []
  (let [admin (admin-user)
        membership-document
        (active-membership
         {:id other-membership-id
          :user-id admin-user-id})
        assignment (admin-assignment)]
    {:user/found? true
     :user/doc admin
     :user/current-membership-found? true
     :user/current-membership
     {:membership/doc membership-document
      :membership/role-assignments
      [{:role-assignment/doc assignment}]}
     :user/effective-roles #{:admin}
     :user/helper? false
     :user/supervisor? false
     :user/admin? true
     :user/staff? true}))
(defn committed-result
  []
  {:commit/status :committed
   :tx-result {:tx-id 1}
   :consistency {:tx-id 1}
   :changes []
   :emit :async
   :publication {:status :submitted}})
(deftest invite-helper-machine-happy-path-test
  (let [captured-plan (atom nil)
        graph-handler
        (fn [_ctx _input query]
          (cond
            (= query user.fx/location-context-query)
            (location-facts)
            (= query user.graph/access-query)
            (admin-access-facts)
            :else
            (throw
             (ex-info "Unexpected Graph query."
                      {:query query}))))
        transaction-handler
        (fn [_ctx plan]
          (reset! captured-plan plan)
          (committed-result))
        result
        (user.fx/invite-helper-to-location
         {:current-user/id admin-user-id
          user.fx/token-generator-key (constantly raw-token)
          :biff.fx/handlers
          {:biff.graph.fx/query graph-handler
           model.fx/transact-effect transaction-handler}}
         {:organization-id organization-id
          :location-id location-id
          :email canonical-email})]
    (is (= raw-token (:token result)))
    (is (invitation/pending? (:invitation result)))
    (is (= (user.fx/hash-token raw-token)
           (get-in result [:invitation :invitation/token-hash])))
    (is (= :committed
           (get-in result [:transaction :commit/status])))
    (is (= 1 (count (:commands @captured-plan))))
    (is (= 4 (count (:authorization-versions @captured-plan))))
    (is (= 1 (count (:assertions @captured-plan))))
    (is (= #{:invitation}
           (topic-set (:changes @captured-plan))))))
(deftest invite-helper-machine-authorization-test
  (let [graph-handler
        (fn [_ctx _input query]
          (cond
            (= query user.fx/location-context-query)
            (location-facts)
            (= query user.graph/access-query)
            (let [facts (admin-access-facts)
                  helper
                  (role-assignment
                   {:id helper-role-id
                    :membership-id other-membership-id
                    :role :helper
                    :scope organization-scope})]
              (-> facts
                  (assoc :user/admin? false
                         :user/effective-roles #{:helper})
                  (assoc-in
                   [:user/current-membership
                    :membership/role-assignments]
                   [{:role-assignment/doc helper}])))
            :else nil))]
    (is (= :user/not-authorized
           (error-type
            #(user.fx/invite-helper-to-location
              {:current-user/id admin-user-id
               user.fx/token-generator-key (constantly raw-token)
               :biff.fx/handlers
               {:biff.graph.fx/query graph-handler
                model.fx/transact-effect
                (fn [& _]
                  (throw (AssertionError. "Must not commit.")))}}
              {:organization-id organization-id
               :location-id location-id
               :email canonical-email}))))))
(deftest accept-invitation-machine-happy-path-test
  (let [invitation-document
        (pending-invitation {:expires-at far-future})
        user-document (active-user)
        captured-plan (atom nil)
        graph-handler
        (fn [_ctx _input query]
          (cond
            (= query user.graph/invitation-command-query)
            {:invitation/found? true
             :invitation/doc invitation-document}
            (= query user.graph/user-command-query)
            {:user/found? true
             :user/doc user-document}
            (= query user.fx/location-context-query)
            (location-facts)
            (= query user.fx/membership-with-roles-query)
            {:user/found? true
             :user/doc user-document
             :user/current-membership-found? false}
            :else
            (throw
             (ex-info "Unexpected Graph query."
                      {:query query}))))
        transaction-handler
        (fn [_ctx plan]
          (reset! captured-plan plan)
          (committed-result))
        result
        (user.fx/accept-invitation
         {:current-user/id user-id
          :biff.fx/handlers
          {:biff.graph.fx/query graph-handler
           model.fx/transact-effect transaction-handler}}
         {:token raw-token})]
    (is (invitation/accepted? (:invitation result)))
    (is (= user-id (get-in result [:user :xt/id])))
    (is (= :helper
           (get-in result [:role-assignment :role-assignment/role])))
    (is (= :committed
           (get-in result [:transaction :commit/status])))
    (is (= 3 (count (:commands @captured-plan))))
    (is (= #{:membership :role-assignment :invitation}
           (topic-set (:changes @captured-plan))))))
(deftest accept-invitation-machine-rejects-missing-and-wrong-recipient-test
  (testing "missing invitation"
    (let [handler
          (fn [_ctx _input query]
            (cond
              (= query user.graph/invitation-command-query)
              {:invitation/found? false}
              (= query user.graph/user-command-query)
              {:user/found? true
               :user/doc (active-user)}))]
      (is (= :invitation/not-found
             (error-type
              #(user.fx/accept-invitation
                {:current-user/id user-id
                 :biff.fx/handlers
                 {:biff.graph.fx/query handler}}
                {:token raw-token}))))))
  (testing "verified contact must match"
    (let [wrong-user
          (active-user
           {:email "someone-else@example.com"})
          handler
          (fn [_ctx _input query]
            (cond
              (= query user.graph/invitation-command-query)
              {:invitation/found? true
               :invitation/doc
               (pending-invitation {:expires-at far-future})}
              (= query user.graph/user-command-query)
              {:user/found? true
               :user/doc wrong-user}))]
      (is (= :invitation/recipient-mismatch
             (error-type
              #(user.fx/accept-invitation
                {:current-user/id user-id
                 :biff.fx/handlers
                 {:biff.graph.fx/query handler}}
                {:token raw-token})))))))
