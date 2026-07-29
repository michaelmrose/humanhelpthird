(ns net.humanhelp.site.model.user-test
  "Tests for the rewritten HumanHelp User model.

   The old User aggregate included Membership, roles, access, and Invitation.
   This suite deliberately tests only the new global User model: domain rules,
   persisted schema, generated gesso.model plumbing, dependency guards,
   uniqueness assertions, planners, and the stable user.core boundary."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.fx :as fx]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.tx :as model.tx]
   [malli.core :as m]
   [net.humanhelp.site.model.user.core :as user]
   [net.humanhelp.site.model.user.domain :as domain]
   [net.humanhelp.site.model.user.schema :as user.schema])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def user-id
  (UUID/fromString "00000000-0000-0000-0000-000000000001"))

(def other-user-id
  (UUID/fromString "00000000-0000-0000-0000-000000000002"))

(def missing-user-id
  (UUID/fromString "00000000-0000-0000-0000-000000000099"))

(def generated-user-id
  (UUID/fromString "70000000-0000-7000-8000-000000000001"))

(def actor-id
  (UUID/fromString "50000000-0000-0000-0000-000000000001"))

(def canonical-phone "+12065550123")
(def replacement-phone "+12065550124")
(def canonical-email "person@example.com")
(def replacement-email "replacement@example.com")

(def t-before (Instant/parse "2026-06-30T23:59:00Z"))
(def t0 (Instant/parse "2026-07-01T00:00:00Z"))
(def t1 (Instant/parse "2026-07-01T00:01:00Z"))
(def t2 (Instant/parse "2026-07-01T00:02:00Z"))
(def t3 (Instant/parse "2026-07-01T00:03:00Z"))

(defn- after [model-command]
  (command/after model-command))

(defn- user-document
  ([]
   (user-document {}))
  ([overrides]
   (after
    (domain/create-user-command
     (merge {:id user-id :now t0} overrides)))))

(defn- verified-user []
  (user-document
   {:phone canonical-phone
    :email canonical-email
    :display-name "Person"
    :phone-verified? true
    :email-verified? true}))

(defn- error-type [f]
  (try
    (f)
    ::did-not-throw
    (catch Throwable error
      (loop [error error]
        (when error
          (or (:error/type (ex-data error))
              (recur (ex-cause error))))))))

(defn- plan-fragment [plan]
  (:transaction-fragment plan))

(defn- plan-command [plan]
  (first (:commands (plan-fragment plan))))

(defn- plan-change [plan]
  (first (:changes (plan-fragment plan))))

(defn- plan-assertions [plan]
  (:assertions (plan-fragment plan)))

(defn- normalize-plan [plan]
  (model.tx/normalize-plan
   (merge (plan-fragment plan)
          (:transaction-options plan))))

(defn- with-users* [documents f]
  (let [documents (vec documents)
        by-id (into {} (map (juxt :xt/id identity)) documents)]
    (with-redefs
     [model/load-by-id
      (fn [descriptor _ctx id]
        (when (= :user (:entity-type descriptor))
          (get by-id id)))

      model/load-by-lookup
      (fn [descriptor _ctx field value]
        (when (= :user (:entity-type descriptor))
          (first (filter #(= value (get % field)) documents))))]
      (f))))

(defmacro with-users [documents & body]
  `(with-users* ~documents (fn [] ~@body)))

;; =============================================================================
;; Values and construction
;; =============================================================================

(deftest value-normalization-test
  (testing "phone and email normalization"
    (is (= canonical-phone
           (user/normalize-phone "  +12065550123  ")))
    (is (= canonical-email
           (user/normalize-email "  PERSON@EXAMPLE.COM  ")))
    (is (nil? (user/normalize-phone "   ")))
    (is (nil? (user/normalize-email "   ")))
    (is (user/phone? canonical-phone))
    (is (user/email? canonical-email))
    (is (false? (user/phone? "2065550123")))
    (is (false? (user/email? "not-an-email"))))

  (testing "blank display names remain invalid"
    (is (= "Person Name"
           (user/normalize-display-name "  Person Name  ")))
    (is (= ""
           (user/normalize-display-name "   ")))
    (is (user/display-name? "Person Name"))
    (is (false? (user/display-name? "   "))))

  (testing "non-string values are not silently erased"
    (is (= 42 (user/normalize-phone 42)))
    (is (= 42 (user/normalize-email 42)))
    (is (= 42 (user/normalize-display-name 42)))
    (is (false? (user/phone? 42)))
    (is (false? (user/email? 42)))
    (is (false? (user/display-name? 42)))))

(deftest zero-contact-user-test
  (let [document (user-document)]
    (testing "User identity is independent of contact/authentication method"
      (is (= user-id (user/user-id document)))
      (is (user/active? document))
      (is (false? (user/has-phone? document)))
      (is (false? (user/has-email? document)))
      (is (false? (user/has-contact? document)))
      (is (false? (user/has-verified-contact? document))))

    (testing "initial version is conventional"
      (is (= 0 (:user/revision document)))
      (is (= t0 (:user/created-at document)))
      (is (= t0 (:user/updated-at document))))

    (testing "domain and Malli accept the same persisted document"
      (is (domain/document-consistent? document))
      (is (m/validate user.schema/user-document-schema document)))))

(deftest create-contact-test
  (let [document
        (user-document
         {:phone "  +12065550123  "
          :email "  PERSON@EXAMPLE.COM  "
          :display-name "  Person  "
          :phone-verified? true
          :email-verified? true})]
    (is (= canonical-phone (user/user-phone document)))
    (is (= canonical-email (user/user-email document)))
    (is (= "Person" (user/user-display-name document)))
    (is (= t0 (:user/phone-verified-at document)))
    (is (= t0 (:user/email-verified-at document)))
    (is (user/phone-verified? document))
    (is (user/email-verified? document))
    (is (user/has-verified-contact? document))
    (is (m/validate user.schema/user-document-schema document))))

(deftest create-validation-test
  (doseq [input
          [{:id user-id :display-name "   " :now t0}
           {:id user-id :phone-verified? true :now t0}
           {:id user-id :email-verified? true :now t0}
           {:id user-id :phone "2065550123" :now t0}
           {:id user-id :email "not-an-email" :now t0}
           {:now t0}
           {:id user-id}]]
    (is (= :user/invalid-create-input
           (error-type
            #(domain/create-user-command input)))))

  (is (false?
       (m/validate
        user.schema/user-document-schema
        (assoc (user-document) :unexpected/value true)))))

(deftest corrupt-document-test
  (let [document (user-document)]
    (doseq [corrupt
            [(assoc document :user/phone-verified-at t0)
             (assoc document :user/email-verified-at t0)
             (assoc document :user/status :suspended)
             (assoc document :user/status :deleted)
             (assoc document
                    :user/phone canonical-phone
                    :user/phone-verified-at t1)
             (assoc document
                    :user/status :suspended
                    :user/suspended-at t1)
             (assoc document :user/suspended-at t0)
             (assoc document :user/deleted-at t0)]]
      (is (false? (domain/document-consistent? corrupt))))))

;; =============================================================================
;; Domain commands
;; =============================================================================

(deftest profile-command-test
  (let [original (user-document)
        command
        (domain/edit-profile-command
         original
         {:display-name "  Person Name  "
          :now t1})
        changed (after command)
        removed
        (after
         (domain/edit-profile-command
          changed
          {:display-name nil
           :now t2}))]
    (is (command/update? command))
    (is (= :edit-profile (command/operation command)))
    (is (= "Person Name" (user/user-display-name changed)))
    (is (= 1 (:user/revision changed)))
    (is (= t1 (:user/updated-at changed)))
    (is (nil? (user/user-display-name removed)))
    (is (= 2 (:user/revision removed)))

    (is (= :user/invalid-input
           (error-type
            #(domain/edit-profile-command
              original
              {:display-name "   "
               :now t1}))))))

(deftest contact-command-test
  (let [original (verified-user)
        phone-replaced
        (after
         (domain/replace-phone-command
          original
          {:phone "  +12065550124  "
           :now t1}))
        email-replaced
        (after
         (domain/replace-email-command
          phone-replaced
          {:email "  REPLACEMENT@EXAMPLE.COM  "
           :now t2}))
        without-phone
        (after
         (domain/remove-phone-command
          original
          {:now t1}))
        without-contact
        (after
         (domain/remove-email-command
          without-phone
          {:now t2}))]
    (testing "replacement canonicalizes and clears only matching verification"
      (is (= replacement-phone (user/user-phone phone-replaced)))
      (is (false? (user/phone-verified? phone-replaced)))
      (is (user/email-verified? phone-replaced))
      (is (= replacement-email (user/user-email email-replaced)))
      (is (false? (user/email-verified? email-replaced)))
      (is (= 2 (:user/revision email-replaced))))

    (testing "zero-contact identity remains valid after removals"
      (is (false? (user/has-contact? without-contact)))
      (is (false? (user/has-verified-contact? without-contact)))
      (is (domain/document-consistent? without-contact))
      (is (m/validate user.schema/user-document-schema without-contact)))

    (testing "same canonical replacement is rejected"
      (is (= :user/contact-unchanged
             (error-type
              #(domain/replace-phone-command
                original
                {:phone canonical-phone :now t1}))))
      (is (= :user/contact-unchanged
             (error-type
              #(domain/replace-email-command
                original
                {:email "PERSON@EXAMPLE.COM" :now t1})))))))

(deftest verification-command-test
  (let [original
        (user-document
         {:phone canonical-phone
          :email canonical-email})
        phone-verified
        (after
         (domain/verify-phone-command
          original
          {:phone canonical-phone :now t1}))
        fully-verified
        (after
         (domain/verify-email-command
          phone-verified
          {:email "PERSON@EXAMPLE.COM" :now t2}))]
    (is (user/phone-verified? phone-verified))
    (is (user/email-verified? fully-verified))
    (is (= t1 (:user/phone-verified-at phone-verified)))
    (is (= t2 (:user/email-verified-at fully-verified)))

    (is (= :user/verification-target-mismatch
           (error-type
            #(domain/verify-phone-command
              original
              {:phone replacement-phone :now t1}))))

    (is (= :user/phone-already-verified
           (error-type
            #(domain/verify-phone-command
              phone-verified
              {:phone canonical-phone :now t2}))))

    (is (= :user/email-already-verified
           (error-type
            #(domain/verify-email-command
              fully-verified
              {:email canonical-email :now t3}))))))

(deftest lifecycle-command-test
  (let [original (user-document)
        suspend-command
        (domain/suspend-user-command
         original
         {:actor-id actor-id
          :reason :test/suspended
          :now t1})
        suspended (after suspend-command)
        reactivate-command
        (domain/reactivate-user-command suspended {:now t2})
        reactivated (after reactivate-command)
        delete-command
        (domain/delete-user-command
         suspended
         {:actor-id actor-id
          :reason :test/deleted
          :now t2})
        deleted (after delete-command)]
    (testing "suspend"
      (is (= :suspend (command/operation suspend-command)))
      (is (user/suspended? suspended))
      (is (= t1 (:user/suspended-at suspended)))
      (is (= actor-id (:user/suspended-by suspended)))
      (is (= :test/suspended (:user/suspension-reason suspended))))

    (testing "reactivate"
      (is (= :reactivate (command/operation reactivate-command)))
      (is (user/active? reactivated))
      (is (nil? (:user/suspended-at reactivated)))
      (is (nil? (:user/suspended-by reactivated)))
      (is (nil? (:user/suspension-reason reactivated))))

    (testing "delete"
      (is (= :delete (command/operation delete-command)))
      (is (user/deleted? deleted))
      (is (= t2 (:user/deleted-at deleted)))
      (is (= actor-id (:user/deleted-by deleted)))
      (is (nil? (:user/suspended-at deleted)))
      (is (domain/document-consistent? deleted)))

    (testing "invalid transitions"
      (is (= :user/not-active
             (error-type
              #(domain/suspend-user-command suspended {:now t2}))))
      (is (= :user/already-active
             (error-type
              #(domain/reactivate-user-command original {:now t1}))))
      (is (= :user/deleted
             (error-type
              #(domain/delete-user-command deleted {:now t3}))))
      (is (= :user/deleted
             (error-type
              #(domain/edit-profile-command
                deleted
                {:display-name "No" :now t3})))))))

(deftest update-time-test
  (is (= :user/invalid-time
         (error-type
          #(domain/edit-profile-command
            (user-document {:display-name "Person"})
            {:display-name "Changed"
             :now t-before})))))

;; =============================================================================
;; Descriptor and generated model
;; =============================================================================

(deftest descriptor-and-module-test
  (testing "descriptor is the complete conventional model declaration"
    (is (model/descriptor? user.schema/user-descriptor))
    (is (= :user (:entity-type user.schema/user-descriptor)))
    (is (= {:graph-key :user/id}
           (:identity user.schema/user-descriptor)))
    (is (= domain/version (:version user.schema/user-descriptor)))
    (is (= [:user/phone :user/email]
           (:lookups user.schema/user-descriptor))))

  (testing "generated registry contains entity and projected Graph values"
    (doseq [key
            [:user
             :user/id
             :user/doc
             :user/found?
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
             :user/deletion-reason]]
      (is (contains? user/schema key))))

  (testing "User intentionally contributes no generated mutation handlers"
    (is (false? (contains? user/module :biff.fx/handlers))))

  (testing "Graph is entirely descriptor-generated"
    (let [expected
          #{(model/by-id-resolver-id user.schema/user-descriptor)
            (model/fields-resolver-id user.schema/user-descriptor)
            (model/lookup-resolver-id
             user.schema/user-descriptor
             :user/phone)
            (model/lookup-resolver-id
             user.schema/user-descriptor
             :user/email)}
          actual (set (map :biff.graph/id user/resolvers))]
      (is (= expected actual))
      (is (= 4 (count user/resolvers))))))

;; =============================================================================
;; Stable core reads
;; =============================================================================

(deftest core-read-api-test
  (let [document (verified-user)
        other
        (user-document
         {:id other-user-id
          :email replacement-email})]
    (with-users
     [document other]

     (testing "ID reads"
       (is (= document (user/user {} user-id)))
       (is (= document (user/require-user {} user-id)))
       (is (nil? (user/user {} missing-user-id)))
       (is (= :user/not-found
              (error-type
               #(user/require-user {} missing-user-id))))
       (is (= :user.core/invalid-user-id
              (error-type
               #(user/user {} "not-a-uuid")))))

     (testing "phone read boundary"
       (is (= document
              (user/user-by-phone {} "  +12065550123  ")))
       (is (= document
              (user/require-user-by-phone {} canonical-phone)))
       (is (= :user.core/invalid-phone
              (error-type
               #(user/user-by-phone {} "2065550123")))))

     (testing "email read boundary"
       (is (= document
              (user/user-by-email {} " PERSON@EXAMPLE.COM ")))
       (is (= other
              (user/require-user-by-email
               {}
               "REPLACEMENT@EXAMPLE.COM")))
       (is (= :user/not-found
              (error-type
               #(user/require-user-by-email
                 {}
                 "missing@example.com"))))
       (is (= :user.core/invalid-email
              (error-type
               #(user/user-by-email {} "not-an-email"))))))))

(deftest core-document-facts-test
  (let [document (verified-user)]
    (is (= user-id (user/user-id document)))
    (is (= canonical-phone (user/user-phone document)))
    (is (= canonical-email (user/user-email document)))
    (is (= "Person" (user/user-display-name document)))
    (is (= :active (user/user-status document)))
    (is (user/active? document))
    (is (false? (user/suspended? document)))
    (is (false? (user/deleted? document)))
    (is (user/has-phone? document))
    (is (user/has-email? document))
    (is (user/has-contact? document))
    (is (user/phone-verified? document))
    (is (user/email-verified? document))
    (is (user/has-verified-contact? document))))

;; =============================================================================
;; Cross-model dependency
;; =============================================================================

(deftest user-dependency-test
  (let [document (verified-user)]
    (with-users
     [document]

     (let [{:keys [user transaction-fragment]}
           (user/require-user-dependency {} user-id)
           guard (first (:guards transaction-fragment))]
       (is (= document user))
       (is (= [[:user user-id]]
              (mapv command/guard-target
                    (:guards transaction-fragment))))
       (is (= {:model/id user-id
               :model/checks
               [[:user/revision 0]
                [:user/updated-at t0]]}
              (:model/expected guard))))

     (is (nil? (user/user-dependency {} missing-user-id)))
     (is (= :user/not-found
            (error-type
             #(user/require-user-dependency
               {}
               missing-user-id)))))))

(deftest dependency-composition-test
  (let [document (user-document {:display-name "Person"})]
    (with-users
     [document]

     (let [dependency
           (user/require-user-dependency {} user-id)
           plan
           (user/plan-edit-profile
            {:biff.fx/now t1}
            {:user-id user-id
             :display-name "Changed"})
           combined
           (model.tx/compose
            (:transaction-fragment dependency)
            (plan-fragment plan))
           normalized
           (model.tx/normalize-plan
            (merge combined (:transaction-options plan)))
           effective
           (model.tx/effective-guards
            (:commands normalized)
            (:guards normalized))]
       (is (= [[:user user-id]]
              (mapv command/guard-target
                    (:guards normalized))))
       (is (empty? effective))))))

;; =============================================================================
;; Planning
;; =============================================================================

(deftest create-plan-test
  (with-redefs
   [fx/uuid7 (fn [_seed _now] [generated-user-id])]

   (let [plan
         (user/plan-create-user
          {:biff.fx/seed 7
           :biff.fx/now t1}
          {:phone canonical-phone
           :email "PERSON@EXAMPLE.COM"
           :display-name "  Created Person  "})
         model-command (plan-command plan)
         document (after model-command)]
     (is (command/create? model-command))
     (is (= :user (:model/entity-type model-command)))
     (is (= generated-user-id (:xt/id document)))
     (is (= canonical-email (:user/email document)))
     (is (= "Created Person" (:user/display-name document)))
     (is (= document (get-in plan [:result :user])))

     (is (= [(model.tx/assert-none
              :user
              [:= :user/phone canonical-phone])
             (model.tx/assert-none
              :user
              [:= :user/email canonical-email])]
            (plan-assertions plan)))

     (is (= {:topic :user
             :id generated-user-id
             :change/kind :created
             :user/operation :create
             :user/id generated-user-id
             :user/status :active
             :user/revision 0}
            (plan-change plan)))

     (is (= {:coalesce-key [:user generated-user-id]}
            ((:entry-fn (:transaction-options plan))
             (plan-change plan)))))))

(deftest zero-contact-create-plan-test
  (with-redefs
   [fx/uuid7 (fn [_seed _now] [generated-user-id])]

   (let [plan
         (user/plan-create-user
          {:biff.fx/seed 7
           :biff.fx/now t1}
          {})]
     (is (empty? (plan-assertions plan)))
     (is (false?
          (user/has-contact?
           (get-in plan [:result :user])))))))

(deftest contact-update-plan-test
  (with-users
   [(verified-user)]

   (let [plan
         (user/plan-replace-email
          {:biff.fx/now t1}
          {:user-id user-id
           :email "REPLACEMENT@EXAMPLE.COM"})
         model-command (plan-command plan)
         changed (after model-command)]
     (is (command/update? model-command))
     (is (= :replace-email
            (command/operation model-command)))
     (is (= replacement-email (:user/email changed)))
     (is (false? (user/email-verified? changed)))
     (is (empty? (:guards (plan-fragment plan))))

     (is (= [(model.tx/assert-none
              :user
              [:= :user/email replacement-email])]
            (plan-assertions plan)))

     (is (= {:topic :user
             :id user-id
             :change/kind :updated
             :user/operation :replace-email
             :user/id user-id
             :user/status :active
             :user/revision 1}
            (plan-change plan))))))

(deftest non-contact-plans-have-no-uniqueness-assertions-test
  (let [active
        (user-document
         {:phone canonical-phone
          :display-name "Person"})
        suspended
        (after
         (domain/suspend-user-command active {:now t1}))]

    (with-users
     [active]
     (doseq [plan
             [(user/plan-edit-profile
               {:biff.fx/now t1}
               {:user-id user-id
                :display-name "Changed"})
              (user/plan-verify-phone
               {:biff.fx/now t1}
               {:user-id user-id
                :phone canonical-phone})
              (user/plan-remove-phone
               {:biff.fx/now t1}
               {:user-id user-id})
              (user/plan-suspend-user
               {:biff.fx/now t1}
               {:user-id user-id})]]
       (is (empty? (plan-assertions plan)))))

    (with-users
     [suspended]
     (is (empty?
          (plan-assertions
           (user/plan-reactivate-user
            {:biff.fx/now t2}
            {:user-id user-id})))))))

(deftest planner-errors-test
  (is (= :user.fx/missing-seed
         (error-type
          #(user/plan-create-user
            {:biff.fx/now t1}
            {}))))

  (is (= :user.fx/missing-now
         (error-type
          #(user/plan-create-user
            {:biff.fx/seed 1}
            {}))))

  (with-users
   [(user-document)]
   (is (= :user.fx/missing-now
          (error-type
           #(user/plan-edit-profile
             {}
             {:user-id user-id
              :display-name "Changed"})))))

  (with-users
   []
   (is (= :user/not-found
          (error-type
           #(user/plan-edit-profile
             {:biff.fx/now t1}
             {:user-id missing-user-id
              :display-name "Changed"})))))

  (is (= :user/invalid-user-id
         (error-type
          #(user/plan-edit-profile
            {:biff.fx/now t1}
            {:user-id "bad-id"
             :display-name "Changed"})))))

(deftest public-planner-operation-test
  (let [active (verified-user)
        unverified
        (user-document
         {:phone canonical-phone
          :email canonical-email})
        suspended
        (after
         (domain/suspend-user-command active {:now t1}))]

    (doseq [[planner document now input expected-operation]
            [[user/plan-edit-profile
              active t1
              {:user-id user-id :display-name "Changed"}
              :edit-profile]
             [user/plan-replace-phone
              active t1
              {:user-id user-id :phone replacement-phone}
              :replace-phone]
             [user/plan-replace-email
              active t1
              {:user-id user-id :email replacement-email}
              :replace-email]
             [user/plan-remove-phone
              active t1
              {:user-id user-id}
              :remove-phone]
             [user/plan-remove-email
              active t1
              {:user-id user-id}
              :remove-email]
             [user/plan-verify-phone
              unverified t1
              {:user-id user-id :phone canonical-phone}
              :verify-phone]
             [user/plan-verify-email
              unverified t1
              {:user-id user-id :email canonical-email}
              :verify-email]
             [user/plan-suspend-user
              active t1
              {:user-id user-id}
              :suspend]
             [user/plan-reactivate-user
              suspended t2
              {:user-id user-id}
              :reactivate]
             [user/plan-delete-user
              active t1
              {:user-id user-id}
              :delete]]]

      (with-users
       [document]
       (let [plan (planner {:biff.fx/now now} input)
             model-command (plan-command plan)]
         (is (command/update? model-command))
         (is (= :user (:model/entity-type model-command)))
         (is (= expected-operation
                (command/operation model-command)))
         (is (= user-id (:model/id model-command)))
         (is (= (after model-command)
                (get-in plan [:result :user]))))))))

(deftest plans-normalize-through-gesso-model-test
  (with-users
   [(verified-user)]

   (doseq [plan
           [(user/plan-edit-profile
             {:biff.fx/now t1}
             {:user-id user-id
              :display-name "Changed"})
            (user/plan-replace-phone
             {:biff.fx/now t1}
             {:user-id user-id
              :phone replacement-phone})
            (user/plan-delete-user
             {:biff.fx/now t1}
             {:user-id user-id})]]
     (let [normalized (normalize-plan plan)]
       (is (= 1 (count (:commands normalized))))
       (is (every? map? (:assertions normalized)))
       (is (= 1 (count (:changes normalized))))
       (is (ifn? (:entry-fn normalized)))))))
