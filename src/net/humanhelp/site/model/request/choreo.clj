(ns net.humanhelp.site.model.request.choreo
  "Model-owned choreography for optimistic Request lifecycle operations.

   This namespace is deliberately above Request's public model boundary:

     request.fx
       -> request.core
       -> request.choreo

   It never requires Request domain, schema, graph, or FX implementation
   namespaces. Request policy, authorization, aggregate reads, commit-time
   guards, and atomic authoritative transitions remain owned by request.core and
   the model beneath it.

   The choreography specializes Gesso Live optimistic protocol v3 rather than
   inventing a second application protocol. Browser-side choreography roles are
   semantic roles, not authorization tokens:

     :helper
       claim/unclaim/progress/complete operations;

     :requestor
       owner cancellation;

     :manager
       authoritative primary-helper reassignment;

     :request-authority
       the trusted role that invokes the corresponding request.core operation
       and establishes the authoritative settlement.

   Role is not principal, actor, host, or authority identity. In particular:

   - a browser actor carrying a :helper, :requestor, or :manager projection does
     not establish an authenticated HumanHelp principal or role membership;
   - the trusted optimistic server binds principal from authenticated server
     context before an operation adapter runs;
   - a concrete Aleph/web node is only a physical host for the
     :request-authority projection;
   - browser-supplied operation arguments, observed basis, scope, fact versions,
     role names, and provisional state remain untrusted protocol data.

   The browser's observed basis records the authority frontier from which its
   semantic command was formed. It is intentionally not turned into a generic
   stale-command rejection rule here. Every request.core operation rereads and
   revalidates current authority and decides whether the command remains valid.

   Successful authoritative execution returns a protocol-v3 :confirmed
   settlement observation at the XTDB basis established by the real commit.
   Unknown/pre-commit/model exceptions are not blindly translated into a
   :rejected settlement, because doing so would risk hiding infrastructure or
   programmer failures. Most importantly, committed post-commit delivery
   failures are allowed to escape unchanged and can never be mislabeled as a
   rolled-back mutation.

   Views consume the exported operation capabilities as inert semantic
   affordance data. Rendering a capability grants no authority; the trusted
   server registry and Request model remain the authority boundary."
  (:require
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.server :as optimistic-server]
   [net.humanhelp.site.model.request.core :as request]))

;; =============================================================================
;; Stable Request choreography vocabulary
;; =============================================================================

(def helper-role
  "Static browser-side choreography role.

   This does not identify a User principal or a particular browser actor."
  :helper)

(def requestor-role
  "Static browser-side choreography role for Request-owner commands.

   This is descriptive protocol structure only; the browser cannot establish
   Request ownership by claiming this role."
  :requestor)

(def manager-role
  "Static browser-side choreography role for manager commands.

   This is descriptive protocol structure only; request.core establishes real
   supervisor/administrator authority from trusted model context."
  :manager)

(def request-authority-role
  "Static trusted choreography role for Request authority.

   Multiple physical web nodes may execute this same logical role."
  :request-authority)

(def claim-operation :request/claim)
(def unclaim-operation :request/unclaim)
(def mark-on-the-way-operation :request/mark-on-the-way)
(def complete-operation :request/complete)
(def cancel-operation :request/cancel)
(def reassign-operation :request/reassign)

(def claim-choreography-name :request/claim-optimistic)
(def unclaim-choreography-name :request/unclaim-optimistic)
(def mark-on-the-way-choreography-name :request/mark-on-the-way-optimistic)
(def complete-choreography-name :request/complete-optimistic)
(def cancel-choreography-name :request/cancel-optimistic)
(def reassign-choreography-name :request/reassign-optimistic)

(def claim-plan-key claim-operation)
(def unclaim-plan-key unclaim-operation)
(def mark-on-the-way-plan-key mark-on-the-way-operation)
(def complete-plan-key complete-operation)
(def cancel-plan-key cancel-operation)
(def reassign-plan-key reassign-operation)

(defn- choreography-options
  [name operation browser-role]
  {:name name
   :operation operation
   :browser-role browser-role
   :authority-role request-authority-role})

(def claim-choreography-options
  (choreography-options claim-choreography-name claim-operation helper-role))

(def unclaim-choreography-options
  (choreography-options unclaim-choreography-name unclaim-operation helper-role))

(def mark-on-the-way-choreography-options
  (choreography-options
   mark-on-the-way-choreography-name
   mark-on-the-way-operation
   helper-role))

(def complete-choreography-options
  (choreography-options complete-choreography-name complete-operation helper-role))

(def cancel-choreography-options
  (choreography-options cancel-choreography-name cancel-operation requestor-role))

(def reassign-choreography-options
  (choreography-options reassign-choreography-name reassign-operation manager-role))

;; =============================================================================
;; Verified global choreographies and browser projections
;; =============================================================================

(defn- command-artifacts
  [options plan-key]
  {:choreography
   (optimistic-choreo/command-choreography options)

   :entry-knowledge
   (optimistic-choreo/command-entry-knowledge options)

   :browser-plan
   (optimistic-choreo/command-plan options (:browser-role options))

   :capability
   (capability/operation-capability
    {:operation (:operation options)
     :plan-key plan-key})})

(def ^:private claim-artifacts
  (command-artifacts claim-choreography-options claim-plan-key))

(def ^:private unclaim-artifacts
  (command-artifacts unclaim-choreography-options unclaim-plan-key))

(def ^:private mark-on-the-way-artifacts
  (command-artifacts
   mark-on-the-way-choreography-options
   mark-on-the-way-plan-key))

(def ^:private complete-artifacts
  (command-artifacts complete-choreography-options complete-plan-key))

(def ^:private cancel-artifacts
  (command-artifacts cancel-choreography-options cancel-plan-key))

(def ^:private reassign-artifacts
  (command-artifacts reassign-choreography-options reassign-plan-key))

(def claim-choreography (:choreography claim-artifacts))
(def unclaim-choreography (:choreography unclaim-artifacts))
(def mark-on-the-way-choreography (:choreography mark-on-the-way-artifacts))
(def complete-choreography (:choreography complete-artifacts))
(def cancel-choreography (:choreography cancel-artifacts))
(def reassign-choreography (:choreography reassign-artifacts))

(def claim-entry-knowledge (:entry-knowledge claim-artifacts))
(def unclaim-entry-knowledge (:entry-knowledge unclaim-artifacts))
(def mark-on-the-way-entry-knowledge (:entry-knowledge mark-on-the-way-artifacts))
(def complete-entry-knowledge (:entry-knowledge complete-artifacts))
(def cancel-entry-knowledge (:entry-knowledge cancel-artifacts))
(def reassign-entry-knowledge (:entry-knowledge reassign-artifacts))

(def claim-browser-plan (:browser-plan claim-artifacts))
(def unclaim-browser-plan (:browser-plan unclaim-artifacts))
(def mark-on-the-way-browser-plan (:browser-plan mark-on-the-way-artifacts))
(def complete-browser-plan (:browser-plan complete-artifacts))
(def cancel-browser-plan (:browser-plan cancel-artifacts))
(def reassign-browser-plan (:browser-plan reassign-artifacts))

;; =============================================================================
;; View-facing inert capabilities
;; =============================================================================

(def claim-capability
  "Inert capability for :request/claim."
  (:capability claim-artifacts))

(def unclaim-capability
  "Inert capability for :request/unclaim."
  (:capability unclaim-artifacts))

(def mark-on-the-way-capability
  "Inert capability for :request/mark-on-the-way."
  (:capability mark-on-the-way-artifacts))

(def complete-capability
  "Inert capability for :request/complete."
  (:capability complete-artifacts))

(def cancel-capability
  "Inert capability for :request/cancel."
  (:capability cancel-artifacts))

(def reassign-capability
  "Inert capability for :request/reassign."
  (:capability reassign-artifacts))

(def capabilities
  "Semantic Request operation -> inert optimistic capability.

   This is convenient for UI composition. It is not a trusted server registry
   and carries no authorization."
  {claim-operation claim-capability
   unclaim-operation unclaim-capability
   mark-on-the-way-operation mark-on-the-way-capability
   complete-operation complete-capability
   cancel-operation cancel-capability
   reassign-operation reassign-capability})

(def browser-plans
  "Semantic Request operation -> canonical browser ExecutablePlan."
  {claim-operation claim-browser-plan
   unclaim-operation unclaim-browser-plan
   mark-on-the-way-operation mark-on-the-way-browser-plan
   complete-operation complete-browser-plan
   cancel-operation cancel-browser-plan
   reassign-operation reassign-browser-plan})

;; =============================================================================
;; Trusted Request result -> authoritative protocol observation
;; =============================================================================

(defn- choreography-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :net.humanhelp.site.model.request.choreo/error
     :error/kind kind}
    data)))

(defn- require-committed-result!
  [operation result]
  (when-not (map? result)
    (throw
     (choreography-error
      (if (= operation claim-operation)
        :invalid-claim-result
        :invalid-operation-result)
      "request.core operation returned a non-map authoritative result."
      {:operation operation
       :result result})))

  (when-not (= :committed (:commit/status result))
    (throw
     (choreography-error
      (if (= operation claim-operation)
        :claim-not-committed
        :operation-not-committed)
      "A successful Request choreography invocation must represent a committed model transition."
      {:operation operation
       :commit/status (:commit/status result)
       :result-keys (set (keys result))})))

  result)

(defn- require-authoritative-request!
  [operation expected-status result]
  (let [request-document (:request result)]
    (when-not (request/request-document? request-document)
      (throw
       (choreography-error
        :invalid-authoritative-request
        "Committed Request operation result does not contain a canonical Request document."
        {:operation operation
         :request request-document})))

    (when-not (= expected-status (request/status request-document))
      (throw
       (choreography-error
        :unexpected-authoritative-request-state
        "Committed Request operation result has an unexpected lifecycle state."
        {:operation operation
         :expected-status expected-status
         :request/id (request/request-id request-document)
         :request/status (request/status request-document)})))

    request-document))

(defn- require-assignment-for-request!
  [operation request-document assignment]
  (when-not (request/assignment-document? assignment)
    (throw
     (choreography-error
      (if (= operation claim-operation)
        :invalid-authoritative-primary-assignment
        :invalid-authoritative-assignment)
      "Committed Request operation result contains a non-canonical RequestAssignment document."
      {:operation operation
       :assignment assignment})))

  (when-not (= (request/request-id request-document)
               (request/assignment-request-id assignment))
    (throw
     (choreography-error
      (if (= operation claim-operation)
        :claim-result-aggregate-mismatch
        :operation-result-aggregate-mismatch)
      "Committed Request and RequestAssignment do not belong to the same Request aggregate."
      {:operation operation
       :request/id (request/request-id request-document)
       :request-assignment/request
       (request/assignment-request-id assignment)})))

  assignment)

(defn- require-active-primary-assignment!
  [operation result request-document]
  (let [assignment
        (require-assignment-for-request!
         operation
         request-document
         (:primary-assignment result))]
    (when-not (request/active-primary-assignment? assignment)
      (throw
       (choreography-error
        :unexpected-authoritative-assignment-state
        "Committed Request operation result does not contain an active primary assignment."
        {:operation operation
         :request-assignment/id (request/assignment-id assignment)
         :request-assignment/role (request/assignment-role assignment)
         :request-assignment/status (request/assignment-status assignment)})))
    assignment))

(defn- require-ended-assignment!
  [operation request-document assignment]
  (let [assignment'
        (require-assignment-for-request!
         operation
         request-document
         assignment)]
    (when-not (request/assignment-ended? assignment')
      (throw
       (choreography-error
        :unexpected-authoritative-assignment-state
        "Committed Request operation expected an ended RequestAssignment."
        {:operation operation
         :request-assignment/id (request/assignment-id assignment')
         :request-assignment/role (request/assignment-role assignment')
         :request-assignment/status (request/assignment-status assignment')})))
    assignment'))

(defn- require-ended-assignments!
  [operation result request-document]
  (let [assignments (:assignments result)]
    (when-not (vector? assignments)
      (throw
       (choreography-error
        :invalid-authoritative-assignments
        "Committed Request operation result must contain a vector of ended RequestAssignments."
        {:operation operation
         :assignments assignments})))
    (mapv
     #(require-ended-assignment! operation request-document %)
     assignments)))

(defn- request-projection
  [request-document]
  {:request/id
   (request/request-id request-document)

   :request/status
   (request/status request-document)

   :request/revision
   (request/revision request-document)})

(defn- assignment-projection
  [assignment]
  {:request-assignment/id
   (request/assignment-id assignment)

   :request-assignment/request
   (request/assignment-request-id assignment)

   :request-assignment/helper
   (request/assignment-helper-id assignment)

   :request-assignment/role
   (request/assignment-role assignment)

   :request-assignment/status
   (request/assignment-status assignment)

   :request-assignment/source
   (request/assignment-source assignment)

   :request-assignment/revision
   (request/assignment-revision assignment)})

(defn- request-with-primary-projection
  [request-document primary-assignment]
  (assoc
   (request-projection request-document)
   :request/primary-assignment
   (assignment-projection primary-assignment)))

(defn- request-fact-versions
  [request-document]
  {:request/revision
   (request/revision request-document)})

(defn- request-with-primary-fact-versions
  [request-document primary-assignment]
  (assoc
   (request-fact-versions request-document)
   :request-assignment/revision
   (request/assignment-revision primary-assignment)))

(defn- committed-basis
  "Return the trusted XTDB basis established by a successful Request commit.

   request.core exposes generic Gesso Live progression because model code should
   not collapse potentially composed requirements. At this application authority
   boundary we know the storage authority is XTDB, so the trusted XTDB adapter
   selects the strongest comparable basis.

   Missing/incompatible progression fails closed. A :confirmed optimistic
   settlement may not fabricate an authoritative basis merely because the model
   transition itself succeeded."
  [operation result]
  (let [progression (:progression result)
        basis
        (when (some? progression)
          (xtdb-live/strongest-required-basis progression))]
    (when-not (some? basis)
      (throw
       (choreography-error
        :missing-commit-progression
        "Committed Request operation cannot be represented as confirmed optimistic authority without transaction-established XTDB progression."
        {:operation operation
         :commit/status (:commit/status result)
         :progression progression})))
    basis))

(defn- authoritative
  [operation result projection fact-versions]
  (protocol/authoritative
   {:presence :present
    :basis (committed-basis operation result)
    :projection projection
    :fact-versions fact-versions}))

(defn- confirmed-authoritative-claim
  [result]
  (let [result (require-committed-result! claim-operation result)
        request-document
        (require-authoritative-request! claim-operation :claimed result)
        primary-assignment
        (require-active-primary-assignment!
         claim-operation
         result
         request-document)]
    (authoritative
     claim-operation
     result
     (request-with-primary-projection request-document primary-assignment)
     (request-with-primary-fact-versions request-document primary-assignment))))

(defn- confirmed-authoritative-unclaim
  [result]
  (let [result (require-committed-result! unclaim-operation result)
        request-document
        (require-authoritative-request! unclaim-operation :open result)]
    (require-ended-assignments! unclaim-operation result request-document)
    (authoritative
     unclaim-operation
     result
     (request-projection request-document)
     (request-fact-versions request-document))))

(defn- confirmed-authoritative-mark-on-the-way
  [result]
  (let [result (require-committed-result! mark-on-the-way-operation result)
        request-document
        (require-authoritative-request!
         mark-on-the-way-operation
         :on-the-way
         result)]
    (authoritative
     mark-on-the-way-operation
     result
     (request-projection request-document)
     (request-fact-versions request-document))))

(defn- confirmed-authoritative-complete
  [result]
  (let [result (require-committed-result! complete-operation result)
        request-document
        (require-authoritative-request! complete-operation :done result)]
    (require-ended-assignments! complete-operation result request-document)
    (authoritative
     complete-operation
     result
     (request-projection request-document)
     (request-fact-versions request-document))))

(defn- confirmed-authoritative-cancel
  [result]
  (let [result (require-committed-result! cancel-operation result)
        request-document
        (require-authoritative-request! cancel-operation :cancelled result)]
    (require-ended-assignments! cancel-operation result request-document)
    (authoritative
     cancel-operation
     result
     (request-projection request-document)
     (request-fact-versions request-document))))

(defn- confirmed-authoritative-reassign
  [result]
  (let [result (require-committed-result! reassign-operation result)
        request-document
        (require-authoritative-request! reassign-operation :claimed result)
        primary-assignment
        (require-active-primary-assignment!
         reassign-operation
         result
         request-document)
        _
        (require-ended-assignment!
         reassign-operation
         request-document
         (:previous-primary-assignment result))
        previous-collaborator
        (:previous-collaborator-assignment result)]
    (when previous-collaborator
      (require-ended-assignment!
       reassign-operation
       request-document
       previous-collaborator))
    (authoritative
     reassign-operation
     result
     (request-with-primary-projection request-document primary-assignment)
     (request-with-primary-fact-versions request-document primary-assignment))))

;; =============================================================================
;; Trusted operation adapters
;; =============================================================================

(defn- execute-authoritative!
  "Execute one trusted Request operation and construct its confirmed settlement.

   optimistic.server has already authenticated/bound the principal, selected the
   operation from its trusted registry, validated command/execution correlation,
   and resumed the :request-authority projection.

   Browser arguments and observed basis remain protocol context. Only arguments
   are passed to the public Request semantic operation, which rereads and
   revalidates current authority and owns all business policy.

   Deliberately do not catch and reinterpret exceptions here. In particular, a
   :commit/status :committed post-commit delivery failure must escape unchanged
   rather than becoming a false :rejected or :failed settlement."
  [model-operation outcome authoritative-fn {:keys [ctx arguments]}]
  (let [result (model-operation ctx arguments)]
    {:resolution :confirmed
     :authoritative (authoritative-fn result)
     :outcome outcome}))

(defn- execute-claim!
  [trusted-context]
  (execute-authoritative!
   request/claim
   :request/claimed
   confirmed-authoritative-claim
   trusted-context))

(defn- execute-unclaim!
  [trusted-context]
  (execute-authoritative!
   request/unclaim
   :request/unclaimed
   confirmed-authoritative-unclaim
   trusted-context))

(defn- execute-mark-on-the-way!
  [trusted-context]
  (execute-authoritative!
   request/mark-on-the-way
   :request/on-the-way
   confirmed-authoritative-mark-on-the-way
   trusted-context))

(defn- execute-complete!
  [trusted-context]
  (execute-authoritative!
   request/complete
   :request/completed
   confirmed-authoritative-complete
   trusted-context))

(defn- execute-cancel!
  [trusted-context]
  (execute-authoritative!
   request/cancel
   :request/cancelled
   confirmed-authoritative-cancel
   trusted-context))

(defn- execute-reassign!
  [trusted-context]
  (execute-authoritative!
   request/reassign
   :request/reassigned
   confirmed-authoritative-reassign
   trusted-context))

(defn- operation-entry
  [options execute!]
  (optimistic-server/operation
   (assoc options :execute! execute!)))

(def claim-operation-entry
  (operation-entry claim-choreography-options execute-claim!))

(def unclaim-operation-entry
  (operation-entry unclaim-choreography-options execute-unclaim!))

(def mark-on-the-way-operation-entry
  (operation-entry
   mark-on-the-way-choreography-options
   execute-mark-on-the-way!))

(def complete-operation-entry
  (operation-entry complete-choreography-options execute-complete!))

(def cancel-operation-entry
  (operation-entry cancel-choreography-options execute-cancel!))

(def reassign-operation-entry
  (operation-entry reassign-choreography-options execute-reassign!))

(def operation-entries
  "Semantic Request operation -> trusted optimistic.server operation entry.

   The surrounding HumanHelp server supplies the authenticated-context
   :principal-fn. Browser capabilities and this map are deliberately separate:
   rendering an operation never registers or authorizes it."
  {claim-operation claim-operation-entry
   unclaim-operation unclaim-operation-entry
   mark-on-the-way-operation mark-on-the-way-operation-entry
   complete-operation complete-operation-entry
   cancel-operation cancel-operation-entry
   reassign-operation reassign-operation-entry})

(def claim-authority-plan (:authority-plan claim-operation-entry))
(def unclaim-authority-plan (:authority-plan unclaim-operation-entry))
(def mark-on-the-way-authority-plan
  (:authority-plan mark-on-the-way-operation-entry))
(def complete-authority-plan (:authority-plan complete-operation-entry))
(def cancel-authority-plan (:authority-plan cancel-operation-entry))
(def reassign-authority-plan (:authority-plan reassign-operation-entry))

(def authority-plans
  "Semantic Request operation -> canonical authority ExecutablePlan retained by
   the trusted registry entry."
  {claim-operation claim-authority-plan
   unclaim-operation unclaim-authority-plan
   mark-on-the-way-operation mark-on-the-way-authority-plan
   complete-operation complete-authority-plan
   cancel-operation cancel-authority-plan
   reassign-operation reassign-authority-plan})
