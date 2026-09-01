(ns net.humanhelp.site.model.request.choreo
  "Model-owned choreography for optimistic Request claim.

   This namespace is deliberately above Request's public model boundary:

     request.fx
       -> request.core/claim
       -> request.choreo

   It never requires Request domain, schema, graph, or FX implementation
   namespaces. Request policy, authorization, aggregate reads, commit-time
   guards, and the atomic Request + primary-assignment transition remain owned
   by request.core/claim and the model beneath it.

   The choreography specializes Gesso Live optimistic protocol v3 rather than
   inventing a second application protocol. The static choreography roles are:

     :helper
       the browser-side role that may derive a provisional local projection and
       communicate a semantic :request/claim command;

     :request-authority
       the trusted role that invokes request.core/claim and establishes the
       authoritative settlement.

   Role is not principal, actor, host, or authority identity. In particular:

   - a browser actor carrying the :helper projection does not establish an
     authenticated HumanHelp principal;
   - the trusted optimistic server binds principal from authenticated server
     context before the operation adapter runs;
   - a concrete Aleph/web node is only a physical host for the
     :request-authority projection;
   - browser-supplied operation arguments, observed basis, scope, fact versions,
     role names, and provisional state remain untrusted protocol data.

   The browser's observed basis records the authority frontier from which its
   semantic command was formed. It is intentionally not turned into a generic
   stale-command rejection rule here. request.core/claim rereads/revalidates the
   current Request world and decides whether the claim remains valid.

   Successful authoritative execution returns a protocol-v3 :confirmed
   settlement observation at the XTDB basis established by the real commit.
   Unknown/pre-commit/model exceptions are not blindly translated into a
   :rejected settlement, because doing so would risk hiding infrastructure or
   programmer failures. Most importantly, committed post-commit delivery
   failures are allowed to escape unchanged and can never be mislabeled as a
   rolled-back claim.

   Views consume claim-capability as inert semantic affordance data. Rendering
   that capability grants no authority; the trusted server registry and Request
   model remain the authority boundary."
  (:require
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.server :as optimistic-server]
   [net.humanhelp.site.model.request.core :as request]))

;; =============================================================================
;; Stable Request claim choreography vocabulary
;; =============================================================================

(def claim-operation
  "Public semantic Request operation coordinated by this choreography."
  :request/claim)

(def claim-choreography-name
  "Stable global choreography name for optimistic Request claim."
  :request/claim-optimistic)

(def helper-role
  "Static browser-side choreography role.

   This does not identify a User principal or a particular browser actor."
  :helper)

(def request-authority-role
  "Static trusted choreography role for Request authority.

   Multiple physical web nodes may execute this same logical role."
  :request-authority)

(def claim-plan-key
  "Application plan key carried by the inert view capability."
  :request/claim)

(def claim-choreography-options
  "The one operation-specific specialization of Gesso's reusable optimistic
   command choreography.

   Keep Gesso's standard derive/resolve local action ids. optimistic.server's
   trusted operation registry projects its authority plan from exactly
   name/operation/role data; inventing different application local-action ids
   here would make the browser and authority plans projections of different
   global choreographies."
  {:name claim-choreography-name
   :operation claim-operation
   :browser-role helper-role
   :authority-role request-authority-role})

;; =============================================================================
;; Verified global choreography and browser projection
;; =============================================================================

(def claim-choreography
  "The declarative global Request claim choreography.

   Verification/projection is performed by claim-browser-plan below and again
   by the trusted operation registry for its authority projection. This value is
   useful for diagnostics/tests and contains no application authorization."
  (optimistic-choreo/command-choreography
   claim-choreography-options))

(def claim-entry-knowledge
  "Precise initial knowledge assumptions for the Request claim choreography.

   Only the Helper/browser role initially knows the semantic command facts.
   Request authority learns the declared command facts through the choreography
   communication boundary."
  (optimistic-choreo/command-entry-knowledge
   claim-choreography-options))

(def claim-browser-plan
  "Canonical verified ExecutablePlan for the :helper role."
  (optimistic-choreo/command-plan
   claim-choreography-options
   helper-role))

;; =============================================================================
;; View-facing inert capability
;; =============================================================================

(def claim-capability
  "Inert application capability used by prepared view models/rendering.

   Binding requires per-render :arguments and :observed-basis and may add
   ordinary scope/fact-version/target correlation data. The resulting browser
   action contains neither authenticated principal nor trusted authority,
   command/execution identity, or settlement state.

   Rollback/timeout/physical replacement policy is intentionally not invented
   here; those optional presentation/runtime policies can be added only when the
   prepared-view/browser realization has a concrete requirement."
  (capability/operation-capability
   {:operation claim-operation
    :plan-key claim-plan-key}))

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

(defn- require-committed-claim!
  [claim-result]
  (when-not (map? claim-result)
    (throw
     (choreography-error
      :invalid-claim-result
      "request.core/claim returned a non-map authoritative result."
      {:result claim-result})))

  (when-not (= :committed
               (:commit/status claim-result))
    (throw
     (choreography-error
      :claim-not-committed
      "A successful Request claim choreography invocation must represent a committed model transition."
      {:commit/status (:commit/status claim-result)
       :result-keys (set (keys claim-result))})))

  claim-result)

(defn- require-claim-request!
  [claim-result]
  (let [request-document
        (:request claim-result)]
    (when-not (request/request-document?
               request-document)
      (throw
       (choreography-error
        :invalid-authoritative-request
        "Committed Request claim result does not contain a canonical Request document."
        {:request request-document})))

    (when-not (request/claimed?
               request-document)
      (throw
       (choreography-error
        :unexpected-authoritative-request-state
        "Committed Request claim result is not in the claimed lifecycle state."
        {:request/id
         (request/request-id request-document)

         :request/status
         (request/status request-document)})))

    request-document))

(defn- require-primary-assignment!
  [claim-result request-document]
  (let [assignment
        (:primary-assignment claim-result)]
    (when-not (request/assignment-document?
               assignment)
      (throw
       (choreography-error
        :invalid-authoritative-primary-assignment
        "Committed Request claim result does not contain a canonical RequestAssignment document."
        {:primary-assignment assignment})))

    (when-not (request/active-primary-assignment?
               assignment)
      (throw
       (choreography-error
        :unexpected-authoritative-assignment-state
        "Committed Request claim result does not contain an active primary assignment."
        {:request-assignment/id
         (request/assignment-id assignment)

         :request-assignment/role
         (request/assignment-role assignment)

         :request-assignment/status
         (request/assignment-status assignment)})))

    (when-not (= (request/request-id request-document)
                 (request/assignment-request-id assignment))
      (throw
       (choreography-error
        :claim-result-aggregate-mismatch
        "Committed Request and primary assignment do not belong to the same Request aggregate."
        {:request/id
         (request/request-id request-document)

         :request-assignment/request
         (request/assignment-request-id assignment)})))

    assignment))

(defn- claim-projection
  "Return the portable model-owned facts needed to reconcile the provisional
   claim with committed Request authority.

   This deliberately projects through request.core accessors rather than
   exposing persisted documents or depending on Request internals."
  [request-document primary-assignment]
  {:request/id
   (request/request-id
    request-document)

   :request/status
   (request/status
    request-document)

   :request/revision
   (request/revision
    request-document)

   :request/primary-assignment
   {:request-assignment/id
    (request/assignment-id
     primary-assignment)

    :request-assignment/request
    (request/assignment-request-id
     primary-assignment)

    :request-assignment/helper
    (request/assignment-helper-id
     primary-assignment)

    :request-assignment/role
    (request/assignment-role
     primary-assignment)

    :request-assignment/status
    (request/assignment-status
     primary-assignment)

    :request-assignment/source
    (request/assignment-source
     primary-assignment)

    :request-assignment/revision
    (request/assignment-revision
     primary-assignment)}})

(defn- claim-fact-versions
  [request-document primary-assignment]
  {:request/revision
   (request/revision
    request-document)

   :request-assignment/revision
   (request/assignment-revision
    primary-assignment)})

(defn- committed-claim-basis
  "Return the trusted XTDB basis established by the successful claim commit.

   request.core exposes generic Gesso Live progression because model code should
   not collapse potentially composed requirements. At this application
   authority boundary we know the storage authority is XTDB, so the trusted XTDB
   adapter is the correct layer to select the strongest comparable basis.

   Missing/incompatible progression fails closed. A :confirmed optimistic
   settlement may not fabricate an authoritative basis merely because the model
   transition itself succeeded."
  [claim-result]
  (let [progression
        (:progression claim-result)

        basis
        (when (some? progression)
          (xtdb-live/strongest-required-basis
           progression))]
    (when-not (some? basis)
      (throw
       (choreography-error
        :missing-commit-progression
        "Committed Request claim cannot be represented as confirmed optimistic authority without transaction-established XTDB progression."
        {:commit/status
         (:commit/status claim-result)

         :progression
         progression})))

    basis))

(defn- confirmed-authoritative-claim
  [claim-result]
  (let [claim-result
        (require-committed-claim!
         claim-result)

        request-document
        (require-claim-request!
         claim-result)

        primary-assignment
        (require-primary-assignment!
         claim-result
         request-document)

        basis
        (committed-claim-basis
         claim-result)]
    (protocol/authoritative
     {:presence :present
      :basis basis
      :projection
      (claim-projection
       request-document
       primary-assignment)
      :fact-versions
      (claim-fact-versions
       request-document
       primary-assignment)})))

;; =============================================================================
;; Trusted operation adapter
;; =============================================================================

(defn- execute-claim!
  "Trusted optimistic.server operation adapter.

   optimistic.server has already:
   - authenticated/bound a typed principal through its server-side
     :principal-fn;
   - selected this operation from the trusted registry;
   - validated command/execution correlation;
   - resumed the :request-authority projection.

   The command's :arguments and observed basis are still browser-originated
   protocol context. We pass only :arguments to Request's public semantic
   operation. request.core/claim rereads/revalidates current authority and owns
   all policy.

   Deliberately do not catch and reinterpret exceptions here. In particular, a
   :commit/status :committed post-commit delivery failure must escape unchanged
   rather than becoming a false :rejected or :failed settlement."
  [{:keys [ctx arguments]}]
  (let [claim-result
        (request/claim
         ctx
         arguments)]
    {:resolution :confirmed
     :authoritative
     (confirmed-authoritative-claim
      claim-result)
     :outcome :request/claimed}))

(def claim-operation-entry
  "Trusted registry entry for :request/claim.

   This is not a complete server by itself: the surrounding HumanHelp server
   supplies the authenticated-context :principal-fn. Constructing this entry
   verifies/projects the canonical :request-authority ExecutablePlan once and
   retains it for request-time execution."
  (optimistic-server/operation
   {:name claim-choreography-name
    :operation claim-operation
    :browser-role helper-role
    :authority-role request-authority-role
    :execute! execute-claim!}))

(def claim-authority-plan
  "Canonical verified ExecutablePlan actually retained by the trusted Request
   claim operation entry."
  (:authority-plan
   claim-operation-entry))
