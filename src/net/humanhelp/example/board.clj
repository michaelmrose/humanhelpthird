(ns net.humanhelp.example.board
  "Production-model-backed read/view composition for the HumanHelp example app.

   This namespace is deliberately *not* a HumanHelp model. It owns only the
   example board's read composition and presentation-oriented view state.

   Authoritative semantics come directly from production namespaces:

     net.humanhelp.site.model.request.core
       Request documents, RequestAssignment documents, lifecycle predicates,
       ownership facts, and authoritative reads;

     net.humanhelp.site.model.user.core
       User documents used for display;

     net.humanhelp.site.model.request.choreo
       inert browser operation capabilities.

   In particular this namespace does not:

   - persist Requests or RequestAssignments;
   - implement lifecycle transitions;
   - decide authorization;
   - manufacture a demo Request revision/basis;
   - reinterpret :take-over or :done as alternate production operations;
   - duplicate Request schema/domain rules.

   The board may choose which already-valid production facts to display, how to
   search/sort/filter them, and which coarse affordances are useful to render.
   Rendering an affordance grants no authority. Every submitted operation is
   still authenticated and revalidated by the trusted production Request model.

   The browser observed basis, when available, comes from Gesso Live's
   authoritative XTDB progression on the read context. Request's own
   :request/revision is retained only as a model-specific fact version; it is
   never substituted for the XTDB authority frontier."
  (:require
   [clojure.string :as str]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.core :as live]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.choreo :as request.choreo]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user])
  (:import
   [java.time Instant]))

;; =============================================================================
;; View-state vocabulary
;; =============================================================================

(def default-created-order :newest)

(def created-orders
  #{:newest
    :oldest})

(def default-view-state
  {:search ""
   :created-order default-created-order
   :mine-first? false
   :unclaimed-first? false
   :show-terminal? false})

(defn present?
  [value]
  (and
   (some? value)
   (not
    (str/blank?
     (str value)))))

(defn- normalize-token
  [value]
  (some-> value str str/trim str/lower-case))

(defn- truthy-param?
  [value]
  (contains?
   #{true
     1
     "1"
     "true"
     "yes"
     "on"}
   (if (string? value)
     (normalize-token value)
     value)))

(defn normalize-created-order
  [value]
  (let [candidate
        (cond
          (keyword? value)
          value

          (present? value)
          (keyword
           (normalize-token value))

          :else
          nil)]
    (if
     (contains?
      created-orders
      candidate)
      candidate
      default-created-order)))

(defn normalize-view-state
  "Normalize only presentation state.

   There is intentionally no demo :visible-revision. Gesso Live progression is
   the authority-consistency mechanism for the production-backed example."
  [view-state]
  (let [view-state
        (or view-state {})]
    {:search
     (or
      (some-> (:search view-state) str str/trim)
      "")

     :created-order
     (normalize-created-order
      (:created-order view-state))

     :mine-first?
     (truthy-param?
      (:mine-first? view-state))

     :unclaimed-first?
     (truthy-param?
      (:unclaimed-first? view-state))

     :show-terminal?
     (truthy-param?
      (:show-terminal? view-state))}))

;; =============================================================================
;; Authoritative read frontier
;; =============================================================================

(defn observed-basis
  "Return the strongest XTDB authority basis justified by ctx, or nil.

   A nil result is meaningful: an initial/non-progressed rendering may still
   render an ordinary server-authoritative action, but must not fabricate an
   optimistic observed basis from :request/revision or wall-clock data."
  [ctx]
  (when-let [requirement
             (live/progression ctx)]
    (xtdb-live/strongest-required-basis
     requirement)))

;; =============================================================================
;; Production read composition
;; =============================================================================

(defn- require-user-id!
  [user-id context]
  (when-not
   (uuid? user-id)
    (throw
     (ex-info
      "HumanHelp example board requires a production User UUID."
      (assoc
       context
       :user-id user-id))))
  user-id)

(defn- display-user
  "Read a production User for display. Missing referenced users fail closed.

   Request persistence owns only User identity references. The example board
   composes the corresponding public User projection because names/contact
   labels are presentation concerns, not Request document fields."
  [ctx user-id context]
  (user/require-user
   ctx
   (require-user-id!
    user-id
    context)))

(defn request-row
  "Compose one production Request into one example-board read row.

   The row retains the authoritative production documents rather than converting
   them into the old example.model Request representation."
  [ctx request-document]
  (when-not
   (request/request-document?
    request-document)
    (throw
     (ex-info
      "HumanHelp example board received an invalid production Request document."
      {:request-id
       (some-> request-document request/request-id)})))
  (let [request-id
        (request/request-id
         request-document)

        primary-assignment
        (request/active-primary-assignment-for-request
         ctx
         request-id)

        requestor-user
        (when
         (request/user-requestor?
          (request/requestor request-document))
          (display-user
           ctx
           (request/requestor-id request-document)
           {:relation :requestor
            :request-id request-id}))

        primary-helper-user
        (when primary-assignment
          (display-user
           ctx
           (request/assignment-helper-id primary-assignment)
           {:relation :primary-helper
            :request-id request-id
            :assignment-id
            (request/assignment-id primary-assignment)}))]
    {:request request-document
     :primary-assignment primary-assignment
     :requestor-user requestor-user
     :primary-helper-user primary-helper-user}))

(defn request-rows-for-location
  "Read all production Requests for one production Location and compose rows.

   Request's public collection read deliberately requires both Organization and
   Location identity because those are persisted Request ownership facts. The
   example board starts from a Location scope, so it derives the owning
   Organization through the public Organization model rather than guessing,
   hard-coding a fixture Organization, or bypassing Request's public API.

   Terminal Requests are included in the authoritative read because hiding or
   showing terminal rows is board presentation state. Query-time omission would
   make :show-terminal? incapable of revealing them and would undercount the
   board's terminal population."
  [ctx location-id]
  (when-not
   (uuid? location-id)
    (throw
     (ex-info
      "HumanHelp example board requires a production Location UUID."
      {:location-id location-id})))
  (let [location-document
        (organization/require-location
         ctx
         location-id)

        organization-id
        (organization/location-organization-id
         location-document)]
    (mapv
     #(request-row ctx %)
     (request/requests-for-location
      ctx
      {:organization-id organization-id
       :location-id location-id
       :include-terminal? true}))))

;; =============================================================================
;; Row facts used only for presentation
;; =============================================================================

(defn row-request
  [row]
  (:request row))

(defn row-primary-assignment
  [row]
  (:primary-assignment row))

(defn row-request-id
  [row]
  (request/request-id
   (row-request row)))

(defn row-requestor-user-id
  [row]
  (some-> row :requestor-user user/user-id))

(defn row-primary-helper-id
  [row]
  (some-> row
          row-primary-assignment
          request/assignment-helper-id))

(defn requested-by-viewer?
  [row viewer-id]
  (request/requested-by-user?
   (row-request row)
   viewer-id))

(defn assigned-to-viewer?
  [row viewer-id]
  (and
   (uuid? viewer-id)
   (=
    viewer-id
    (row-primary-helper-id row))))

(defn mine?
  "Board sorting concept only: requested by me or primarily assigned to me."
  [row viewer-id]
  (or
   (requested-by-viewer?
    row
    viewer-id)
   (assigned-to-viewer?
    row
    viewer-id)))

;; =============================================================================
;; Search/filter/sort
;; =============================================================================

(defn- display-user-text
  [user-document]
  (when user-document
    (str/join
     " "
     (remove
      str/blank?
      [(or (user/user-display-name user-document) "")
       (or (user/user-email user-document) "")
       (or (user/user-phone user-document) "")]))))

(defn- searchable-row-text
  [row]
  (let [request-document
        (row-request row)]
    (->>
     [(request/request-id request-document)
      (request/status request-document)
      (:request/title request-document)
      (:request/details request-document)
      (:request/location-detail request-document)
      (display-user-text (:requestor-user row))
      (display-user-text (:primary-helper-user row))]
     (remove nil?)
     (map str)
     (str/join " ")
     str/lower-case)))

(defn search-match?
  [row search]
  (let [needle
        (normalize-token search)]
    (or
     (str/blank? (or needle ""))
     (str/includes?
      (searchable-row-text row)
      needle))))

(defn visible-by-terminal-filter?
  [row show-terminal?]
  (or
   show-terminal?
   (request/active?
    (row-request row))))

(defn- instant-epoch-milli
  [value]
  (if
   (instance? Instant value)
    (.toEpochMilli ^Instant value)
    0))

(defn- created-sort-value
  [row created-order]
  (let [created-ms
        (instant-epoch-milli
         (request/created-at
          (row-request row)))]
    (case created-order
      :oldest
      created-ms

      :newest
      (- created-ms)

      (- created-ms))))

(defn- true-first
  [value]
  (if value 0 1))

(defn row-sort-key
  [row viewer-id view-state]
  (let [{:keys
         [created-order
          mine-first?
          unclaimed-first?]}
        (normalize-view-state
         view-state)]
    (cond-> []
      mine-first?
      (conj
       (true-first
        (mine? row viewer-id)))

      unclaimed-first?
      (conj
       (true-first
        (request/claimable?
         (row-request row))))

      true
      (conj
       (created-sort-value row created-order)
       (str
        (row-request-id row))))))

(defn visible-rows
  "Apply only example presentation search/filter/sort to production rows."
  [rows viewer-id view-state]
  (let [view-state
        (normalize-view-state
         view-state)]
    (->>
     rows
     (filter
      #(search-match?
        %
        (:search view-state)))
     (filter
      #(visible-by-terminal-filter?
        %
        (:show-terminal? view-state)))
     (sort-by
      #(row-sort-key
        %
        viewer-id
        view-state))
     vec)))

;; =============================================================================
;; Production Choreo affordances
;; =============================================================================

(def operation-order
  "Stable presentation ordering for simple per-card lifecycle affordances.

   Reassign is intentionally not a one-click card affordance because it requires
   choosing a target helper. Its production capability remains available from
   request.choreo/capabilities for the later manager UI."
  [request.choreo/claim-operation
   request.choreo/mark-on-the-way-operation
   request.choreo/complete-operation
   request.choreo/unclaim-operation
   request.choreo/cancel-operation])

(defn- operation-visible?
  "Conservative presentation eligibility based only on already-public Request
   facts.

   This is not authorization. The trusted operation and Request model rerun all
   policy against current authoritative state. In particular, :request/claim may
   still be rejected when the viewer is not an effective helper."
  [operation row viewer-id]
  (let [request-document
        (row-request row)]
    (case operation
      :request/claim
      (request/claimable?
       request-document)

      :request/unclaim
      (and
       (request/unclaimable?
        request-document)
       (assigned-to-viewer?
        row
        viewer-id))

      :request/mark-on-the-way
      (and
       (request/markable-on-the-way?
        request-document)
       (assigned-to-viewer?
        row
        viewer-id))

      :request/complete
      (and
       (request/completable?
        request-document)
       (assigned-to-viewer?
        row
        viewer-id))

      :request/cancel
      (and
       (request/cancellable?
        request-document)
       (requested-by-viewer?
        row
        viewer-id))

      false)))

(defn operation-affordances
  "Return inert production Choreo affordances for one rendered Request row.

   No capability returned here is durable authorization. The map contains only
   semantic operation identity, the model-owned inert capability, and the
   operation arguments needed by the browser command."
  [row viewer-id]
  (let [request-id
        (row-request-id row)]
    (->>
     operation-order
     (filter
      #(operation-visible?
        %
        row
        viewer-id))
     (mapv
      (fn [operation]
        {:operation operation
         :capability
         (get
          request.choreo/capabilities
          operation)
         :arguments
         {:request-id request-id}})))))

(defn optimistic-binding
  "Return a protocol-v3 binding for one production operation when ctx carries a
   justified authoritative XTDB observation basis.

   :request/revision is a fact version only. It is never used as observed basis."
  [ctx row operation arguments target-id]
  (when-let [basis
             (observed-basis ctx)]
    {:arguments arguments
     :observed-basis basis
     :scope
     [:request
      (row-request-id row)]
     :fact-versions
     {:request/revision
      (request/revision
       (row-request row))}
     :target-id target-id
     :capability
     (or
      (get
       request.choreo/capabilities
       operation)
      (throw
       (ex-info
        "HumanHelp example board has no production Choreo capability for operation."
        {:operation operation
         :known-operations
         (set
          (keys
           request.choreo/capabilities))})))}))

;; =============================================================================
;; Board query
;; =============================================================================

(defn board-data
  "Read one production-backed example board.

   Input:

     {:location-id uuid
      :viewer      production User document
      :view-state  presentation map}

   The result intentionally exposes :rows rather than the old :requests shape so
   downstream example UI must consciously render production documents instead of
   silently depending on the former parallel demo model."
  [ctx {:keys
        [location-id
         viewer
         view-state]}]
  (let [viewer-id
        (-> viewer
            user/user-id
            (require-user-id!
             {:relation :viewer}))

        view-state
        (normalize-view-state
         view-state)

        rows
        (request-rows-for-location
         ctx
         location-id)

        visible
        (visible-rows
         rows
         viewer-id
         view-state)]
    {:location-id location-id
     :viewer viewer
     :viewer-id viewer-id
     :view-state view-state
     :observed-basis (observed-basis ctx)
     :rows visible
     :total-count (count rows)
     :active-count
     (count
      (filter
       #(request/active?
         (row-request %))
       rows))
     :terminal-count
     (count
      (filter
       #(request/terminal?
         (row-request %))
       rows))}))
