(ns net.humanhelp.site.model.request.domain
  "Pure domain rules for HumanHelp assistance requests.

   Request owns the persisted Request document: organization and location,
   requestor ownership, customer-editable content, lifecycle, invariants, and
   Request-document commands.

   Helper participation is deliberately not stored on the Request document.
   Primary helpers and collaborators are persisted as Request Assignment
   documents in net.humanhelp.site.model.request.assignment. Request FX
   coordinates lifecycle changes with assignment creation or termination.

   This separation lets one Request have a primary helper plus collaborators
   while preserving assignment history without embedding historical helper IDs
   into the Request document.

   This namespace does not query XTDB, prove referenced entities exist,
   authenticate capabilities, authorize actors, inspect helper eligibility or
   skills, execute transactions, or publish Gesso Live changes."
  (:require
   [clojure.string :as str]
   [net.humanhelp.site.model.common :as model.common]))

;; =============================================================================
;; Identity, versioning, and vocabulary
;; =============================================================================

(def request-entity-type
  :request)

(def request-version
  {:revision-key :request/revision
   :created-at-key :request/created-at
   :updated-at-key :request/updated-at})

(def requestor-types
  #{:user
    :capability})

(def status-order
  [:open
   :claimed
   :on-the-way
   :done
   :cancelled])

(def statuses
  (set status-order))

(def active-statuses
  #{:open
    :claimed
    :on-the-way})

(def assigned-statuses
  #{:claimed
    :on-the-way})

(def terminal-statuses
  #{:done
    :cancelled})

(def document-operation-order
  [:create
   :edit
   :claim
   :unclaim
   :mark-on-the-way
   :complete
   :cancel])

(def assignment-operation-order
  "Request-model operations whose persisted state is primarily represented by
   Request Assignment documents rather than by fields on the Request itself."
  [:add-collaborator
   :remove-collaborator
   :reassign])

(def operation-order
  (into
   document-operation-order
   assignment-operation-order))

(def operations
  (set operation-order))

(def document-operations
  (set document-operation-order))

(def assignment-operations
  (set assignment-operation-order))

(def transitions
  {[:open :claim]
   :claimed

   [:claimed :unclaim]
   :open

   [:claimed :mark-on-the-way]
   :on-the-way

   [:claimed :complete]
   :done

   [:on-the-way :complete]
   :done

   [:open :cancel]
   :cancelled

   [:claimed :cancel]
   :cancelled

   [:on-the-way :cancel]
   :cancelled})

(def lifecycle-fields
  #{:request/claimed-at
    :request/on-the-way-at
    :request/completed-at
    :request/cancelled-at
    :request/cancellation-reason})

(defn requestor-type?
  [value]
  (contains?
   requestor-types
   value))

(defn status?
  [value]
  (contains?
   statuses
   value))

(defn operation?
  [value]
  (contains?
   operations
   value))

(defn document-operation?
  [value]
  (contains?
   document-operations
   value))

(defn assignment-operation?
  [value]
  (contains?
   assignment-operations
   value))

;; =============================================================================
;; Requestor values
;; =============================================================================

(defn requestor-reference?
  [value]
  (and
   (map?
    value)

   (=
    #{:requestor/type
      :requestor/id}
    (set
     (keys value)))

   (requestor-type?
    (:requestor/type value))

   (uuid?
    (:requestor/id value))))

(defn user-requestor
  [user-id]
  {:requestor/type
   :user

   :requestor/id
   user-id})

(defn capability-requestor
  [capability-id]
  {:requestor/type
   :capability

   :requestor/id
   capability-id})

(defn user-requestor?
  [value]
  (and
   (requestor-reference?
    value)

   (=
    :user
    (:requestor/type value))))

(defn capability-requestor?
  [value]
  (and
   (requestor-reference?
    value)

   (=
    :capability
    (:requestor/type value))))

;; =============================================================================
;; Customer-editable content
;; =============================================================================

(def title-max
  60)

(def details-max
  500)

(def location-detail-max
  120)

(def content-keys
  #{:title
    :details
    :location-detail})

(defn normalize-text
  "Trims strings and turns blank strings into nil.

   Non-strings are left unchanged so validation can report their actual type."
  [value]
  (if
   (string?
    value)
    (let [normalized
          (str/trim value)]
      (when-not
       (str/blank?
        normalized)
        normalized))
    value))

(defn canonical-text?
  [value max-length]
  (and
   (string?
    value)

   (not
    (str/blank?
     value))

   (=
    value
    (str/trim value))

   (<=
    (count value)
    max-length)))

(defn optional-canonical-text?
  [value max-length]
  (or
   (nil?
    value)

   (canonical-text?
    value
    max-length)))

(defn title?
  [value]
  (canonical-text?
   value
   title-max))

(defn details?
  [value]
  (optional-canonical-text?
   value
   details-max))

(defn location-detail?
  [value]
  (optional-canonical-text?
   value
   location-detail-max))

(defn normalize-content
  [input]
  (let [input
        (or
         input
         {})]
    {:title
     (normalize-text
      (:title input))

     :details
     (normalize-text
      (:details input))

     :location-detail
     (normalize-text
      (:location-detail input))}))

(defn content?
  [value]
  (and
   (map?
    value)

   (=
    content-keys
    (set
     (keys value)))

   (title?
    (:title value))

   (details?
    (:details value))

   (location-detail?
    (:location-detail value))))

(defn content-errors
  [input]
  (let [{:keys
         [title
          details
          location-detail]}
        (normalize-content
         input)]
    (cond-> {}
      (not
       (title?
        title))
      (assoc
       :title
       (str
        "A non-blank request title of at most "
        title-max
        " characters is required."))

      (not
       (details?
        details))
      (assoc
       :details
       (str
        "Request details must contain at most "
        details-max
        " characters."))

      (not
       (location-detail?
        location-detail))
      (assoc
       :location-detail
       (str
        "The within-location description must contain at most "
        location-detail-max
        " characters.")))))

(defn valid-content?
  [input]
  (empty?
   (content-errors
    input)))

(defn content
  [request]
  {:title
   (:request/title request)

   :details
   (:request/details request)

   :location-detail
   (:request/location-detail request)})

(defn same-content?
  [request value]
  (=
   (content request)
   (normalize-content value)))

(defn- require-content
  [input]
  (let [normalized
        (normalize-content
         input)

        errors
        (content-errors
         normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :request/invalid-content
       "The Request content is invalid."
       errors))
    normalized))

(defn- apply-content
  [request input]
  (let [{:keys
         [title
          details
          location-detail]}
        (require-content
         input)]
    (cond->
     (-> request
         (dissoc
          :request/details
          :request/location-detail)
         (assoc
          :request/title
          title))

      details
      (assoc
       :request/details
       details)

      location-detail
      (assoc
       :request/location-detail
       location-detail))))

;; =============================================================================
;; Projections and facts
;; =============================================================================

(defn request-id
  [request]
  (:xt/id request))

(defn organization-id
  [request]
  (:request/organization request))

(defn location-id
  [request]
  (:request/location request))

(defn requestor-type
  [request]
  (:request/requestor-type request))

(defn requestor-id
  [request]
  (:request/requestor-id request))

(defn status
  [request]
  (:request/status request))

(defn revision
  [request]
  (:request/revision request))

(defn created-at
  [request]
  (:request/created-at request))

(defn updated-at
  [request]
  (:request/updated-at request))

(defn requestor
  [request]
  {:requestor/type
   (requestor-type request)

   :requestor/id
   (requestor-id request)})

(defn belongs-to-organization?
  [request expected-organization-id]
  (and
   (uuid?
    expected-organization-id)

   (=
    expected-organization-id
    (organization-id request))))

(defn at-location?
  [request expected-location-id]
  (and
   (uuid?
    expected-location-id)

   (=
    expected-location-id
    (location-id request))))

(defn belongs-to-location?
  [request expected-organization-id expected-location-id]
  (and
   (belongs-to-organization?
    request
    expected-organization-id)

   (at-location?
    request
    expected-location-id)))

(defn requested-by?
  [request requestor-reference]
  (and
   (requestor-reference?
    requestor-reference)

   (=
    requestor-reference
    (requestor request))))

(defn requested-by-user?
  [request user-id]
  (and
   (uuid?
    user-id)

   (requested-by?
    request
    (user-requestor
     user-id))))

(defn requested-by-capability?
  [request capability-id]
  (and
   (uuid?
    capability-id)

   (requested-by?
    request
    (capability-requestor
     capability-id))))

(defn controlled-by?
  "Compares an already-authenticated User or capability identity with the
   stored Request requestor.

   Authentication belongs outside this namespace."
  [request {:keys
            [user-id
             capability-id]}]
  (or
   (requested-by-user?
    request
    user-id)

   (requested-by-capability?
    request
    capability-id)))

(defn open?
  [request]
  (=
   :open
   (status request)))

(defn claimed?
  [request]
  (=
   :claimed
   (status request)))

(defn on-the-way?
  [request]
  (=
   :on-the-way
   (status request)))

(defn done?
  [request]
  (=
   :done
   (status request)))

(defn cancelled?
  [request]
  (=
   :cancelled
   (status request)))

(defn active?
  [request]
  (contains?
   active-statuses
   (status request)))

(defn terminal?
  [request]
  (contains?
   terminal-statuses
   (status request)))

(defn editable?
  [request]
  (active?
   request))

(defn lifecycle-expects-primary-assignment?
  "Returns true while Request lifecycle requires one active primary Request
   Assignment.

   This is a cross-document expectation. Request FX/Graph must establish that
   the corresponding assignment actually exists."
  [request]
  (contains?
   assigned-statuses
   (status request)))

(defn next-status
  [request operation]
  (get
   transitions
   [(status request)
    operation]))

(defn transition-allowed?
  [request operation]
  (some?
   (next-status
    request
    operation)))

(defn claimable?
  [request]
  (transition-allowed?
   request
   :claim))

(defn unclaimable?
  [request]
  (transition-allowed?
   request
   :unclaim))

(defn markable-on-the-way?
  [request]
  (transition-allowed?
   request
   :mark-on-the-way))

(defn completable?
  [request]
  (transition-allowed?
   request
   :complete))

(defn cancellable?
  [request]
  (transition-allowed?
   request
   :cancel))

;; =============================================================================
;; Complete document consistency
;; =============================================================================

(defn- optional-timestamp?
  [value]
  (or
   (nil?
    value)

   (model.common/timestamp-value?
    value)))

(defn- optional-reason?
  [value]
  (or
   (nil?
    value)

   (qualified-keyword?
    value)))

(defn- all-absent?
  [request keys]
  (every?
   #(nil?
     (get
      request
      %))
   keys))

(defn- timestamp-within-request?
  [request value]
  (model.common/optional-between?
   (created-at request)
   value
   (updated-at request)))

(defn- timestamps-ordered-if-present?
  [earlier later]
  (or
   (nil?
    earlier)

   (nil?
    later)

   (model.common/timestamp<=
    earlier
    later)))

(defn- requestor-consistent?
  [request]
  (requestor-reference?
   (requestor request)))

(defn- content-consistent?
  [request]
  (content?
   (content request)))

(defn- lifecycle-values-consistent?
  [request]
  (let [{:request/keys
         [claimed-at
          on-the-way-at
          completed-at
          cancelled-at
          cancellation-reason]}
        request]
    (and
     (optional-timestamp?
      claimed-at)

     (optional-timestamp?
      on-the-way-at)

     (optional-timestamp?
      completed-at)

     (optional-timestamp?
      cancelled-at)

     (optional-reason?
      cancellation-reason)

     (every?
      #(timestamp-within-request?
        request
        %)
      [claimed-at
       on-the-way-at
       completed-at
       cancelled-at])

     (timestamps-ordered-if-present?
      claimed-at
      on-the-way-at)

     (timestamps-ordered-if-present?
      claimed-at
      completed-at)

     (timestamps-ordered-if-present?
      claimed-at
      cancelled-at)

     (timestamps-ordered-if-present?
      on-the-way-at
      completed-at)

     (timestamps-ordered-if-present?
      on-the-way-at
      cancelled-at))))

(defn- open-state-consistent?
  [request]
  (all-absent?
   request
   lifecycle-fields))

(defn- claimed-state-consistent?
  [request]
  (and
   (some?
    (:request/claimed-at request))

   (all-absent?
    request
    [:request/on-the-way-at
     :request/completed-at
     :request/cancelled-at
     :request/cancellation-reason])))

(defn- on-the-way-state-consistent?
  [request]
  (and
   (some?
    (:request/claimed-at request))

   (some?
    (:request/on-the-way-at request))

   (all-absent?
    request
    [:request/completed-at
     :request/cancelled-at
     :request/cancellation-reason])))

(defn- done-state-consistent?
  [request]
  (and
   (some?
    (:request/claimed-at request))

   (some?
    (:request/completed-at request))

   (all-absent?
    request
    [:request/cancelled-at
     :request/cancellation-reason])))

(defn- cancelled-state-consistent?
  [request]
  (and
   (some?
    (:request/cancelled-at request))

   (nil?
    (:request/completed-at request))

   (or
    ;; Cancelled before anyone claimed it.
    (and
     (nil?
      (:request/claimed-at request))

     (nil?
      (:request/on-the-way-at request)))

    ;; Cancelled after claim, optionally after on-the-way.
    (some?
     (:request/claimed-at request)))))

(defn lifecycle-consistent?
  [request]
  (and
   (status?
    (status request))

   (lifecycle-values-consistent?
    request)

   (case
    (status request)

    :open
    (open-state-consistent?
     request)

    :claimed
    (claimed-state-consistent?
     request)

    :on-the-way
    (on-the-way-state-consistent?
     request)

    :done
    (done-state-consistent?
     request)

    :cancelled
    (cancelled-state-consistent?
     request)

    false)))

(defn request-document-consistent?
  "Returns true when value is a complete valid persisted Request document.

   Assignment consistency is intentionally not part of this predicate. For
   example, a :claimed Request expects one active primary Request Assignment,
   but proving that requires cross-document persistence facts."
  [value]
  (and
   (map?
    value)

   (model.common/versioned-document-consistent?
    value
    request-version)

   (uuid?
    (:request/organization value))

   (uuid?
    (:request/location value))

   (requestor-consistent?
    value)

   (content-consistent?
    value)

   (lifecycle-consistent?
    value)))

(defn- request-context
  [request]
  {:request/id
   (:xt/id request)

   :request/organization
   (:request/organization request)

   :request/location
   (:request/location request)

   :request/requestor
   (requestor request)

   :request/status
   (:request/status request)

   :request/revision
   (:request/revision request)})

(defn require-request-document
  [request]
  (when-not
   (request-document-consistent?
    request)
    (model.common/throw-invalid!
     :request/invalid-document
     "The Request document is invalid."
     {:request
      "The Request ownership, content, lifecycle, or version fields are inconsistent."}
     (request-context request)))
  request)

;; =============================================================================
;; Construction
;; =============================================================================

(defn normalize-create-input
  [input]
  (let [input
        (or
         input
         {})]
    {:id
     (:id input)

     :organization-id
     (:organization-id input)

     :location-id
     (:location-id input)

     :requestor
     (:requestor input)

     :content
     (normalize-content
      (:content input))

     :now
     (:now input)}))

(defn create-input-errors
  [input]
  (let [{:keys
         [id
          organization-id
          location-id
          requestor
          content
          now]}
        (normalize-create-input
         input)

        errors
        (content-errors
         content)]
    (cond-> {}
      (not
       (uuid?
        id))
      (assoc
       :id
       "A Request UUID is required.")

      (not
       (uuid?
        organization-id))
      (assoc
       :organization-id
       "An Organization UUID is required.")

      (not
       (uuid?
        location-id))
      (assoc
       :location-id
       "A Location UUID is required.")

      (not
       (requestor-reference?
        requestor))
      (assoc
       :requestor
       "A valid User or capability requestor reference is required.")

      (seq errors)
      (assoc
       :content
       errors)

      (not
       (model.common/timestamp-value?
        now))
      (assoc
       :now
       "A valid Request creation time is required."))))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors
    input)))

(defn new-request
  [input]
  (let [{:keys
         [id
          organization-id
          location-id
          requestor
          content
          now]
         :as normalized}
        (normalize-create-input
         input)

        errors
        (create-input-errors
         normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :request/invalid-create-input
       "A valid Request could not be created."
       errors
       {:request/id
        id

        :request/organization
        organization-id

        :request/location
        location-id

        :request/requestor
        requestor}))

    (-> {:xt/id
         id

         :request/organization
         organization-id

         :request/location
         location-id

         :request/requestor-type
         (:requestor/type requestor)

         :request/requestor-id
         (:requestor/id requestor)

         :request/status
         :open

         :request/revision
         0

         :request/created-at
         now

         :request/updated-at
         now}
        (apply-content
         content)
        require-request-document)))

;; =============================================================================
;; Guarded revision
;; =============================================================================

(defn- immutable-identity
  [request]
  {:xt/id
   (:xt/id request)

   :request/organization
   (:request/organization request)

   :request/location
   (:request/location request)

   :request/requestor-type
   (:request/requestor-type request)

   :request/requestor-id
   (:request/requestor-id request)

   :request/created-at
   (:request/created-at request)})

(defn- version-state
  [request]
  {:request/revision
   (:request/revision request)

   :request/updated-at
   (:request/updated-at request)})

(defn- revise-request
  [request now mutation-fn]
  (require-request-document
   request)

  (when-not
   (model.common/valid-change-time?
    request
    request-version
    now)
    (model.common/throw-invalid!
     :request/invalid-change-time
     "The Request change time is invalid."
     {:now
      "The change time must be an Instant at or after the current update time."}
     (request-context request)))

  (when-not
   (fn?
    mutation-fn)
    (model.common/throw-invalid!
     :request/invalid-mutation
     "The Request mutation is invalid."
     {:mutation
      "The mutation must be callable."}
     (request-context request)))

  (let [changed
        (mutation-fn
         request)]
    (when-not
     (map?
      changed)
      (model.common/throw-invalid!
       :request/invalid-mutation
       "The Request mutation is invalid."
       {:mutation
        "The mutation must return a Request map."}
       (request-context request)))

    (when-not
     (=
      (immutable-identity request)
      (immutable-identity changed))
      (model.common/throw-invalid!
       :request/immutable-identity
       "The Request mutation is invalid."
       {:request
        "Request identity, organization, location, requestor, and creation time are immutable."}
       (request-context request)))

    (when-not
     (=
      (version-state request)
      (version-state changed))
      (model.common/throw-invalid!
       :request/invalid-version-mutation
       "The Request mutation is invalid."
       {:request
        "The mutation must not directly change revision or updated-at."}
       (request-context request)))

    (when
     (=
      request
      changed)
      (model.common/throw-invalid!
       :request/unchanged
       "The Request mutation is invalid."
       {:request
        "The mutation would not change the Request."}
       (request-context request)))

    (-> changed
        (model.common/bump-revision
         request-version
         now)
        require-request-document)))

;; =============================================================================
;; Content editing
;; =============================================================================

(defn normalize-edit-input
  [input]
  (let [input
        (or
         input
         {})]
    {:content
     (normalize-content
      (:content input))

     :now
     (:now input)}))

(defn edit-input-errors
  [input]
  (let [{:keys
         [content
          now]}
        (normalize-edit-input
         input)

        errors
        (content-errors
         content)]
    (cond-> {}
      (seq errors)
      (assoc
       :content
       errors)

      (not
       (model.common/timestamp-value?
        now))
      (assoc
       :now
       "A valid Request edit time is required."))))

(defn valid-edit-input?
  [input]
  (empty?
   (edit-input-errors
    input)))

(defn edit-request
  [request input]
  (require-request-document
   request)

  (when-not
   (editable?
    request)
    (model.common/throw-invalid!
     :request/not-editable
     "The Request cannot be edited."
     {:status
      "Only an active Request can be edited."}
     (request-context request)))

  (let [{:keys
         [content
          now]
         :as normalized}
        (normalize-edit-input
         input)

        errors
        (edit-input-errors
         normalized)]
    (when
     (seq errors)
      (model.common/throw-invalid!
       :request/invalid-edit-input
       "The Request edit is invalid."
       errors
       (request-context request)))

    (revise-request
     request
     now
     #(apply-content
       %
       content))))

;; =============================================================================
;; Lifecycle transitions
;; =============================================================================

(defn- require-transition!
  [request operation]
  (when-not
   (transition-allowed?
    request
    operation)
    (model.common/throw-invalid!
     :request/invalid-transition
     "The Request lifecycle transition is invalid."
     {:operation
      (str
       "Operation "
       operation
       " is not allowed from status "
       (status request)
       ".")}
     (request-context request))))

(defn- require-cancellation-reason!
  [request reason]
  (when-not
   (optional-reason?
    reason)
    (model.common/throw-invalid!
     :request/invalid-cancellation-reason
     "The Request cancellation reason is invalid."
     {:reason
      "A cancellation reason must be a qualified keyword when supplied."}
     (request-context request)))
  reason)

(defn claim-request
  "Moves an open Request to :claimed.

   Request FX must atomically create the corresponding active primary Request
   Assignment. The helper identity is therefore intentionally absent from this
   pure Request mutation."
  [request {:keys [now]}]
  (require-request-document
   request)

  (require-transition!
   request
   :claim)

  (revise-request
   request
   now
   #(assoc
     %
     :request/status
     :claimed

     :request/claimed-at
     now)))

(defn unclaim-request
  "Returns a claimed Request to :open.

   Request FX must atomically end the active primary Request Assignment."
  [request {:keys [now]}]
  (require-request-document
   request)

  (require-transition!
   request
   :unclaim)

  (revise-request
   request
   now
   #(-> %
        (assoc
         :request/status
         :open)
        (dissoc
         :request/claimed-at
         :request/on-the-way-at))))

(defn mark-request-on-the-way
  "Moves a claimed Request to :on-the-way.

   Request FX is responsible for proving that the actor owns the active primary
   Request Assignment."
  [request {:keys [now]}]
  (require-request-document
   request)

  (require-transition!
   request
   :mark-on-the-way)

  (revise-request
   request
   now
   #(assoc
     %
     :request/status
     :on-the-way

     :request/on-the-way-at
     now)))

(defn complete-request
  "Completes a claimed or on-the-way Request.

   Request FX must atomically end every active Request Assignment when the
   Request becomes terminal."
  [request {:keys [now]}]
  (require-request-document
   request)

  (require-transition!
   request
   :complete)

  (revise-request
   request
   now
   #(assoc
     %
     :request/status
     :done

     :request/completed-at
     now)))

(defn cancel-request
  "Cancels an active Request.

   Request FX must atomically end every active Request Assignment when the
   Request becomes terminal."
  [request {:keys
            [now
             reason]}]
  (require-request-document
   request)

  (require-transition!
   request
   :cancel)

  (require-cancellation-reason!
   request
   reason)

  (revise-request
   request
   now
   #(cond->
     (assoc
      %
      :request/status
      :cancelled

      :request/cancelled-at
      now)

     reason
     (assoc
      :request/cancellation-reason
      reason))))

;; =============================================================================
;; Model commands
;; =============================================================================

(defn create-command
  [input]
  (model.common/create-command
   request-entity-type
   (new-request
    input)
   request-version))

(defn- change-command
  [operation before after]
  (model.common/update-command
   request-entity-type
   operation
   before
   after
   request-version))

(defn edit-command
  [request input]
  (change-command
   :edit
   request
   (edit-request
    request
    input)))

(defn claim-command
  [request input]
  (change-command
   :claim
   request
   (claim-request
    request
    input)))

(defn unclaim-command
  [request input]
  (change-command
   :unclaim
   request
   (unclaim-request
    request
    input)))

(defn mark-on-the-way-command
  [request input]
  (change-command
   :mark-on-the-way
   request
   (mark-request-on-the-way
    request
    input)))

(defn complete-command
  [request input]
  (change-command
   :complete
   request
   (complete-request
    request
    input)))

(defn cancel-command
  [request input]
  (change-command
   :cancel
   request
   (cancel-request
    request
    input)))
