(ns net.humanhelp.site.model.request.domain
  "Pure domain rules for HumanHelp Requests and Request Assignments.

   Request owns two persisted entity types:

   - :request owns organization/location identity, requestor ownership,
     customer-editable content, and lifecycle;
   - :request-assignment owns one helper's participation in one Request,
     including primary/collaborator role and assignment history.

   Request Assignment is intentionally part of the Request model rather than a
   separate top-level model. It describes participation in a particular
   Request, not employment, Organization membership, or authorization.

   This namespace owns only local Request-model facts and canonical
   gesso.model commands. It does not:

   - query persistence;
   - prove that referenced Users, Organizations, Locations, Memberships, or
     capabilities exist;
   - decide whether a User is currently an effective helper;
   - authorize actors;
   - enforce cross-document assignment uniqueness;
   - execute transactions;
   - publish Gesso Live changes.

   Cross-document invariants such as 'a claimed Request has exactly one active
   primary assignment' belong to Request Graph/FX, where current persisted
   Request and Request Assignment facts can be considered together."
  (:require
   [clojure.string :as str]
   [gesso.model.command :as command])
  (:import
   [java.time Instant]))

;; =============================================================================
;; Entity identity and versioning
;; =============================================================================

(def request-entity-type
  :request)

(def assignment-entity-type
  :request-assignment)

(def request-version
  {:revision-key
   :request/revision

   :created-at-key
   :request/created-at

   :updated-at-key
   :request/updated-at})

(def assignment-version
  {:revision-key
   :request-assignment/revision

   :created-at-key
   :request-assignment/created-at

   :updated-at-key
   :request-assignment/updated-at})

;; =============================================================================
;; Shared local helpers
;; =============================================================================

(defn- instant?
  [value]
  (instance?
   Instant
   value))

(defn- at-or-before?
  [^Instant left ^Instant right]
  (not
   (.isAfter
    left
    right)))

(defn- optional-uuid?
  [value]
  (or
   (nil?
    value)

   (uuid?
    value)))

(defn- optional-reason?
  [value]
  (or
   (nil?
    value)

   (qualified-keyword?
    value)))

(defn- none-present?
  [document keys]
  (every?
   #(nil?
     (get
      document
      %))
   keys))

(defn- optional-time-within?
  [created-at value updated-at]
  (or
   (nil?
    value)

   (and
    (instant?
     created-at)

    (instant?
     value)

    (instant?
     updated-at)

    (at-or-before?
     created-at
     value)

    (at-or-before?
     value
     updated-at))))

(defn- fail!
  [type message errors context]
  (throw
   (ex-info
    message
    {:error/type
     type

     :error/details
     {:errors
      errors

      :context
      context}})))

(defn- ensure!
  [test type message errors context]
  (when-not
   test
    (fail!
     type
     message
     errors
     context)))

(defn- valid-change-time?
  [document version now]
  (let [updated-at-key
        (:updated-at-key
         version)

        updated-at
        (get
         document
         updated-at-key)]
    (and
     (instant?
      now)

     (instant?
      updated-at)

     (at-or-before?
      updated-at
      now))))

;; =============================================================================
;; Requestor values
;; =============================================================================

(def requestor-types
  #{:user
    :capability})

(defn requestor-type?
  [value]
  (contains?
   requestor-types
   value))

(defn requestor-reference?
  [value]
  (and
   (map?
    value)

   (=
    #{:requestor/type
      :requestor/id}
    (set
     (keys
      value)))

   (requestor-type?
    (:requestor/type
     value))

   (uuid?
    (:requestor/id
     value))))

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
    (:requestor/type
     value))))

(defn capability-requestor?
  [value]
  (and
   (requestor-reference?
    value)

   (=
    :capability
    (:requestor/type
     value))))

;; =============================================================================
;; Request content values
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
  "Trims a string and canonicalizes blank text to nil.

   Non-string values pass through unchanged so validation can distinguish a
   type error from absence."
  [value]
  (cond
    (nil?
     value)
    nil

    (string?
     value)
    (let [value
          (str/trim
           value)]
      (when-not
       (str/blank?
        value)
        value))

    :else
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
    (str/trim
     value))

   (<=
    (count
     value)
    max-length)))

(defn title?
  [value]
  (canonical-text?
   value
   title-max))

(defn details?
  [value]
  (or
   (nil?
    value)

   (canonical-text?
    value
    details-max)))

(defn location-detail?
  [value]
  (or
   (nil?
    value)

   (canonical-text?
    value
    location-detail-max)))

(defn normalize-content
  [input]
  (let [input
        (or
         input
         {})]
    {:title
     (normalize-text
      (:title
       input))

     :details
     (normalize-text
      (:details
       input))

     :location-detail
     (normalize-text
      (:location-detail
       input))}))

(defn content?
  [value]
  (and
   (map?
    value)

   (=
    content-keys
    (set
     (keys
      value)))

   (title?
    (:title
     value))

   (details?
    (:details
     value))

   (location-detail?
    (:location-detail
     value))))

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
        "A non-blank Request title of at most "
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

;; =============================================================================
;; Request lifecycle vocabulary
;; =============================================================================

(def request-statuses
  #{:open
    :claimed
    :on-the-way
    :done
    :cancelled})

(def active-request-statuses
  #{:open
    :claimed
    :on-the-way})

(def assigned-request-statuses
  #{:claimed
    :on-the-way})

(def terminal-request-statuses
  #{:done
    :cancelled})

(def request-operations
  #{:create
    :edit
    :claim
    :unclaim
    :mark-on-the-way
    :complete
    :cancel
    :add-collaborator
    :remove-collaborator
    :reassign})

(def request-document-operations
  #{:create
    :edit
    :claim
    :unclaim
    :mark-on-the-way
    :complete
    :cancel})

(def request-assignment-operations
  #{:add-collaborator
    :remove-collaborator
    :reassign})

(def request-transitions
  {[:open
    :claim]
   :claimed

   [:claimed
    :unclaim]
   :open

   [:claimed
    :mark-on-the-way]
   :on-the-way

   [:claimed
    :complete]
   :done

   [:on-the-way
    :complete]
   :done

   [:open
    :cancel]
   :cancelled

   [:claimed
    :cancel]
   :cancelled

   [:on-the-way
    :cancel]
   :cancelled})

(defn request-status?
  [value]
  (contains?
   request-statuses
   value))

(defn request-operation?
  [value]
  (contains?
   request-operations
   value))

(defn request-document-operation?
  [value]
  (contains?
   request-document-operations
   value))

(defn request-assignment-operation?
  [value]
  (contains?
   request-assignment-operations
   value))

;; =============================================================================
;; Request projections
;; =============================================================================

(defn request-id
  [request]
  (:xt/id
   request))

(defn organization-id
  [request]
  (:request/organization
   request))

(defn location-id
  [request]
  (:request/location
   request))

(defn requestor-type
  [request]
  (:request/requestor-type
   request))

(defn requestor-id
  [request]
  (:request/requestor-id
   request))

(defn request-status
  [request]
  (:request/status
   request))

(defn request-revision
  [request]
  (:request/revision
   request))

(defn request-created-at
  [request]
  (:request/created-at
   request))

(defn request-updated-at
  [request]
  (:request/updated-at
   request))

(defn requestor
  [request]
  {:requestor/type
   (requestor-type
    request)

   :requestor/id
   (requestor-id
    request)})

(defn content
  [request]
  {:title
   (:request/title
    request)

   :details
   (:request/details
    request)

   :location-detail
   (:request/location-detail
    request)})

;; =============================================================================
;; Request ownership and location facts
;; =============================================================================

(defn belongs-to-organization?
  [request expected-organization-id]
  (and
   (uuid?
    expected-organization-id)

   (=
    expected-organization-id
    (organization-id
     request))))

(defn at-location?
  [request expected-location-id]
  (and
   (uuid?
    expected-location-id)

   (=
    expected-location-id
    (location-id
     request))))

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
  [request expected-requestor]
  (and
   (requestor-reference?
    expected-requestor)

   (=
    expected-requestor
    (requestor
     request))))

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
  "Returns whether one already-authenticated User or capability owns Request.

   Authentication and capability verification are deliberately outside the
   Request domain."
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

;; =============================================================================
;; Request lifecycle facts
;; =============================================================================

(defn open?
  [request]
  (=
   :open
   (request-status
    request)))

(defn claimed?
  [request]
  (=
   :claimed
   (request-status
    request)))

(defn on-the-way?
  [request]
  (=
   :on-the-way
   (request-status
    request)))

(defn done?
  [request]
  (=
   :done
   (request-status
    request)))

(defn cancelled?
  [request]
  (=
   :cancelled
   (request-status
    request)))

(defn active?
  [request]
  (contains?
   active-request-statuses
   (request-status
    request)))

(defn terminal?
  [request]
  (contains?
   terminal-request-statuses
   (request-status
    request)))

(defn editable?
  [request]
  (active?
   request))

(defn lifecycle-expects-primary-assignment?
  "Returns true when Request lifecycle requires one active primary
   RequestAssignment.

   This is intentionally only an expectation. Graph/FX establish whether the
   corresponding persisted assignment actually exists."
  [request]
  (contains?
   assigned-request-statuses
   (request-status
    request)))

(defn next-request-status
  [request operation]
  (get
   request-transitions
   [(request-status
     request)
    operation]))

(defn transition-allowed?
  [request operation]
  (some?
   (next-request-status
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
;; Request document consistency
;; =============================================================================

(def ^:private request-lifecycle-fields
  #{:request/claimed-at
    :request/on-the-way-at
    :request/completed-at
    :request/cancelled-at
    :request/cancellation-reason})

(defn- request-version-consistent?
  [request]
  (let [created-at
        (request-created-at
         request)

        updated-at
        (request-updated-at
         request)]
    (and
     (command/versioned-document?
      request
      request-version)

     (instant?
      created-at)

     (instant?
      updated-at)

     (at-or-before?
      created-at
      updated-at))))

(defn- request-lifecycle-values-consistent?
  [request]
  (let [created-at
        (request-created-at
         request)

        updated-at
        (request-updated-at
         request)

        claimed-at
        (:request/claimed-at
         request)

        on-the-way-at
        (:request/on-the-way-at
         request)

        completed-at
        (:request/completed-at
         request)

        cancelled-at
        (:request/cancelled-at
         request)

        cancellation-reason
        (:request/cancellation-reason
         request)]
    (and
     (every?
      #(optional-time-within?
        created-at
        %
        updated-at)
      [claimed-at
       on-the-way-at
       completed-at
       cancelled-at])

     (optional-reason?
      cancellation-reason)

     (or
      (nil?
       claimed-at)

      (nil?
       on-the-way-at)

      (at-or-before?
       claimed-at
       on-the-way-at))

     (or
      (nil?
       claimed-at)

      (nil?
       completed-at)

      (at-or-before?
       claimed-at
       completed-at))

     (or
      (nil?
       claimed-at)

      (nil?
       cancelled-at)

      (at-or-before?
       claimed-at
       cancelled-at))

     (or
      (nil?
       on-the-way-at)

      (nil?
       completed-at)

      (at-or-before?
       on-the-way-at
       completed-at))

     (or
      (nil?
       on-the-way-at)

      (nil?
       cancelled-at)

      (at-or-before?
       on-the-way-at
       cancelled-at)))))

(defn- request-state-consistent?
  [request]
  (case
   (request-status
    request)

    :open
    (none-present?
     request
     request-lifecycle-fields)

    :claimed
    (and
     (instant?
      (:request/claimed-at
       request))

     (none-present?
      request
      [:request/on-the-way-at
       :request/completed-at
       :request/cancelled-at
       :request/cancellation-reason]))

    :on-the-way
    (and
     (instant?
      (:request/claimed-at
       request))

     (instant?
      (:request/on-the-way-at
       request))

     (none-present?
      request
      [:request/completed-at
       :request/cancelled-at
       :request/cancellation-reason]))

    :done
    (and
     (instant?
      (:request/claimed-at
       request))

     (instant?
      (:request/completed-at
       request))

     (none-present?
      request
      [:request/cancelled-at
       :request/cancellation-reason]))

    :cancelled
    (and
     (instant?
      (:request/cancelled-at
       request))

     (nil?
      (:request/completed-at
       request))

     (or
      ;; Cancelled while still open.
      (and
       (nil?
        (:request/claimed-at
         request))

       (nil?
        (:request/on-the-way-at
         request)))

      ;; Cancelled after claim, optionally after on-the-way.
      (instant?
       (:request/claimed-at
        request))))

    false))

(defn request-document-consistent?
  "Returns true when value satisfies every local persisted Request invariant.

   Organization/Location existence, requestor authentication, and Request
   Assignment consistency are deliberately outside this predicate."
  [value]
  (and
   (map?
    value)

   (uuid?
    (request-id
     value))

   (uuid?
    (organization-id
     value))

   (uuid?
    (location-id
     value))

   (requestor-reference?
    (requestor
     value))

   (content?
    (content
     value))

   (request-status?
    (request-status
     value))

   (request-version-consistent?
    value)

   (request-lifecycle-values-consistent?
    value)

   (request-state-consistent?
    value)))

(defn- request-context
  [request]
  {:request/id
   (request-id
    request)

   :request/organization
   (organization-id
    request)

   :request/location
   (location-id
    request)

   :request/requestor
   (requestor
    request)

   :request/status
   (request-status
    request)

   :request/revision
   (request-revision
    request)})

(defn require-request-document
  [request]
  (ensure!
   (request-document-consistent?
    request)

   :request/invalid-document

   "The Request operation is invalid."

   {:request
    "The Request document is internally inconsistent."}

   (request-context
    request))

  request)

;; =============================================================================
;; Request construction
;; =============================================================================

(defn- normalize-create-request-input
  [input]
  (let [input
        (or
         input
         {})]
    {:id
     (:id
      input)

     :organization-id
     (:organization-id
      input)

     :location-id
     (:location-id
      input)

     :requestor
     (:requestor
      input)

     :content
     (normalize-content
      (:content
       input))

     :now
     (:now
      input)}))

(defn- create-request-input-errors
  [{:keys
    [id
     organization-id
     location-id
     requestor
     content
     now]}]
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

    (seq
     (content-errors
      content))
    (assoc
     :content
     (content-errors
      content))

    (not
     (instant?
      now))
    (assoc
     :now
     "A valid Request creation time is required.")))

(defn- new-request
  [input]
  (let [{:keys
         [id
          organization-id
          location-id
          requestor
          content
          now]
         :as normalized}
        (normalize-create-request-input
         input)

        errors
        (create-request-input-errors
         normalized)]
    (when
     (seq
      errors)
      (fail!
       :request/invalid-create-input
       "A valid Request could not be created."
       errors
       {:request/id
        id

        :request/organization
        organization-id

        :request/location
        location-id}))

    (let [{:keys
           [title
            details
            location-detail]}
          content]
      (require-request-document
       (cond->
        {:xt/id
         id

         :request/organization
         organization-id

         :request/location
         location-id

         :request/requestor-type
         (:requestor/type
          requestor)

         :request/requestor-id
         (:requestor/id
          requestor)

         :request/title
         title

         :request/status
         :open

         :request/revision
         0

         :request/created-at
         now

         :request/updated-at
         now}

         details
         (assoc
          :request/details
          details)

         location-detail
         (assoc
          :request/location-detail
          location-detail))))))

;; =============================================================================
;; Request mutation mechanics
;; =============================================================================

(defn- update-request
  [request now mutation]
  (require-request-document
   request)

  (ensure!
   (valid-change-time?
    request
    request-version
    now)

   :request/invalid-time

   "The Request operation is invalid."

   {:now
    "The change time must not precede the current Request update time."}

   (request-context
    request))

  (let [changed
        (mutation
         request)]

    (ensure!
     (not=
      request
      changed)

     :request/unchanged

     "The Request operation is invalid."

     {:request
      "The operation would not change the Request."}

     (request-context
      request))

    (require-request-document
     (command/bump-version
      changed
      request-version
      now))))

(defn- require-request-transition!
  [request operation]
  (ensure!
   (transition-allowed?
    request
    operation)

   :request/invalid-transition

   "The Request lifecycle transition is invalid."

   {:operation
    (str
     "Operation "
     operation
     " is not allowed from status "
     (request-status
      request)
     ".")}

   (request-context
    request)))

(defn- apply-content
  [request input]
  (let [normalized
        (normalize-content
         input)

        errors
        (content-errors
         normalized)]
    (when
     (seq
      errors)
      (fail!
       :request/invalid-content
       "The Request content is invalid."
       errors
       (request-context
        request)))

    (let [{:keys
           [title
            details
            location-detail]}
          normalized]
      (cond->
       (-> request
           (assoc
            :request/title
            title)
           (dissoc
            :request/details
            :request/location-detail))

        details
        (assoc
         :request/details
         details)

        location-detail
        (assoc
         :request/location-detail
         location-detail)))))

;; =============================================================================
;; Request content mutation
;; =============================================================================

(defn- edit-request
  [request {:keys
            [content
             now]}]
  (require-request-document
   request)

  (ensure!
   (editable?
    request)

   :request/not-editable

   "The Request operation is invalid."

   {:status
    "Only an active Request can be edited."}

   (request-context
    request))

  (update-request
   request
   now
   #(apply-content
     %
     content)))

;; =============================================================================
;; Request lifecycle mutations
;; =============================================================================

(defn- claim-request
  [request {:keys
            [now]}]
  (require-request-document
   request)

  (require-request-transition!
   request
   :claim)

  (update-request
   request
   now
   #(assoc
     %
     :request/status
     :claimed

     :request/claimed-at
     now)))

(defn- unclaim-request
  [request {:keys
            [now]}]
  (require-request-document
   request)

  (require-request-transition!
   request
   :unclaim)

  (update-request
   request
   now
   #(-> %
        (assoc
         :request/status
         :open)

        (dissoc
         :request/claimed-at
         :request/on-the-way-at))))

(defn- mark-request-on-the-way
  [request {:keys
            [now]}]
  (require-request-document
   request)

  (require-request-transition!
   request
   :mark-on-the-way)

  (update-request
   request
   now
   #(assoc
     %
     :request/status
     :on-the-way

     :request/on-the-way-at
     now)))

(defn- complete-request
  [request {:keys
            [now]}]
  (require-request-document
   request)

  (require-request-transition!
   request
   :complete)

  (update-request
   request
   now
   #(assoc
     %
     :request/status
     :done

     :request/completed-at
     now)))

(defn- cancel-request
  [request {:keys
            [now
             reason]}]
  (require-request-document
   request)

  (require-request-transition!
   request
   :cancel)

  (ensure!
   (optional-reason?
    reason)

   :request/invalid-cancellation-reason

   "The Request cancellation is invalid."

   {:reason
    "The cancellation reason must be a qualified keyword when supplied."}

   (request-context
    request))

  (update-request
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
;; Canonical Request commands
;; =============================================================================

(defn create-request-command
  [input]
  (command/create
   request-entity-type
   (new-request
    input)
   request-version))

(defn- request-update-command
  [operation request transition input]
  (command/update-command
   request-entity-type
   operation
   request
   (transition
    request
    input)
   request-version))

(defn edit-request-command
  [request input]
  (request-update-command
   :edit
   request
   edit-request
   input))

(defn claim-request-command
  [request input]
  (request-update-command
   :claim
   request
   claim-request
   input))

(defn unclaim-request-command
  [request input]
  (request-update-command
   :unclaim
   request
   unclaim-request
   input))

(defn mark-on-the-way-command
  [request input]
  (request-update-command
   :mark-on-the-way
   request
   mark-request-on-the-way
   input))

(defn complete-request-command
  [request input]
  (request-update-command
   :complete
   request
   complete-request
   input))

(defn cancel-request-command
  [request input]
  (request-update-command
   :cancel
   request
   cancel-request
   input))

;; =============================================================================
;; Request Assignment vocabulary
;; =============================================================================

(def assignment-roles
  #{:primary
    :collaborator})

(def assignment-statuses
  #{:active
    :ended})

(defn assignment-role?
  [value]
  (contains?
   assignment-roles
   value))

(defn assignment-status?
  [value]
  (contains?
   assignment-statuses
   value))

(defn assignment-source?
  [value]
  (qualified-keyword?
   value))

;; =============================================================================
;; Request Assignment projections
;; =============================================================================

(defn assignment-id
  [assignment]
  (:xt/id
   assignment))

(defn assignment-request-id
  [assignment]
  (:request-assignment/request
   assignment))

(defn assignment-helper-id
  [assignment]
  (:request-assignment/helper
   assignment))

(defn assignment-role
  [assignment]
  (:request-assignment/role
   assignment))

(defn assignment-status
  [assignment]
  (:request-assignment/status
   assignment))

(defn assignment-source
  [assignment]
  (:request-assignment/source
   assignment))

(defn assignment-assigned-by
  [assignment]
  (:request-assignment/assigned-by
   assignment))

(defn assignment-assigned-at
  "Returns when the helper was assigned.

   RequestAssignment creation is itself the assignment event, so this is the
   generic persisted creation time rather than a duplicate assigned-at field."
  [assignment]
  (:request-assignment/created-at
   assignment))

(defn assignment-ended-at
  [assignment]
  (:request-assignment/ended-at
   assignment))

(defn assignment-ended-by
  [assignment]
  (:request-assignment/ended-by
   assignment))

(defn assignment-end-reason
  [assignment]
  (:request-assignment/end-reason
   assignment))

(defn assignment-revision
  [assignment]
  (:request-assignment/revision
   assignment))

(defn assignment-created-at
  [assignment]
  (:request-assignment/created-at
   assignment))

(defn assignment-updated-at
  [assignment]
  (:request-assignment/updated-at
   assignment))

;; =============================================================================
;; Request Assignment facts
;; =============================================================================

(defn assignment-active?
  [assignment]
  (=
   :active
   (assignment-status
    assignment)))

(defn assignment-ended?
  [assignment]
  (=
   :ended
   (assignment-status
    assignment)))

(defn primary-assignment?
  [assignment]
  (=
   :primary
   (assignment-role
    assignment)))

(defn collaborator-assignment?
  [assignment]
  (=
   :collaborator
   (assignment-role
    assignment)))

(defn active-primary-assignment?
  [assignment]
  (and
   (assignment-active?
    assignment)

   (primary-assignment?
    assignment)))

(defn active-collaborator-assignment?
  [assignment]
  (and
   (assignment-active?
    assignment)

   (collaborator-assignment?
    assignment)))

(defn assignment-for-request?
  [assignment expected-request-id]
  (and
   (uuid?
    expected-request-id)

   (=
    expected-request-id
    (assignment-request-id
     assignment))))

(defn assignment-for-helper?
  [assignment expected-helper-id]
  (and
   (uuid?
    expected-helper-id)

   (=
    expected-helper-id
    (assignment-helper-id
     assignment))))

(defn active-assignment-for-helper?
  [assignment expected-helper-id]
  (and
   (assignment-active?
    assignment)

   (assignment-for-helper?
    assignment
    expected-helper-id)))

(defn active-assignment-for-request?
  [assignment expected-request-id]
  (and
   (assignment-active?
    assignment)

   (assignment-for-request?
    assignment
    expected-request-id)))

;; =============================================================================
;; Request Assignment collection facts
;; =============================================================================

(defn active-assignments
  [assignments]
  (filterv
   assignment-active?
   assignments))

(defn ended-assignments
  [assignments]
  (filterv
   assignment-ended?
   assignments))

(defn primary-assignments
  [assignments]
  (filterv
   primary-assignment?
   assignments))

(defn collaborator-assignments
  [assignments]
  (filterv
   collaborator-assignment?
   assignments))

(defn active-primary-assignments
  [assignments]
  (filterv
   active-primary-assignment?
   assignments))

(defn active-collaborator-assignments
  [assignments]
  (filterv
   active-collaborator-assignment?
   assignments))

(defn active-assignment-for-helper
  "Returns the helper's one active assignment, nil when absent, and throws when
   the collection contains more than one.

   Persistence workflows should prevent this ambiguity. Failing here keeps
   corrupted Request assignment sets from silently selecting an arbitrary
   record."
  [assignments expected-helper-id]
  (let [matches
        (filterv
         #(active-assignment-for-helper?
           %
           expected-helper-id)
         assignments)]
    (case
     (count
      matches)

     0
     nil

     1
     (first
      matches)

     (fail!
      :request-assignment/ambiguous-helper
      "The Request assignment set is invalid."
      {:helper
       "A helper may have at most one active assignment on a Request."}
      {:request-assignment/helper
       expected-helper-id

       :request-assignment/count
       (count
        matches)}))))

(defn active-primary-assignment
  "Returns the one active primary assignment, nil when absent, and throws when
   the collection contains more than one."
  [assignments]
  (let [matches
        (active-primary-assignments
         assignments)]
    (case
     (count
      matches)

     0
     nil

     1
     (first
      matches)

     (fail!
      :request-assignment/ambiguous-primary
      "The Request assignment set is invalid."
      {:role
       "A Request may have at most one active primary assignment."}
      {:request-assignment/count
       (count
        matches)}))))

(defn active-helper-ids
  [assignments]
  (into
   #{}
   (map
    assignment-helper-id)
   (active-assignments
    assignments)))

(defn active-collaborator-helper-ids
  [assignments]
  (into
   #{}
   (map
    assignment-helper-id)
   (active-collaborator-assignments
    assignments)))

;; =============================================================================
;; Request Assignment document consistency
;; =============================================================================

(defn- assignment-version-consistent?
  [assignment]
  (let [created-at
        (assignment-created-at
         assignment)

        updated-at
        (assignment-updated-at
         assignment)]
    (and
     (command/versioned-document?
      assignment
      assignment-version)

     (instant?
      created-at)

     (instant?
      updated-at)

     (at-or-before?
      created-at
      updated-at))))

(defn- assignment-state-consistent?
  [assignment]
  (case
   (assignment-status
    assignment)

    :active
    (none-present?
     assignment
     [:request-assignment/ended-at
      :request-assignment/ended-by
      :request-assignment/end-reason])

    :ended
    (and
     (instant?
      (assignment-ended-at
       assignment))

     (qualified-keyword?
      (assignment-end-reason
       assignment)))

    false))

(defn assignment-document-consistent?
  "Returns true when value satisfies every local persisted RequestAssignment
   invariant.

   This does not establish that the Request exists, that helper-id names an
   eligible helper, or that the Request's other assignments satisfy
   collection-level uniqueness rules."
  [value]
  (and
   (map?
    value)

   (uuid?
    (assignment-id
     value))

   (uuid?
    (assignment-request-id
     value))

   (uuid?
    (assignment-helper-id
     value))

   (assignment-role?
    (assignment-role
     value))

   (assignment-status?
    (assignment-status
     value))

   (assignment-source?
    (assignment-source
     value))

   (optional-uuid?
    (assignment-assigned-by
     value))

   (optional-uuid?
    (assignment-ended-by
     value))

   (or
    (nil?
     (assignment-end-reason
      value))

    (qualified-keyword?
     (assignment-end-reason
      value)))

   (assignment-version-consistent?
    value)

   (optional-time-within?
    (assignment-created-at
     value)
    (assignment-ended-at
     value)
    (assignment-updated-at
     value))

   (assignment-state-consistent?
    value)))

(defn- assignment-context
  [assignment]
  {:request-assignment/id
   (assignment-id
    assignment)

   :request-assignment/request
   (assignment-request-id
    assignment)

   :request-assignment/helper
   (assignment-helper-id
    assignment)

   :request-assignment/role
   (assignment-role
    assignment)

   :request-assignment/status
   (assignment-status
    assignment)

   :request-assignment/revision
   (assignment-revision
    assignment)})

(defn require-assignment-document
  [assignment]
  (ensure!
   (assignment-document-consistent?
    assignment)

   :request-assignment/invalid-document

   "The Request assignment operation is invalid."

   {:request-assignment
    "The Request assignment document is internally inconsistent."}

   (assignment-context
    assignment))

  assignment)

;; =============================================================================
;; Request Assignment construction
;; =============================================================================

(defn- normalize-create-assignment-input
  [input]
  (let [input
        (or
         input
         {})]
    {:id
     (:id
      input)

     :request-id
     (:request-id
      input)

     :helper-id
     (:helper-id
      input)

     :role
     (:role
      input)

     :source
     (:source
      input)

     :actor-id
     (:actor-id
      input)

     :now
     (:now
      input)}))

(defn- create-assignment-input-errors
  [{:keys
    [id
     request-id
     helper-id
     role
     source
     actor-id
     now]}]
  (cond-> {}
    (not
     (uuid?
      id))
    (assoc
     :id
     "A Request assignment UUID is required.")

    (not
     (uuid?
      request-id))
    (assoc
     :request-id
     "A Request UUID is required.")

    (not
     (uuid?
      helper-id))
    (assoc
     :helper-id
     "A helper User UUID is required.")

    (not
     (assignment-role?
      role))
    (assoc
     :role
     "The assignment role must be primary or collaborator.")

    (not
     (assignment-source?
      source))
    (assoc
     :source
     "The assignment source must be a qualified keyword.")

    (not
     (optional-uuid?
      actor-id))
    (assoc
     :actor-id
     "The assigning actor must be a UUID when supplied.")

    (not
     (instant?
      now))
    (assoc
     :now
     "A valid Request assignment creation time is required.")))

(defn- new-assignment
  [input]
  (let [{:keys
         [id
          request-id
          helper-id
          role
          source
          actor-id
          now]
         :as normalized}
        (normalize-create-assignment-input
         input)

        errors
        (create-assignment-input-errors
         normalized)]
    (when
     (seq
      errors)
      (fail!
       :request-assignment/invalid-create-input
       "A valid Request assignment could not be created."
       errors
       {:request-assignment/id
        id

        :request-assignment/request
        request-id

        :request-assignment/helper
        helper-id}))

    (require-assignment-document
     (cond->
      {:xt/id
       id

       :request-assignment/request
       request-id

       :request-assignment/helper
       helper-id

       :request-assignment/role
       role

       :request-assignment/status
       :active

       :request-assignment/source
       source

       :request-assignment/revision
       0

       :request-assignment/created-at
       now

       :request-assignment/updated-at
       now}

       actor-id
       (assoc
        :request-assignment/assigned-by
        actor-id)))))

;; =============================================================================
;; Request Assignment mutation
;; =============================================================================

(defn- update-assignment
  [assignment now mutation]
  (require-assignment-document
   assignment)

  (ensure!
   (valid-change-time?
    assignment
    assignment-version
    now)

   :request-assignment/invalid-time

   "The Request assignment operation is invalid."

   {:now
    "The change time must not precede the current assignment update time."}

   (assignment-context
    assignment))

  (let [changed
        (mutation
         assignment)]

    (ensure!
     (not=
      assignment
      changed)

     :request-assignment/unchanged

     "The Request assignment operation is invalid."

     {:request-assignment
      "The operation would not change the Request assignment."}

     (assignment-context
      assignment))

    (require-assignment-document
     (command/bump-version
      changed
      assignment-version
      now))))

(defn- end-assignment
  [assignment {:keys
               [actor-id
                reason
                now]}]
  (require-assignment-document
   assignment)

  (ensure!
   (assignment-active?
    assignment)

   :request-assignment/already-ended

   "The Request assignment operation is invalid."

   {:status
    "Only an active Request assignment can be ended."}

   (assignment-context
    assignment))

  (ensure!
   (optional-uuid?
    actor-id)

   :request-assignment/invalid-end-input

   "The Request assignment end is invalid."

   {:actor-id
    "The ending actor must be a UUID when supplied."}

   (assignment-context
    assignment))

  (ensure!
   (qualified-keyword?
    reason)

   :request-assignment/invalid-end-input

   "The Request assignment end is invalid."

   {:reason
    "A qualified assignment end reason is required."}

   (assignment-context
    assignment))

  (update-assignment
   assignment
   now
   #(cond->
     (assoc
      %
      :request-assignment/status
      :ended

      :request-assignment/ended-at
      now

      :request-assignment/end-reason
      reason)

     actor-id
     (assoc
      :request-assignment/ended-by
      actor-id))))

;; =============================================================================
;; Canonical Request Assignment commands
;; =============================================================================

(defn create-assignment-command
  [input]
  (command/create
   assignment-entity-type
   (new-assignment
    input)
   assignment-version))

(defn end-assignment-command
  [assignment input]
  (command/update-command
   assignment-entity-type
   :end
   assignment
   (end-assignment
    assignment
    input)
   assignment-version))
