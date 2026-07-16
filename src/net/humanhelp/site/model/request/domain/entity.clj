(ns net.humanhelp.site.model.request.domain.entity
  "Pure structural rules for the persisted Request entity.

   This namespace owns:

   - Request identity and version metadata;
   - Organization and location references;
   - flattening Request-owned requestor and content values into the document;
   - structural document consistency;
   - construction of a new open Request;
   - guarded revision of an existing Request.

   It deliberately does not own:

   - valid lifecycle statuses or transitions;
   - whether a Request may currently be edited, claimed, or cancelled;
   - User or Organization authorization;
   - capability-token authentication;
   - audit-event policy;
   - model command construction;
   - XTDB execution or Gesso Live invalidation.

   Lifecycle-specific consistency is added by domain.lifecycle and exposed
   through domain.core."
  (:require
   [net.humanhelp.site.model.common :as model.common]
   [net.humanhelp.site.model.request.domain.content :as content]
   [net.humanhelp.site.model.request.domain.requestor :as requestor]))

;; =============================================================================
;; Entity identity and versioning
;; =============================================================================

(def entity-type
  :request)

(def version
  {:revision-key :request/revision
   :created-at-key :request/created-at
   :updated-at-key :request/updated-at})

(def initial-status
  :open)

(defn request-id
  [request]
  (:xt/id request))

(defn organization-id
  [request]
  (:request/organization request))

(defn location-id
  [request]
  (:request/location request))

(defn revision
  [request]
  (:request/revision request))

(defn created-at
  [request]
  (:request/created-at request))

(defn updated-at
  [request]
  (:request/updated-at request))

(defn belongs-to-organization?
  [request expected-organization-id]
  (and
   (uuid? expected-organization-id)
   (=
    expected-organization-id
    (organization-id request))))

(defn at-location?
  [request expected-location-id]
  (and
   (uuid? expected-location-id)
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

;; =============================================================================
;; Stable entity projections
;; =============================================================================

(defn identity
  "Returns fields that no Request-domain operation may change."
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

(defn version-state
  "Returns version fields that a mutation function must leave untouched.

   revise updates these fields exactly once after applying the mutation."
  [request]
  {:request/revision
   (:request/revision request)

   :request/updated-at
   (:request/updated-at request)})

(defn same-identity?
  [left right]
  (=
   (identity left)
   (identity right)))

(defn same-version-state?
  [left right]
  (=
   (version-state left)
   (version-state right)))

;; =============================================================================
;; Structural consistency
;; =============================================================================

(defn structurally-consistent?
  "Returns true when value is a structurally valid persisted Request.

   This predicate intentionally accepts any keyword lifecycle status. The exact
   status vocabulary and all status-specific field combinations belong to
   domain.lifecycle. domain.core combines both predicates."
  [value]
  (and
   (map? value)

   (model.common/versioned-document-consistent?
    value
    version)

   (uuid?
    (:request/organization value))

   (uuid?
    (:request/location value))

   (requestor/requestor-consistent?
    value)

   (content/content-consistent?
    value)

   (keyword?
    (:request/status value))))

(defn- context
  [request]
  {:request/id
   (:xt/id request)

   :request/organization
   (:request/organization request)

   :request/location
   (:request/location request)

   :request/requestor
   (requestor/requestor request)

   :request/status
   (:request/status request)

   :request/revision
   (:request/revision request)})

(defn- fail!
  [error-type message errors details]
  (model.common/throw-invalid!
   error-type
   message
   errors
   details))

(defn require-structurally-consistent
  [request]
  (when-not
   (structurally-consistent?
    request)
    (fail!
     :request/invalid-entity
     "The Request entity is structurally invalid."
     {:request
      "The persisted Request fields are inconsistent."}
     (context request)))

  request)

;; =============================================================================
;; Construction input
;; =============================================================================

(def create-input-keys
  #{:id
    :organization-id
    :location-id
    :requestor
    :content
    :now})

(defn normalize-create-input
  "Returns the canonical constructor input.

   Expected shape:

     {:id              uuid
      :organization-id uuid
      :location-id     uuid
      :requestor       {:requestor/type :user|:capability
                        :requestor/id   uuid}
      :content         {:title ...
                        :details ...
                        :location-detail ...}
      :now             instant}"
  [input]
  (let [input
        (or input {})]
    {:id
     (:id input)

     :organization-id
     (:organization-id input)

     :location-id
     (:location-id input)

     :requestor
     (:requestor input)

     :content
     (content/normalize-content
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
        (normalize-create-input input)

        content-errors
        (content/content-errors
         content)]
    (cond-> {}
      (not
       (uuid? id))
      (assoc
       :id
       "A Request UUID is required.")

      (not
       (uuid? organization-id))
      (assoc
       :organization-id
       "An Organization UUID is required.")

      (not
       (uuid? location-id))
      (assoc
       :location-id
       "A location UUID is required.")

      (not
       (requestor/requestor-reference?
        requestor))
      (assoc
       :requestor
       "A valid User or capability requestor reference is required.")

      (seq content-errors)
      (assoc
       :content
       content-errors)

      (not
       (model.common/timestamp-value?
        now))
      (assoc
       :now
       "A valid Request creation time is required."))))

(defn valid-create-input?
  [input]
  (empty?
   (create-input-errors input)))

;; =============================================================================
;; Construction
;; =============================================================================

(defn- apply-requestor
  [request requestor-reference]
  (assoc
   request
   :request/requestor-type
   (:requestor/type requestor-reference)
   :request/requestor-id
   (:requestor/id requestor-reference)))

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
        (normalize-create-input input)

        errors
        (create-input-errors
         normalized)]
    (when
     (seq errors)
      (fail!
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

         :request/status
         initial-status

         :request/revision
         0

         :request/created-at
         now

         :request/updated-at
         now}
        (apply-requestor
         requestor)
        (content/apply-content
         content)
        require-structurally-consistent)))

;; =============================================================================
;; Guarded revision
;; =============================================================================

(defn- require-change-time!
  [request now]
  (when-not
   (model.common/timestamp-value?
    now)
    (fail!
     :request/invalid-change-time
     "The Request change time is invalid."
     {:now
      "A valid change time is required."}
     (context request)))

  (when-not
   (model.common/valid-change-time?
    request
    version
    now)
    (fail!
     :request/invalid-change-time
     "The Request change time is invalid."
     {:now
      "The change time must not precede the current Request update time."}
     (context request))))

(defn revise
  "Applies one pure mutation and bumps the Request revision exactly once.

   The mutation function may change content, lifecycle, assignment, or audit
   fields. It may not change stable Request identity, creation time, revision,
   or current updated-at. Lifecycle calls must additionally validate their
   resulting lifecycle state.

   Throws when:

   - request is structurally invalid;
   - now is not a valid monotonic change time;
   - mutation-fn is not callable;
   - mutation-fn returns a non-map;
   - stable identity or version fields were changed;
   - no actual change was made;
   - the revised entity is structurally invalid."
  [request now mutation-fn]
  (require-structurally-consistent
   request)

  (require-change-time!
   request
   now)

  (when-not
   (ifn? mutation-fn)
    (fail!
     :request/invalid-mutation
     "The Request mutation is invalid."
     {:mutation
      "The mutation must be callable."}
     (context request)))

  (let [changed
        (mutation-fn request)]
    (when-not
     (map? changed)
      (fail!
       :request/invalid-mutation
       "The Request mutation is invalid."
       {:mutation
        "The mutation must return a Request map."}
       (context request)))

    (when-not
     (same-identity?
      request
      changed)
      (fail!
       :request/immutable-identity
       "The Request mutation is invalid."
       {:request
        "Request identity, ownership, location, and creation time are immutable."}
       (context request)))

    (when-not
     (same-version-state?
      request
      changed)
      (fail!
       :request/invalid-version-mutation
       "The Request mutation is invalid."
       {:request
        "The mutation must not directly change revision or updated-at."}
       (context request)))

    (when
     (=
      request
      changed)
      (fail!
       :request/unchanged
       "The Request mutation is invalid."
       {:request
        "The mutation would not change the Request."}
       (context request)))

    (-> changed
        (model.common/bump-revision
         version
         now)
        require-structurally-consistent)))
