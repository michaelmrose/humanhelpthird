(ns net.humanhelp.site.model.common
  "Shared mechanics for HumanHelp persisted model documents.

   This namespace owns only model-independent behavior:

   - timestamp predicates and ordering;
   - standardized domain-validation failures;
   - version-metadata validation;
   - versioned-document consistency;
   - guarded revision updates;
   - expected-version descriptions;
   - authorization-version guard construction;
   - generic create and update command descriptions.

   Individual models continue to own their document fields, lifecycle rules,
   invariants, authorization policy, queries, assertions, and semantic changes."
  (:import
   [java.time Instant]))

;; =============================================================================
;; Errors
;; =============================================================================

(defn- fail!
  ([error-type message]
   (fail!
    error-type
    message
    nil))

  ([error-type message details]
   (throw
    (ex-info
     message
     (cond->
      {:error/type
       error-type}

       (some?
        details)
       (assoc
        :error/details
        details))))))

(defn exactly-one-present?
  "Returns true when exactly one supplied value is non-nil."
  [& values]
  (=
   1
   (count
    (filter
     some?
     values))))

(defn at-most-one-present?
  "Returns true when zero or one supplied values are non-nil."
  [& values]
  (<=
   (count
    (filter
     some?
     values))
   1))

;; =============================================================================
;; Timestamp values
;; =============================================================================

(defn timestamp-value?
  "Returns true when value is a java.time.Instant."
  [value]
  (instance?
   Instant
   value))

(defn timestamp<
  "Returns true when left and right are Instants and left is before right."
  [left right]
  (and
   (timestamp-value?
    left)

   (timestamp-value?
    right)

   (.isBefore
    ^Instant left
    ^Instant right)))

(defn timestamp<=
  "Returns true when left and right are Instants and left is not after right."
  [left right]
  (and
   (timestamp-value?
    left)

   (timestamp-value?
    right)

   (not
    (.isAfter
     ^Instant left
     ^Instant right))))

(defn timestamps-ordered?
  "Returns true when every supplied timestamp is at or after the preceding one.

   An empty or single-value sequence is considered ordered."
  [& values]
  (and
   (every?
    timestamp-value?
    values)

   (every?
    true?
    (map
     timestamp<=
     values
     (rest values)))))

(defn between?
  "Returns true when value falls inclusively between start and end."
  [start value end]
  (and
   (timestamp<=
    start
    value)

   (timestamp<=
    value
    end)))

(defn optional-between?
  "Returns true when value is nil or falls inclusively between start and end."
  [start value end]
  (or
   (nil?
    value)

   (between?
    start
    value
    end)))

;; =============================================================================
;; Domain-validation failures
;; =============================================================================

(defn throw-invalid!
  "Throws the standard exception used when domain input cannot construct or
   update a valid model document.

   errors should be a structured map suitable for programmatic handling.

   context is optional and should contain only information that is safe and
   useful for diagnosing the failure. Callers should not automatically include
   credentials, raw bearer tokens, unredacted messages, or arbitrary HTTP
   input."
  ([error-type message errors]
   (throw-invalid!
    error-type
    message
    errors
    nil))

  ([error-type message errors context]
   (fail!
    error-type
    message
    (cond->
     {:errors
      errors}

      (some?
       context)
      (assoc
       :context
       context)))))

;; =============================================================================
;; Versioned-document metadata
;; =============================================================================

(defn valid-version-metadata?
  "Returns true when metadata identifies three distinct document attributes:

   - :revision-key
   - :created-at-key
   - :updated-at-key"
  [{:keys
    [revision-key
     created-at-key
     updated-at-key]}]
  (and
   (keyword?
    revision-key)

   (keyword?
    created-at-key)

   (keyword?
    updated-at-key)

   (=
    3
    (count
     (set
      [revision-key
       created-at-key
       updated-at-key])))))

(defn versioned-document-consistent?
  "Returns true when document satisfies the shared persisted-document
   conventions described by metadata.

   A versioned document must have:

   - a UUID :xt/id
   - a natural-number revision
   - Instant creation and update timestamps
   - an update timestamp at or after creation"
  [document
   {:keys
    [revision-key
     created-at-key
     updated-at-key]
    :as metadata}]
  (and
   (map?
    document)

   (valid-version-metadata?
    metadata)

   (uuid?
    (:xt/id document))

   (nat-int?
    (get
     document
     revision-key))

   (timestamp-value?
    (get
     document
     created-at-key))

   (timestamp-value?
    (get
     document
     updated-at-key))

   (timestamp<=
    (get
     document
     created-at-key)

    (get
     document
     updated-at-key))))

(defn valid-change-time?
  "Returns true when now is a valid mutation time for document.

   The document must satisfy the shared versioned-document conventions, and now
   must be at or after its most recent update time."
  [document
   {:keys
    [updated-at-key]
    :as metadata}
   now]
  (and
   (versioned-document-consistent?
    document
    metadata)

   (timestamp-value?
    now)

   (timestamp<=
    (get
     document
     updated-at-key)

    now)))

(defn version-update-consistent?
  "Returns true when after is a valid next version of before.

   This checks shared version mechanics only:

   - both documents satisfy the same metadata convention
   - document identity is unchanged
   - creation time is unchanged
   - revision increases by exactly one
   - update time does not move backward

   Entity-specific invariants remain the responsibility of the model domain."
  [before
   after
   {:keys
    [revision-key
     created-at-key
     updated-at-key]
    :as metadata}]
  (and
   (versioned-document-consistent?
    before
    metadata)

   (versioned-document-consistent?
    after
    metadata)

   (=
    (:xt/id before)
    (:xt/id after))

   (=
    (get
     before
     created-at-key)

    (get
     after
     created-at-key))

   (=
    (inc
     (get
      before
      revision-key))

    (get
     after
     revision-key))

   (timestamp<=
    (get
     before
     updated-at-key)

    (get
     after
     updated-at-key))))

;; =============================================================================
;; Versioned-document operations
;; =============================================================================

(defn bump-revision
  "Returns document with its revision incremented and update time set to now.

   The supplied document and mutation time must already satisfy the shared
   version conventions. A missing revision is rejected rather than silently
   manufactured."
  [document
   {:keys
    [revision-key
     updated-at-key]
    :as metadata}
   now]
  (when-not
   (valid-change-time?
    document
    metadata
    now)
    (fail!
     :model/invalid-change-time
     "Cannot update the document at the supplied time."
     {:model/id
      (:xt/id document)

      :metadata
      metadata

      :now
      now}))

  (-> document
      (update
       revision-key
       inc)

      (assoc
       updated-at-key
       now)))

(defn expected-version
  "Returns the standardized compare-and-set description for document.

   Attribute keys are included so a generic persistence implementation can
   compare entity-specific revision and update-time attributes.

   metadata must include all three version keys. The created-at attribute is
   validated as part of the complete versioned-document contract even though it
   is not copied into the compare-and-set description."
  [document
   {:keys
    [revision-key
     updated-at-key]
    :as metadata}]
  (when-not
   (versioned-document-consistent?
    document
    metadata)
    (fail!
     :model/invalid-versioned-document
     "Cannot describe the version of an invalid document."
     {:model/id
      (:xt/id document)

      :metadata
      metadata}))

  {:model/id
   (:xt/id document)

   :model/revision-key
   revision-key

   :model/revision
   (get
    document
    revision-key)

   :model/updated-at-key
   updated-at-key

   :model/updated-at
   (get
    document
    updated-at-key)})

(defn authorization-version
  "Returns one generic authorization-version guard for document.

   Model-specific code decides whether a document actually establishes
   authorization for an operation. This helper only validates the entity type
   and document version contract, then packages the expected version in the
   shape understood by model.fx:

     {:model/entity-type entity-type
      :model/expected    expected-version}

   The complete metadata map is passed through expected-version, so missing
   :created-at-key metadata or an invalid creation timestamp is rejected even
   though created-at is not part of the resulting compare-and-set map."
  [entity-type document metadata]
  (when-not
   (keyword?
    entity-type)
    (fail!
     :model/invalid-entity-type
     "An authorization version requires a keyword entity type."
     {:entity-type
      entity-type

      :model/id
      (:xt/id document)

      :metadata
      metadata}))

  {:model/entity-type
   entity-type

   :model/expected
   (expected-version
    document
    metadata)})

;; =============================================================================
;; Model command descriptions
;; =============================================================================

(defn create-command
  "Describes creation of a persisted model document.

   Newly created documents must satisfy their version metadata and begin at
   revision zero."
  [entity-type
   document
   {:keys
    [revision-key]
    :as metadata}]
  (when-not
   (keyword?
    entity-type)
    (fail!
     :model/invalid-entity-type
     "A model command requires a keyword entity type."
     {:entity-type
      entity-type}))

  (when-not
   (and
    (versioned-document-consistent?
     document
     metadata)

    (zero?
     (get
      document
      revision-key)))
    (fail!
     :model/invalid-create-command
     "Cannot create a command from an invalid initial document."
     {:entity-type
      entity-type

      :model/id
      (:xt/id document)

      :metadata
      metadata}))

  {:model/entity-type
   entity-type

   :model/operation
   :create

   :model/id
   (:xt/id document)

   :model/after
   document})

(defn update-command
  "Describes a version-checked update of a persisted model document.

   before and after must form a valid single-revision progression according to
   metadata. The expected compare-and-set description is generated from before
   rather than supplied independently."
  [entity-type
   operation
   before
   after
   metadata]
  (when-not
   (keyword?
    entity-type)
    (fail!
     :model/invalid-entity-type
     "A model command requires a keyword entity type."
     {:entity-type
      entity-type}))

  (when-not
   (and
    (keyword?
     operation)

    (not=
     :create
     operation))
    (fail!
     :model/invalid-operation
     "An update command requires a non-create keyword operation."
     {:entity-type
      entity-type

      :operation
      operation}))

  (when-not
   (version-update-consistent?
    before
    after
    metadata)
    (fail!
     :model/invalid-update-command
     "Before and after do not describe a valid versioned update."
     {:entity-type
      entity-type

      :operation
      operation

      :model/id
      (:xt/id before)

      :metadata
      metadata}))

  {:model/entity-type
   entity-type

   :model/operation
   operation

   :model/id
   (:xt/id before)

   :model/expected
   (expected-version
    before
    metadata)

   :model/before
   before

   :model/after
   after})

(defn command-document
  "Returns the resulting document from a model command."
  [command]
  (or
   (:model/after command)

   (fail!
    :model/missing-command-document
    "Model command does not contain a resulting document."
    {:command
     command})))
