(ns net.humanhelp.site.model.common
  "Shared mechanics for persisted HumanHelp model documents.

   This namespace contains conventions common to multiple domain entities:

   - timestamp ordering
   - mutation-time validation
   - revision updates
   - structured domain-validation failures
   - model command descriptions
   - small model-input cardinality checks

   General application, presentation, collection, and string utilities do not
   belong here."
  (:import
   [java.time Instant ZonedDateTime]))

;; =============================================================================
;; Model input mechanics
;; =============================================================================

(defn exactly-one-present?
  "Returns true when exactly one supplied value is non-nil."
  [& values]
  (= 1
     (count
      (filter some? values))))

;; =============================================================================
;; Timestamp mechanics
;; =============================================================================

(defn timestamp-value?
  "Returns true for timestamp types currently used by HumanHelp models.

   Existing user-model documents use ZonedDateTime, while the current request
   model uses Instant."
  [value]
  (or
   (instance? Instant value)
   (instance? ZonedDateTime value)))

(defn compatible-timestamps?
  "Returns true when both values are supported timestamps of the same type.

   Mixed timestamp representations are rejected rather than converted
   implicitly."
  [a b]
  (and
   (timestamp-value? a)
   (timestamp-value? b)
   (= (class a)
      (class b))))

(defn timestamp<=
  "Returns true when a and b are compatible timestamps and a is not after b."
  [a b]
  (and
   (compatible-timestamps? a b)
   (not
    (pos?
     (compare a b)))))

(defn timestamp<
  "Returns true when a and b are compatible timestamps and a is before b."
  [a b]
  (and
   (compatible-timestamps? a b)
   (neg?
    (compare a b))))

(defn optional-between?
  "Returns true when value is nil or falls inclusively between start and end.

   All non-nil timestamps must use the same representation."
  [start value end]
  (or
   (nil? value)
   (and
    (timestamp<= start value)
    (timestamp<= value end))))

(defn valid-change-time?
  "Returns true when now is a valid mutation time for document.

   now must:

   - use the same timestamp representation as the document
   - be at or after the document's creation time
   - be at or after its most recent update time"
  [document created-at-key updated-at-key now]
  (and
   (timestamp-value? now)

   (timestamp<=
    (get document created-at-key)
    now)

   (timestamp<=
    (get document updated-at-key)
    now)))

;; =============================================================================
;; Versioned-document mechanics
;; =============================================================================

(defn bump-revision
  "Increments a document revision and records its new update time."
  [document revision-key updated-at-key now]
  (-> document
      (update
       revision-key
       (fnil inc 0))

      (assoc
       updated-at-key
       now)))

;; =============================================================================
;; Domain validation
;; =============================================================================

(defn throw-invalid!
  "Throws the standard exception used when domain input cannot construct or
   update a valid document."
  [error-type message errors input]
  (throw
   (ex-info
    message
    {:error/type error-type
     :errors errors
     :input input})))

;; =============================================================================
;; Model command descriptions
;; =============================================================================

(defn create-command
  "Describes creation of a persisted model document."
  [entity-type document]
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

   expected is the entity-specific version description used by the commit
   implementation for atomic compare-and-set."
  [entity-type operation expected before after]
  {:model/entity-type
   entity-type

   :model/operation
   operation

   :model/id
   (:xt/id before)

   :model/expected
   expected

   :model/before
   before

   :model/after
   after})

(defn command-document
  "Returns the resulting document from a model command."
  [command]
  (or
   (:model/after command)

   (throw
    (ex-info
     "Model command does not contain a resulting document."
     {:error/type
      :model/missing-command-document

      :command
      command}))))
