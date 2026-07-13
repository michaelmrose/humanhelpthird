(ns net.humanhelp.site.model.common
  "Shared mechanics for persisted HumanHelp model documents.

   This namespace contains conventions common to versioned domain entities:

   - validating mutation timestamps
   - incrementing document revisions
   - throwing structured domain-validation exceptions

   General string, map, and presentation utilities do not belong here."
  (:import
   [java.time Instant ZonedDateTime]))

;; =============================================================================
;; Timestamp mechanics
;; =============================================================================

(defn timestamp-value?
  "Returns true for timestamp types currently used by HumanHelp models.

   Existing user documents use ZonedDateTime, while newer request documents
   currently use Instant."
  [value]
  (or
   (instance? Instant value)
   (instance? ZonedDateTime value)))

(defn timestamp<=
  "Returns true when a and b are compatible timestamps and a is not after b.

   Mixed timestamp representations are rejected rather than converted
   implicitly."
  [a b]
  (and
   (timestamp-value? a)
   (timestamp-value? b)
   (= (class a)
      (class b))
   (not
    (pos?
     (compare a b)))))

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
