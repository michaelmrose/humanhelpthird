(ns net.humanhelp.schema.common
  "Shared Malli registry entries used by HumanHelp model schemas.

   Model-specific schema namespaces should require this namespace rather than
   requiring net.humanhelp.schema. This keeps the root schema namespace free to
   assemble all model registries without creating dependency cycles."
  (:require
   [clojure.string :as str]
   [tick.core :as tick]))

(def ?
  "Malli map-entry properties for an optional field."
  {:optional true})

;; =============================================================================
;; Shared predicates
;; =============================================================================

(defn non-blank-string?
  [value]
  (and (string? value)
       (not (str/blank? value))))

;; =============================================================================
;; Shared schema registry
;; =============================================================================

(def schema
  {::id
   :uuid

   ::string
   [:string {:max 1000}]

   ::non-blank-string
   [:and
    ::string
    [:fn non-blank-string?]]

   ::email
   [:and
    [:string {:min 3
              :max 320}]
    [:fn non-blank-string?]]

   ::phone-digits
   [:and
    [:string {:min 10
              :max 10}]
    [:re #"^[0-9]{10}$"]]

   ::phone-display
   [:string {:max 20}]

   ::display-name
   [:and
    [:string {:min 1
              :max 120}]
    [:fn non-blank-string?]]

   ::token-hash
   [:and
    [:string {:min 32
              :max 256}]
    [:fn non-blank-string?]]

   ;; Existing user and authentication documents currently use ZonedDateTime.
   ::zdt
   [:fn tick/zoned-date-time?]

   ;; Gesso FX and the current request model use java.time.Instant.
   ;; Both schemas are retained until persisted timestamps are standardized.
   ::instant
   'inst?

   ::revision
   [:int {:min 0}]})
