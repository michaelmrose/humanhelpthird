(ns net.humanhelp.site.model.request.domain.content
  "Pure rules for the customer-editable content of an assistance request.

   This namespace owns only:

   - the request title;
   - optional additional details;
   - an optional within-location description;
   - normalization and validation of those values;
   - projecting and replacing that content on a Request document.

   It does not own Request identity, organization or location references,
   requestor ownership, lifecycle state, audit actors, revisions, persistence,
   authorization, or Live invalidation."
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; Limits
;; =============================================================================

(def title-max
  60)

(def details-max
  500)

(def location-detail-max
  120)

;; =============================================================================
;; Canonical text
;; =============================================================================

(defn normalize-text
  "Trims a string and converts blank text to nil.

   Non-string values are returned unchanged so validation can report their
   actual type instead of silently coercing them."
  [value]
  (if
   (string? value)
    (let [normalized
          (str/trim value)]
      (when-not
       (str/blank? normalized)
        normalized))
    value))

(defn canonical-text?
  [value max-length]
  (and
   (string? value)
   (not
    (str/blank? value))
   (=
    value
    (str/trim value))
   (<=
    (count value)
    max-length)))

(defn optional-canonical-text?
  [value max-length]
  (or
   (nil? value)
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

;; =============================================================================
;; Content values
;; =============================================================================

(def content-keys
  #{:title
    :details
    :location-detail})

(def request-content-keys
  #{:request/title
    :request/details
    :request/location-detail})

(defn content?
  [value]
  (and
   (map? value)
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

(defn content
  "Projects customer-editable content from a Request document.

   Optional absent fields are represented as nil so the returned value always
   has one stable shape."
  [request]
  {:title
   (:request/title request)

   :details
   (:request/details request)

   :location-detail
   (:request/location-detail request)})

(defn content-consistent?
  [request]
  (content?
   (content request)))

;; =============================================================================
;; Normalization and validation
;; =============================================================================

(defn normalize-content
  [input]
  (let [input
        (or input {})]
    {:title
     (normalize-text
      (:title input))

     :details
     (normalize-text
      (:details input))

     :location-detail
     (normalize-text
      (:location-detail input))}))

(defn same-content?
  [request value]
  (=
   (content request)
   (normalize-content value)))

(defn content-errors
  [input]
  (let [{:keys
         [title
          details
          location-detail]}
        (normalize-content input)]
    (cond-> {}
      (not
       (title? title))
      (assoc
       :title
       (str
        "A non-blank request title of at most "
        title-max
        " characters is required."))

      (not
       (details? details))
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
   (content-errors input)))

(defn require-content
  "Returns normalized canonical content or throws with field-level errors."
  [input]
  (let [normalized
        (normalize-content input)

        errors
        (content-errors normalized)]
    (when
     (seq errors)
      (throw
       (ex-info
        "The request content is invalid."
        {:error/type
         :request/invalid-content

         :errors
         errors

         :input
         input})))

    normalized))

;; =============================================================================
;; Request-document application
;; =============================================================================

(defn apply-content
  "Replaces all customer-editable fields on a Request document.

   Blank optional values are normalized to nil and therefore remove the
   corresponding persisted keys."
  [request input]
  (let [{:keys
         [title
          details
          location-detail]}
        (require-content input)]
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
