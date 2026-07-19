(ns net.humanhelp.site.model.authorization-scope
  "Shared structural values for HumanHelp authorization scopes.

   An authorization scope identifies the Organization hierarchy level at which
   a User role assignment applies:

     {:scope/type :organization|:organization-group|:location
      :scope/id   uuid}

   Organization owns whether referenced entities exist, which parent
   relationships are valid, how ancestry is traversed, and how authoritative
   scope contexts are constructed.

   User owns whether role assignments apply through an authoritative scope
   context and which effective roles or capabilities result.

   This namespace owns only the common structural vocabulary. It does not query
   XTDB, inspect persisted documents, authorize actors, calculate hierarchy
   ancestry, or define Gesso Live delivery scopes.")

;; =============================================================================
;; Scope types
;; =============================================================================

(def scope-types
  #{:organization
    :organization-group
    :location})

(def parent-scope-types
  #{:organization
    :organization-group})

(defn scope-type?
  [value]
  (contains?
   scope-types
   value))

(defn parent-scope-type?
  [value]
  (contains?
   parent-scope-types
   value))

;; =============================================================================
;; Scope references
;; =============================================================================

(defn scope-reference?
  "Returns true when value structurally references one supported authorization
   scope.

   This does not establish that the referenced entity exists, belongs to a
   particular Organization, or occupies a valid place in the hierarchy."
  [value]
  (and
   (map? value)

   (scope-type?
    (:scope/type value))

   (uuid?
    (:scope/id value))))

(defn parent-scope-reference?
  "Returns true when value may structurally serve as the parent of an
   Organization Group or Location.

   Whether a particular parent relationship is valid remains Organization
   policy."
  [value]
  (and
   (scope-reference?
    value)

   (parent-scope-type?
    (:scope/type value))))

(defn organization-scope
  [organization-id]
  {:scope/type :organization
   :scope/id organization-id})

(defn organization-group-scope
  [organization-group-id]
  {:scope/type :organization-group
   :scope/id organization-group-id})

(defn location-scope
  [location-id]
  {:scope/type :location
   :scope/id location-id})

(defn organization-scope?
  [scope]
  (and
   (scope-reference?
    scope)

   (=
    :organization
    (:scope/type scope))))

(defn organization-group-scope?
  [scope]
  (and
   (scope-reference?
    scope)

   (=
    :organization-group
    (:scope/type scope))))

(defn location-scope?
  [scope]
  (and
   (scope-reference?
    scope)

   (=
    :location
    (:scope/type scope))))

(defn same-scope?
  [a b]
  (and
   (scope-reference?
    a)

   (scope-reference?
    b)

   (=
    a
    b)))

;; =============================================================================
;; Applicable scope chains
;; =============================================================================

(defn applicable-scopes?
  "Returns true for a non-empty, target-first vector of distinct structural
   scope references.

   This predicate does not prove hierarchy or ownership. Organization must
   derive the collection from valid Organization data.

   Use scope-context? when the Organization ID, target position, Organization
   root, and operational value must also be validated."
  [scopes]
  (and
   (vector?
    scopes)

   (seq
    scopes)

   (every?
    scope-reference?
    scopes)

   (=
    (count scopes)
    (count
     (distinct scopes)))))

(defn scope-context?
  "Returns true for the compact structural contract shared by Organization,
   User, and Request:

     {:organization/id   uuid
      :scope/target      scope-reference
      :scope/applicable  [target ... organization-scope]
      :scope/operational? boolean}

   Applicable scopes must be ordered target-first and Organization-last.
   This predicate validates structure only. It does not prove that the supplied
   chain represents the persisted Organization hierarchy."
  [value]
  (let [organization-id
        (:organization/id value)

        target
        (:scope/target value)

        applicable
        (:scope/applicable value)]

    (boolean
     (and
      (map?
       value)

      (uuid?
       organization-id)

      (scope-reference?
       target)

      (applicable-scopes?
       applicable)

      (same-scope?
       target
       (first applicable))

      (same-scope?
       (organization-scope organization-id)
       (peek applicable))

      (boolean?
       (:scope/operational? value))))))
