(ns net.humanhelp.site.model.user.domain.common
  "Small pure values shared by User identity, membership, role-assignment,
   invitation, and access namespaces.

   This namespace owns:

   - canonical email and phone values;
   - organization-local skill names;
   - User-model role values.

   Skills are intentionally simple organization-local strings. HumanHelp does
   not define what a skill means, what qualifications establish it, or whether
   two organizations use the same requirements for similarly named skills.
   Membership owns which skills an employee has within one Organization.

   Structural HumanHelp authorization scopes are owned by
   net.humanhelp.site.model.authorization-scope. Compatibility aliases remain
   here so existing User domain namespaces do not need to depend on two shared
   namespaces immediately.

   This namespace does not query XTDB, inspect Organization hierarchy,
   authorize actors, calculate effective access, or define Gesso Live delivery
   scopes."
  (:require
   [clojure.string :as str]
   [net.humanhelp.site.model.authorization-scope :as authorization-scope])
  (:import
   [java.util Locale]))

;; =============================================================================
;; Contact values
;; =============================================================================

(def email-max
  254)

(def email-pattern
  #"^[^\s@]+@[^\s@]+\.[^\s@]+$")

(def phone-pattern
  #"^\+[1-9][0-9]{7,14}$")

(defn normalize-email
  "HumanHelp treats email addresses as case-insensitive identifiers."
  [value]
  (when
   (string? value)
    (let [value'
          (.toLowerCase
           ^String
           (str/trim value)
           Locale/ROOT)]
      (when-not
       (str/blank? value')
        value'))))

(defn email?
  [value]
  (and
   (string? value)

   (=
    value
    (normalize-email value))

   (<=
    (count value)
    email-max)

   (boolean
    (re-matches
     email-pattern
     value))))

(defn normalize-phone
  "Trims only. Friendly local formatting must be parsed before domain entry."
  [value]
  (when
   (string? value)
    (let [value'
          (str/trim value)]
      (when-not
       (str/blank? value')
        value'))))

(defn phone?
  [value]
  (and
   (string? value)

   (=
    value
    (normalize-phone value))

   (boolean
    (re-matches
     phone-pattern
     value))))

;; =============================================================================
;; Skill values
;; =============================================================================

(def skill-max
  120)

(defn normalize-skill
  "Returns the canonical representation of an organization-local skill name.

   Skills are compared as case-insensitive strings. HumanHelp deliberately
   assigns no universal semantic meaning to the resulting value."
  [value]
  (when
   (string? value)
    (let [value'
          (.toLowerCase
           ^String
           (str/trim value)
           Locale/ROOT)]
      (when-not
       (str/blank? value')
        value'))))

(defn skill?
  [value]
  (and
   (string? value)

   (=
    value
    (normalize-skill value))

   (<=
    (count value)
    skill-max)))

(defn normalize-skills
  "Canonicalizes a collection of organization-local skill names.

   Returns a set so Membership cannot contain duplicate skills. Invalid values
   remain represented by nil in the result and are rejected by skills?."
  [values]
  (when
   (coll? values)
    (into
     #{}
     (map normalize-skill)
     values)))

(defn skills?
  [value]
  (and
   (set? value)

   (every?
    skill?
    value)))

;; =============================================================================
;; Role values
;; =============================================================================

(def roles
  #{:helper
    :supervisor
    :admin})

(defn role?
  [value]
  (contains?
   roles
   value))

;; =============================================================================
;; Shared authorization-scope compatibility aliases
;; =============================================================================

(def scope-types
  authorization-scope/scope-types)

(def scope-type?
  authorization-scope/scope-type?)

(def scope-reference?
  authorization-scope/scope-reference?)

(def organization-scope?
  authorization-scope/organization-scope?)

(def same-scope?
  authorization-scope/same-scope?)
