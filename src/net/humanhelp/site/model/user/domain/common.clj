(ns net.humanhelp.site.model.user.domain.common
  (:require
   [clojure.string :as str])
  (:import
   [java.util Locale]))

;; Contact values --------------------------------------------------------------

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

;; Role values -----------------------------------------------------------------

(def roles
  #{:helper
    :supervisor
    :admin})

(defn role?
  [value]
  (contains?
   roles
   value))

;; Scope references ------------------------------------------------------------

(def scope-types
  #{:organization
    :organization-group
    :location})

(defn scope-type?
  [value]
  (contains?
   scope-types
   value))

(defn scope-reference?
  "Returns true when value structurally references one supported access scope.

   This does not establish that the referenced organization, group, or location
   exists or that it belongs to a particular organization."
  [value]
  (and
   (map? value)

   (scope-type?
    (:scope/type value))

   (uuid?
    (:scope/id value))))

(defn organization-scope?
  [scope]
  (and
   (scope-reference?
    scope)

   (=
    :organization
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
