(ns net.humanhelp.site.mock-data
  "Persisted development/demo data for the Get Help flow.

   The mock Organization and Locations have fixed UUIDs so development links
   and QR-style entry remain deterministic across restarts.

   ensure! creates only missing documents. Existing documents are never
   rewritten; instead, they are checked to make sure the fixed fixture IDs
   still refer to the Organization and Locations this namespace expects.

   Organization creation is intentionally not performed through Organization's
   ordinary planners: fixtures require fixed IDs and a fixed creation time.
   This bootstrap namespace therefore uses Organization domain create-command
   constructors as a narrow fixture-only exception.

   Reads and document inspection go through organization.core. Transaction
   execution goes through gesso.model.tx."
  (:require
   [gesso.model.tx :as model.tx]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.organization.domain :as organization.domain])
  (:import
   [java.time Instant]
   [java.util UUID]))

(def organization-id
  (UUID/fromString
   "10000000-0000-0000-0000-000000000001"))

(def organization-name
  "HumanHelp Demo")

(def created-at
  (Instant/parse
   "2026-07-01T00:00:00Z"))

(def locations
  [{:location/id
    (UUID/fromString
     "30000000-0000-0000-0000-000000000001")

    :location/name
    "Northgate"

    :location/distance
    "0.3 mi away"

    :location/likely?
    true}

   {:location/id
    (UUID/fromString
     "30000000-0000-0000-0000-000000000002")

    :location/name
    "Lake City"

    :location/distance
    "2.1 mi away"}

   {:location/id
    (UUID/fromString
     "30000000-0000-0000-0000-000000000003")

    :location/name
    "University Village"

    :location/distance
    "3.8 mi away"}])

(def default-location-id
  (:location/id
   (first locations)))

(defn location-by-id
  [location-id]
  (when
   (uuid?
    location-id)
    (some
     (fn [location]
       (when
        (=
         location-id
         (:location/id
          location))
         location))
     locations)))

(defn- fail!
  [error-type message details]
  (throw
   (ex-info
    message
    {:error/type
     error-type

     :error/details
     details})))

;; =============================================================================
;; Existing persisted fixture documents
;; =============================================================================

(defn- existing-organization
  [ctx]
  (organization/organization
   ctx
   organization-id))

(defn- existing-location
  [ctx location-id]
  (organization/location
   ctx
   location-id))

;; =============================================================================
;; Fixture conflict checks
;; =============================================================================

(defn- require-expected-organization!
  [document]
  (when-not
   (and
    (=
     organization-name
     (organization/organization-name
      document))

    (organization/organization-active?
     document))
    (fail!
     :mock-data/organization-conflict
     "The mock Organization ID is already in use by a different or inactive Organization."
     {:organization/id
      organization-id

      :expected-name
      organization-name

      :organization
      document}))

  document)

(defn- require-expected-location!
  [fixture document]
  (let [location-id
        (:location/id
         fixture)

        location-name
        (:location/name
         fixture)

        parent-scope
        (organization/organization-scope
         organization-id)]

    (when-not
     (and
      (=
       location-name
       (organization/location-name
        document))

      (=
       organization-id
       (organization/location-organization-id
        document))

      (organization/same-scope?
       parent-scope
       (organization/location-parent-scope
        document))

      (organization/location-active?
       document))
      (fail!
       :mock-data/location-conflict
       "A mock Location ID is already in use by a different or inactive Location."
       {:location/id
        location-id

        :expected-name
        location-name

        :expected-organization-id
        organization-id

        :expected-parent-scope
        parent-scope

        :location
        document}))

    document))

;; =============================================================================
;; Fixed fixture commands
;; =============================================================================

(defn- organization-command
  []
  (organization.domain/create-organization-command
   {:id
    organization-id

    :name
    organization-name

    :now
    created-at}))

(defn- location-command
  [location]
  (organization.domain/create-location-command
   {:id
    (:location/id
     location)

    :organization-id
    organization-id

    :parent-scope
    (organization/organization-scope
     organization-id)

    :name
    (:location/name
     location)

    :now
    created-at}))

(defn missing-commands
  "Returns create commands for missing fixture documents.

   Existing documents are first checked against the fixture definition. This
   function never returns update commands."
  [ctx]
  (let [organization-document
        (existing-organization
         ctx)

        _
        (when
         organization-document
          (require-expected-organization!
           organization-document))

        location-documents
        (mapv
         (fn [fixture]
           [fixture
            (existing-location
             ctx
             (:location/id
              fixture))])
         locations)]

    (doseq
     [[fixture document]
      location-documents
      :when
      document]
      (require-expected-location!
       fixture
       document))

    (cond->
     []
      (nil?
       organization-document)
      (conj
       (organization-command))

      true
      (into
       (keep
        (fn [[fixture document]]
          (when-not
           document
            (location-command
             fixture)))
        location-documents)))))

;; =============================================================================
;; Fixture installation
;; =============================================================================

(defn ensure!
  "Ensures the fixed mock Organization and Locations exist.

   Only missing documents are created. Existing documents are never rewritten.
   If a fixed fixture UUID already refers to incompatible data, this fails
   loudly instead of mutating or silently accepting that data.

   Fixture creation is intentionally silent in Gesso Live.

   Returns the fixture IDs and the number of documents created."
  [ctx]
  (let [commands
        (missing-commands
         ctx)]

    (when
     (seq
      commands)
      (model.tx/transact!
       ctx
       {:commands
        commands

        :emit
        false}))

    {:organization-id
     organization-id

     :locations
     locations

     :created-count
     (count
      commands)}))
