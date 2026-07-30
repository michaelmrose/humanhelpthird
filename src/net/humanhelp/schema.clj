(ns net.humanhelp.schema
  "Application-wide Malli schema registry.

   Shared application primitives and the public schema contributions of every
   top-level HumanHelp model are assembled here and registered with Biff 2.

   Model internals must not require this namespace. Model schema namespaces may
   depend on net.humanhelp.schema.common for shared primitive schemas, while
   application assembly depends only on each model's public core boundary:

     schema.common
       -> model schema
         -> model core
           -> application schema assembly"
  (:require
   [com.biffweb.core :as biff.core]
   [net.humanhelp.schema.common :as common]
   [net.humanhelp.site.model.invitation.core :as invitation]
   [net.humanhelp.site.model.membership.core :as membership]
   [net.humanhelp.site.model.organization.core :as organization]
   [net.humanhelp.site.model.request.core :as request]
   [net.humanhelp.site.model.user.core :as user]))

;; =============================================================================
;; Application-owned schemas
;; =============================================================================

(def app-schema
  {:msg
   [:map
    {:closed true}

    [:xt/id
     ::common/id]

    [:msg/user
     ::common/id]

    [:msg/content
     [:string
      {:max
       10000}]]

    [:msg/sent-at
     ::common/zdt]]})

;; =============================================================================
;; Registry assembly
;; =============================================================================

(defn- duplicate-keys
  [registries]
  (->> registries
       (mapcat keys)
       frequencies
       (keep
        (fn [[key occurrence-count]]
          (when
           (< 1 occurrence-count)
            key)))
       set))

(defn- merge-disjoint
  [& registries]
  (when-let [duplicates
             (not-empty
              (duplicate-keys
               registries))]
    (throw
     (ex-info
      "HumanHelp schema registries contain duplicate keys."
      {:error/type
       :humanhelp.schema/duplicate-schema-keys

       :error/details
       {:keys
        duplicates}})))

  (apply
   merge
   registries))

(def schema
  "Complete HumanHelp Malli registry.

   Each top-level model owns and exposes its schema contribution through its
   public core namespace. This assembly layer does not reach into model schema
   implementation namespaces."
  (merge-disjoint
   common/schema
   user/schema
   organization/schema
   membership/schema
   invitation/schema
   request/schema
   app-schema))

;; =============================================================================
;; Biff 2 module
;; =============================================================================

(defn- register-schema!
  [_modules-var]
  (biff.core/register
   schema)

  {})

(def module
  "Registers the complete HumanHelp Malli registry during Biff 2 startup.

   :schema is retained temporarily because current Gesso compatibility code
   still reads schema contributions from that key."
  {:biff.core/init
   register-schema!

   :schema
   schema})
