(ns net.humanhelp.schema
  "Application-wide Malli schema registry.

   Shared primitives and model-owned schema registries are assembled here and
   contributed to Biff through module.

   Model schema namespaces may require net.humanhelp.schema.common, but must not
   require this namespace. That preserves the dependency direction:

     schema.common
       -> model schemas
         -> application schema assembly"
  (:require
   [com.biffweb :as biff]
   [net.humanhelp.schema.common :as common]
   [net.humanhelp.site.model.request.schema :as request.schema]
   [net.humanhelp.site.model.user.schema :as user.schema]))

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
     [:string {:max 10000}]]

    [:msg/sent-at
     ::common/zdt]]})

;; =============================================================================
;; Complete application registry
;; =============================================================================

(def schema
  (biff/safe-merge
   common/schema
   user.schema/schema
   request.schema/schema
   app-schema))

;; =============================================================================
;; Biff module
;; =============================================================================

(def module
  {:schema schema})
