(ns net.humanhelp.schema
  "Application-wide Malli schema registry.

   Shared application primitives and the public schema contributions of every
   top-level HumanHelp model are assembled here and contributed to Biff.

   Model internals must not require this namespace. Model schema namespaces may
   depend on net.humanhelp.schema.common for shared primitive schemas, while
   application assembly depends only on each model's public core boundary:

     schema.common
       -> model schema
         -> model core
           -> application schema assembly"
  (:require
   [com.biffweb :as biff]
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
;; Complete application registry
;; =============================================================================

(def schema
  "Complete HumanHelp Malli registry.

   Each top-level model owns and exposes its schema contribution through its
   public core namespace. This assembly layer does not reach into model schema
   implementation namespaces."
  (biff/safe-merge
   common/schema
   user/schema
   organization/schema
   membership/schema
   invitation/schema
   request/schema
   app-schema))

;; =============================================================================
;; Biff module
;; =============================================================================

(def module
  {:schema
   schema})
