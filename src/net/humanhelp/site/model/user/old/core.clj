(ns net.humanhelp.site.model.user.core
  "Assembly point for the complete HumanHelp user model.

   Application code may require the individual domain, Graph, or FX namespaces
   directly when it needs a specific operation. This namespace exposes the
   complete model as one Biff module."
  (:require
   [net.humanhelp.site.model.user.domain :as user.domain]
   [net.humanhelp.site.model.user.fx :as user.fx]
   [net.humanhelp.site.model.user.graph :as user.graph]
   [net.humanhelp.site.model.user.schema :as user.schema]))

;; =============================================================================
;; Public user-model components
;; =============================================================================

(def schema
  user.schema/schema)

(def resolvers
  user.graph/resolvers)

(def fx-handlers
  user.fx/handlers)

(def domain
  user.domain/model)

;; =============================================================================
;; Module assembly
;; =============================================================================

(def module
  {:biff.graph/resolvers
   resolvers

   :biff.fx/handlers
   fx-handlers})
