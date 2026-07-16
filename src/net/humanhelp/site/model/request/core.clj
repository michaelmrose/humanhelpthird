(ns net.humanhelp.site.model.request.core
  (:require
   [net.humanhelp.site.model.request.domain :as request.domain]
   [net.humanhelp.site.model.request.fx :as request.fx]
   [net.humanhelp.site.model.request.graph :as request.graph]
   [net.humanhelp.site.model.request.schema :as request.schema]))

;; =============================================================================
;; Public request-model components
;; =============================================================================

(def schema
  request.schema/schema)

(def resolvers
  request.graph/resolvers)

(def fx-handlers
  request.fx/handlers)

;; Requiring the domain namespace here makes this namespace the root of the
;; complete request model, even though application code that needs individual
;; domain operations should require request.domain directly.
(def domain
  request.domain/model)

;; =============================================================================
;; Module assembly
;; =============================================================================

(def module
  {:biff.graph/resolvers resolvers
   :biff.fx/handlers fx-handlers})
