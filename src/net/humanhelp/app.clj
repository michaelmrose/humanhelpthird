(ns net.humanhelp.app
  "Generic /app entrypoint.

   During the Gesso/Choreo proving phase, HumanHelp deliberately points /app at
   the example application. The example UI consumes the production site models
   and model-owned choreographies, making it the richer immediate integration
   surface while the production site UI remains unfinished."
  (:require
   [net.humanhelp.example.app :as example]))

(def module
  example/module)
