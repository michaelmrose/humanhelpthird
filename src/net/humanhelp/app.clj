(ns net.humanhelp.app
  "Generic /app entrypoint.

   HumanHelp points /app at the real site application. The removable example
   app remains available under net.humanhelp.example.* for Gesso integration
   work, but it is not the production application module."
  (:require
   [net.humanhelp.site.app :as site]))

(def module
  site/module)
