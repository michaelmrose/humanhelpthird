(ns net.humanhelp.app
  "Generic /app entrypoint.

   HumanHelp now points /app at the real site application rather than the
   removable example app."
  (:require
   [net.humanhelp.example.app :as site]))

(def module
  site/module)
