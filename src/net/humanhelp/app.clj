(ns net.humanhelp.app
  "Generic /app entrypoint.

   The generated template points /app at the removable Human Help example app.

   Template users can either:
   - replace this namespace with their own app code, or
   - keep this namespace as a thin adapter and point it at their own feature app."
  (:require
   [net.humanhelp.example.app :as humanhelp]))

(def module
  humanhelp/module)
