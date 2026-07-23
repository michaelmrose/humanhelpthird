(ns net.humanhelp.site.components.glow.core
  "HumanHelp's pulsing attention glow.

   The glow is intentionally site-specific rather than a Gesso primitive. It
   marks something HumanHelp believes deserves the user's attention, such as a
   likely Location or newly-available Request data.

   glow owns the outer aura frame and adds the glow-surface class directly to
   the supplied Hiccup child, so callers do not need to reproduce the two-layer
   structure."
  (:require
   [clojure.string :as str]))

(defn- class-tokens
  [value]
  (cond
    (nil? value)
    []

    (string? value)
    [value]

    (keyword? value)
    [(name value)]

    (sequential? value)
    (mapcat class-tokens value)

    :else
    [(str value)]))

(defn- classes
  [& values]
  (->> values
       (mapcat class-tokens)
       (remove str/blank?)
       (str/join " ")))

(defn- add-class
  [node class-name]
  (when-not (vector? node)
    (throw
     (ex-info
      "HumanHelp glow requires a Hiccup element vector."
      {:error/type :humanhelp.glow/invalid-child
       :child node})))

  (let [tag   (first node)
        attrs (second node)]
    (if (map? attrs)
      (assoc node
             1
             (assoc attrs
                    :class
                    (classes (:class attrs)
                             class-name)))
      (into [tag {:class class-name}]
            (rest node)))))

(defn glow
  "Wrap child in the HumanHelp attention glow.

   Options:
     :active?
       Whether the glow is active.

     :as
       Hiccup tag for the aura frame. Defaults to :div.

     :class
       Additional frame classes. Use this for layout, for example
       \"block\" around a card or \"inline-grid\" around a button.

     :attrs
       Additional frame attributes.

   The supplied child receives the `humanhelp-glow-surface` class directly.

   Example:

     (glow
      {:active? likely?
       :class \"block\"}
      [:div {:class \"rounded-xl border bg-card\"}
       ...])"
  [{:keys [active? as class attrs]
    :or {as :div}}
   child]
  [as
   (merge
    attrs
    {:class
     (classes
      "humanhelp-glow-frame"
      (when active?
        "humanhelp-glow-active")
      class)})

   (add-class
    child
    "humanhelp-glow-surface")])
