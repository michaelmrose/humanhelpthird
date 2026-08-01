(ns net.humanhelp.site.components.glow.core
  (:require
   [clojure.string :as str]))

(defn- class-names
  [& values]
  (->> values
       flatten
       (remove nil?)
       (remove false?)
       (map str)
       (remove str/blank?)
       (str/join " ")))

(defn- add-class
  [node class-name]
  (when-not
   (and
    (vector? node)
    (keyword? (first node)))
    (throw
     (ex-info
      "Glow child must be a Hiccup element."
      {:error/type :humanhelp.glow/invalid-child
       :child      node})))

  (let [[tag maybe-attrs & children] node
        attrs?                       (map? maybe-attrs)
        attrs                        (if attrs? maybe-attrs {})
        children                     (if attrs?
                                       children
                                       (cons maybe-attrs children))]
    (into
     [tag
      (assoc
       attrs
       :class
       (class-names
        (:class attrs)
        class-name))]
     children)))

(defn glow
  "Wrap one Hiccup element in the HumanHelp glow treatment.

   Options:

   :active?  show the glow
   :as       wrapper element, defaults to :div
   :class    additional wrapper classes
   :attrs    additional wrapper attributes"
  [{:keys [active? as class attrs]
    :or   {as :div}}
   child]
  [as
   (merge
    attrs
    {:class
     (class-names
      "humanhelp-glow-frame"
      (when active?
        "humanhelp-glow-active")
      class)})
   (add-class
    child
    "humanhelp-glow-surface")])
