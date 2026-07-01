(ns net.humanhelp.components.one-time-code.attr
  (:require
   [clojure.string :as str]))

(def default-length 5)

(defn class-names
  [& xs]
  (->> xs
       flatten
       (remove nil?)
       (remove false?)
       (map str)
       (remove str/blank?)
       (str/join " ")))

(defn digits-only
  [x]
  (->> (str (or x ""))
       (filter #(Character/isDigit ^char %))
       (apply str)))

(defn code-length
  [opts]
  (let [n (:length opts default-length)]
    (if (and (integer? n) (pos? n))
      n
      default-length)))

(defn display-value
  [opts]
  (let [length (code-length opts)
        value  (digits-only (:value opts))]
    (subs value 0 (min length (count value)))))

(defn described-by-id
  [{:keys [id help error]}]
  (let [ids (cond-> []
              help  (conj (str id "-help"))
              error (conj (str id "-error")))]
    (when (seq ids)
      (str/join " " ids))))

(defn root-attrs
  [{:keys [id class root-attrs]}]
  (merge
   {:data-one-time-code-root true
    :class (class-names "content-stack-theme" "gap-field" class)}
   (when id
     {:id (str id "-root")})
   root-attrs))

(defn label-attrs
  [{:keys [id label-attrs]}]
  (merge
   {:for id
    :data-one-time-code-label true
    :class (class-names
            "font-heading"
            "text-sm-theme"
            "leading-tight-theme"
            "tracking-tight-theme"
            "weight-medium-theme")}
   label-attrs))

(defn input-style
  [length]
  (str
   "--one-time-code-length:" length ";"
   "--one-time-code-cell:2.75rem;"
   "--one-time-code-gap:var(--space-2,0.625rem);"

   "inline-size:calc((var(--one-time-code-cell) * var(--one-time-code-length))"
   " + (var(--one-time-code-gap) * (var(--one-time-code-length) - 1)));"
   "max-inline-size:100%;"

   "min-block-size:var(--control-height);"
   "padding-inline:calc(var(--space-3,0.75rem) + 0.16ch);"
   "padding-block:var(--control-py,0.625rem);"

   "font-family:var(--font-mono);"
   "font-size:var(--text-xl);"
   "font-weight:var(--weight-semibold);"
   "line-height:var(--leading-tight);"
   "letter-spacing:0.72em;"
   "text-align:left;"

   "color:var(--foreground);"
   "background-color:var(--card);"
   "border:var(--border-width,1px) solid var(--input);"
   "border-radius:var(--radius-md);"
   "outline:none;"
   "box-shadow:var(--shadow-xs,none);"

   "background-image:"
   "repeating-linear-gradient("
   "90deg,"
   "transparent 0,"
   "transparent var(--one-time-code-cell),"
   "var(--background) var(--one-time-code-cell),"
   "var(--background) calc(var(--one-time-code-cell) + var(--one-time-code-gap))"
   ");"
   "background-origin:content-box;"
   "background-clip:content-box;"))

(defn input-attrs
  [{:keys [id
           name
           value
           placeholder
           required?
           disabled?
           readonly?
           autofocus?
           autocomplete
           input-class
           input-attrs]
    :as opts}]
  (let [length (code-length opts)]
    (merge
     {:id id
      :name (or name "code")
      :type "text"
      :value (display-value opts)
      :placeholder (or placeholder "")
      :maxlength length
      :minlength (when required? length)
      :inputmode "numeric"
      :pattern "[0-9]*"
      :autocomplete (or autocomplete "one-time-code")
      :spellcheck "false"
      :autocapitalize "none"
      :autocorrect "off"
      :aria-invalid (when (:error opts) "true")
      :aria-describedby (described-by-id opts)
      :data-one-time-code-input true
      :data-one-time-code-length length
      :class (class-names
              "font-mono"
              "text-xl-theme"
              "leading-tight-theme"
              "weight-semibold-theme"
              "control-height-theme"
              "radius-md"
              "border-theme"
              input-class)
      :style (input-style length)}
     (when required?
       {:required true})
     (when disabled?
       {:disabled true})
     (when readonly?
       {:readonly true})
     (when autofocus?
       {:autofocus true})
     input-attrs)))

(defn help-attrs
  [{:keys [id help-attrs]}]
  (merge
   {:id (str id "-help")
    :data-one-time-code-help true
    :class (class-names
            "text-sm-theme"
            "leading-body")
    :style "color:var(--muted-foreground);"}
   help-attrs))

(defn error-attrs
  [{:keys [id error-attrs]}]
  (merge
   {:id (str id "-error")
    :data-one-time-code-error true
    :class (class-names
            "text-sm-theme"
            "leading-body"
            "weight-medium-theme")
    :style "color:var(--destructive);"}
   error-attrs))
