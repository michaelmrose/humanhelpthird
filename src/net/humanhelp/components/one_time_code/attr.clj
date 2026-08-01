(ns net.humanhelp.components.one-time-code.attr
  (:require
   [clojure.string :as str]))

(def default-length 6)

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
    :class                   (class-names "content-stack-theme" "gap-field" class)}
   (when id
     {:id (str id "-root")})
   root-attrs))

(defn label-attrs
  [{:keys [id label-attrs]}]
  (merge
   {:for                      id
    :data-one-time-code-label true
    :class                    (class-names
                               "font-heading"
                               "text-sm-theme"
                               "leading-tight-theme"
                               "tracking-tight-theme"
                               "weight-medium-theme")}
   label-attrs))

(defn input-style
  [_length]
  {:inline-size    "min(100%, 18rem)"
   :min-block-size "var(--control-height)"
   :padding-inline "var(--control-px,0.875rem)"
   :padding-block  "var(--control-py,0.625rem)"

   :font-family "var(--font-mono)"
   :font-size   "var(--text-2xl)"
   :font-weight "var(--weight-semibold)"
   :line-height "var(--leading-tight)"
   :text-align  "center"

   :color            "var(--foreground)"
   :background-color "var(--card)"
   :border           "var(--border-width,1px) solid var(--input)"
   :border-radius    "var(--radius-md)"
   :outline          "none"
   :box-shadow       "var(--shadow-xs,none)"})

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
    :as   opts}]
  (let [length (code-length opts)]
    (merge
     {:id                        id
      :name                      (or name "code")
      :type                      "text"
      :value                     (display-value opts)
      :placeholder               (or placeholder "")
      :maxlength                 length
      :minlength                 (when required? length)
      :inputmode                 "numeric"
      :pattern                   "[0-9]*"
      :autocomplete              (or autocomplete "one-time-code")
      :spellcheck                "false"
      :autocapitalize            "none"
      :autocorrect               "off"
      :aria-invalid              (when (:error opts) "true")
      :aria-describedby          (described-by-id opts)
      :data-one-time-code-input  true
      :data-one-time-code-length length
      :class                     (class-names
                                  "font-mono"
                                  "text-2xl-theme"
                                  "leading-tight-theme"
                                  "weight-semibold-theme"
                                  "control-height-theme"
                                  "radius-md"
                                  "border-theme"
                                  input-class)
      :style                     (input-style length)}
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
   {:id                      (str id "-help")
    :data-one-time-code-help true
    :class                   (class-names
                              "text-sm-theme"
                              "leading-body")
    :style                   {:color "var(--muted-foreground)"}}
   help-attrs))

(defn error-attrs
  [{:keys [id error-attrs]}]
  (merge
   {:id                       (str id "-error")
    :data-one-time-code-error true
    :class                    (class-names
                               "text-sm-theme"
                               "leading-body"
                               "weight-medium-theme")
    :style                    {:color "var(--destructive)"}}
   error-attrs))
