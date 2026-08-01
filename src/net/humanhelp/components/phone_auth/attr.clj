(ns net.humanhelp.components.phone-auth.attr
  (:require
   [clojure.string :as str]))

(defn class-names
  [& xs]
  (->> xs
       flatten
       (remove nil?)
       (remove false?)
       (map str)
       (remove str/blank?)
       (str/join " ")))

(defn clean-attrs
  [attrs]
  (into {}
        (remove (comp nil? val))
        attrs))

(defn merge-attrs
  [& maps]
  (let [classes (->> maps
                     (keep :class)
                     (apply class-names))
        attrs   (->> maps
                     (map #(dissoc % :class))
                     (apply merge)
                     clean-attrs)]
    (cond-> attrs
      (not (str/blank? classes))
      (assoc :class classes))))

(defn panel-attrs
  [{:keys [id class attrs]}]
  (merge-attrs
   {:id                    id
    :data-phone-auth-panel true
    :class                 "content-stack-theme gap-form"}
   {:class class}
   attrs))

(defn copy-attrs
  [{:keys [class attrs]}]
  (merge-attrs
   {:data-phone-auth-copy true
    :class                "content-stack-theme gap-field"}
   {:class class}
   attrs))

(defn title-attrs
  [{:keys [class attrs]}]
  (merge-attrs
   {:data-phone-auth-title true
    :class                 "font-heading text-xl-theme leading-heading tracking-tight-theme weight-semibold-theme"}
   {:class class}
   attrs))

(defn body-attrs
  [{:keys [class attrs]}]
  (merge-attrs
   {:data-phone-auth-body true
    :class                "text-sm-theme leading-body"
    :style                {:color "var(--muted-foreground)"}}
   {:class class}
   attrs))

(defn form-attrs
  [{:keys [id method action class attrs]}]
  (merge-attrs
   {:id     id
    :method (or method "post")
    :action action
    :class  "form-theme"}
   {:class class}
   attrs))

(defn inline-form-attrs
  [{:keys [id method action class attrs]}]
  (merge-attrs
   {:id     id
    :method (or method "post")
    :action action
    :style  {:display "contents"}}
   {:class class}
   attrs))

(defn field-control-attrs
  [{:keys [class attrs]}]
  (merge-attrs
   {:data-phone-auth-field-control true
    :class                         "content-stack-theme gap-field"}
   {:class class}
   attrs))

(defn described-by-id
  [{:keys [help-id error-id]}]
  (let [ids (cond-> []
              help-id  (conj help-id)
              error-id (conj error-id))]
    (when (seq ids)
      (str/join " " ids))))

(defn phone-display-style
  []
  {:inline-size      "100%"
   :min-block-size   "var(--control-height)"
   :padding-inline   "var(--control-px,0.875rem)"
   :padding-block    "var(--control-py,0.625rem)"
   :font-family      "var(--font-mono)"
   :font-size        "var(--text-lg)"
   :font-weight      "var(--weight-semibold)"
   :line-height      "var(--leading-tight)"
   :letter-spacing   "0.02em"
   :color            "var(--foreground)"
   :background-color "var(--card)"
   :border           "var(--border-width,1px) solid var(--input)"
   :border-radius    "var(--radius-md)"
   :outline          "none"
   :box-shadow       "var(--shadow-xs,none)"})

(defn phone-display-input-attrs
  [{:keys [id
           value
           placeholder
           hidden-id
           error-id
           help-id
           required-message
           invalid-message
           disabled?
           readonly?
           autofocus?
           autocomplete
           class
           attrs]}]
  (merge-attrs
   {:id                               id
    :type                             "tel"
    :value                            (or value "")
    :placeholder                      (or placeholder "123-456-7890")
    :inputmode                        "numeric"
    :autocomplete                     (or autocomplete "tel-national")
    :spellcheck                       "false"
    :autocapitalize                   "none"
    :autocorrect                      "off"
    :aria-invalid                     "false"
    :aria-describedby                 (described-by-id {:help-id  help-id
                                                        :error-id error-id})
    :data-phone-auth-phone-display    true
    :data-phone-auth-hidden-id        hidden-id
    :data-phone-auth-error-id         error-id
    :data-phone-auth-required-message (or required-message
                                          "Please enter a 10-digit US mobile number.")
    :data-phone-auth-invalid-message  (or invalid-message
                                          "Please enter a 10-digit US mobile number.")
    :class                            "control-height-theme radius-md border-theme font-mono"
    :style                            (phone-display-style)}
   {:class class}
   (when disabled?
     {:disabled true})
   (when readonly?
     {:readonly true})
   (when autofocus?
     {:autofocus true})
   attrs))

(defn phone-hidden-input-attrs
  [{:keys [id name value attrs]}]
  (merge-attrs
   {:id                           id
    :name                         (or name "phone")
    :type                         "hidden"
    :value                        (or value "")
    :data-phone-auth-phone-hidden true}
   attrs))

(defn help-attrs
  [{:keys [id class attrs]}]
  (merge-attrs
   {:id                   id
    :data-phone-auth-help true
    :class                "text-sm-theme leading-body"
    :style                {:color "var(--muted-foreground)"}}
   {:class class}
   attrs))

(defn error-attrs
  [{:keys [id hidden? class attrs]}]
  (merge-attrs
   {:id                    id
    :data-phone-auth-error true
    :role                  "alert"
    :class                 (class-names
                            "text-sm-theme"
                            "leading-body"
                            "weight-medium-theme"
                            (when hidden? "hidden"))
    :style                 {:color "var(--destructive)"}}
   {:class class}
   attrs))

(defn action-row-attrs
  [{:keys [class attrs]}]
  (merge-attrs
   {:data-phone-auth-actions true
    :class                   "cluster-theme gap-inline text-sm-theme leading-body"}
   {:class class}
   attrs))

(defn link-style
  []
  {:color                 "var(--primary)"
   :text-decoration       "underline"
   :text-underline-offset "0.16em"})

(defn link-attrs
  [{:keys [href class attrs]}]
  (merge-attrs
   {:href                 href
    :data-phone-auth-link true
    :class                "text-sm-theme leading-body"
    :style                (link-style)}
   {:class class}
   attrs))

(defn link-button-style
  []
  {:padding               "0"
   :border                "0"
   :background            "transparent"
   :color                 "var(--primary)"
   :font                  "inherit"
   :line-height           "inherit"
   :text-decoration       "underline"
   :text-underline-offset "0.16em"
   :cursor                "pointer"})

(defn link-button-attrs
  [{:keys [class attrs]}]
  (merge-attrs
   {:type                        "submit"
    :data-phone-auth-link-button true
    :class                       "text-sm-theme leading-body"
    :style                       (link-button-style)}
   {:class class}
   attrs))

(defn separator-attrs
  []
  {:aria-hidden               "true"
   :data-phone-auth-separator true
   :style                     {:color "var(--muted-foreground)"}})

(defn submit-style
  []
  {:inline-size "100%"})
