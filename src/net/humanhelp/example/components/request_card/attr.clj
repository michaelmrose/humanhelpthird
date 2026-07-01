(ns net.humanhelp.example.components.request-card.attr
  (:require
   [clojure.string :as str]))

(def terminal-statuses
  #{:done
    :cancelled})

(defn- compact-style
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- compact-attrs
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- classes
  [& xs]
  (->> xs
       flatten
       (remove nil?)
       (map str)
       (remove str/blank?)
       (str/join " ")))

(defn terminal-request?
  [request]
  (contains? terminal-statuses (:request/status request)))

(defn fading-terminal?
  [request]
  (true? (:board/fading-terminal? request)))

(defn pending-request?
  [request]
  (true? (:ui/pending? request)))

(defn optimistic-request?
  [request]
  (true? (:ui/optimistic? request)))

(defn terminal-fade-remaining-ms
  [request]
  (:board/terminal-fade-remaining-ms request))

(defn card-surface
  []
  "color-mix(in srgb, var(--background) 82%, var(--card) 18%)")

(defn terminal-card-surface
  []
  "color-mix(in srgb, var(--background) 90%, var(--muted) 10%)")

(defn card-style
  ([]
   (card-style nil))
  ([request]
   (compact-style
    {:position "relative"
     :border-style "solid"
     :border-width "1px"
     :border-color "color-mix(in srgb, var(--border) 80%, transparent)"
     :background (if (terminal-request? request)
                   (terminal-card-surface)
                   (card-surface))
     :color "var(--foreground)"
     :box-shadow "var(--shadow-sm)"
     :--humanhelp-terminal-fade-ms (when-let [remaining-ms (terminal-fade-remaining-ms request)]
                                     (str remaining-ms "ms"))})))

(defn item-class
  ([]
   (item-class nil nil))
  ([request _open?]
   (classes
    "radius-xl border-theme transition-all"
    (when (terminal-request? request)
      "humanhelp-request-card--terminal")
    (when (fading-terminal? request)
      "humanhelp-request-card--fading-terminal")
    (when (pending-request? request)
      "humanhelp-request-card--pending")
    (when (optimistic-request? request)
      "humanhelp-request-card--optimistic"))))

(defn item-attrs
  [request open?]
  (compact-attrs
   {:id (str "humanhelp-request-" (:request/id request))
    :data-humanhelp-request-card true
    :data-humanhelp-request-selected (when open? "true")
    :data-humanhelp-request-terminal (when (terminal-request? request) "true")
    :data-humanhelp-request-fading-terminal (when (fading-terminal? request) "true")
    :data-humanhelp-request-pending (when (pending-request? request) "true")
    :data-humanhelp-request-optimistic (when (optimistic-request? request) "true")
    :data-humanhelp-request-pending-action (some-> (:ui/pending-action request) name)
    :data-terminal-fade-remaining-ms (terminal-fade-remaining-ms request)
    :style (card-style request)}))

(defn summary-attrs
  []
  {:class "cursor-pointer w-full list-none flex items-start justify-between gap-inline outline-none"
   :data-humanhelp-request-summary true
   :style {:padding "1.25rem 1.25rem 0.75rem"
           :background "transparent"
           :color "var(--foreground)"
           :font-weight 500
           :box-shadow "none"}})

(defn header-stack-attrs
  []
  {:class "content-stack-theme gap-field min-w-0"})

(defn title-attrs
  []
  {:class "font-heading text-md-theme leading-heading tracking-heading weight-semibold-theme min-w-0"
   :style {:color "var(--foreground)"}})

(defn meta-attrs
  []
  {:class "cluster-theme items-center"
   :style {:color "var(--muted-foreground)"}})

(defn customer-row-attrs
  []
  {:class "cluster-theme items-center"
   :style {:color "var(--foreground)"}})

(defn chevron-attrs
  [_open?]
  {:data-accordion-chevron true
   :aria-hidden "true"
   :style {:color "var(--muted-foreground)"
           :opacity "0.9"
           :transform "rotate(0deg)"}})

(defn details-attrs
  []
  {:class "content-stack-theme"
   :attrs {:style {:padding "0 1.25rem 1.25rem"
                   :background "transparent"
                   :border-top "0"
                   :color "var(--foreground)"}}})

(defn actions-attrs
  []
  {:class "cluster-theme items-center justify-end"
   :data-humanhelp-request-actions true
   :style {:padding-top "0.875rem"
           :background "transparent"
           :border-top "0"}})

(defn action-form-attrs
  [{:keys [to board-state-selector attrs]}]
  (merge
   (compact-attrs
    {:method "post"
     :hx-post to
     :hx-swap "none"
     :hx-include board-state-selector
     :class "inline-flex"
     :data-humanhelp-request-action-form true})
   attrs))
