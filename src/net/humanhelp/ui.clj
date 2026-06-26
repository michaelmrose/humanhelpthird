(ns net.humanhelp.ui
  (:require
   [clojure.java.io :as io]
   [com.biffweb :as biff]
   [gesso.core :as g]
   [net.humanhelp.settings :as settings]
   [ring.util.response :as ring-response]
   [rum.core :as rum]))

;; -----------------------------------------------------------------------------
;; Theme defaults
;; -----------------------------------------------------------------------------

(def default-theme
  {:color-theme "cosmicnight"
   :density "default"
   :typography "sans-headings"
   :shape "default"})

(def default-mode
  :dark)

(def axis-specs
  [{:axis :color-theme
    :attr "data-color-theme"
    :label "Color"
    :description "Choose the app color palette."}

   {:axis :density
    :attr "data-density"
    :label "Density"
    :description "Adjust spacing, control size, and layout rhythm."}

   {:axis :typography
    :attr "data-typography"
    :label "Typography"
    :description "Choose the body and heading type system."}

   {:axis :shape
    :attr "data-shape"
    :label "Shape"
    :description "Adjust border radius and component softness."}])

;; -----------------------------------------------------------------------------
;; Static assets
;; -----------------------------------------------------------------------------

(defn static-path
  [path]
  (if-some [last-modified (some-> (io/resource (str "public" path))
                                  ring-response/resource-data
                                  :last-modified
                                  (.getTime))]
    (str path "?t=" last-modified)
    path))

;; -----------------------------------------------------------------------------
;; Theme discovery
;; -----------------------------------------------------------------------------

(defn- theme-css-resources
  []
  (keep io/resource
        ["public/gesso/themes.css"
         "public/gesso/app-themes.css"]))

(defn- options-from-css
  [css attr]
  (let [pattern (re-pattern
                 (str "html(?:\\.dark)?\\["
                      (java.util.regex.Pattern/quote attr)
                      "~=\"([^\"]+)\"\\]"))]
    (->> (re-seq pattern css)
         (map second)
         distinct
         sort
         vec)))

(defn discovered-theme-options
  "Discover available theme axis values from bundled/app-generated CSS.

   When app themes are generated into public/gesso/app-themes.css, they appear
   in the UI without hand-editing Clojure.

   The configured default for each axis is always included first so changing
   default-theme affects the initial picker state."
  []
  (let [css-blobs (map slurp (theme-css-resources))]
    (reduce
     (fn [m {:keys [axis attr]}]
       (let [default-opt (get default-theme axis)
             discovered  (->> css-blobs
                              (mapcat #(options-from-css % attr))
                              distinct
                              sort)]
         (assoc m axis
                (cond-> []
                  default-opt
                  (conj default-opt)

                  true
                  (into (remove #(= % default-opt) discovered))))))
     {}
     axis-specs)))

(defn theme-state
  "Return the server-rendered theme state for ctx.

   The theme dialog applies changes client-side to document.documentElement.
   These values are the initial render defaults."
  [ctx]
  {:color-theme (or (:color-theme ctx)
                    (:data-color-theme ctx)
                    (:color-theme default-theme))
   :density (or (:density ctx)
                (:data-density ctx)
                (:density default-theme))
   :typography (or (:typography ctx)
                   (:data-typography ctx)
                   (:typography default-theme))
   :shape (or (:shape ctx)
              (:data-shape ctx)
              (:shape default-theme))
   :mode (or (:mode ctx)
             (:data-color-theme-mode ctx)
             default-mode)})

(defn- mode-token
  [mode]
  (cond
    (keyword? mode) (name mode)
    (nil? mode) (name default-mode)
    :else (str mode)))

(defn- select-option
  [selected opt]
  [:option
   (cond-> {:value opt}
     (= (str selected) (str opt))
     (assoc :selected true))
   opt])

(defn- theme-select-id
  [id-prefix axis]
  (str id-prefix "-" (name axis)))

(defn- control-style
  []
  {:border-style "solid"
   :border-color "var(--border)"
   :background "var(--background)"
   :color "var(--foreground)"})

(defn- theme-select
  [{:keys [axis attr label description options selected id-prefix]}]
  (let [id (theme-select-id id-prefix axis)]
    [:label {:class "content-stack-theme gap-field"}
     [:span {:class "font-heading text-sm-theme leading-heading tracking-heading weight-semibold-theme"}
      label]

     [:span {:class "font-body text-xs-theme leading-body"
             :style {:color "var(--muted-foreground)"}}
      description]

     [:select
      {:id id
       :name (name axis)
       :class "control-theme radius-md border-theme font-body text-sm-theme"
       :style (control-style)
       :onchange (str "document.documentElement.setAttribute('"
                      attr
                      "', this.value)")}
      (for [opt options]
        (select-option selected opt))]]))

(defn- mode-select
  [{:keys [selected id-prefix]}]
  [:label {:class "content-stack-theme gap-field"}
   [:span {:class "font-heading text-sm-theme leading-heading tracking-heading weight-semibold-theme"}
    "Mode"]

   [:span {:class "font-body text-xs-theme leading-body"
           :style {:color "var(--muted-foreground)"}}
    "Switch between light, dark, or your browser preference."]

   [:select
    {:id (str id-prefix "-mode")
     :name "mode"
     :class "control-theme radius-md border-theme font-body text-sm-theme"
     :style (control-style)
     :onchange
     "var root = document.documentElement;
      root.setAttribute('data-color-theme-mode', this.value);
      if (this.value === 'dark') {
        root.classList.add('dark');
      } else {
        if (this.value === 'light') {
          root.classList.remove('dark');
        } else {
          if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
            root.classList.add('dark');
          } else {
            root.classList.remove('dark');
          }
        }
      }"}
    (for [opt ["dark" "light" "system"]]
      (select-option (mode-token selected) opt))]])

;; -----------------------------------------------------------------------------
;; Theme dialog
;; -----------------------------------------------------------------------------

(defn theme-dialog
  "Render a theme button plus modal dialog.

   Intended placement: the standard page-shell bar.

   Options:
     :trigger-label?
       Whether the trigger button includes the text label \"Theme\".

     :id
       Dialog root id. Defaults to \"net.humanhelp-theme-dialog\".

     :id-prefix
       Prefix for internal select ids. Defaults to :id."
  ([ctx]
   (theme-dialog ctx {}))
  ([ctx {:keys [trigger-label? id id-prefix]
         :or {trigger-label? true}}]
   (let [dialog-id     (or id "net.humanhelp-theme-dialog")
         id-prefix'    (or id-prefix dialog-id)
         state         (theme-state ctx)
         theme-options (discovered-theme-options)]
     (g/dialog
      {:attrs {:id dialog-id}}

      (g/dialog-trigger
       {:attrs {:aria-label "Theme settings"}}
       (g/icon "palette" {:size :sm})
       (when trigger-label?
         [:span "Theme"]))

      (g/dialog-overlay)

      (g/dialog-content
       {:title "Theme"
        :description "Explore the generated Gesso theme dimensions. Changes apply to this page immediately."
        :body
        [[:div {:class "form-theme"}
          (for [{:keys [axis attr label description] :as spec} axis-specs]
            (theme-select
             (assoc spec
                    :axis axis
                    :attr attr
                    :label label
                    :description description
                    :id-prefix id-prefix'
                    :options (get theme-options axis)
                    :selected (get state axis))))

          (mode-select
           {:selected (:mode state)
            :id-prefix id-prefix'})]]
        :footer
        [(g/dialog-close {:text "Done"})]})))))

;; -----------------------------------------------------------------------------
;; Standard bar pieces
;; -----------------------------------------------------------------------------

(defn brand
  "Render the brand node used by the standard page-shell bar.

   Options:
     :href
     :image-src
     :text
     :alt"
  ([] (brand {}))
  ([{:keys [href image-src text alt]
     :or {href "/app"
          image-src "/img/hh.png"
          text settings/app-name
          alt ""}}]
   [:a {:href href
        :class "cluster-theme items-center"
        :style {:color "var(--foreground)"
                :text-decoration "none"}}
    [:img (cond-> {:src image-src
                   :alt alt
                   :style {:width "1.5rem"
                           :height "1.5rem"
                           :object-fit "contain"
                           :display "block"}}
            (not (seq alt))
            (assoc :aria-hidden "true"))]
    [:span {:class "font-heading text-md-theme leading-heading tracking-heading weight-semibold-theme"}
     text]]))

(defn- account-email
  [user]
  (:user/email user))

(defn- account-label
  [user]
  (or (account-email user)
      "Not signed in"))

(defn logout-form
  []
  (biff/form
   {:action "/auth/signout"}
   (g/button
    {:variant :ghost
     :text "Log out"
     :class "w-full justify-start"
     :attrs {:type "submit"}})))

(defn user-menu
  [user]
  (let [email      (account-email user)
        label      (account-label user)
        signed-in? (some? email)]
    (g/dropdown-menu
     {}
     (g/dropdown-menu-trigger
      {:class "inline-flex items-center justify-center control-theme radius-md"
       :attrs {:aria-label "Account"
               :title label
               :style {:width "3rem"
                       :height "3rem"}}}
      (g/icon "circle-user-round"
              {:size :2xl
               :title "Account"
               :attrs {:stroke-width 1.5}}))

     (g/dropdown-menu-content
      {:align :end}
      (g/dropdown-menu-label
       {}
       [:div {:class "content-stack-theme gap-field"}
        [:span (if signed-in?
                 "Signed in as"
                 "Account")]
        [:span {:class "font-body text-sm-theme leading-body weight-medium-theme"
                :style {:display "block"
                        :max-width "16rem"
                        :overflow "hidden"
                        :text-overflow "ellipsis"
                        :white-space "nowrap"
                        :color "var(--foreground)"}}
         label]])

      (when signed-in?
        (g/dropdown-menu-separator))

      (when signed-in?
        (logout-form))))))

;; -----------------------------------------------------------------------------
;; Page shell options
;; -----------------------------------------------------------------------------

(def page-shell-option-keys
  #{:user
    :brand
    :rightmost
    :main-attrs
    :main-class
    :shell-attrs
    :shell-class
    :toaster
    :toaster?})

(defn- page-shell-options?
  [x]
  (and (map? x)
       (boolean
        (some #(contains? x %)
              page-shell-option-keys))))

(defn- normalize-page-shell-args
  [body]
  (if (page-shell-options? (first body))
    [(first body) (rest body)]
    [{} body]))

(defn- page-shell-main
  [{:keys [main-attrs main-class]
    :or {main-class "flex-grow"}}
   body]
  (into [:main (merge {:class main-class}
                      main-attrs)]
        body))

(defn- page-shell-toaster
  [{:keys [toaster toaster?]}]
  (cond
    (= false toaster?)
    nil

    toaster
    toaster

    :else
    (g/toaster {:id "app-toaster"
                :position :bottom-right})))

(defn- page-shell-rightmost
  [ctx opts]
  (or (:rightmost opts)
      [[:div {:class "cluster-theme items-center justify-end"}
        (theme-dialog ctx {:trigger-label? false})
        (user-menu (:user opts))]]))

(defn- page-shell-bar
  [ctx opts]
  (g/bars
   {:topbar-only? true
    :brand (or (:brand opts)
               (brand))
    :rightmost (page-shell-rightmost ctx opts)}))

;; -----------------------------------------------------------------------------
;; Base page shell
;; -----------------------------------------------------------------------------

(defn base
  [{:keys [::recaptcha] :as ctx} & body]
  (let [{:keys [color-theme density typography shape mode]} (theme-state ctx)]
    (apply
     biff/base-html
     (-> ctx
         (merge
          (g/theme {:color-theme color-theme
                    :density density
                    :typography typography
                    :shape shape}
                   mode))
         (update :base/head
                 (fn [head]
                   (concat
                    head
                    [[:script {:src (static-path "/js/gesso-theme.js")
                               :defer true}]
                     [:script {:src (static-path "/js/gesso-live.js")
                               :defer true}]
                     [:link {:rel "stylesheet"
                             :href (static-path "/css/main.css")}]
                     [:link {:rel "stylesheet"
                             :href "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.11/dist/basecoat.cdn.min.css"}]
                     [:link {:rel "stylesheet"
                             :href (static-path "/gesso/themes.css")}]
                     (when (io/resource "public/gesso/app-themes.css")
                       [:link {:rel "stylesheet"
                               :href (static-path "/gesso/app-themes.css")}])
                     [:link {:rel "icon"
                             :href "/favicon.ico"
                             :sizes "any"}]

                     [:script {:src "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.11/dist/js/all.min.js"
                               :defer true}]
                     [:script {:src (static-path "/js/main.js")
                               :defer true}]
                     [:script {:src "https://unpkg.com/htmx.org@2.0.7"}]
                     [:script {:src "https://cdn.jsdelivr.net/npm/htmx-ext-sse@2.2.4"}]
                     [:script {:src "https://unpkg.com/htmx-ext-ws@2.0.2/ws.js"}]
                     [:script {:src "https://unpkg.com/hyperscript.org@0.9.14"}]
                     (when recaptcha
                       [:script {:src "https://www.google.com/recaptcha/api.js"
                                 :async "async"
                                 :defer "defer"}])])))
         (merge
          #:base{:title settings/app-name
                 :lang "en-US"
                 :icon "/img/glider.png"
                 :description (str settings/app-name " Description")
                 :image "https://clojure.org/images/clojure-logo-120b.png"}))
     body)))

(defn container
  [& children]
  (into [:div {:class "w-full max-w-4xl mx-auto px-4 sm:px-6 lg:px-8"}]
        children))

(defn page
  "Centered standard page shell.

   This shell does not render the standard app bar. Use page-shell for app
   pages that should include the always-present bar."
  [ctx & body]
  (base ctx
        [:div {:class "min-h-screen flex flex-col bg-background text-foreground"}
         [:main {:class "flex-grow py-10"}
          (apply container body)]

         (g/toaster {:id "app-toaster"
                     :position :bottom-right})]))

(defn page-shell
  "Full-width app shell with the standard Gesso bars topbar.

   Existing body-only usage still works:

     (page-shell ctx body...)

   App pages should pass the current user:

     (page-shell
      ctx
      {:user user}
      body...)

   The bar is rendered directly through g/bars. App-specific view namespaces
   should not hand-write data-bars markup.

   Supported opts:
     :user
       Current user for the account menu.

     :brand
       Optional brand node. Defaults to ui/brand.

     :rightmost
       Optional rightmost bar segment content. Defaults to theme dialog plus
       account menu.

     :main-class
       Class for the generated main element. Defaults to \"flex-grow\".

     :main-attrs
       Additional attrs for the generated main element.

     :shell-class
       Class for the outer shell div.

     :shell-attrs
       Additional attrs for the outer shell div.

     :toaster
       Custom toaster node.

     :toaster?
       Set false to omit the default toaster."
  [ctx & body]
  (let [[opts body] (normalize-page-shell-args body)
        shell-attrs (merge
                     {:class (or (:shell-class opts)
                                 "min-h-screen flex flex-col bg-background text-foreground")}
                     (:shell-attrs opts))
        topbar      (page-shell-bar ctx opts)
        main        (page-shell-main opts body)
        toaster     (page-shell-toaster opts)]
    (base ctx
          (into [:div shell-attrs]
                (concat
                 [topbar
                  main]
                 (when toaster
                   [toaster]))))))

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn on-error
  [{:keys [status] :as ctx}]
  {:status status
   :headers {"content-type" "text/html"}
   :body (rum/render-static-markup
          (page
           ctx
           [:h1 {:class "font-heading text-2xl-theme leading-heading tracking-heading weight-semibold-theme"}
            (if (= status 404)
              "Page not found."
              "Something went wrong.")]))})
