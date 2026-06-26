#!/usr/bin/env bb

(ns to-template
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

(def excluded-dirs
  #{".git" ".clj-kondo/.cache" ".cpcache" ".lsp" ".calva"
    ".shadow-cljs" "target" "node_modules" ".direnv"
    "storage"})

(def excluded-files
  #{".nrepl-port" ".DS_Store" "config.env"})

(def binary-exts
  #{".png" ".jpg" ".jpeg" ".gif" ".bmp" ".ico" ".webp"
    ".pdf" ".zip" ".jar" ".class" ".so" ".dylib" ".dll"
    ".woff" ".woff2" ".ttf" ".otf"})

(def empty-dir-marker
  ".gitkeep")

(def main-ns-placeholder
  "net.humanhelp")

(def main-file-placeholder
  "net/humanhelp")

(defn usage!
  []
  (binding [*out* *err*]
    (println "Usage:")
    (println "  bb to-template.clj TEMPLATE-DIR SOURCE-PRIMARY-NS [TEMPLATE-NAME]")
    (println)
    (println "Example:")
    (println "  bb to-template.clj gesso-template-deps-new net.humanhelp")
    (println)
    (println "TEMPLATE-NAME defaults to local/TEMPLATE-DIR."))
  (System/exit 1))

(defn fail-path-exists!
  [path]
  (binding [*out* *err*]
    (println "Path already exists:" (str (fs/normalize path))))
  (System/exit 2))

(defn path-str
  [p]
  (str (fs/normalize p)))

(defn io-str
  [p]
  (str p))

(defn rel-str
  [root p]
  (path-str (fs/relativize root p)))

(defn ext
  [p]
  (let [n (fs/file-name p)]
    (if (str/starts-with? n ".")
      n
      (or (some-> n fs/extension (str "."))
          ""))))

(defn ns->file
  [s]
  (-> s
      (str/replace "." "/")
      (str/replace "-" "_")))

(defn template-path-parts
  [template-name]
  (str/split (ns->file template-name) #"/"))

(defn default-template-name
  [template-dir-name]
  (str "local/" (fs/file-name template-dir-name)))

(defn sibling-path
  [source-root dir-name]
  (let [p (fs/path dir-name)]
    (if (fs/absolute? p)
      p
      (fs/path (fs/parent source-root) dir-name))))

(defn excluded?
  [source-root p]
  (let [rel      (rel-str source-root p)
        parts    (str/split rel #"/")
        name     (fs/file-name p)
        prefixes (rest
                  (reductions
                   (fn [a b]
                     (if (str/blank? a)
                       b
                       (str a "/" b)))
                   ""
                   parts))]
    (or (contains? excluded-files name)
        (some excluded-dirs prefixes))))

(defn has-nul-byte?
  [p]
  (try
    (with-open [in (io/input-stream (io/file (io-str p)))]
      (let [buf (byte-array 8192)
            n   (.read in buf)]
        (loop [i 0]
          (cond
            (neg? n)
            false

            (>= i n)
            false

            (zero? (aget buf i))
            true

            :else
            (recur (inc i))))))
    (catch Throwable _e
      false)))

(defn binary-file?
  [p]
  (or (contains? binary-exts
                 (str/lower-case (ext p)))
      (has-nul-byte? p)))

(defn source-ns-pattern
  [source-ns]
  (re-pattern
   (str "(^|[^A-Za-z0-9_.-])"
        (java.util.regex.Pattern/quote source-ns)
        "(?![A-Za-z0-9_])")))

(defn source-file-pattern
  [source-file]
  (re-pattern
   (str "(^|[^A-Za-z0-9_.-])"
        (java.util.regex.Pattern/quote source-file)
        "(?![A-Za-z0-9])")))

(defn source-file-path-pattern
  [source-file]
  (re-pattern
   (str "(^|/)"
        (java.util.regex.Pattern/quote source-file)
        "(?=$|/|_|\\.)")))

(defn replace-token
  [s pattern replacement]
  (str/replace
   s
   pattern
   (fn [match]
     (let [prefix (second match)]
       (str prefix replacement)))))

(defn parameterize-content
  [s source-primary-ns]
  (let [source-file (ns->file source-primary-ns)]
    (-> s
        (replace-token
         (source-file-path-pattern source-file)
         main-file-placeholder)

        (replace-token
         (source-ns-pattern source-primary-ns)
         main-ns-placeholder))))

(defn parameterize-path
  [rel source-primary-ns]
  (let [source-file (ns->file source-primary-ns)]
    (replace-token
     rel
     (source-file-pattern source-file)
     main-file-placeholder)))

(defn read-text-safe
  [p]
  (try
    (slurp (io-str p))
    (catch Throwable _e
      nil)))

(defn write-text-file!
  [p s]
  (fs/create-dirs (fs/parent p))
  (spit (io-str p) s))

(defn copy-raw-file!
  [src dst]
  (fs/create-dirs (fs/parent dst))
  (fs/copy src dst {:replace-existing true}))

(defn copy-template-file!
  [{:keys [source-root
           root-dir
           rename-dir
           raw-dir
           raw-rename-dir
           source-primary-ns]}
   p]
  (let [rel         (rel-str source-root p)
        renamed-rel (parameterize-path rel source-primary-ns)
        rename?     (not= rel renamed-rel)
        binary?     (binary-file? p)]
    (cond
      (and binary? (not rename?))
      (do
        (copy-raw-file! p (fs/path raw-dir rel))
        {:kind :raw
         :from rel
         :to rel})

      (and binary? rename?)
      (do
        (copy-raw-file! p (fs/path raw-rename-dir rel))
        {:kind :raw-rename
         :from rel
         :to renamed-rel})

      rename?
      (if-let [txt (read-text-safe p)]
        (do
          (write-text-file!
           (fs/path rename-dir rel)
           (parameterize-content txt source-primary-ns))
          {:kind :rename
           :from rel
           :to renamed-rel})
        (do
          (copy-raw-file! p (fs/path raw-rename-dir rel))
          {:kind :raw-rename
           :from rel
           :to renamed-rel}))

      :else
      (if-let [txt (read-text-safe p)]
        (do
          (write-text-file!
           (fs/path root-dir rel)
           (parameterize-content txt source-primary-ns))
          {:kind :root
           :from rel
           :to rel})
        (do
          (copy-raw-file! p (fs/path raw-dir rel))
          {:kind :raw
           :from rel
           :to rel})))))

(defn visible-source-children
  [source-root dir]
  (->> (fs/list-dir dir)
       (remove #(excluded? source-root %))))

(defn empty-template-leaf-dir?
  "Return true when DIR has no included children.

   deps-new transforms files rather than directory-only entries, so an empty
   source directory needs a marker file to survive project generation."
  [source-root dir]
  (empty? (visible-source-children source-root dir)))

(defn preserve-empty-template-dir!
  "Stage a marker file for an otherwise-empty source directory.

   The marker uses the same path-renaming machinery as ordinary template files.
   This allows:

     src/net/humanhelp/components

   to become:

     src/net/humanhelp/components

   and finally:

     src/net/humanhelp/components"
  [{:keys [source-root
           root-dir
           rename-dir
           source-primary-ns]}
   dir]
  (let [rel         (rel-str source-root dir)
        marker-rel  (str rel "/" empty-dir-marker)
        renamed-rel (parameterize-path marker-rel source-primary-ns)
        rename?     (not= marker-rel renamed-rel)]
    (if rename?
      (do
        (write-text-file!
         (fs/path rename-dir marker-rel)
         "")
        {:kind :rename
         :from marker-rel
         :to renamed-rel
         :synthetic? true})
      (do
        (write-text-file!
         (fs/path root-dir marker-rel)
         "")
        {:kind :root
         :from marker-rel
         :to marker-rel
         :synthetic? true}))))

(defn transform-entry
  [src file-map opts]
  (cond
    (seq file-map)
    (into [src file-map] opts)

    (seq opts)
    (into [src] opts)

    :else
    nil))

(defn template-edn
  [rename-map raw? raw-rename-map]
  (let [transforms
        (cond-> []
          (seq rename-map)
          (conj
           (transform-entry
            "rename"
            rename-map
            [:only]))

          raw?
          (conj
           (transform-entry
            "raw"
            nil
            [:raw]))

          (seq raw-rename-map)
          (conj
           (transform-entry
            "raw-rename"
            raw-rename-map
            [:only :raw])))]
    (cond-> {:description "FIXME: generated project from template."
             :root "root"}
      (seq transforms)
      (assoc :transform transforms))))

(defn write-template-edn!
  [template-resource-dir rename-map raw? raw-rename-map]
  (write-text-file!
   (fs/path template-resource-dir "template.edn")
   (with-out-str
     (pprint/pprint
      (template-edn
       rename-map
       raw?
       raw-rename-map)))))

(defn write-deps-edn!
  [template-dir]
  (write-text-file!
   (fs/path template-dir "deps.edn")
   "{:paths [\"resources\"]}\n"))

(defn write-readme!
  [template-dir template-name template-resource-dir]
  (write-text-file!
   (fs/path template-dir "README.md")
   (str
    "# Generated deps-new Template\n\n"
    "Template name:\n\n"
    "```text\n"
    template-name
    "\n```\n\n"
    "Use locally with:\n\n"
    "```sh\n"
    "clojure -Sdeps '{:deps {local/template {:local/root \""
    (path-str template-dir)
    "\"}}}' \\\n"
    "  -Tnew create \\\n"
    "  :template "
    template-name
    " \\\n"
    "  :name gargle/gargle \\\n"
    "  :target-dir gargle\n"
    "```\n\n"
    "Template EDN:\n\n"
    "```text\n"
    (path-str
     (fs/relativize
      template-dir
      (fs/path template-resource-dir "template.edn")))
    "\n```\n")))

(defn remaining-content-matches
  [dirs source-primary-ns]
  (let [source-file (ns->file source-primary-ns)
        patterns
        [(re-pattern
          (java.util.regex.Pattern/quote source-primary-ns))
         (re-pattern
          (java.util.regex.Pattern/quote source-file))]]
    (->> dirs
         (mapcat #(fs/glob % "**"))
         (filter fs/regular-file?)
         (remove binary-file?)
         (keep
          (fn [p]
            (when-let [txt (read-text-safe p)]
              (let [hits
                    (for [[idx line]
                          (map-indexed
                           vector
                           (str/split-lines txt))
                          :when
                          (some #(re-find % line)
                                patterns)]
                      [(inc idx) line])]
                (when (seq hits)
                  [(path-str p) hits]))))))))

(defn warn-leftovers!
  [dirs source-primary-ns]
  (let [leftovers
        (remaining-content-matches
         dirs
         source-primary-ns)]
    (when (seq leftovers)
      (binding [*out* *err*]
        (println)
        (println
         "WARNING: old primary namespace text remains in template file content:")

        (doseq [[file hits] leftovers]
          (println)
          (println file)

          (doseq [[line text] (take 20 hits)]
            (println " " line ":" text))))

      (println)
      (println
       "Template was still written, but inspect the warnings above."))))

(defn main
  [& args]
  (let [[template-dir-name
         source-primary-ns
         template-name
         & more]
        args]

    (when (or (nil? template-dir-name)
              (nil? source-primary-ns)
              (seq more))
      (usage!))

    (let [source-root
          (fs/cwd)

          template-name
          (or template-name
              (default-template-name template-dir-name))

          template-dir
          (sibling-path source-root template-dir-name)

          template-resource-dir
          (apply fs/path
                 template-dir
                 "resources"
                 (template-path-parts template-name))

          root-dir
          (fs/path template-resource-dir "root")

          rename-dir
          (fs/path template-resource-dir "rename")

          raw-dir
          (fs/path template-resource-dir "raw")

          raw-rename-dir
          (fs/path template-resource-dir "raw-rename")]

      (when (fs/exists? template-dir)
        (fail-path-exists! template-dir))

      (fs/create-dirs root-dir)
      (fs/create-dirs rename-dir)
      (fs/create-dirs raw-dir)
      (fs/create-dirs raw-rename-dir)

      (let [ctx
            {:source-root source-root
             :root-dir root-dir
             :rename-dir rename-dir
             :raw-dir raw-dir
             :raw-rename-dir raw-rename-dir
             :source-primary-ns source-primary-ns}

            file-results
            (doall
             (for [p (fs/glob source-root "**")
                   :when
                   (and (fs/regular-file? p)
                        (not (excluded? source-root p)))]
               (copy-template-file! ctx p)))

            empty-dir-results
            (doall
             (for [p (fs/glob source-root "**")
                   :when
                   (and
                    (fs/directory? p)
                    (not (excluded? source-root p))
                    (not
                     (contains?
                      #{"" "."}
                      (rel-str source-root p)))
                    (empty-template-leaf-dir?
                     source-root
                     p))]
               (preserve-empty-template-dir! ctx p)))

            results
            (concat
             file-results
             empty-dir-results)

            rename-map
            (into
             (sorted-map)
             (for [{:keys [kind from to]} results
                   :when (= :rename kind)]
               [from to]))

            raw?
            (boolean
             (some #(= :raw (:kind %))
                   results))

            raw-rename-map
            (into
             (sorted-map)
             (for [{:keys [kind from to]} results
                   :when (= :raw-rename kind)]
               [from to]))

            preserved-empty-dirs
            (count empty-dir-results)]

        (write-template-edn!
         template-resource-dir
         rename-map
         raw?
         raw-rename-map)

        (write-deps-edn! template-dir)

        (write-readme!
         template-dir
         template-name
         template-resource-dir)

        (warn-leftovers!
         [root-dir
          rename-dir
          raw-dir
          raw-rename-dir]
         source-primary-ns)

        (println "Created deps-new template project:")
        (println " " (path-str template-dir))
        (println)

        (println "Preserved empty source directories:")
        (println " " preserved-empty-dirs)
        (println)

        (println "Template name:")
        (println " " template-name)
        (println)

        (println "Template EDN:")
        (println
         " "
         (path-str
          (fs/path
           template-resource-dir
           "template.edn")))
        (println)

        (println "Use locally with:")
        (println
         (str
          "clojure -Sdeps '{:deps {local/template {:local/root \""
          (path-str template-dir)
          "\"}}}' "
          "-Tnew create "
          ":template "
          template-name
          " "
          ":name gargle/gargle "
          ":target-dir gargle"))))))

(apply main *command-line-args*)
