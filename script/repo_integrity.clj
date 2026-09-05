#!/usr/bin/env bb

(ns humanhelp.script.repo-integrity
  "Validate canonical Clojure namespace placement under src/ and test/.

   This is the Babashka replacement for the old Fish/nsof repository-integrity
   gate. It intentionally checks repository structure only; compilation and
   runtime behavior belong to later test gates."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))

(def ^:private source-trees
  ["src" "test"])

(def ^:private source-extensions
  ["clj" "cljc" "cljs"])

(def ^:private namespace-declaration-pattern
  #"(?m)^[\t ]*\(ns[\t \r\n]+([^\s()\[\]{};]+)")

(defn- fail-configuration!
  [message]
  (binding [*out* *err*]
    (println (str "repo-integrity: " message)))
  (System/exit 2))

(defn- repository-root
  []
  (let [cwd (fs/normalize (fs/absolutize (fs/cwd)))]
    (if (fs/regular-file? (fs/path cwd "deps.edn"))
      cwd
      (fail-configuration!
       "run from the HumanHelp repository root containing deps.edn"))))

(defn- source-files
  [repo-root]
  (->> source-trees
       (mapcat
        (fn [tree]
          (let [root (fs/path repo-root tree)]
            (mapcat
             (fn [extension]
               (fs/glob root (str "**/*." extension)))
             source-extensions))))
       (map fs/normalize)
       distinct
       (sort-by str)
       vec))

(defn- relative-path
  [repo-root path]
  (-> (fs/relativize repo-root path)
      str
      (str/replace "\\" "/")))

(defn- canonical-path
  [tree namespace extension]
  (str tree
       "/"
       (-> (str namespace)
           (str/replace "." "/")
           (str/replace "-" "_"))
       "."
       extension))

(defn- test-namespace?
  [namespace]
  (let [value (str namespace)]
    (or (str/ends-with? value "-test")
        (some #{"test"}
              (str/split value #"\.")))))

(defn- file-tree
  [relative]
  (first (str/split relative #"/" 2)))

(defn- file-extension
  [relative]
  (second (re-find #"\.([^./]+)$" relative)))

(defn- inspect-file
  [repo-root path]
  (let [relative (relative-path repo-root path)
        contents (slurp (str path))]
    (if (str/blank? contents)
      {:file     relative
       :warnings
       [(str "empty source placeholder: " relative)]
       :problems []}
      (let [namespaces (->> (re-seq namespace-declaration-pattern contents)
                            (map second)
                            (map symbol)
                            vec)]
        (cond
          (not= 1 (count namespaces))
          {:file     relative
           :warnings []
           :problems
           [(str "cannot determine exactly one namespace for " relative
                 " (found " (count namespaces) ")")]}

          :else
          (let [namespace (first namespaces)
                tree      (file-tree relative)
                extension (file-extension relative)
                expected  (canonical-path tree namespace extension)]
            {:file      relative
             :namespace namespace
             :warnings  []
             :problems
             (cond-> []
               (not= relative expected)
               (conj
                (str "namespace/path mismatch: " relative
                     " declares " namespace
                     "; canonical path is " expected))

               (and (= tree "src")
                    (test-namespace? namespace))
               (conj
                (str "test namespace is installed under src/: " relative
                     " declares " namespace)))}))))))

(defn- duplicate-namespace-problems
  [inspections]
  (->> inspections
       (keep
        (fn [{:keys [file namespace]}]
          (when namespace
            [namespace file])))
       (reduce
        (fn [{:keys [seen problems] :as state} [namespace file]]
          (if-let [first-file (get seen namespace)]
            (assoc state
                   :problems
                   (conj problems
                         (str "duplicate namespace " namespace
                              ": " first-file " and " file)))
            (assoc state
                   :seen
                   (assoc seen namespace file))))
        {:seen     {}
         :problems []})
       :problems))

(defn- validate-trees!
  [repo-root]
  (doseq [tree source-trees]
    (when-not (fs/directory? (fs/path repo-root tree))
      (fail-configuration!
       (str "missing repository tree: " tree "/")))))

(defn- run!
  []
  (let [repo-root (repository-root)]
    (validate-trees! repo-root)
    (let [files (source-files repo-root)]
      (when (empty? files)
        (fail-configuration!
         "no Clojure/ClojureScript namespace files found under src/ or test/"))
      (let [inspections
            (mapv
             (fn [path]
               (try
                 (inspect-file repo-root path)
                 (catch Throwable error
                   {:file     (relative-path repo-root path)
                    :warnings []
                    :problems
                    [(str "cannot read namespace for "
                          (relative-path repo-root path)
                          ": "
                          (ex-message error))]})))
             files)

            warnings
            (vec (mapcat :warnings inspections))

            problems
            (into
             (vec (mapcat :problems inspections))
             (duplicate-namespace-problems inspections))]
        (binding [*out* *err*]
          (doseq [warning warnings]
            (println (str "repo-integrity: WARNING — " warning))))
        (if (seq problems)
          (do
            (binding [*out* *err*]
              (doseq [problem problems]
                (println (str "repo-integrity: " problem)))
              (println
               (str "repo-integrity: FAIL — "
                    (count problems)
                    " problem(s), "
                    (count warnings)
                    " warning(s) across "
                    (count files)
                    " source file(s)")))
            (System/exit 1))
          (println
           (str "repo-integrity: PASS — "
                (count files)
                " source file(s), "
                (count warnings)
                " warning(s)")))))))

(run!)
