(ns humanhelp.script.humanhelp-test
  "Portable HumanHelp test-gate orchestrator.

   This harness intentionally mirrors Gesso's gate-oriented test entry point
   without pretending HumanHelp already owns Gesso's CLJS/runtime/browser
   suites. The current HumanHelp gate is:

     repository integrity -> JVM tests

   Use the optional `local` token while HumanHelp is being developed against
   the sibling ../gesso checkout:

     bb script/humanhelp_test.clj all local
     bb script/humanhelp_test.clj jvm local

   Omitting `local` uses the pinned Gesso dependency declared by deps.edn."
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clojure.string :as str]))

(def repo-integrity-script
  "script/repo_integrity.clj")

(def gate-failure-key
  ::gate-failure)

(defn fail!
  [message data]
  (throw
   (ex-info
    message
    data)))

(defn gate-failure!
  [message data]
  (throw
   (ex-info
    message
    (assoc data gate-failure-key true))))

(defn gate-failure?
  [error]
  (boolean
   (and (instance? clojure.lang.ExceptionInfo error)
        (get (ex-data error) gate-failure-key))))

(defn ensure-project-root!
  []
  (when-not
   (fs/regular-file? "deps.edn")
    (fail!
     "Run the HumanHelp test harness from the project root containing deps.edn."
     {:cwd (str (fs/cwd))})))

(defn monotonic-nanos
  []
  (System/nanoTime))

(defn elapsed-seconds
  [started-at]
  (/ (double
      (-
       (monotonic-nanos)
       started-at))
     1000000000.0))

(defn print-stage-pass!
  [label started-at]
  (println
   (format
    "%s: PASS (%.2fs)"
    label
    (elapsed-seconds started-at))))

(defn run-command!
  [command]
  (let [result
        @(process/process
          command
          {:in  :inherit
           :out :inherit
           :err :inherit})]
    (when-not
     (zero? (:exit result))
      (gate-failure!
       "External test command failed."
       {:command command
        :exit    (:exit result)}))
    result))

(defn clojure-command
  []
  (or
   (some->
    (System/getenv "CLOJURE")
    str/trim
    not-empty)
   (some
    (fn [candidate]
      (some->
       (fs/which candidate)
       str))
    ["clojure" "clj"])
   (fail!
    "No Clojure CLI executable was found. Set CLOJURE or put clojure/clj on PATH."
    {})))

(defn parse-command-line
  [args]
  (let [args        (vec args)
        local-count (count (filter #{"local"} args))
        positional  (vec (remove #{"local"} args))]
    (when (> local-count 1)
      (fail!
       "The local Gesso mode token may be supplied at most once."
       {:args args}))
    (when (> (count positional) 1)
      (fail!
       "Expected at most one test command plus the optional `local` token."
       {:args args}))
    {:command      (or (first positional) "all")
     :local-gesso? (pos? local-count)}))

(defn test-alias
  [local-gesso?]
  (if local-gesso?
    "-M:local-gesso:test"
    "-M:test"))

(defn run-repo-integrity!
  [_opts]
  (println)
  (println "== Repository integrity ==")

  (when-not
   (fs/regular-file? repo-integrity-script)
    (fail!
     "Repository integrity script is missing."
     {:script repo-integrity-script}))

  (let [started-at (monotonic-nanos)]
    (run-command!
     [(or
       (some-> (fs/which "bb") str)
       (fail!
        "Repository integrity checking requires Babashka on PATH."
        {:script repo-integrity-script}))
      repo-integrity-script])
    (print-stage-pass!
     "Repository integrity"
     started-at)))

(defn run-jvm-tests!
  [{:keys [local-gesso?]}]
  (println)
  (println "== JVM tests ==")
  (println
   (if local-gesso?
     "Gesso dependency: local ../gesso checkout"
     "Gesso dependency: pinned deps.edn release"))

  (let [started-at (monotonic-nanos)]
    (run-command!
     [(clojure-command)
      (test-alias local-gesso?)])
    (print-stage-pass!
     "JVM tests"
     started-at)))

(def gate-specs
  {:integrity
   {:label "Repo integrity"
    :run   run-repo-integrity!}

   :jvm
   {:label "JVM"
    :run   run-jvm-tests!}})

(defn run-gate
  [gate-id opts]
  (let [{:keys [label run]}
        (or
         (get gate-specs gate-id)
         (fail!
          "Unknown HumanHelp test gate."
          {:gate-id     gate-id
           :known-gates (set (keys gate-specs))}))]
    (try
      (run opts)
      {:gate   gate-id
       :label  label
       :status :pass}
      (catch Throwable error
        (if (gate-failure? error)
          (let [data (dissoc (ex-data error) gate-failure-key)]
            (println)
            (println (str label ": FAIL"))
            (println (ex-message error))
            {:gate    gate-id
             :label   label
             :status  :fail
             :message (ex-message error)
             :data    data})
          (throw error))))))

(defn print-gate-summary!
  [results]
  (println)
  (println "== Gate summary ==")
  (doseq [{:keys [label status]} results]
    (println
     (format
      "%-18s %s"
      label
      (if (= :pass status)
        "PASS"
        "FAIL"))))
  (let [failed (filter #(= :fail (:status %)) results)]
    (println)
    (if (seq failed)
      (println (count failed) "gate(s) failed.")
      (println "All selected HumanHelp gates passed.")))
  results)

(defn run-gates!
  [gate-ids opts]
  (let [results
        (mapv
         #(run-gate % opts)
         gate-ids)]
    (print-gate-summary! results)))

(defn failed-gates?
  [results]
  (boolean
   (some
    #(= :fail (:status %))
    results)))

(defn command-gates
  [command]
  (case command
    "all"
    [:integrity
     :jvm]

    "integrity"
    [:integrity]

    "jvm"
    [:integrity
     :jvm]

    nil))

(defn usage!
  []
  (println
   "Usage: bb script/humanhelp_test.clj [all|integrity|jvm] [local]")
  (println)
  (println "  all        Repository integrity + JVM tests (default)")
  (println "  integrity  Repository namespace/path integrity only")
  (println "  jvm        Repository integrity + JVM tests")
  (println "  local      Use sibling ../gesso through :local-gesso")
  (System/exit 2))

(defn -main
  []
  (ensure-project-root!)
  (let [{:keys [command] :as opts}
        (parse-command-line *command-line-args*)
        gates
        (command-gates command)]
    (when-not gates
      (usage!))
    (let [results
          (run-gates! gates opts)]
      (when (failed-gates? results)
        (System/exit 1)))))

(-main)
