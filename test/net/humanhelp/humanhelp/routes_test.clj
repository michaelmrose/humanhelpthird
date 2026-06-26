(ns net.humanhelp.humanhelp.routes-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [net.humanhelp.humanhelp.routes :as routes])
  (:import
   [java.net URLDecoder]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn decode
  [s]
  (URLDecoder/decode (str s) "UTF-8"))

(defn split-url
  [url]
  (let [[path query] (str/split (str url) #"\?" 2)]
    {:path path
     :query query}))

(defn path-of
  [url]
  (:path (split-url url)))

(defn query-part
  [url-or-query]
  (let [s (str url-or-query)
        query (:query (split-url s))]
    (cond
      query
      query

      (str/starts-with? s "?")
      (subs s 1)

      :else
      s)))

(defn parse-query
  [url-or-query]
  (let [query (query-part url-or-query)]
    (if (str/blank? query)
      {}
      (reduce
       (fn [m pair]
         (let [[k v] (str/split pair #"=" 2)]
           (update m
                   (decode k)
                   (fnil conj [])
                   (decode (or v "")))))
       {}
       (remove str/blank? (str/split query #"&"))))))

(defn query-value
  [url-or-query k]
  (first (get (parse-query url-or-query) (name k))))

(defn query-values
  [url-or-query k]
  (get (parse-query url-or-query) (name k)))

(defn has-query?
  [url]
  (some? (:query (split-url url))))

(defn no-query?
  [url]
  (not (has-query? url)))

(defn route-path-has-placeholder?
  [path]
  (let [s (str path)]
    (or (str/includes? s ":")
        (str/includes? s "{")
        (str/includes? s "}"))))

(defn request-id-from-action-url
  [url]
  (let [path (path-of url)
        prefix "/app/requests/"
        action-start (str/last-index-of path "/")]
    (decode
     (subs path
           (count prefix)
           action-start))))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def full-view-state
  {:search "garden rake"
   :visible-revision 3})

(def blank-view-state
  {:search ""
   :visible-revision nil})

(def board-options-view-state
  {:search "paint"
   :visible-revision 7
   :created-order :oldest
   :mine-first? true
   :unclaimed-first? true
   :show-terminal? true})

(def all-route-fragments
  [routes/request-toolbar-fragment-route
   routes/request-list-fragment-route
   routes/create-request-dialog-fragment-route
   routes/request-toolbar-stream-route
   routes/request-list-stream-route
   routes/create-request-route
   routes/refresh-requests-route
   routes/search-requests-route
   routes/apply-board-options-route
   routes/claim-request-route
   routes/unclaim-request-route
   routes/take-over-request-route
   routes/done-request-route
   routes/cancel-request-route
   routes/reset-demo-route])

(def request-specific-route-fragments
  [routes/claim-request-route
   routes/unclaim-request-route
   routes/take-over-request-route
   routes/done-request-route
   routes/cancel-request-route])

(def non-request-route-fragments
  [routes/request-toolbar-fragment-route
   routes/request-list-fragment-route
   routes/create-request-dialog-fragment-route
   routes/request-toolbar-stream-route
   routes/request-list-stream-route
   routes/create-request-route
   routes/refresh-requests-route
   routes/search-requests-route
   routes/apply-board-options-route
   routes/reset-demo-route])

;; -----------------------------------------------------------------------------
;; Constants / route fragments
;; -----------------------------------------------------------------------------

(deftest public-route-constants-test
  (is (= "/app" routes/base-path))
  (is (= "demo-store" routes/store-id))

  (is (= "request-id" routes/request-id-param))

  (is (= "q" routes/search-param))
  (is (= "visible-revision" routes/visible-revision-param))
  (is (= "created-order" routes/created-order-param))
  (is (= "mine-first" routes/mine-first-param))
  (is (= "unclaimed-first" routes/unclaimed-first-param))
  (is (= "show-terminal" routes/show-terminal-param))

  (doseq [route all-route-fragments]
    (is (string? route))
    (is (str/starts-with? route "/"))
    (is (not (str/starts-with? route routes/base-path))
        (str "Route fragment should not include base path: " route))))

(deftest path-test
  (is (= "/app/fragments/request-toolbar"
         (routes/path routes/request-toolbar-fragment-route)))
  (is (= "/app/fragments/requests"
         (routes/path routes/request-list-fragment-route)))
  (is (= "/app/fragments/create-request-dialog"
         (routes/path routes/create-request-dialog-fragment-route)))
  (is (= "/app/streams/request-toolbar"
         (routes/path routes/request-toolbar-stream-route)))
  (is (= "/app/streams/requests"
         (routes/path routes/request-list-stream-route)))
  (is (= "/app/requests"
         (routes/path routes/create-request-route)))
  (is (= "/app/requests/refresh"
         (routes/path routes/refresh-requests-route)))
  (is (= "/app/requests/search"
         (routes/path routes/search-requests-route)))
  (is (= "/app/humanhelp/board-options"
         (routes/path routes/apply-board-options-route)))
  (is (= "/app/demo/reset"
         (routes/path routes/reset-demo-route)))

  (doseq [route non-request-route-fragments]
    (is (not (str/includes? (routes/path route) "//"))
        (str "Path contains double slash: " (routes/path route)))))

(deftest route-placeholder-test
  (doseq [route request-specific-route-fragments]
    (is (route-path-has-placeholder? route)
        (str "Expected placeholder in route: " route)))

  (doseq [route non-request-route-fragments]
    (is (not (route-path-has-placeholder? route))
        (str "Unexpected placeholder in route: " route))))

;; -----------------------------------------------------------------------------
;; Query helpers
;; -----------------------------------------------------------------------------

(deftest query-string-empty-test
  (is (nil? (routes/query-string nil)))
  (is (nil? (routes/query-string {})))
  (is (nil? (routes/query-string {:q nil})))
  (is (nil? (routes/query-string {:q ""})))
  (is (nil? (routes/query-string {:q "   "})))
  (is (nil? (routes/query-string {:tag []})))
  (is (nil? (routes/query-string {:tag [nil "" "   "]}))))

(deftest query-string-scalar-test
  (let [qs (routes/query-string
            {:q "jon rake/garden"
             :visible-revision 3
             :status :open
             :thing 'hello})]
    (is (str/starts-with? qs "?"))
    (is (= "jon rake/garden" (query-value qs :q)))
    (is (= "3" (query-value qs :visible-revision)))
    (is (= ":open" (query-value qs :status)))
    (is (= "hello" (query-value qs :thing)))))

(deftest query-string-sequential-test
  (let [qs (routes/query-string
            {:q "garden"
             :tag ["a" "b" nil "" "  "]
             :visible-revision 9})]
    (is (= "garden" (query-value qs :q)))
    (is (= ["a" "b"] (query-values qs :tag)))
    (is (= "9" (query-value qs :visible-revision)))))

(deftest with-query-test
  (is (= "/app" (routes/with-query "/app" nil)))
  (is (= "/app" (routes/with-query "/app" {})))
  (is (= "/app" (routes/with-query "/app" {:q ""})))

  (let [url (routes/with-query
             "/app"
             {:q "garden"
              :visible-revision 3})]
    (is (= "/app" (path-of url)))
    (is (= "garden" (query-value url :q)))
    (is (= "3" (query-value url :visible-revision)))))

(deftest view-state-query-test
  (is (= {"q" "garden"
          "visible-revision" 3}
         (routes/view-state-query
          {:search "garden"
           :visible-revision 3})))

  (is (= {"q" nil
          "visible-revision" nil}
         (routes/view-state-query nil))))

(deftest view-state-query-board-options-test
  (is (= {"q" "paint"
          "visible-revision" 7
          "created-order" "oldest"
          "mine-first" "true"
          "unclaimed-first" "true"
          "show-terminal" "true"}
         (routes/view-state-query board-options-view-state)))

  (is (= {"q" "paint"
          "visible-revision" 7}
         (routes/view-state-query
          {:search "paint"
           :visible-revision 7
           :created-order :newest
           :mine-first? false
           :unclaimed-first? false
           :show-terminal? false}))))

;; -----------------------------------------------------------------------------
;; Page / fragment / stream URLs
;; -----------------------------------------------------------------------------

(deftest page-url-test
  (is (= routes/base-path (routes/page-url)))
  (is (= routes/base-path (routes/page-url nil)))
  (is (= routes/base-path (routes/page-url {})))
  (is (= routes/base-path (routes/page-url blank-view-state)))

  (let [url (routes/page-url full-view-state)]
    (is (= routes/base-path (path-of url)))
    (is (= "garden rake" (query-value url routes/search-param)))
    (is (= "3" (query-value url routes/visible-revision-param))))

  (let [url (routes/page-url board-options-view-state)]
    (is (= routes/base-path (path-of url)))
    (is (= "paint" (query-value url routes/search-param)))
    (is (= "7" (query-value url routes/visible-revision-param)))
    (is (= "oldest" (query-value url routes/created-order-param)))
    (is (= "true" (query-value url routes/mine-first-param)))
    (is (= "true" (query-value url routes/unclaimed-first-param)))
    (is (= "true" (query-value url routes/show-terminal-param)))))

(deftest fragment-url-test
  (is (= "/app/fragments/request-toolbar"
         (routes/request-toolbar-fragment-url)))
  (is (= "/app/fragments/requests"
         (routes/request-list-fragment-url)))
  (is (= "/app/fragments/create-request-dialog"
         (routes/create-request-dialog-fragment-url)))

  (is (no-query? (routes/request-toolbar-fragment-url)))
  (is (no-query? (routes/request-list-fragment-url)))

  (let [url (routes/request-toolbar-fragment-url full-view-state)]
    (is (= "/app/fragments/request-toolbar" (path-of url)))
    (is (= "garden rake" (query-value url routes/search-param)))
    (is (= "3" (query-value url routes/visible-revision-param))))

  (let [url (routes/request-list-fragment-url full-view-state)]
    (is (= "/app/fragments/requests" (path-of url)))
    (is (= "garden rake" (query-value url routes/search-param)))
    (is (= "3" (query-value url routes/visible-revision-param))))

  (let [url (routes/request-list-fragment-url board-options-view-state)]
    (is (= "/app/fragments/requests" (path-of url)))
    (is (= "paint" (query-value url routes/search-param)))
    (is (= "7" (query-value url routes/visible-revision-param)))
    (is (= "oldest" (query-value url routes/created-order-param)))
    (is (= "true" (query-value url routes/mine-first-param)))
    (is (= "true" (query-value url routes/unclaimed-first-param)))
    (is (= "true" (query-value url routes/show-terminal-param))))

  (is (no-query? (routes/request-toolbar-fragment-url blank-view-state)))
  (is (no-query? (routes/request-list-fragment-url blank-view-state))))

(deftest stream-url-test
  (is (= "/app/streams/request-toolbar"
         (routes/request-toolbar-stream-url)))
  (is (= "/app/streams/requests"
         (routes/request-list-stream-url)))

  (is (no-query? (routes/request-toolbar-stream-url)))
  (is (no-query? (routes/request-list-stream-url)))

  (let [url (routes/request-toolbar-stream-url full-view-state)]
    (is (= "/app/streams/request-toolbar" (path-of url)))
    (is (= "garden rake" (query-value url routes/search-param)))
    (is (= "3" (query-value url routes/visible-revision-param))))

  (let [url (routes/request-list-stream-url full-view-state)]
    (is (= "/app/streams/requests" (path-of url)))
    (is (= "garden rake" (query-value url routes/search-param)))
    (is (= "3" (query-value url routes/visible-revision-param))))

  (is (no-query? (routes/request-toolbar-stream-url blank-view-state)))
  (is (no-query? (routes/request-list-stream-url blank-view-state))))

;; -----------------------------------------------------------------------------
;; Request control URLs
;; -----------------------------------------------------------------------------

(deftest request-control-url-test
  (is (= "/app/requests" (routes/create-request-url)))

  (is (= "/app/requests/refresh"
         (routes/refresh-requests-url)))
  (is (= "/app/requests/search"
         (routes/search-requests-url)))
  (is (= "/app/humanhelp/board-options"
         (routes/apply-board-options-url)))

  (let [url (routes/refresh-requests-url full-view-state)]
    (is (= "/app/requests/refresh" (path-of url)))
    (is (= "garden rake" (query-value url routes/search-param)))
    (is (= "3" (query-value url routes/visible-revision-param))))

  (let [url (routes/search-requests-url full-view-state)]
    (is (= "/app/requests/search" (path-of url)))
    (is (= "garden rake" (query-value url routes/search-param)))
    (is (= "3" (query-value url routes/visible-revision-param))))

  (let [url (routes/refresh-requests-url board-options-view-state)]
    (is (= "/app/requests/refresh" (path-of url)))
    (is (= "paint" (query-value url routes/search-param)))
    (is (= "7" (query-value url routes/visible-revision-param)))
    (is (= "oldest" (query-value url routes/created-order-param)))
    (is (= "true" (query-value url routes/mine-first-param)))
    (is (= "true" (query-value url routes/unclaimed-first-param)))
    (is (= "true" (query-value url routes/show-terminal-param))))

  (is (= "/app/requests/refresh"
         (routes/refresh-requests-url blank-view-state)))
  (is (= "/app/requests/search"
         (routes/search-requests-url blank-view-state))))

;; -----------------------------------------------------------------------------
;; Request path substitution / action URLs
;; -----------------------------------------------------------------------------

(deftest request-route-test
  (let [path (routes/request-route routes/claim-request-route "hh-req-1")]
    (is (= "/requests/hh-req-1/claim" path))
    (is (not (route-path-has-placeholder? path))))

  (let [request-id "id with spaces/slash?and=query"
        path (routes/request-route routes/claim-request-route request-id)
        encoded-id (second (re-find #"^/requests/(.*)/claim$" path))]
    (is (str/starts-with? path "/requests/"))
    (is (str/ends-with? path "/claim"))
    (is (= request-id (decode encoded-id)))))

(deftest lifecycle-action-url-test
  (is (= "/app/requests/hh-req-1/claim"
         (routes/claim-request-url "hh-req-1")))
  (is (= "/app/requests/hh-req-1/unclaim"
         (routes/unclaim-request-url "hh-req-1")))
  (is (= "/app/requests/hh-req-1/take-over"
         (routes/take-over-request-url "hh-req-1")))
  (is (= "/app/requests/hh-req-1/done"
         (routes/done-request-url "hh-req-1")))
  (is (= "/app/requests/hh-req-1/cancel"
         (routes/cancel-request-url "hh-req-1"))))

(deftest action-url-test
  (is (= "/app/requests/hh-req-1/claim"
         (routes/action-url "hh-req-1" :claim)))
  (is (= "/app/requests/hh-req-1/unclaim"
         (routes/action-url "hh-req-1" :unclaim)))
  (is (= "/app/requests/hh-req-1/take-over"
         (routes/action-url "hh-req-1" :take-over)))
  (is (= "/app/requests/hh-req-1/done"
         (routes/action-url "hh-req-1" :done)))
  (is (= "/app/requests/hh-req-1/cancel"
         (routes/action-url "hh-req-1" :cancel)))

  (let [request-id "id with spaces/slash?and=query"
        url (routes/action-url request-id :claim)]
    (is (= request-id (request-id-from-action-url url))))

  (try
    (routes/action-url "hh-req-1" :explode)
    (is false "Expected action-url to throw.")
    (catch clojure.lang.ExceptionInfo e
      (is (str/includes? (ex-message e)
                         "Unknown Human Help request action"))
      (is (= "hh-req-1" (:request-id (ex-data e))))
      (is (= :explode (:action (ex-data e)))))))

;; -----------------------------------------------------------------------------
;; Reset URL
;; -----------------------------------------------------------------------------

(deftest reset-demo-url-test
  (is (= "/app/demo/reset" (routes/reset-demo-url))))

;; -----------------------------------------------------------------------------
;; Builder / route-fragment compatibility
;; -----------------------------------------------------------------------------

(deftest route-builder-compatibility-test
  (is (= (routes/path routes/request-toolbar-fragment-route)
         (path-of (routes/request-toolbar-fragment-url full-view-state))))
  (is (= (routes/path routes/request-list-fragment-route)
         (path-of (routes/request-list-fragment-url full-view-state))))

  (is (= (routes/path routes/request-toolbar-stream-route)
         (path-of (routes/request-toolbar-stream-url full-view-state))))
  (is (= (routes/path routes/request-list-stream-route)
         (path-of (routes/request-list-stream-url full-view-state))))

  (is (= (routes/path routes/create-request-route)
         (routes/create-request-url)))
  (is (= (routes/path routes/refresh-requests-route)
         (path-of (routes/refresh-requests-url full-view-state))))
  (is (= (routes/path routes/search-requests-route)
         (path-of (routes/search-requests-url full-view-state))))
  (is (= (routes/path routes/apply-board-options-route)
         (routes/apply-board-options-url)))
  (is (= (routes/path routes/reset-demo-route)
         (routes/reset-demo-url))))
