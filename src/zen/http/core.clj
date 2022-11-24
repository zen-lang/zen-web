(ns zen.http.core
  (:require
   [ring.util.response :as mw-util]
   [clojure.java.io :as io]
   [zen.core :as zen]
   [ring.util.codec :as codec]
   [org.httpkit.server :as http-kit]
   [clojure.string :as str]
   [zen.http.oauth.core :as oauth]
   [zen.http.utils :as utils]
   [zen.http.httpkit]
   [zen.http.routemap :as rm]
   [zen.http.middlewares :as mw]))

(defmulti middleware-in
  (fn [ztx cfg request]
    (zen/engine-or-name cfg)))

(defmulti middleware-out
  (fn [ztx cfg request response]
    (zen/engine-or-name cfg)))

(defmethod middleware-in 'zen.http.oauth/verify-jwt
  [ztx cfg req & args]
  (oauth/verify-jwt ztx cfg req))

(defmethod middleware-in 'zen.http.oauth/snap-config
  [ztx cfg req & args]
  (oauth/snap-config ztx cfg req))

(defmethod zen/op 'zen.http.oauth/redirect
  [ztx cfg req & opts]
  (oauth/redirect ztx cfg req))

(defmethod zen/op 'zen.http.oauth/callback
  [ztx cfg req & opts]
  (oauth/callback ztx cfg req))

(defmethod middleware-in 'zen.http.engines/basic-auth
  [ztx cfg req & args]
  (mw/verify-basic-auth ztx cfg req))

(defmethod middleware-out 'zen.http/cors
  [ztx cfg req resp & args]
  (mw/set-cors-headers ztx cfg req resp))

(defmethod middleware-in 'zen.http/parse-params
  [ztx cfg req & args]
  (mw/parse-params ztx cfg req))

(defmethod middleware-in 'zen.http/cookies
  [ztx cfg req & args]
  (mw/parse-cookies ztx cfg req))

(defmethod middleware-out 'zen.http/cookies
  [ztx cfg req resp & args]
  (mw/set-cookies ztx cfg req resp))

(defmethod middleware-in 'zen.http.engines/all-of
  [ztx cfg req & args]
  (mw/all-of-in ztx cfg req))

(defmethod middleware-out 'zen.http.engines/all-of
  [ztx cfg req resp & args]
  (mw/all-of-out ztx cfg req resp))

(defmulti resolve-route
  (fn [_ztx cfg _path {_params :params _mws :middlewares _pth :path}]
    (zen/engine-or-name cfg)))

;; TODO wrap in zen/op
(defmethod resolve-route 'zen.http/routemap
  [ztx cfg path ctx]
  (rm/resolve-route ztx cfg path ctx))

(defmethod resolve-route
  :default
  [ztx cfg _path ctx]
  (zen/error ztx 'zen.http/no-resolve-route-method {:method (zen/engine-or-name cfg) :path (:path ctx)})
  nil)

(def initial-ctx {:path [] :params {} :middlewares []})

(defn *resolve-route
  [ztx cfg-or-name path]
  (if (symbol? cfg-or-name)
    (let [cfg (zen/get-symbol ztx cfg-or-name)]
      (resolve-route ztx cfg path initial-ctx))
    (resolve-route ztx cfg-or-name path initial-ctx)))

(defmulti routes
  "collect routes"
  (fn [_ztx cfg _ctx] (zen/engine-or-name cfg)))

(defmethod routes
  :default
  [ztx cfg ctx]
  [(-> (assoc ctx :op 'unknown :error (str "method zen.http.methods/routes is not implemented for " (:zen/name cfg)))
       (update :path conj :?))])

(defmethod routes 'zen.http/routemap
  [ztx cfg ctx]
  (rm/*routes ztx cfg ctx))

(defn *routes
  "collect routes"
  [ztx cfg-or-name]
  (let [ctx {:path [] :middlewares [] :params #{} :by []}]
    (->>
     (if (symbol? cfg-or-name)
       (let [cfg (zen/get-symbol ztx cfg-or-name)]
         (routes ztx cfg ctx))
       (routes ztx cfg-or-name ctx))
     (sort-by (fn [x] (str/join "/" (:path x)))))))

;; TODO wrap in zen/op
(defn dispatch [ztx comp-symbol {uri :uri meth :request-method :as req}]
  (let [api-config (zen/get-symbol ztx comp-symbol)
        path (conj (rm/pathify uri) (-> (or meth :get) name str/upper-case keyword))
        ;; TODO understand if we need session param for zen/op
        session {}]
    (if-let [{op :op  params :params mw :middlewares}
             (resolve-route ztx api-config path initial-ctx)]
      ;; apply inbound middlewares
      (let [all-mws (map #(utils/resolve-mw ztx %) mw)
            req*
            (loop [mws (filter #(contains? (:dir %) :in)
                               all-mws)
                   req (assoc req :route-params params)]
              (if (empty? mws)
                req
                (let [patch (middleware-in ztx (first mws) req)]
                  (if-let [resp (::response patch)]
                    resp
                    (recur (rest mws)
                           (if (map? patch)
                             (utils/deep-merge req patch)
                             req))))))

            ;; call the handler if needed
            resp
            (if (:status req*)
              req*
              (zen/op-call ztx op req* session))]

        ;; apply outbound middlewares
        (loop [mws (filter #(contains? (:dir %) :out)
                           all-mws)
               resp resp]
          (if (empty? mws)
            resp
            (let [patch (middleware-out ztx (first mws) req resp)]
              (if-let [resp (::response patch)]
                resp
                (recur (rest mws)
                       (if (map? patch)
                         (utils/deep-merge resp patch)
                         resp)))))))

      {:status 404 :body "route not found"})))

;; TODO format response
(defn handle [ztx api-symbol {:keys [request-method headers] :as request}]
  (let [method-override (and (= :post request-method) (get headers "x-http-method-override"))
        parsed-request
        (cond-> request
          :always (update :uri codec/url-decode)
          method-override (assoc :request-method (keyword (str/lower-case method-override))))]
    ;; TODO move this matching to cors mw sometimes, see Issues/1
    (if (= :options request-method)
      {:status 200
       :headers
       ;; TODO refer to https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
       ;; set headers accordingly
       {"Access-Control-Allow-Headers" (get headers "access-control-request-headers")
        "Access-Control-Allow-Methods" (get headers "access-control-request-method")
        "Access-Control-Allow-Origin" (get headers "origin")
        "Access-Control-Allow-Credentials" "true"
        "Access-Control-Expose-Headers"
        "Location, Transaction-Meta, Content-Location, Category, Content-Type, X-total-count"}}
      (dispatch ztx api-symbol parsed-request))))

(defmethod zen/start 'zen.http/httpkit
  [ztx config]
  (let [web-config
        (merge {:worker-name-prefix "w"
                :thread 8
                :max-body 20971520}
               config)
        req-fn
        (fn [request] (handle ztx (:api config) request))]
    {:server (http-kit/run-server req-fn web-config)}))

(defmethod zen/stop 'zen.http/httpkit
  [ztx config state]
  (when-let [srv (:server state)]
    (srv)))

(defn merge-unset [acc v]
  (if (map? acc)
    (utils/deep-merge acc v)
    v))

(defmethod zen/op 'zen.http.engines/response
  [ztx {:keys [select response] :as cfg} req & args]
  (cond-> response
    (vector? select)
    (update :body merge-unset (select-keys req select))

    (keyword? select)
    (update :body merge-unset (get req select))))

(defmethod zen/op 'zen.http.engines/redirect
  [ztx {:keys [to]} req & args]
  {:status 301
   :headers {"Location" to}})

(defmethod zen/op 'zen.http.engines/serve-static
  [ztx {:keys [serve]} {uri :uri rp :route-params :as req} & args]
  (let [file-path (str/join "/" (:* rp))]
    (if-let [f (or (io/resource file-path)
                   (->> serve
                        (map (fn [p]
                               (str (System/getProperty "user.dir") p)))
                        (map (fn [path]
                               (let [file-path* (str path "/" file-path)
                                     f (io/file file-path*)]
                                 (when (.exists f) f))))
                        (filter identity)
                        (first)))]
    ;; TODO get rid of mw util dep
      (mw-util/file-response (.getPath f))
      {:status 404
       :body "file not found"})))
