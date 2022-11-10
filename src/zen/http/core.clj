(ns zen.http.core
  (:require
   [ring.util.response :as mw-util]
   [clojure.java.io :as io]
   [zen.core :as zen]
   [ring.util.codec :as codec]
   [org.httpkit.server :as http-kit]
   [clojure.string :as str]
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

(defn resolve-mw [ztx sym]
  ;; take opts from instance or engine
  (let [mw-cfg (zen/get-symbol ztx sym)]
    (->> (zen/engine-or-name mw-cfg)
         (zen/get-symbol ztx)
         (merge mw-cfg))))

;; TODO wrap in zen/op
(defn dispatch [ztx comp-symbol {uri :uri meth :request-method :as req}]
  (let [api-config (zen/get-symbol ztx comp-symbol)
        path (conj (rm/pathify uri) (-> (or meth :get) name str/upper-case keyword))
        session {}]
    (if-let [{op :op  params :params mw :middlewares :as route}
             (resolve-route ztx api-config path initial-ctx)]
      (let [req*
            (loop [mws mw
                   req (assoc req :route-params params)]
              (let [{:keys [dir] :as mw-cfg} (resolve-mw ztx (first mws))]
                (cond
                  (empty? mws) req
              ;; TODO add error if middleware not found?
                  (nil? mw-cfg) (recur (rest mws) req)
                  (or (nil? dir) (contains? dir :in))
                  (let [patch (middleware-in ztx mw-cfg req)]
                    (if-let [resp (::response patch)]
                      resp
                      (recur (rest mws)
                             (if (map? patch)
                               (utils/deep-merge req patch)
                               req))))
                  :else (recur (rest mws) req))))

            resp
            (if (:status req*)
              req*
              (zen/op-call ztx op req* session))]

        (reduce (fn [resp* mw-symbol]
                  (let [{:keys [dir] :as mw-cfg} (resolve-mw ztx mw-symbol)]
                    (cond
                      ;; TODO impl ::response for outcoming mw
                      (nil? mw-cfg) resp*
                      (or (nil? dir) (contains? dir :out))
                      (utils/deep-merge resp* (middleware-out ztx mw-cfg req* resp*))
                      :else resp*)))
                resp
                mw))
      {:status 404 :body "route not found"})))

;; TODO format response, parse body
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

(defmethod zen/op 'zen.http.engines/response
  [ztx config req & args]
  (cond-> (:response config)
    (keyword? (:select config))
    (assoc :body (str (get req (:select config))))))

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
