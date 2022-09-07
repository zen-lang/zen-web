(ns zen.http.core
  (:require [zen.core :as zen]
            [ring.util.codec :as codec]
            [org.httpkit.server :as http-kit]
            [zen.http.methods :as meth]
            [clojure.string :as str]
            [zen.http.httpkit]
            [zen.http.routemap :as rm]))

(defn prepare-request  [ztx request]
  )

(defn prepare-response [ztx request response]
  )

(def initial-ctx {:path [] :params {} :middlewares []})

(defn resolve-route
  [ztx cfg-or-name path]
  (if (symbol? cfg-or-name)
    (let [cfg (zen/get-symbol ztx cfg-or-name)]
      (meth/resolve-route ztx cfg path initial-ctx))
    (meth/resolve-route ztx cfg-or-name path initial-ctx)))

(defn routes
  "collect routes"
  [ztx cfg-or-name]
  (let [ctx {:path [] :middlewares [] :params #{} :by []}]
    (->>
     (if (symbol? cfg-or-name)
       (let [cfg (zen/get-symbol ztx cfg-or-name)]
         (meth/routes ztx cfg ctx))
       (meth/routes ztx cfg-or-name ctx))
     (sort-by (fn [x] (str/join "/" (:path x)))))))

(defn dispatch [ztx comp-symbol {uri :uri meth :request-method :as req}]
  (let [api-config (zen/get-symbol ztx comp-symbol)
        path (conj (rm/pathify uri) (-> (or meth :get) name str/upper-case keyword))
        session {}]
    (if-let [{op :op  params :params :as _route} (meth/resolve-route ztx api-config path initial-ctx)]
      (zen/op-call ztx op (assoc req :route-params params) session)
      {:status 404})))

;; TODO format response, enable cors, parse body, etc
(defn handle-request [ztx api-symbol request]
  (let [parsed-request
        (-> request
            (update :uri codec/url-decode))]
    (dispatch ztx api-symbol parsed-request)))

(defmethod zen/start 'zen.http/httpkit
  [ztx config]
  (let [web-config
        (merge {:worker-name-prefix "w"
                :thread 8
                :max-body 20971520}
               config)
        req-fn
        (fn [request] (handle-request ztx (:api config) request))]
    {:server (http-kit/run-server req-fn web-config)}))

(defmethod zen/stop 'zen.http/httpkit
  [ztx config state]
  (when-let [srv (:server state)]
    (srv)))

(defmethod zen/op 'zen.http/response-op
  [ztx config req & opts]
  (:response config))
