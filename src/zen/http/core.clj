(ns zen.http.core
  (:require [zen.core :as zen]
            [ring.util.codec :as codec]
            [org.httpkit.server :as http-kit]
            [zen.http.methods :as meth]
            [clojure.string :as str]
            [zen.http.httpkit]
            [zen.http.routemap :as rm])
  (:import java.util.Base64))

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
    (if-let [{op :op  params :params mw :middlewares :as _route}
             (meth/resolve-route ztx api-config path initial-ctx)]
      (let [req*
            (loop [mws mw
                   req (assoc req :route-params params)]
              (if (empty? mws)
                req
                ;; TODO add error if middleware not found
                (if-let [mw-cfg (zen/get-symbol ztx (first mws))]
                  (let [patch (meth/middleware-in ztx mw-cfg req)]
                    (if-let [resp (:zen.http/response patch)]
                      ;; TODO add headers, deep-merge
                      resp
                      (recur (rest mws) (merge req patch))))
                  (recur (rest mws) req))))
            resp
            (if (:status req*)
              req*
              (zen/op-call ztx op req* session))]
        (reduce (fn [resp* mw-symbol]
                  (if-let [mw-cfg (zen/get-symbol ztx mw-symbol)]
                    (meth/middleware-out ztx mw-cfg req* resp*)
                    req*))
                resp
                mw))
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

(defn byte-transform [direction-fn string]
  (try
    (str/join (map char (direction-fn (.getBytes ^String string))))
    (catch Exception _)))

(defn decode-base64 [string]
  (byte-transform #(.decode (Base64/getDecoder) ^bytes %) string))

(defn basic-error [{:keys [request-method]}]
  {:zen.http/response
   (cond-> {:status 401}
     (not= request-method :head) (assoc :body "access denied"))
   :zen.http/headers {"Content-Type" "text/plain"}})

(defmethod meth/middleware-in 'zen.http/basic-auth
  [ztx {:keys [user password]} req]
  (if-let [auth (get-in req [:headers "authorization"])]
    (let [cred (and auth (decode-base64 (last (re-find #"^Basic (.*)$" auth))))
          [u p] (and cred (str/split (str cred) #":" 2))]
      (if (and (= user u) (= password p))
        {:basic-authentication {:u user :p password}}
        (basic-error req)))
    (basic-error req)))

(defmethod meth/middleware-out 'zen.http/basic-auth
  [ztx cfg req resp]
  resp)
