(ns zen.http.core
  (:require [zen.core :as zen]
            [ring.util.codec :as codec]
            [clojure.walk :as walk]
            [org.httpkit.server :as http-kit]
            [zen.http.methods :as meth]
            [clojure.string :as str]
            [zen.http.httpkit]
            [zen.http.routemap :as rm]
            [ring.util.parsing :refer [re-token]])
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
                    (if-let [resp (and (map? patch) (:zen.http/response patch))]
                      ;; TODO add deep-merge
                      resp
                      (recur (rest mws)
                             (if (map? patch)
                               (merge req patch)
                               req))))
                  (recur (rest mws) req))))
            resp
            (if (:status req*)
              req*
              (zen/op-call ztx op req* session))]
        (reduce (fn [resp* mw-symbol]
                  (if-let [mw-cfg (zen/get-symbol ztx mw-symbol)]
                    ;; TODO add deep merge
                    (merge resp* (meth/middleware-out ztx mw-cfg req* resp*))
                    req*))
                resp
                mw))
      {:status 404 :body "route not found"})))

;; TODO format response, parse body
(defn handle [ztx api-symbol {:keys [request-method headers] :as request}]
  (let [parsed-request
        (-> request
            (update :uri codec/url-decode))]
    ;; maybe try to move this to cors middleware?
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

(defmethod zen/op 'zen.http/response-op
  [ztx config req & opts]
  (cond-> (:response config)
    (keyword? (:select config))
    (assoc :body (str (get req (:select config))))))

(defn byte-transform [direction-fn string]
  (try
    (str/join (map char (direction-fn (.getBytes ^String string))))
    (catch Exception _)))

(defn decode-base64 [string]
  (byte-transform #(.decode (Base64/getDecoder) ^bytes %) string))

(defn basic-error [{:keys [request-method]}]
  {:zen.http/response
   (cond-> {:status 401 :headers {"Content-Type" "text/plain"}}
     (not= request-method :head) (assoc :body "access denied"))})

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
  )

(defmethod meth/middleware-in 'zen.http/cors
  [ztx cfg {:keys [request-method headers] :as req}]
  ;; TODO discuss options http meth routing
  )

(defmethod meth/middleware-out 'zen.http/cors
  [ztx cfg req resp]
  (when-let [origin (get-in req [:headers "origin"])]
    (update resp :headers merge
            {"Access-Control-Allow-Origin" origin
             "Access-Control-Allow-Credentials" "true"
             "Access-Control-Expose-Headers" "Location, Content-Location, Category, Content-Type, X-total-count"})))

(defmethod meth/middleware-in 'zen.http/parse-params
  [ztx cfg {qs :query-string}]
  ;; TODO implement form-params parsing, other encodings
  (when qs
    (let [parsed (walk/keywordize-keys (ring.util.codec/form-decode qs))
          params (if (string? parsed)
                   {(keyword parsed) nil}
                   parsed)]
      {:params params
       :query-params params})))

(defmethod meth/middleware-out 'zen.http/parse-params
  [ztx cfg req resp]
  )

(def re-cookie-octet #"[!#$%&'()*+\-./0-9:<=>?@A-Z\[\]\^_`a-z\{\|\}~]")

(def re-cookie-value (re-pattern (str "\"" re-cookie-octet "*\"|" re-cookie-octet "*")))

(def re-cookie (re-pattern (str "\\s*(" re-token ")=(" re-cookie-value ")\\s*[;,]?")))

(defmethod meth/middleware-in 'zen.http/cookies
  [ztx cfg {:keys [headers] :as req}]
  (when-let [cookie (get headers "cookie")]
    {:cookies
     (->> (for [[_ name value] (re-seq re-cookie cookie)]
            [name value])
          (map (fn [[name value]]
                 (when-let [value (codec/form-decode-str
                                   (str/replace value #"^\"|\"$" ""))]
                   [name {:value value}])))
          (remove nil?)
          (into {}))}))

(defmethod meth/middleware-out 'zen.http/cookies
  [ztx cfg req resp])
