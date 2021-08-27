(ns zenbox.web.core
  (:require
   [clojure.string :as str]
   [org.httpkit.server :as http-kit]
   [ring.util.codec :as codec]
   [zenbox.web.formats]
   [ring.middleware.cookies :as cookies]
   [ring.util.response]
   [ring.util.request]
   [ring.middleware.head]
   [clj-yaml.core]
   [clojure.walk]
   [zen.core :as zen]
   [ring.middleware.content-type]
   [zenbox.web.router]
   [zenbox.rpc]
   [zenbox.services :as srv])
  (:use [ring.middleware.resource]
        [ring.middleware.file]
        [ring.middleware.not-modified]))

(defn form-decode [s] (clojure.walk/keywordize-keys (ring.util.codec/form-decode s)))

(defn prepare-request [{meth :request-method qs :query-string body :body ct :content-type headers :headers :as req}]
  (let [params (when qs (form-decode qs))
        params (if (string? params) {(keyword params) nil} params)
        method-override (and (= :post meth) (get headers "x-http-method-override"))
        body (zenbox.web.formats/parse-body req)]
    (cond-> req
      body (merge body)
      method-override (assoc :request-method (keyword (str/lower-case method-override)))
      params (update :params merge (or params {})))))


(defn preflight
  [{meth :request-method hs :headers :as req}]
  (let [headers (get hs "access-control-request-headers")
        origin (get hs "origin")
        meth  (get hs "access-control-request-method")]
    {:status 200
     :headers {"Access-Control-Allow-Headers" headers
               "Access-Control-Allow-Methods" meth
               "Access-Control-Allow-Origin" origin
               "Access-Control-Allow-Credentials" "true"
               "Access-Control-Expose-Headers" "Location, Transaction-Meta, Content-Location, Category, Content-Type, X-total-count"}}))

(defn allow [resp req]
  (if-let [origin (get-in req [:headers "origin"])]
    (update resp :headers merge
            {"Access-Control-Allow-Origin" origin
             "Access-Control-Allow-Credentials" "true"
             "Access-Control-Expose-Headers" "Location, Content-Location, Category, Content-Type, X-total-count"})
    resp))

(defn healthcheck [h]
  (fn [req]
    (if  (and (= :get (:request-method req))
              (= "/__healthcheck" (:uri req)))
      {:status 200 :body "healthy" :headers {"content-type" "text/htlm"}}
      (h req))))

(defn mk-handler [dispatch]
  (fn [req]
    (if (= :options (:request-method req))
      (preflight req)
      (let [req (prepare-request req)
            resp (dispatch req)]
        (-> resp
            (zenbox.web.formats/format-response req)
            (allow req))))))

(defn handle-static [h {meth :request-method uri :uri :as req}]
  (if (and (#{:get :head} meth)
           (or (str/starts-with? (or uri "") "/static/")
               (str/starts-with? (or uri "") "/favicon.ico")))
    (let [opts {:root "public"
                :index-files? true
                :allow-symlinks? true}
          path (subs (codec/url-decode (:uri req)) 8)]
      (-> (ring.util.response/resource-response path opts)
          (ring.middleware.head/head-response req)))
    (h req)))

(defn wrap-static [h]
  (fn [req]
    (handle-static h req)))

(defmulti operation (fn [ztx op req] (:zen/name op)))

(defn rpc [ztx req]
  (if-let [op (zen/get-symbol ztx (:method req))]
    (if-let [schemas (:params op)]
      (let  [{:keys [errors]} (zen/validate ztx schemas (:params req))]
        (if (empty? errors)
          (zenbox.rpc/rpc ztx op req)
          {:error errors}))
      (zenbox.rpc/rpc ztx op req))
    {:error {:message (str "No operation defined for " (:method req))}}))

(defmethod operation
  'zenbox/json-rpc
  [ztx op req]
  (try
    (let [resource (:resource req)
          resp     (rpc ztx resource)]
      (if (:result resp)
        {:status 200 :body resp}
        {:status 422 :body resp}))
    (catch Exception e
      (println "ERROR:" e)
      {:status 500 :body {:error (str e)}})))

(defn dispatch-op [ztx route request]
  (if route
    (if-let [op (zen/get-symbol ztx (get-in route [:match :operation]))]
      (operation ztx op (assoc request :route-params (:params route)))
      {:status 404 :body "No operation definition"})
    {:status 404 :body "No route"}))

(defn start
  [ctx]
  (->> (zen/get-tag ctx 'zenbox/server)
       (map (fn [sym] (zen/get-symbol ctx sym)))
       (mapv (fn [srv-def]
               (println "srv" srv-def)
               (doseq [srv-nm (:services srv-def)]
                 (when-let [srv (zen/get-symbol ctx srv-nm)]
                   (println "start service " srv)
                   (srv/start ctx srv)))
               (let [dispatch (fn [req] (dispatch-op ctx (zenbox.web.router/route ctx srv-def req) req))
                     handler (-> (mk-handler dispatch)
                                 (wrap-static))
                     ;; todo add more http-kit configs
                     srv (http-kit/run-server handler srv-def)]
                 (swap! ctx assoc-in [:zenbox/servers (:zen/name srv-def)] {:server srv :handler handler :def srv-def})))))
  ::started)


(defn stop [ctx]
  (doseq [[nm inst] (:zenbox/servers @ctx)]
    (when-let [srv (:server inst)]
      (srv)
      (swap! ctx update :zenbox/servers dissoc nm)
      (for [srv-nm (get-in inst [:def :services])]
        (when-let [srv (zen/get-symbol ctx srv-nm)]
          (println "stop service " srv)
          (srv/stop ctx srv))))))
