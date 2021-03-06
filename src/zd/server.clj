(ns zd.server
  (:require
   [zd.parser]
   [zd.pages]
   [clojure.java.io :as io]
   [clojure.pprint]
   [clojure.string :as str]
   [clojure.walk]
   [edamame.core]
   [zen.core :as zen]
   [zenbox.pg.core]
   [zenbox.rpc :refer [rpc-call]]
   [zenbox.web.core :as web]
   [zenbox.web.router :refer [get-all-paths]]))

(defmulti operation (fn [ctx op req] (:zen/name op)))

(defn rpc [ctx req]
  (if-let [op (zen/get-symbol ctx (:method req))]
    (if-let [schemas (:params op)]
      (let  [{:keys [errors]} (zen/validate ctx schemas (:params req))]
        (if (empty? errors)
          (rpc-call ctx op req)
          {:error errors}))
      (rpc-call ctx op req))
    {:error {:message (str "No operation defined for " (:method req))}}))


(defmethod operation 'zenbox/json-rpc
  [ctx op req]
  (try
    (let [resource (:resource req)
          resp (rpc ctx resource)]
      (if (:result resp)
        {:status 200 :body resp}
        {:status 422 :body resp}))
    (catch Exception e
      (println "ERROR:" e)
      {:status 500 :body {:error (str e)}}))
  )

(defmethod operation 'zd/render-symbol
  [ztx op {{sym :symbol} :route-params :as req}]
  (if-let [doc (zd.parser/load-doc ztx (symbol sym))]
    {:status 200
     :body  (zd.pages/render-page ztx doc)}
    {:status 404
     :body  (str "No page for " sym)}))

(defmethod operation 'zd/render-index
  [ctx op req]
  {:status 200 :body "Index"})


(defmethod rpc-call 'zen-ui/get-symbol
  [ctx rpc {{nm :name} :params}])

(defn dispatch-op [ctx route request]
  (if route
    (if-let [op (zen/get-symbol ctx (get-in route [:match :operation]))]
      (operation ctx op (assoc request :route-params (:params route)))
      {:status 404 :body "No operation definition"})
    {:status 404 :body "No route"}))

(defn start [ctx]
  (web/start ctx #'dispatch-op))

(defn stop [ctx]
  (web/stop ctx))

(comment
  (def ztx (zen/new-context {:zd/path "zd"}))


  (zen/read-ns ztx 'zd)

  (:errors @ztx)

  (start ztx)

  (stop ztx)


  )
