(ns zen.http.core
  (:require [zen.core :as zen]
            [org.httpkit.server :as http-kit]
            [zen.http.methods :as meth]
            [clojure.string :as str]
            [zen.http.httpkit]
            [zen.http.routemap]))

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

(defn dispatch [ztx api request]

  )

;; TODO handle from bks prototype
;; prepares request, formats response, enables cors, etc

(defmethod zen/start 'zen.http/httpkit
  [ztx config]
  (let [web-config
        (merge {:worker-name-prefix "w"
                :thread 8
                :max-body 20971520}
               config)
        req-fn
        ;; wrap in handle?
        (fn [request] (dispatch ztx config request))]
    {:server (http-kit/run-server req-fn web-config)}))

(defmethod zen/stop 'zen.http/httpkit
  [ztx config state]
  (when-let [srv (:server state)]
    (srv)))
