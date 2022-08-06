(ns zen.web.methods
  (:require [zen.core :as zen]))

(defmulti resolve-route
  (fn [_ztx cfg _path {_params :params _mws :middlewares _pth :path}]
    (zen/engine-or-name cfg)))

(defmethod resolve-route
  :default
  [ztx cfg _path ctx]
  (zen/error ztx 'zen.web/no-resolve-route-method {:method (zen/engine-or-name cfg) :path (:path ctx)})
  nil)

(defmulti routes
  "collect routes"
  (fn [_ztx cfg _ctx] (zen/engine-or-name cfg)))


(defmethod routes
  :default
  [ztx cfg ctx]
  [(-> (assoc ctx :op 'unknown :error (str "method zen.web.methods/routes is not implemented for " (:zen/name cfg)))
       (update :path conj :?))])
