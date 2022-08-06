(ns zen.web.methods
  (:require [zen.core :as zen]))

(defmulti resolve-route
  (fn [ztx cfg {path :path}] (zen/engine-or-name cfg)))
