(ns zen.http.formats
  (:require [zen.http.methods :as meth]))


(defmethod meth/middleware-in
  'zen.http/formats-middleware
  [ztx cfg request]
  request)

(defmethod meth/middleware-out
  'zen.http/formats-middleware
  [ztx cfg request response]
  response)
