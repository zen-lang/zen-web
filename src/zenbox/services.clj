(ns zenbox.services)

(defmulti start (fn [ctx inst] (println "dispatch" (:service inst)) (:service inst)))

(defmethod start :default
  [ctx inst]
  (println "ERROR: no impl for start " (:service inst)))

(defmulti stop  (fn [ctx inst] (:service inst)))
(defmethod stop :default
  [ctx inst]
  (println "ERROR: no impl for stop " (:service inst)))
