(ns zenbox.rpc)

(defmulti rpc (fn [ctx rpc req] (or (:operation rpc) (:zen/name rpc))))
