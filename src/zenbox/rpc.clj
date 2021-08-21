(ns zenbox.rpc)

(defmulti rpc-call (fn [ctx rpc req] (or (:operation rpc) (:zen/name rpc))))
