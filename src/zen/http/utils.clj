(ns zen.http.utils
  (:require
   [clojure.string :as str]
   [zen.core :as zen]))

(defn deep-merge
  ;; TODO rewrite to use transients
  [a b]
  (loop [[[k v :as i] & ks] b, acc a]
    (if (nil? i)
      acc
      (let [av (get a k)]
        (if (= v av)
          (recur ks acc)
          (recur ks (if (and (map? v) (map? av))
                      (assoc acc k (deep-merge av v))
                      (assoc acc k v))))))))

(defn content-type [hs]
  (when-let [ct (get hs "content-type")]
    (if-let [i (str/index-of ct ";")]
      (subs ct 0 i)
      ct)))

(defn resolve-mw [ztx sym]
  ;; take config from instance or engine
  (let [mw-cfg (zen/get-symbol ztx sym)]
    (->> (zen/engine-or-name mw-cfg)
         (zen/get-symbol ztx)
         (deep-merge mw-cfg))))

