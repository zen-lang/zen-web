(ns zen.http.utils)

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
