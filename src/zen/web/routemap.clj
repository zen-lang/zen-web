(ns zen.web.routemap
  (:require [clojure.string :as str]
            [zen.web.methods :as meth]
            [zen.core :as zen]))

(defn pathify [path]
  (filterv #(not (str/blank? %)) (str/split path #"/")))

(defn is-glob? [k] (str/ends-with? (name k) "*"))

(defn- get-params [node]
  (when (map? node)
    (filter (fn [[k v]] (vector? k)) node)))

(defn- get-param [node]
  (first (filter (fn [[k v]] (vector? k)) node)))

(defn fn-param? [k]
  (and (vector? k)
       (let [f (first k)]
         (and (not (keyword? f)) (fn? f) ))))

(defn match-fn-params [node x]
  (when (map? node)
    (->> node
         (filter (fn [[k v]] (fn-param? k)))
         (reduce (fn  [acc [k v]]
                   (if-let [params ((first k) x)]
                     (conj acc [params v])
                     acc))
                 [])
         first)))

(defn regexp?
  [x]
  (instance? java.util.regex.Pattern x))

(defn match [ztx
             {node-mws :middlewares :as node}
             [x & rpath :as path]
             {params :params mws :middlewares :as ctx}]
  (if (empty? path)
    (when (symbol? node) ;; match!
      (assoc ctx :op node))
    ;; try exact branch
    (let [ctx (cond-> ctx
                node-mws (update :middlewares (fn [x] (into (or x []) node-mws))))]
      (if-let [res (when-let [next-node (get node x)]
                     (match ztx next-node rpath (-> ctx
                                                    (update :path (fn [p] (conj (or p []) x)))
                                                    (update :resolution-path (fn [p] (conj (or p []) x))))))]
        res
        ;; try params branches [:param]
        (if-let [res (->> (get-params node)
                          (map (fn [[[k] next-node]]
                                 (match ztx next-node rpath (->
                                                             ctx
                                                             (assoc-in [:params k] x)
                                                             (update :path (fn [p] (conj (or p []) k)))
                                                             (update :resolution-path (fn [p] (conj (or p []) [k])))))))
                          (filter identity)
                          (first))]
          res
          (when-let [apis (:apis node)] ;; try apis
            (->> apis
                 (map (fn [api-name]
                        (if-let [api (zen/get-symbol ztx api-name)]
                          (meth/resolve-route ztx api path ctx)
                          (do
                            (zen/error ztx 'zen.web/api-not-found {:api api-name})
                            nil))))
                 (filter identity)
                 (first))))))))

(defmethod meth/resolve-route
  'zen.web/routemap
  [ztx cfg path ctx]
  (match ztx cfg path (-> ctx
                          (update :resolution-path (fn [p] (conj (or p []) (:zen/name cfg)))))))


(defn routes [ztx cfg ctx]
  (let [ctx (cond-> ctx (:middlewares cfg)
                    (update :middlewares into (:middlewares cfg)))]
    (->> cfg
         (reduce (fn [acc [k v]]
                   (cond
                     (contains? #{:GET :POST :PUT :DELETE :OPTION :PATCH} k)
                     (conj acc (-> ctx
                                   (update :path conj k)
                                   (update :by conj k)
                                   (assoc :op v)))

                     (string? k)
                     (into acc (routes ztx v (-> (update ctx :path conj k)
                                                 (update :by conj k))))
                     (vector? k)
                     (into acc (routes ztx v (-> ctx
                                                 (update :path conj (first k))
                                                 (update :params conj (first k))
                                                 (update :by conj k))))
                     (= :apis k)
                     (->> v
                          (reduce (fn [acc api-name]
                                    (if-let [api (zen/get-symbol ztx api-name)]
                                      (into acc (meth/routes ztx api ctx))
                                      acc))
                                  acc))
                     :else acc))
                 []))))

(defmethod meth/routes
  'zen.web/routemap
  [ztx cfg ctx]
  (routes ztx cfg (update ctx :by conj (:zen/name cfg))))