(ns zenbox.web.router
  (:require [clojure.string :as str]
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

;; {"users" {:GET {:op :x}}
;;  :GET {:op :x}
;;  [:param] {:GET {:op :x}}}

(defn -match [ctx acc node [x & rpth :as pth] params parents wgt]
  (if (nil? node)
    acc
    (if (empty? pth)
      (conj acc {:parents parents :match node :w wgt :params params})
      (let [pnode (and (map? node) (assoc node :params params))
            acc (if-let [apis (:apis node)]
                  (->> apis
                       (reduce (fn [acc api-sym]
                                 (if-let [api (zen/get-symbol ctx api-sym)]
                                   (-match ctx acc api pth params parents wgt)
                                   acc))
                               acc))
                  acc)
            acc (if-let [branch (get node x)]
                  (-match ctx acc branch rpth params (conj parents pnode) (+ wgt 10))
                  acc)]
        (if (keyword? x)
          acc
          (->> (get-params node)
               (reduce (fn [acc [[k] branch]]
                         (-match ctx acc branch rpth (assoc params k x) (conj parents pnode) (+ wgt 2)))
                       acc)))))))

(defn match
  "path [:get \"/your/path\"] or just \"/your/path\""
  [ctx meth uri routes]
  (let [path (conj (pathify uri) (-> meth name str/upper-case keyword))
        result (-match ctx  [] routes path {} [] 0)]
    (->> result (sort-by :w) last)))

(defn route [ctx server request]
  (match ctx (:request-method request) (:uri request) (select-keys server [:apis])))

(defn apis-paths [ctx api path acc]
  (let [acc (if-let [apis (api :apis)]
              (->> apis
                   (mapv (fn [srv-nm] (zen/get-symbol ctx srv-nm)))
                   (reduce (fn [acc next-api]
                             (apis-paths ctx next-api path acc)
                             ) acc))
              acc)]
    (->> api
         (reduce (fn [acc [k v]]
                   (cond
                     (contains? #{:GET :POST :PUT :PATCH :OPTION :DELETE} k)
                     (conj acc (merge v {:path path
                                         :method k
                                         :uri (str "/" (str/join "/" path))}))

                     (string? k)
                     (apis-paths ctx v (conj path k) acc)

                     (and (vector? k) (keyword? (first k)))
                     (apis-paths ctx v (conj path
                                             (first k)
                                             ;; (str "{" (subs (str (first k)) 1) "}")
                                             ) acc)

                     :else acc))
                 acc))))

(defn get-all-paths [ctx]
  (->> (zen/get-tag ctx 'zenbox/server)
       (mapv (fn [srv-nm] (zen/get-symbol ctx srv-nm)))
       (reduce (fn [acc srv]
                 (apis-paths ctx (select-keys srv [:apis]) [] acc))
               [])
       (sort-by (fn [x] [(:uri x) (:method x)]))))

(defn get-api-paths [ctx api]
  (->> (apis-paths ctx api [] [])
       (sort-by (fn [x] [(:uri x) (:method x)]))))
