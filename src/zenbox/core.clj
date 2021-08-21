(ns zenbox.core
  (:require
   [clojure.java.io :as io]
   [clojure.pprint]
   [clojure.string :as str]
   [clojure.walk]
   [edamame.core]
   [zen.core :as zen]
   [zenbox.pg.core]
   [zenbox.rpc :refer [rpc-call]]
   [zenbox.storage.core]
   [zenbox.web.core :as web]
   [zenbox.web.router :refer [get-all-paths]]))

(defmulti operation (fn [ctx op req] (:operation op)))

(defn rpc [ctx req]
  (if-let [op (zen/get-symbol ctx (:method req))]
    (if-let [schemas (:params op)]
      (let  [{:keys [errors]} (zen/validate ctx schemas (:params req))]
        (if (empty? errors)
          (rpc-call ctx op req)
          {:error errors}))
      (rpc-call ctx op req))
    {:error {:message (str "No operation defined for " (:method req))}}))


(defmethod operation 'zenbox/json-rpc
  [ctx op req]
  (try
    (let [resource (:resource req)
          resp (rpc ctx resource)]
      (if (:result resp)
        {:status 200 :body resp}
        {:status 422 :body resp}))
    (catch Exception e
      (println "ERROR:" e)
      {:status 500 :body {:error (str e)}}))
  )

(defmethod operation 'zenbox/response
  [ctx op req]
  (:response op))

;; (defmethod operation 'zen-ui/get-tags
;;   [ctx op req]
;;   {:status 200 :body {:tags "123"}})

;; (defmethod operation 'zen-ui/get-symbols
;;   [ctx op req]
;;   {:status 200 :body {:tags "123"}})

;; (defmethod rpc-call 'zen-ui/all-tags
;;   [ctx rpc req]
;;   {:result (:tags @ctx)})

(defmulti view (fn [ctx view model] (:zen/name view)))

(defmethod view 'zen-ui/view-for-schema
  [ctx view model]
  model)

(defmethod view 'zen-ui/view-for-tag
  [ctx view model]
  (let [tag (:zen/name model)]
    (->> (zen/get-tag ctx tag)
         (mapv (fn [x]
                 (let [m (zen/get-symbol ctx x)]
                   {:name x :desc (:zen/desc m) :tags (:zen/tags m)}))))))

(defmethod view 'zen-ui/view-for-valuset
  [ctx view model]
  model)

(defmethod view 'zen-ui/view-for-rpc
  [ctx view model]
  model)

(defmethod view 'zen-ui/view-for-edn
  [ctx view model]
  model)

(defmethod view 'zen-ui/view-for-validate
  [ctx view model]
  model)

(defmethod view 'zen-ui/view-for-validate
  [ctx view model]
  model)

(defmethod view 'zen-ui/view-for-api
  [ctx view model]
  (zenbox.web.router/get-api-paths ctx model))

(defmethod view :default [ctx view model]
  {:status :error
   :message (str "No impl for " (:zen/name view))})

(defn resolve-views [ctx model]
  (let [tags (:zen/tags model)]
    (->> (zen/get-tag ctx 'zen-ui/tag-view)
         (reduce (fn [acc tv]
                   (let [v (zen/get-symbol ctx tv)]
                     (if (or (nil? (:tag v)) (contains? tags (:tag v)))
                       (assoc acc tv {:view v :data (view ctx v model)})
                       acc)
                     )) {}))))

(defmethod rpc-call 'zen-ui/get-symbol
  [ctx rpc {{nm :name} :params}]
  (let [model (zen/get-symbol ctx (symbol nm))
        views (resolve-views ctx  model)]
    {:result {:views views :model model}}))

(defn fix-local-syms [ns model]
  (clojure.walk/postwalk (fn [x]
                           (if (and (symbol? x) (= (str ns) (namespace x)))
                             (symbol (name x))
                             x)) model))

;; (fix-local-syms 'myns {:a 'myns/x :b 'ons/y})

(defn write-ns [f x]
  (println "write" f)
  (with-open [w (clojure.java.io/writer f)]
    (binding [*out* w]
      (clojure.pprint/pprint x))))

(defmethod rpc-call 'zenbox/validate
  [ctx rpc-def {{sch :schema data :data} :params}]
  (let [res (zen/validate ctx #{sch} data)]
    {:result (select-keys res [:errors])}))

(defmethod rpc-call 'zen-ui/update-symbol
  [ctx rpc-def {{model :data nm :name} :params}]
  (let [[nsm jnm] (mapv symbol (str/split (str nm) #"/" 2))
        orig (zen/get-symbol ctx nm)
        ns (get-in @ctx [:ns nsm])
        new-ns (assoc ns jnm (fix-local-syms nsm (dissoc model [:zen/name :zen/file])))]
    (println "Write" nm (fix-local-syms nsm (dissoc model [:zen/name :zen/file])))
    (write-ns (:zen/file orig) new-ns)
    (zen/read-ns ctx nsm)
    (rpc ctx {:method 'zen-ui/get-symbol :params {:name nm}})))

(defmethod rpc-call 'zen-ui/create-symbol
  [ctx rpc-def {{ns-nm :ns nm :name} :params}]
  (let [snm (symbol (name ns-nm) (name nm))
        ns (get-in @ctx [:ns ns-nm])
        new-ns (assoc ns nm {:zen/tags #{} :zen/desc "docs here..."})]
    (write-ns (:zen/file ns) new-ns)
    (zen/read-ns ctx ns-nm)
    {:result {:name snm}}))

(defmethod rpc-call 'zen-ui/navigation
  [ctx rpc req]
  (let [symbols (->>
                 (:symbols @ctx)
                 (reduce (fn [acc [nm m]]
                           (let [[ns snm] (str/split (str nm) #"/" 2)]
                             (assoc-in acc [ns :symbols snm] (assoc (select-keys m [:zen/name :zen/tags :zen/desc])
                                                                    :name snm)))) {}))
        tags (zen/get-tag ctx 'zen/tag)]
    {:result {:symbols symbols :tags tags}}))

(defmethod rpc-call 'zen-ui/errors
  [ctx rpc req]
  {:result {:errors (:errors @ctx)}})

(defmethod rpc-call 'zen-ui/rpc-methods
  [ctx rpc req]
  {:result {:methods (sort (zen/get-tag ctx 'zenbox/rpc))}})

(defmethod rpc-call 'zen-ui/endpoints
  [ctx rpc req]
  {:result {:endpoints (get-all-paths ctx)}})

(defn dispatch-op [ctx route request]
  (if route
    (if-let [op (zen/get-symbol ctx (get-in route [:match :operation]))]
      (operation ctx op request)
      {:status 404})
    {:status 404}))

(defn start [ctx]
  (web/start ctx #'dispatch-op))

(defn stop [ctx]
  (web/stop ctx))

(comment
  (def ctx (zen/new-context))

  (zen/read-ns ctx 'demo)

  (start ctx)

  (stop ctx)

  (:zenbox/servers @ctx)

  (->>
   (zen/get-tag ctx 'zenbox/server)
   (mapv (fn [sym] (zen/get-symbol ctx sym))))

  (resolve-views ctx #{'zen/schema})



  )


