(ns zenbox.storage.atom
  (:require [zen.core :as zen]))

(defmulti handle (fn [ctx rpc storage params] (:operation rpc)))

(defmethod handle
  'zenbox/insert
  [ctx rpc storage params]
  (let  [{:keys [errors]} (zen/validate ctx (:schemas storage) params)
        path (into [:zen/atom-storage] (:path storage))]
    (if (empty? errors)
      (do (swap! ctx assoc-in (into path [(:resourceType params) (:id params)]) params)
          {:result params})
      {:error errors})))

(defn read [ctx storage params]
  (let [path (into [:zen/atom-storage] (:path storage))]
    (get-in @ctx (into path [(:resourceType params) (:id params)]))))

(defmethod handle
  'zenbox/delete
  [ctx rpc storage params]
  (let [path (into [:zen/atom-storage] (:path storage))]
    (if (nil? (read ctx storage params))
      {:error [{:message "resource doesn't exists"}]}
      (do(swap! ctx update-in (into path [(:resourceType params)]) dissoc (:id params))
         {:result params}))))

(defmethod handle
  'zenbox/read
  [ctx rpc storage params]
  (let [resource (read ctx storage params)]
    (if (nil? resource)
      {:error [{:message "resource doesn't exists"}]}
      {:result resource })))
