(ns zenbox.storage.postgres
  (:require [zenbox.rpc :refer [rpc-call]]
            [zenbox.pg.core :as pg]
            [zen.core :as zen]))


(defmulti handle (fn [ctx rpc storage params] (:operation rpc)))
(defmethod handle :default
  [ctx rpc storage params]
  {:error {:message (str "pg: " (:operation rpc) " is not impl")}})

(defmulti create-store (fn [ctx store] (:engine store)))

(defn table-ddl [tbl]
  (format "
  CREATE TABLE IF NOT EXISTS \"%s\" (
    id serial primary key,
    ts timestamptz DEFAULT current_timestamp,
    resource jsonb
  ); " tbl))


(defn get-conn [ctx store]
  (if-let [db (get-in @ctx [:services (:db store)])]
    db
    {:error (str "No connection for " (:db store))}))

(defmethod create-store 'zenbox/jsonb-store
  [ctx {tbl :table-name db-nm :db}]
  (if-let [db (get-in @ctx [:services db-nm])]
    {:result (pg/exec! db (table-ddl tbl))}
    {:error (str "No connection to " db-nm)}))

(defmethod rpc-call 'zenbox/sql
  [ctx {db-nm :db} {q :query}]
  (if-let [db (get-in @ctx [:services db-nm])]
    {:result (pg/query db q)}
    {:error {:message (str "No connection to " db-nm)}}))


(defmethod handle 'zenbox/insert
  [ctx rpc store {res :resource}]
  (if-let [errs (and (:schemas store)
                     (let [{errs :errors} (zen/validate ctx (:schemas store) res)]
                       (when-not (empty? errs)
                         errs)))]
    {:error {:errors errs}}
    (let [db (get-conn ctx store)
          query {:ql/type :pg/insert
                 :into (keyword (:table-name store))
                 :value (cond->
                            {:resource [:pg/jsonb (dissoc res :id)]}
                          (:id res) (assoc :id res))
                 :returning :*}
          _ (println "Q" query)
          result (let [res (first (pg/query db query))]
                   (merge (:resource res) (dissoc res :resource)))]
      {:result result})))

(defmethod handle 'zenbox/search
  [ctx rpc store params]
  (let [db (get-conn ctx store)
        query {:ql/type :pg/select
               :select :*
               :from (keyword (:table-name store))}
        result (->>
                (pg/query db query)
                (mapv (fn [res] (merge (:resource res) (dissoc res :resource)))))]
    {:result {:resources result}}))

(defmethod handle 'zenbox/read
  [ctx rpc store {id :id}]
  (let [db (get-conn ctx store)
        query {:ql/type :pg/select
               :select :*
               :from (keyword (:table-name store))
               :where [:= :id id]}
        result (->>
                (pg/query db query)
                (mapv (fn [res] (merge (:resource res) (dissoc res :resource))))
                (first))]
    {:result {:resource result}}))

(defmethod rpc-call 'zenbox/ensure-stores
  [ctx rpc req]
  (let [dbs (:dbs rpc)
        stores (->> (zen/get-tag ctx 'zenbox/store)
                    (mapv (fn [s] (zen/get-symbol ctx s)))
                    (mapv (fn [store]
                            (when (contains? dbs (:db store))
                              (create-store ctx store)))))]
    {:result {:dbs dbs :stores stores}}))

