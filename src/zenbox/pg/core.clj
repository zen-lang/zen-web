(ns zenbox.pg.core
  (:require
   [zen.core :as zen]
   [zenbox.services :as srv]
   [zenbox.pg.pool :as pool]
   [zenbox.pg.coerce]
   [ring.util.codec]
   [clojure.walk]
   [clojure.string :as str]
   [dsql.pg]
   [clojure.java.jdbc :as jdbc])
  (:import org.postgresql.util.PSQLException
           java.sql.BatchUpdateException))

(defmacro pr-error [& body]
  `(try
     ~@body
     (catch java.sql.BatchUpdateException e#
       (if-let [ne# (.getNextException e#)] ;; rethrow exception containing SQL error
         (let [msg# (.getMessage ne#)]
           (throw (java.sql.SQLException. msg#)))
         (do
           (throw e#))))
     (catch org.postgresql.util.PSQLException e#
       (if-let [ne# (.getNextException e#)] ;; rethrow exception containing SQL error
         (let [msg# (.getMessage ne#)]
           (throw (java.sql.SQLException. msg#)))
         (do
           (throw e#))))))

(defn env [v]
  (-> v (name)
      (str/upper-case)
      (str/replace #"-" "_")
      (System/getenv)))

(defn query [ds q]
  (pr-error
   (if (map? q)
     (let [sql (dsql.pg/format q)]
       (println "SQL:" sql)
       (jdbc/query ds sql))
     (jdbc/query ds q))))

(defn database-url [spec]
  (let [conn spec]
    (str "jdbc:postgresql://" (:host conn) ":" (or (:port! conn) (:port conn))
         "/" (:database conn)
         "?user=" (:user conn)
         "&password=" (:password conn) "&stringtype=unspecified"
         (when-let [params (:params spec)]
           (str "&" (->> params (mapv (fn [[k v]] (str (name k) "=" v)))
                 (str/join "&")))))))

(defn datasource [spec]
  (let [ds-opts   (let [database-url (database-url spec)]
                    (merge {:connection-timeout  30000
                            :idle-timeout        10000
                            :minimum-idle        0
                            :maximum-pool-size   30
                            :connection-init-sql "select 1"
                            :data-source.url     database-url}
                           (select-keys spec [:connection-timeout :idle-timeout :minimum-idle :maximum-pool-size :connection-init-sql])))
        ds (pool/create-pool ds-opts)]
    {:datasource ds}))

(defn retry-datasource [db-spec max-retry & [timeout]]
  (loop [retry-num max-retry]
    (let [res (try (let [datasource (datasource db-spec)] (query datasource "SELECT 1") datasource)
                   (catch Exception e
                     (println "Error while connecting to " (dissoc db-spec :password) " - " (.getMessage e))))]
      (cond
        res res

        (> 0 retry-num)
        (let [msg (str "Unable to connect to " (dissoc db-spec :password))]
          (println msg)
          (throw (Exception. msg)))

        :else (do
                (println "Retry connection to " (dissoc db-spec :password))
                (Thread/sleep (or timeout 2000))
                  (recur (dec retry-num)))))))

(defn shutdown [{conn :datasource}]
  (pool/close-pool conn))

(defn connection
  "open root connection"
  [db-spec]
  {:connection (jdbc/get-connection {:connection-uri (database-url db-spec)})})

(defn retry-connection
  "open root connection"
  [db-spec & [max-retry timeout]]
  (let [max-retry (or max-retry 20)]
    (loop [retry-num max-retry]
      (let [res (try (let [conn (connection db-spec)] (query conn "SELECT 1") conn)
                     (catch Exception e
                       (println (str "Error while connecting to " (dissoc db-spec :password) " - " (.getMessage e)))))]
        (cond
          res res

          (> 0 retry-num)
          (let [msg (str "Unable to connect to " (dissoc db-spec :password))]
            (println msg)
            (throw (Exception. msg)))

          :else (do
                  (println "Retry connection to " (dissoc db-spec :password))
                  (Thread/sleep (or timeout 2000))
                  (recur (dec retry-num))))))))



(defn close-connection [conn]
  (.close (:connection conn)))

(defn with-connection
  [db-spec f]
  (with-open [c (:connection (connection db-spec))]
    (f {:connection c})))

(defn with-retry-connection
  [db-spec f & [max-retry timeout]]
  (with-open [c (:connection (retry-connection db-spec max-retry timeout))]
    (f {:connection c})))

(defn user-exists? [db user]
  (let [user (if (map? user) (:user user) user)]
    (->> (first (query db ["select true from pg_catalog.pg_roles where rolename = ?" user]))
         (some?))))


(defn exec! [db sql]
  (jdbc/execute! db sql))

(defn create-user [db {user :user password :password}]
  (when-not (user-exists? db user)
    (jdbc/execute! db (format "CREATE USER %s WITH ENCRYPTED PASSWORD '%s'" user password))))

(defn drop-user [db {user :user}]
  (jdbc/execute! db (format "DROP USER IF EXISTS %s" user)))

(defn drop-database [db {dbname :database :as spec}]
  ;; (klog/log :db/drop-db {:db (:database spec)})
  (jdbc/execute! db (format "DROP DATABASE IF EXISTS %s" dbname)))

(defn database-exists? [db dbname]
  #_(pg/database-exists? db dbname))

(defn create-database
  [db {dbname :database user :user}]
  ;; (klog/log :db/create-db {:db  dbname})
  (let [db-sql    (format "CREATE DATABASE %s OWNER %s" dbname user)
        grant-sql (format "GRANT ALL PRIVILEGES ON DATABASE %s TO %s" dbname user)]
    (when-not (database-exists? db dbname)
      (exec! db db-sql)
      (exec! db grant-sql))))

(defn grant-privileges
  [db {dbname :database user :user}]
  (let [grant-sql (format "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public to %s" user)
        grant-fn-sql (format "GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA public to %s" user)]
    (when (database-exists? db dbname)
      (exec! db grant-sql)
      (exec! db grant-fn-sql))))

(defn ensure-database [conn db-name]
  (when-not (database-exists? conn db-name)
    #_(pg/create-database conn db-name)))

(defn archive-db [db archive-name db-spec]
  ;; TODO: close jdbc connections
  ;; close other connections
  ;; SELECT pg_terminate_backend(pg_stat_activity.procpid)
  ;; FROM pg_stat_get_activity(NULL::integer)
  ;; WHERE datid=(SELECT oid from pg_database where datname = 'your_database');
  ;; remove from cache
  (jdbc/execute! db (format "ALTER DATABASE %s RENAME TO %s " (:dbname db-spec) archive-name)))

(defn archive-user [db archive-name db-spec]
  (jdbc/execute! db (format "ALTER USER %s RENAME TO %s " (:user db-spec) archive-name)))

(defmethod srv/start 'zenbox/pg
  [ctx inst]
  (println "Starting pg" inst)
  (let [ds (datasource inst)]
    (swap! ctx assoc-in [:services (:zen/name inst)] ds)))

(defmethod srv/stop 'zenbox/pg
  [ctx inst]
  (when-let [conn (get-in @ctx [:services (:zen/name inst)])]
    (println "Stop pg" inst conn)
    (shutdown conn)
    (swap! ctx assoc-in [:services (:zen/name inst)] nil)))

(comment
  (def ctx (zen/new-context))

  (zen/read-ns ctx 'demo)

  (def pg (zen/get-symbol ctx 'demo/db))


  (srv/start ctx pg)
  (srv/stop ctx pg)





  )
