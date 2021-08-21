(ns zenbox.pg.coerce
  (:require [cheshire.core :refer [generate-string parse-string]]
            [clojure.java.jdbc :as jdbc]
            [clj-time.coerce :as tc]
            [clj-time.core :as t]
            [clojure.string :as str])
  (:import clojure.lang.IPersistentMap
           clojure.lang.IPersistentVector
           (org.joda.time DateTime)
           java.time.format.DateTimeFormatter
           java.time.ZoneOffset
           [java.sql
            BatchUpdateException
            Date
            Timestamp
            PreparedStatement]
           [org.postgresql.jdbc PgArray]
           org.postgresql.util.PGobject))


(def time-fmt
  (->
   (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss")
   (.withZone (java.time.ZoneOffset/UTC))))


(defn- to-date [sql-time]
  (str (.format time-fmt (.toInstant sql-time)) "." (format "%06d"  (/ (.getNanos sql-time) 1000)) "Z"))

(defn- to-sql-date [clj-time]
  (tc/to-sql-time clj-time))


(extend-type java.util.Date
  jdbc/ISQLParameter
  (set-parameter [v ^PreparedStatement stmt idx]
    (.setTimestamp stmt idx (java.sql.Timestamp. (.getTime v)))))

(defn parse-int-range [s]
  (let [pair (-> (str/replace s #"\[|\]|\(|\)" "")
                 (str/split #","))]
    (mapv read-string pair)))


(extend-protocol jdbc/IResultSetReadColumn
  Date
  (result-set-read-column [v _ _] (.toString v))

  Timestamp
  (result-set-read-column [v _ _]
    (.toString (.toInstant v)))

  PgArray
  (result-set-read-column [v _ _] (vec (.getArray v)))

  PGobject
  (result-set-read-column [pgobj _metadata _index]
    (let [type  (.getType pgobj)
          value (.getValue pgobj)]
      (case type
        "json"      (parse-string value true)
        "jsonb"     (parse-string value true)
        "int8range" (parse-int-range value)
        "citext" (str value)
        value))))

(extend-type java.util.Date
  jdbc/ISQLParameter
  (set-parameter [v ^PreparedStatement stmt idx]
    (.setTimestamp stmt idx (Timestamp. (.getTime v)))))

(defn to-pg-json [value]
  (doto (PGobject.)
    (.setType "jsonb")
    (.setValue (generate-string value))))

(defn to-pg-array
  ([conn value & [sql-type]]
   (.createArrayOf conn (or sql-type "text") (into-array value)))
  ([value]
   (println "Create array without connection")
   (str "{" (clojure.string/join "," (map #(str "\"" % "\"") value)) "}")))


(extend-protocol jdbc/ISQLValue
  clojure.lang.Keyword
  (sql-value [value] (name value))
  org.joda.time.DateTime
  (sql-value [value] (to-sql-date value))
  java.util.Date
  (sql-value [value] (java.sql.Timestamp. (.getTime value)))
  IPersistentMap
  (sql-value [value] (to-pg-json value))
  IPersistentVector
  (sql-value [value] (to-pg-array value)))
