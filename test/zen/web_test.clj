(ns zen.system-test
  (:require
   [zen.core :as zen]
   [zen.system :as sys]
   [clojure.test :as t]))

(defmethod sys/op
  'lib/info
  [_ztx _zdef _state meth params]
  (println :LOG (or (:ev params) :log) (dissoc params :ev)))

(defmethod sys/comp-start
  'lib/logs
  [_ztx definition _ctx]
  (println :logs/start)
  {:state (atom {})})

(defmethod sys/comp-start
  'lib/metrics
  [ztx zdef {logs :logs}]
  (sys/send ztx logs 'lib/info {:ev :metrics/start})
  {:state (atom {})})

(defmethod sys/comp-start
  'lib/storage
  [ztx zdef {db :db logs :logs}]
  (sys/send ztx logs 'lib/info {:ev :storage/start})
  (doseq [[rt repo-nm] (:repos zdef)]
    (let [repo (zen/get-symbol ztx repo-nm)]
      (sys/send ztx db 'lib/db-create-table {:table (:table repo)})))
  {:state {}})


(defmethod sys/op
  'web/dispatch
  [_ztx {{logs :logs} :deps} _state _meth params]
  {:status 200 :body "TBD"})


(defmethod sys/comp-start
  'lib/http-kit
  [ztx zdef {logs :logs}]
  (sys/send ztx logs 'lib/info {:ev :http/start :port (:port zdef)})
  {:state {:dispatch (fn [_req] {:status 200})}})


(defmethod sys/op
  'lib/db-create-table
  [ztx {{logs :logs} :deps} state meth params]
  (sys/send ztx logs 'lib/info {:ev :db/create-table :table (:table params)})
  (swap! (:state state) assoc (:table params) {:def params :data {}}))

(defmethod sys/comp-start
  'lib/in-memory
  [ztx zdef {logs :logs}]
  (sys/send ztx logs 'lib/info {:msg "Start HTTP"})
  {:state (atom {})})


(defmulti rpc (fn [ztx ctx opdef req] (or (:engine opdef) (:zen/name opdef))))

(defmethod rpc :default
  [ztx ctx opdef req]
  (println :rpc/no-impl (or (:engine opdef) (:method req))))

(defn call-rpc
  [ztx {logs :logs :as ctx} req]
  (sys/send ztx logs 'lib/info {:ev :rpc :method (:method req)})
  (if-let [op (zen/get-symbol ztx (:method req))]
    (rpc ztx ctx op req)
    {:error (str "No " (:method req))}))

(defmethod sys/op
  'web/rpc
  [ztx {{logs :logs :as ctx} :deps :as zdef} state meth params]
  (if-let [op (zen/get-symbol ztx (:method params))]
    (call-rpc ztx ctx params)
    {:error (str "No " (:method params))}))

(defmethod sys/op
  'repo/search
  [ztx zdef state meth params]
  zdef)

(defmethod rpc
  'lib/search-rpc
  [ztx ctx {repo :repo} req]
  (let [data (sys/send ztx repo 'repo/search {})]
    {:result data}))

(t/deftest test-zen-system

  (def ztx (zen/new-context {:paths ["/Users/niquola/zen-system/test"]}))

  (zen/read-ns ztx 'example)
  (zen/errors ztx)

  (zen/get-symbol ztx 'example/system)

  (sys/start ztx 'example/system)

  (sys/send ztx 'example/web 'web/dispatch {:uri "/Patient"})

  (sys/send ztx 'example/web 'web/rpc {:method 'example/pt-search :params {}})
  




  )
