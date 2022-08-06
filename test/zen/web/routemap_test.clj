(ns zen.web.routemap-test
  (:require [zen.core :as zen]
            [zen.web.core :as web]
            [zen.web.methods :as meth]
            [zen.web.routemap :as routemap]
            [clojure.test :as t]
            [matcho.core :as matcho]))


(defmethod meth/resolve-route
  'myweb/custom-api
  [_ztx cfg path ctx]
  (-> (assoc ctx :op 'myweb/custom-op)
      (update :path into path)
      (update :resolution-path into (into [(:zen/name cfg)] path))))

(defmethod meth/routes
  'myweb/custom-api
  [_ztx cfg ctx]
  [(-> (assoc ctx :op 'myweb/custom-op)
       (update :path into [:* :GET])
       (update :by conj (:zen/name cfg)))])

(t/deftest zen-web-routemap-test

  (def ztx (zen/new-context {}))

  (zen/load-ns
   ztx
  '{ns myweb
    import #{zen.web}

    index-op
    {:zen/tags #{zen/op zen.web/op}
     :engine zen.web/response-op
     :status 200
     :body {:message "Hello!"}}

    well-known-op
    {:zen/tags #{zen/op zen.web/op}
     :engine zen.web/response-op
     :status 200
     :body {:message "WK"}}

    get-pt-op
    {:zen/tags #{zen/op zen.web/op}
     :engine zen.web/response-op
     :status 200
     :body {:message "PT"}}

    get-users-op
    {:zen/tags #{zen/op zen.web/op}
     :engine zen.web/response-op
     :status 200
     :body {:message "Users"}}

    get-user-op
    {:zen/tags #{zen/op zen.web/op}
     :engine zen.web/response-op
     :status 200
     :body {:message "User"}}

    admin-api
    {:zen/tags #{zen.web/api}
     :engine zen.web/routemap
     :middlewares [zen.web/debug-middleware]
     "users" {:GET get-users-op
              [:id] {:GET get-user-op}}}

    custom-op
    {:zen/tags #{zen/op zen.web/op}
     :engine zen.web/response-op
     :status 200
     :body {:message "custom"}}

    custom-api
    {:zen/tags #{zen.web/api}
     :zen/desc "just custom api - match all routes with custom-op"}

    tenant-pt-op
    {:zen/tags #{zen/op zen.web/op}
     :engine zen.web/response-op
     :status 200
     :body {:message "tenant-pt"}}

    route
    {:zen/tags #{zen.web/api}
     :engine zen.web/routemap
     :apis [zen.web/rpc-api]
     :middlewares []
     :GET index-op
     ".well-known" {:GET well-known-op}
     "Patient" {[:id] {:GET get-pt-op}}
     "custom" {:apis [custom-api]}
     "admin" {:apis [admin-api]}
     [:tenant] {"patients" {:GET tenant-pt-op}}}})

  (t/is (empty? (zen/errors ztx)))

  (t/is (nil? (web/resolve-route ztx 'myweb/route {:path ["undefined" :GET]})))

  ;; test root path
  (matcho/match
   (web/resolve-route ztx 'myweb/route [:GET])
   {:op 'myweb/index-op
    :resolution-path ['myweb/route :GET],
    :middlewares empty?})

  ;; test simple match
  (matcho/match
   (web/resolve-route ztx 'myweb/route  [".well-known" :GET])
   {:op 'myweb/well-known-op
    :resolution-path ['myweb/route ".well-known" :GET],
    :middlewares empty?})


  ;; test match with params
  (matcho/match
   (web/resolve-route ztx 'myweb/route ["Patient" "pt-1" :GET])
   {:op 'myweb/get-pt-op
    :params {:id "pt-1"}
    :resolution-path ['myweb/route "Patient" [:id] :GET],
    :middlewares empty?})

  ;; test two route engines
  (matcho/match
   (web/resolve-route ztx 'myweb/route  ["custom" "ups" :GET])
   {:op 'myweb/custom-op
    :resolution-path ['myweb/route "custom" 'myweb/custom-api "ups" :GET]})

  ;; test two route maps

  (matcho/match
   (web/resolve-route ztx 'myweb/route  ["admin" "users" :GET])
   {:path ["admin" "users" :GET],
    :middlewares ['zen.web/debug-middleware],
    :resolution-path ['myweb/route "admin" 'myweb/admin-api "users" :GET],
    :op 'myweb/get-users-op})

  (matcho/match
   (web/resolve-route ztx 'myweb/route  ["admin" "users" "u-1" :GET])
   {:path ["admin" "users" :id :GET],
    :params {:id "u-1"},
    :middlewares ['zen.web/debug-middleware],
    :resolution-path ['myweb/route "admin" 'myweb/admin-api "users" [:id] :GET],
    :op 'myweb/get-user-op})

  ;; test root api

  (matcho/match
   (web/resolve-route ztx 'myweb/route  [:POST])
   {:path [:POST],
    :resolution-path ['myweb/route 'zen.web/rpc-api :POST],
    :op 'zen.web/rpc})

  (zen/get-symbol ztx 'myweb/route)
  (zen/engine-or-name (zen/get-symbol ztx 'myweb/route))

  (t/testing "routes"
    (matcho/match
     (web/routes ztx 'myweb/route)
     [{:path [".well-known" :GET],
       :by ['myweb/route ".well-known" :GET],
       :op 'myweb/well-known-op}
      {:path [:GET],
       :by ['myweb/route :GET],
       :op 'myweb/index-op}
      {:path [:POST],
       :by ['myweb/route 'zen.web/rpc-api :POST],
       :op 'zen.web/rpc}
      {:path [:tenant "patients" :GET],
       :params #{:tenant},
       :by ['myweb/route [:tenant] "patients" :GET],
       :op 'myweb/tenant-pt-op}
      {:path ["Patient" :id :GET],
       :params #{:id},
       :by ['myweb/route "Patient" [:id] :GET],
       :op 'myweb/get-pt-op}
      {:path ["admin" "users" :GET],
       :middlewares ['zen.web/debug-middleware],
       :by ['myweb/route "admin" 'myweb/admin-api "users" :GET],
       :op 'myweb/get-users-op}
      {:path ["admin" "users" :id :GET],
       :middlewares ['zen.web/debug-middleware],
       :params #{:id},
       :by ['myweb/route "admin" 'myweb/admin-api "users" [:id] :GET],
       :op 'myweb/get-user-op}
      {:path ["custom" :* :GET],
       :by ['myweb/route "custom" 'myweb/custom-api],
       :op 'myweb/custom-op}]))



  )
