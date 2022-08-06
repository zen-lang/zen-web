(ns zen.web.routemap-test
  (:require [zen.core :as zen]
            [zen.web.core :as web]
            [zen.web.routemap :as routemap]
            [clojure.test :as t]
            [matcho.core :as matcho]))


(t/deftest zen-web-routemap-test

  (def ztx (zen/new-context {}))

  (zen/load-ns
   ztx
  '{ns myweb
    import #{zen.web}

    index-op
    {:zen/tags #{zen/op zen.web/op}
     :engine bks.http/static
     :status 200
     :body {:message "Hello!"}}

    well-known-op
    {:zen/tags #{bks/op bks.http/op}
     :engine bks.http/static
     :status 200
     :body {:message "WK"}}

    get-pt-op
    {:zen/tags #{bks/op bks.http/op}
     :engine bks.http/static
     :status 200
     :body {:message "PT"}}

    get-users-op
    {:zen/tags #{bks/op bks.http/op}
     :engine bks.http/static
     :status 200
     :body {:message "Users"}}

    admin-api
    {:zen/tags #{bks.http/api}
     :middlewares []
     "users" {:GET get-users-op}}

    route
    {:zen/tags #{bks.http/api}
     :middlewares []
     :GET index-op
     ".well-known" {:GET well-known-op}
     "Patient" {[:id] {:GET get-pt-op}}
     "admin" {:apis [admin-api]}}})


  ;; (t/is (nil? (web/resolve-route ztx 'myweb/route {:uri "/ups"})))


  ;; (matcho/match
  ;;  (web/resolve-route ztx 'myweb/route {:uri "/"})
  ;;  {:op 'http.routing-test/index-op})

  ;; (matcho/match
  ;;  (sut/resolve-route ztx 'http.routing-test/route {:uri "/.well-known"})
  ;;  {:op 'http.routing-test/well-known-op})

  ;; (matcho/match
  ;;  (sut/resolve-route ztx 'http.routing-test/route {:uri "/Patient/pt1"})
  ;;  {:op 'http.routing-test/get-pt-op})

  ;; (matcho/match
  ;;  (sut/resolve-route ztx 'http.routing-test/route {:uri "/admin/users"})
  ;;  {:op 'http.routing-test/get-users-op})


  )
