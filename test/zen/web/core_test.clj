(ns zen.web.core-test
  (:require
   [zen.core :as zen]
   [zen.web.core :as web]
   [clojure.test :as t]))

(t/deftest test-zen-web

  (def ztx (zen/new-context {}))

  (zen/load-ns
   ztx
   '{ns myweb
     import #{zen.web}

     index-op
     {:zen/tags #{zen/op zen.web/op}
      :engine zen.web/basic-op
      :response {:status 200
                 :body "Hello"}}

     admin-index-op
     {:zen/tags #{zen/op zen.web/op}
      :engine zen.web/basic-op
      :response {:status 200
                 :body "Hello"}}

     admin-api
     {:zen/tags #{zen.web/api}
      :engine zen.web/route-map
      ;; :middleware [basic-auth]
      :GET admin-index-op}

     api
     {:zen/tags #{zen.web/api}
      :engine zen.web/route-map
      :apis [admin-api]
      :GET index-op
      :POST zen.web/rpc
      "admin" {:apis [admin-api]}}

     http
     {:zen/tags #{zen/start zen.web/http}
      :engine zen.web/httpkit
      :port 8080
      :api api
      ;; :formats #{zen.web/json zen.web/yaml zen.web/html}
      }

     system
     {:zen/tags #{zen/system}
      :start [http]}

     })

  (t/is (empty? (zen/errors ztx)))

  (zen/get-symbol ztx 'zen.web/http)
  (zen/get-symbol ztx 'zen/system)

  (zen/errors ztx)

  (web/resolve-route ztx 'myweb/api {:path [:get]})

  (web/resolve-route ztx 'myweb/api {:path ["admin" :get]})

  (web/dispatch ztx 'myweb/api {:uri "/"})

  ;; (sys/send ztx 'example/web 'web/dispatch {:uri "/Patient"})
  ;; (sys/send ztx 'example/web 'web/rpc {:method 'example/pt-search :params {}})


  )
