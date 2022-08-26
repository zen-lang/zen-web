(ns zen.http.core-test
  (:require
   [zen.core :as zen]
   [zen.http.core :as web]
   [clojure.test :as t]))

(t/deftest test-zen.http

  (def ztx (zen/new-context {}))

  (zen/load-ns
   ztx
   '{ns myweb
     import #{zen.http}

     index-op
     {:zen/tags #{zen/op zen.http/op}
      :engine zen.http/response-op
      :response {:status 200
                 :body "Hello"}}

     admin-index-op
     {:zen/tags #{zen/op zen.http/op}
      :engine zen.http/response-op
      :response {:status 200
                 :body "Hello"}}

     admin-api
     {:zen/tags #{zen.http/api}
      :engine zen.http/routemap
      ;; :middleware [basic-auth]
      :GET admin-index-op}

     api
     {:zen/tags #{zen.http/api}
      :engine zen.http/routemap
      :apis [admin-api]
      :GET index-op
      :POST zen.http/rpc
      "admin" {:apis [admin-api]}}

     http
     {:zen/tags #{zen/start zen.http/http}
      :engine zen.http/httpkit
      :port 8080
      :api api
      ;; :formats #{zen.http/json zen.http/yaml zen.http/html}
      }

     system
     {:zen/tags #{zen/system}
      :start [http]}

     })

  (t/is (empty? (zen/errors ztx)))

  (zen/get-symbol ztx 'zen.http/http)
  (zen/get-symbol ztx 'zen/system)

  (zen/errors ztx)

  (web/resolve-route ztx 'myweb/api {:path [:GET]})

  (web/resolve-route ztx 'myweb/api {:path ["admin" :GET]})

  (web/dispatch ztx 'myweb/api {:uri "/"})

  ;; (sys/send ztx 'example/web 'web/dispatch {:uri "/Patient"})
  ;; (sys/send ztx 'example/web 'web/rpc {:method 'example/pt-search :params {}})


  )
