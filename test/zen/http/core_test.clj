(ns zen.http.core-test
  (:require
   [zen.core :as zen]
   [zen.http.core :as web]
   [clojure.test :as t]))

(t/deftest test-zen.http

  #_(zen/stop-system ztx)

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
                 :body "Hello, admin"}}

     basic-auth
     {:zen/tags #{zen.http/middleware}
      :engine zen.http/basic-auth
      :user "john"
      :password "123"}

     admin-api
     {:zen/tags #{zen.http/api}
      :engine zen.http/routemap
      :mw [basic-auth]
      :GET admin-index-op}

     api
     {:zen/tags #{zen.http/api}
      :engine zen.http/routemap
      :GET index-op
      :POST zen.http/rpc
      "admin" {:apis [admin-api]}}

     http
     {:zen/tags #{zen/start zen.http/http}
      :engine zen.http/httpkit
      :port 8080
      :api api
      :formats #{zen.http/json zen.http/yaml zen.http/html}}

     system
     {:zen/tags #{zen/system}
      :start [http]}})

  (t/is (empty? (zen/errors ztx)))

  (zen/get-symbol ztx 'zen.http/http)
  (zen/get-symbol ztx 'zen/system)

  (zen/start-system ztx 'myweb/system)

  (web/routes ztx 'myweb/api)

  (t/is (= {:status 200, :body "Hello"}
           (web/dispatch ztx 'myweb/api {:uri "/" :request-method :get})))

  (t/is (= {:status 401 :body "access denied"}
           (web/dispatch ztx 'myweb/api {:uri "/admin" :request-method :get})))

  (t/is (= {:status 200, :body "Hello, admin"}
           (web/dispatch ztx 'myweb/api {:uri "/admin"
                                         :request-method :get
                                         :headers {"authorization" "Basic am9objoxMjM="}})))

  (zen/stop-system ztx)

  ;; (sys/send ztx 'example/web 'web/dispatch {:uri "/Patient"})
  ;; (sys/send ztx 'example/web 'web/rpc {:method 'example/pt-search :params {}})


  )
