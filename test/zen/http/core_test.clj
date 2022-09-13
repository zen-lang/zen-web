(ns zen.http.core-test
  (:require
   [org.httpkit.client :as client]
   [zen.core :as zen]
   [zen.http.core :as web]
   [matcho.core :as matcho]
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
                 :body "Hello, admin"}}

     form-params
     {:zen/tags #{zen/op zen.http/op}
      :engine zen.http/response-op
      :response {:status 200
                 :body "form params parsed"}}

     query-params
     {:zen/tags #{zen/op zen.http/op}
      :engine zen.http/response-op
      :select :params
      :response {:status 200}}

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

     cors
     {:zen/tags #{zen.http/middleware}
      :engine zen.http/cors}

     parse-params
     {:zen/tags #{zen.http/middleware}
      :engine zen.http/parse-params}

     api
     {:zen/tags #{zen.http/api}
      :engine zen.http/routemap
      :mw [cors]
      :GET index-op
      :POST zen.http/rpc
      "params-mw" {:mw [parse-params]
                   ;; TODO implement
                   :POST form-params
                   :GET query-params}
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

  (web/routes ztx 'myweb/api)

  (t/is (= {:status 200, :body "Hello"}
           (web/dispatch ztx 'myweb/api {:uri "/" :request-method :get})))

  (t/testing "cors mw"
    #_(t/is (= {:status 200}
               (web/dispatch ztx 'myweb/api {:uri "/" :request-method :options})))

    "if origin is provided cors headers are returned"
    (matcho/match
     (web/dispatch ztx 'myweb/api {:uri "/"
                                   :request-method :get
                                   :headers {"origin" "localhost:8080"}})
     {:status 200
      :headers
      {"Access-Control-Allow-Origin" "localhost:8080"
       "Access-Control-Allow-Credentials" "true"
       "Access-Control-Expose-Headers"
       "Location, Content-Location, Category, Content-Type, X-total-count"}}))

  (t/testing "basic auth mw"

    (t/is (= 401 (:status (web/dispatch ztx 'myweb/api {:uri "/admin" :request-method :get}))))

    (t/is (= {:status 200, :body "Hello, admin"}
             (web/dispatch ztx 'myweb/api {:uri "/admin"
                                           :request-method :get
                                           :headers {"authorization" "Basic am9objoxMjM="}}))))

  (t/testing "parse querystring mw"

    (matcho/match
     (web/dispatch ztx 'myweb/api {:uri "params-mw" :method :get :query-string "msg=hello+love"})
     {:status 200 :body "{:msg \"hello love\"}"}))

  (comment
    #_(zen/start-system ztx 'myweb/system)
    #_(zen/stop-system ztx)

    )

  ;; (sys/send ztx 'example/web 'web/dispatch {:uri "/Patient"})
  ;; (sys/send ztx 'example/web 'web/rpc {:method 'example/pt-search :params {}})


  )
