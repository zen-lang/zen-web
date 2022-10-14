(ns zen.http.core-test
  (:require
   [zen.core :as zen]
   [zen.http.core :as web]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]))

(def config
  '{:ns myweb
    :import #{zen.http}

    index-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http/response-op
     :response {:status 200
                :body "Hello"}}

    override-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http/response-op
     :response {:status 200}}

    basic-auth
    {:zen/tags #{zen.http/middleware}
     :engine zen.http/basic-auth
     :user "john"
     :password "123"}

    serve-static
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http/serve-static
     :serve ["/test/zen/http/static"]}

    api
    {:zen/tags #{zen.http/api}
     :engine zen.http/routemap
     "static" {:* {:GET serve-static}}
     :GET index-op
     "test-mw" {:mw [basic-auth]
                :GET index-op}
     "method-override" {:PUT override-op}}

    http
    {:zen/tags #{zen/start zen.http/http}
     :engine zen.http/httpkit
     :port 8080
     :api api
     :formats #{zen.http/json zen.http/yaml zen.http/html}}

    system
    {:zen/tags #{zen/system}
     :start [http]}})

(def ztx (zen/new-context {}))

(zen/load-ns ztx config)

(deftest config-test
  (comment
    #_(zen/start-system ztx 'myweb/system)
    #_(zen/stop-system ztx))

  (zen/load-ns ztx config)

  (is (empty? (zen/errors ztx))))

(deftest routes-test
  (matcho/match
   (web/*routes ztx 'myweb/api)
   [{:path [:GET]}
    {:path ["method-override" :PUT]}]))

(deftest execution-stack

  (testing "mw-in is executed after route is resolved"
    (is (= {:status 404, :body "route not found"}
           (web/handle ztx 'myweb/api {:uri "/not-found"
                                       :request-method :get
                                       :headers {"authorization" "wrong-auth"}})))

    (is (= {:status 200, :body "Hello"}
           (web/handle ztx 'myweb/api {:uri "/test-mw"
                                       :request-method :get
                                       :headers {"authorization" "Basic am9objoxMjM="}})))))

(deftest http-headers
  (testing "method override works"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'myweb/api {:uri "/method-override"
                                 :request-method :post
                                 :headers {"x-http-method-override" "PUT"}}))))

(deftest serve-static
  (matcho/assert
   {:status 200
    :body #(instance? java.io.File %)}
   (web/handle ztx 'myweb/api {:uri "/static/content.txt" :request-method :get}))

  (matcho/assert
   {:status 404
    :body "file not found"}
   (web/handle ztx 'myweb/api {:uri "/static/not-found.jpg" :request-method :get})))

