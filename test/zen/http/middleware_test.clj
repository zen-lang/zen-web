(ns zen.http.middleware-test
  (:require
   [zen.core :as zen]
   [zen.http.core :as web]
   [matcho.core :as matcho]
   [clojure.test :refer [is deftest testing]]))

(def config
  '{:ns myweb
    :import #{zen.http}

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

    cookies
    {:zen/tags #{zen.http/middleware}
     :engine zen.http/cookies}

    cookies-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http/response-op
     :select :cookies
     :response {:status 200}}

    cookie-test
    {:zen/tags #{zen/op}}

    cookie-set-engine
    {:zen/tags #{zen/tag zen.http/op-engine zen/schema}
     :type zen/map}

    cookie-get
    {:zen/tags #{zen/op zen.http/op}
     :engine cookie-set-engine}

    override-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http/response-op
     :response {:status 200}}

    api
    {:zen/tags #{zen.http/api}
     :engine zen.http/routemap
     :mw [cors]
     :GET index-op
     :POST zen.http/rpc
     #_"search" #_{:api [zd.plugins/search]}
     "params-mw" {:mw [#_zen.http/defaults
                       parse-params]
                   ;; TODO implement
                  :POST form-params
                  :GET query-params}
     "cookies-mw" {:mw [cookies]
                   :GET cookies-op
                   "get-cookies" {:GET cookie-get}}
     "method-override" {:PUT override-op}
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

(def ztx (zen/new-context {}))

(zen/load-ns ztx config)

(deftest middleware-config-test
  (zen/load-ns ztx config)

  (is (empty? (zen/errors ztx)))

  (is (= {:status 200, :body "Hello"}
         (web/handle ztx 'myweb/api {:uri "/" :request-method :get}))))

(deftest cors-middleware-test
  (testing "options always returns cors headers"
    (matcho/match
     (web/handle ztx 'myweb/api {:uri "/" :request-method :options})
      {:status 200,
       :headers not-empty})

    (matcho/match
     (web/handle ztx 'myweb/api {:uri "/admin" :request-method :options})
      {:status 200,
       :headers not-empty}))

  (testing "if origin is provided cors headers are returned"
    (matcho/match
     (web/handle ztx 'myweb/api {:uri "/"
                                 :request-method :get
                                 :headers {"origin" "localhost:8080"}})
      {:status 200
       :headers
       {"Access-Control-Allow-Origin" "localhost:8080"
        "Access-Control-Allow-Credentials" "true"
        "Access-Control-Expose-Headers"
        "Location, Content-Location, Category, Content-Type, X-total-count"}})))

(deftest basic-auth-test
  (is (= 401 (:status (web/handle ztx 'myweb/api {:uri "/admin" :request-method :get}))))

  (is (= {:status 200, :body "Hello, admin"}
         (web/handle ztx 'myweb/api {:uri "/admin"
                                     :request-method :get
                                     :headers {"authorization" "Basic am9objoxMjM="}}))))

(deftest cookies-test

  (defmethod zen/op 'myweb/cookie-set-engine
    [ztx config req & opts]
    {:status 200
     :cookies {"token" {:value "justvalue"
                        :max-age 1000
                        :path "/"}
               "another-token" "another-value"}})

  (matcho/match
   (web/handle ztx 'myweb/api {:uri "/cookies-mw"
                               :request-method :get
                               :headers {"cookie" "USER_TOKEN=yes"}})
    {:status 200
     :body string?})

  (matcho/assert
   {:status 200
    :headers {"Set-Cookie" ["token=justvalue;Max-Age=1000;Path=/" "another-token=another-value"]}}
   (web/handle ztx 'myweb/api {:uri "/cookies-mw/get-cookies" :request-method :get})))

(deftest querystring-test
  (matcho/match
   (web/handle ztx 'myweb/api {:uri "/params-mw"
                               :request-method :get
                               :query-string "msg=hello+love"})
    {:status 200 :body "{:msg \"hello love\"}"}))
