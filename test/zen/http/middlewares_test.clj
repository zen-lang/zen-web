(ns zen.http.middlewares-test
  (:require
   [ring.util.codec :as codec :refer [form-encode]]
   [clojure.java.io :as io]
   [zen.core :as zen]
   [zen.http.core :as web]
   [matcho.core :as matcho]
   [clojure.test :refer [is deftest testing]]))

(def config
  '{:ns myweb
    :import #{zen.http}

    index-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http.engines/response
     :response {:status 200
                :body "Hello"}}

    admin-index-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http.engines/response
     :response {:status 200
                :body "Hello, admin"}}

    form-params
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http.engines/response
     :select :form-params
     :response {:status 200}}

    query-params
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http.engines/response
     :select :params
     :response {:status 200}}

    basic-auth
    {:zen/tags #{zen.http/middleware}
     :engine zen.http.engines/basic-auth
     :user "john"
     :password "123"}

    admin-api
    {:zen/tags #{zen.http/api}
     :engine zen.http/routemap
     :mw [basic-auth]
     :GET admin-index-op}

    cookies-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http.engines/response
     :select :cookies
     :response {:status 200}}

    *cookie-set
    {:zen/tags #{zen.http.engines/op zen/schema}
     :type zen/map
     :keys {:max-age {:type zen/integer}}}

    cookie-set
    {:zen/tags #{zen/op zen.http/op}
     :engine *cookie-set
     :max-age 1000}

    override-op
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http.engines/response
     :response {:status 200}}

    api
    {:zen/tags #{zen.http/api}
     :engine zen.http/routemap
     :mw [zen.http/cors]
     :GET index-op
     :POST zen.http/rpc
     ;; TODO implement mw combinators
     "params-mw" {:mw [#_zen.http/defaults
                       zen.http/parse-params]
                  :POST form-params
                  :GET query-params}
     "cookies-mw" {:mw [zen.http/cookies]
                   :GET cookies-op
                   "get-cookies" {:GET cookie-set}}
     "method-override" {:PUT override-op}
     "admin" {:apis [admin-api]}}

    http
    {:zen/tags #{zen/start zen.http/http}
     :engine zen.http/httpkit
     :port 8080
     :api api
     #_:formats #_#{zen.http/json zen.http/yaml zen.http/html}}

    system
    {:zen/tags #{zen/system}
     :start [http]}})

(def ztx (zen/new-context {}))

(zen/load-ns ztx config)

(deftest middleware-config
  (comment
    #_(zen/start-system ztx 'myweb/system)
    #_(zen/stop-system ztx))

  (zen/load-ns ztx config)

  (is (empty? (zen/errors ztx)))

  (is (= {:status 200, :body "Hello"}
         (web/handle ztx 'myweb/api {:uri "/" :request-method :get}))))

(deftest cors-middleware
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

(deftest basic-auth

  (matcho/assert
   {:status 401
    :headers {"Content-Type" "text/plain"
              "WWW-Authenticate" "Basic realm=restricted area"}
    :body "access denied"}
   (web/handle ztx 'myweb/api {:uri "/admin" :request-method :get}))

  (is (= {:status 200, :body "Hello, admin"}
         (web/handle ztx 'myweb/api {:uri "/admin"
                                     :request-method :get
                                     :headers {"authorization" "Basic am9objoxMjM="}})))

  #_(testing "when req method is head no body is returned"
    (matcho/assert
     {:status 401
      :headers {"Content-Type" "text/plain"
                "WWW-Authenticate" "Basic realm=restricted area"}
      :body nil?}
     (web/handle ztx 'myweb/api {:uri "/admin" :request-method :head}))))

(deftest cookies

  (defmethod zen/op 'myweb/*cookie-set
    [ztx {:keys [max-age]} req & opts]
    {:status 200
     :cookies {"token" {:value "justvalue"
                        :max-age max-age
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

(deftest query-string
  (matcho/match
   (web/handle ztx 'myweb/api {:uri "/params-mw"
                               :request-method :get
                               :query-string "msg=hello+love"})
   {:status 200 :body "{:msg \"hello love\"}"})

  (matcho/match
   (web/handle ztx 'myweb/api {:uri "/params-mw"
                               :request-method :get
                               :query-string "msg"})
    {:status 200 :body "{:msg nil}"})

  (matcho/match
   (web/handle ztx 'myweb/api {:uri "/params-mw"
                               :request-method :post
                               :headers {"content-type" "application/x-www-form-urlencoded"}
                               :body (-> (str "value1=" (form-encode "a string")
                                              "&value2=" (form-encode " yet another string %"))
                                         .getBytes
                                         (io/input-stream))})
   {:status 200 :body "{:value1 \"a string\", :value2 \" yet another string %\"}"}))
