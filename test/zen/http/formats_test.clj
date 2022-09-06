(ns zen.http.formats-test
  (:require
   [zen.core :as zen]
   [zen.http.formats :as fmt]
   [zen.http.methods :as meth]
   [clojure.test :as t]))

(t/deftest test-zen.http

  (def ztx (zen/new-context {}))

  (zen/load-ns ztx '{ns myapp
                     import #{zen.http}

                     formats
                     {:zen/tags #{zen.http/middleware}
                      :engine zen.http/formats-middleware
                      :formats #{zen.http/json zen.http/yaml zen.http/transit zen.http/html}}})

  (t/is (empty? (zen/errors ztx)))

  (def fmt-mw (zen/get-symbol ztx 'myapp/formats))

  (meth/middleware-in ztx fmt-mw {:params {:_format "json"}
                                  :body "{\"a\": 1}"})

  (meth/middleware-out ztx fmt-mw {:params {:_format "json"}
                                   :body {:a 1}}
                       {})

  ;; (sys/send ztx 'example/web 'web/dispatch {:uri "/Patient"})
  ;; (sys/send ztx 'example/web 'web/rpc {:method 'example/pt-search :params {}})

  )
