(ns zen.http.oauth.core-test
  (:require
   [zen.core :as zen]
   [zen.http.core :as http]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]))

(declare ztx)

(defn prepare! [])

(def pth (str (System/getProperty "user.dir") "/docs"))

(def system-ns
  '{ns oauth.example
    import #{zen.http zen.http.oauth}

    google
    {:zen/tags #{zen.http.oauth/provider}
     :client-id "my-client-id"
     :client-secret "my-client-secret"
     :href "/auth/google"
     :redirect-uri "/auth/callback/google"
     :scopes         ["https://www.googleapis.com/auth/userinfo.profile"
                      "https://www.googleapis.com/auth/userinfo.email"]
     :userinfo-endpoint   "https://www.googleapis.com/oauth2/v1/userinfo"
     :token-endpoint      "https://www.googleapis.com/oauth2/v4/token"
     :authorize-endpoint       "https://accounts.google.com/o/oauth2/v2/auth"
     :name "google"
     :display "Google"
     :system "https://google.com"

     :organizations-notice ["myOrg"]}

    github
    {:zen/tags #{zen.http.oauth/provider}
     :client-id "my-client-id-1"
     :client-secret "my-client-secret-1"
     :href "/auth/github"
     :redirect-uri "/auth/callback/github"
     :organizations ["my2ndOrg"]
     :scopes         ["user" "read:org"]
     :name "github"
     :display "Github"
     :system "https://github.com"
     :userinfo-endpoint   "https://api.github.com/user"
     :token-endpoint      "https://github.com/login/oauth/access_token"
     :authorize-endpoint  "https://github.com/login/oauth/authorize"

     :additional-scopes ["repo"]
     :org-endpoint "https://api.github.com/user/orgs"
     :user-email-endpoint "https://api.github.com/user/emails"}

    oauth-config
    {:zen/bind zen.http.oauth/config-binding
     :zen/tags #{zen.http.oauth/config}
     :providers [google github]
     :base-uri "http://127.0.0.1.nip.io:8789"
     :cookie "token"
     :secret "secret-string"
     :public ["/public" "/auth" "/auth/.*"]}

    simple-response
    {:zen/tags #{zen/op zen.http/op}
     :engine zen.http.engines/response
     :response {:status 200}}

    api
    {:zen/tags #{zen.http/api}
     :engine zen.http/routemap
     :apis [zen.http.oauth/api]
     :mw [zen.http/defaults
          zen.http.oauth/verify-jwt]
     "public" {:GET simple-response}
     "private" {:GET simple-response}}})

(defn prepare! []
  (def ztx (zen/new-context {:zd/paths [pth] :paths [pth]}))
  (zen/read-ns ztx 'zen.http)
  (zen/read-ns ztx 'zen.http.oauth)
  (zen/load-ns ztx system-ns))

(deftest config
  (prepare!)

  (is (empty? (zen/errors ztx))))

(deftest oauth-api
  ;; TODO how to test oauth callback?

  (prepare!)

;; whitelist works
  (matcho/assert
   {:status 200}
   (http/handle ztx 'oauth.example/api {:request-method :get :uri "/public"}))

  (http/handle ztx 'oauth.example/api {:request-method :get :uri "/private"
                                       :headers {"Authorization" "Bearer 123"}})

  (matcho/assert
   {:status 302
    :headers
    {"location" "/auth?state=L3ByaXZhdGU="
     "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
     "pragma" "no-cache"}}
   (http/handle ztx 'oauth.example/api {:request-method :get :uri "/private"}))

  (defmethod zen/op 'zen.http.oauth/index
    [ztx cfg {{:keys [providers]} :zen.http.oauth.core/config} & opts]
    {:status 200
     :body (keys providers)})

;; providers list
  (matcho/assert
   {:status 200
    :body ["google" "github"]}
   ;; TODO test that providers are rendered
   (http/handle ztx 'oauth.example/api {:request-method :get :uri "/auth"}))

  ;; redirects to provider
  (matcho/assert
   {:status 302 :headers {"location" #"accounts.google.com"}}
   (http/handle ztx 'oauth.example/api {:request-method :get :uri "/auth/google"})))
