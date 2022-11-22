(ns zen.http.oauth.core
  (:require
   [zen.http.oauth.jwt :as jwt]
   [clojure.set]
   [zen.core :as zen]
   [ring.util.codec]
   [clojure.string :as str]
   [cheshire.core :as json]
   [clj-http.client :as client]))

(defn decode64 [to-decode]
  (->> to-decode
       (.decode (java.util.Base64/getDecoder))
       String.))

(defn get-userinfo [provider token]
  (let [info
          (-> (:userinfo-endpoint provider)
              (client/get {:accept :json
                           :headers {"Authorization" (str "Bearer " token)}
                           :throw-exceptions false})
              :body
              (json/parse-string keyword))

          email-info
          (when-let [email-endpoint (:user-email-endpoint provider)]
            (-> email-endpoint
                (client/get {:accept :json
                             :throw-exceptions false
                             :headers {"Authorization" (str "Bearer " token)}})
                :body
                (json/parse-string keyword)))

          primary-email (->> email-info
                             (filter :primary)
                             first
                             :email)]

      (cond-> info
        (not-empty email-info)
        (assoc :email-info email-info)

        (and (not (:email info))
             primary-email)
        (assoc :email primary-email))))

(defn callback
  [ztx cfg {{:keys [provider-id]} :route-params
            {:keys [code state]} :query-params
            {:keys [providers base-uri secret] :as config} ::config} & opts]
  ;; TODO process logical errors like redirect_uri mismatch
  (let [{:keys [token-endpoint organizations org-endpoint]
         :as provider}
        (get providers provider-id)

        ;; exchange auth code for access token
        params {:client_id (:client-id provider)
                :client_secret (:client-secret provider)
                :redirect_uri (str base-uri (:redirect-uri provider))
                :grant_type "authorization_code"
                :code code}

        req  {:accept :json
              :form-params params
              :throw-exceptions false
              :content-type :x-www-form-urlencoded}

        resp (client/post token-endpoint req)]

    (if (> (:status resp) 399)
      {:status 403
       :body "could not get auth token"}

      (let [token (json/parse-string (:body resp) keyword)

            ;; get userinfo from provider api
            userinfo (get-userinfo provider (:access_token token))

            req-opts
            {:accept :json
             :throw-exceptions false
             :headers {"Authorization" (str "Bearer " (:access_token token))}}

            user-orgs
            (when (and (not-empty organizations) org-endpoint)
              (->> (-> org-endpoint
                       (client/get req-opts)
                       :body
                       (json/parse-string keyword))
                   (map :login)
                   (into #{})))

            jwt (merge {:token token
                        :provider (select-keys provider [:name :system])
                        :userinfo userinfo
                        :user-orgs user-orgs}
                       (select-keys userinfo [:email :url :name]))]

        ;; check that user belongs to desired orgs
        (if (clojure.set/intersection (set organizations) user-orgs)
          {:status 302
           :headers {"location" (decode64 state)}
           :cookies {"token" {:value (jwt/sign secret jwt :HS256)
                              :max-age 31536000
                              :path "/"}}}

          {:status 403
           :body (str "You should be member of [" (str/join "," organizations)
                      "] organizations. But only [" (str/join "," user-orgs) "]")})))))

(defn redirect
  [ztx cfg {{:keys [provider-id]} :route-params
            {:keys [state]} :query-params
            {:keys [providers base-uri]} ::config} & opts]
  ;; redirect to provider auth endpoint based on the provider configuration
  (let [{:keys [authorize-endpoint redirect-uri] :as prov} (get providers provider-id)
        params
        (cond-> {:response_type "code"
                 :scope (str/join " " (concat (:scopes prov) (:additional-scopes prov)))
                 :client_id (get-in prov [:client-id])
                 :redirect_uri (str base-uri redirect-uri)}
          (not (nil? state)) (assoc :state state))]
    {:status 302
     :headers {"location" (str authorize-endpoint "?" (ring.util.codec/form-encode params))
               "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
               "pragma" "no-cache"}}))

(defn encode64 [s]
  (when-not (str/blank? (str s))
    (.encodeToString (java.util.Base64/getEncoder) (if (string? s) (.getBytes s) s))))

(defn auth-redirect [{qs :query-string url :uri :as _req}]
  (let [state (encode64 (str url (when-not (empty? qs) (str "?" qs))))]
    {:status 302
     :headers {"location" (str "/auth?state=" state)
               "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
               "pragma" "no-cache"}}))

(defn verify-jwt [ztx {config-sym :config} {:keys [cookies uri] :as req}]
  (let [{:keys [secret cookie public]} (zen/get-symbol ztx config-sym)
        public-uris (map re-pattern public)
        ;; TODO remove regexes impl
        public?
        (->> public-uris
             (map #(re-matches % uri))
             (filter identity)
             (not-empty))]
    (when-not public?
      (if-let [token (get-in cookies [cookie :value])]
        (let [jwt (try (jwt/parse token)
                   ;; TODO think about this exception
                       (catch Exception _e
                         (throw (ex-info "Wrong token" {:status 403} _e))))]
          (if (jwt/verify jwt secret)
            {:user (:claims jwt)}
            {:zen.http.core/response (auth-redirect req)}))
        {:zen.http.core/response (auth-redirect req)}))))

(defn snap-config
  "mount oauth configuration for handler ops"
  [ztx {config-sym :config} {:keys [cookies uri] :as req}]
  {::config (update (zen/get-symbol ztx config-sym)
                    :providers
                    (fn [provs]
                      (->> (map #(zen/get-symbol ztx %) provs)
                           (reduce (fn [acc p] (assoc acc (:name p) p)) {}))))})
