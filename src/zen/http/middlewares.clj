(ns zen.http.middlewares
  (:require
   [clojure.string :as str]
   [ring.util.parsing :refer [re-token]]
   [clojure.walk :as walk]
   [ring.util.codec :as codec]
   [ring.middleware.cookies :as cookies])
  (:import java.util.Base64))

;; TODO impl middlewares, get rid of ring.* dependencies

(defn byte-transform [direction-fn string]
  (try
    (str/join (map char (direction-fn (.getBytes ^String string))))
    (catch Exception _)))

(defn decode-base64 [string]
  (byte-transform #(.decode (Base64/getDecoder) ^bytes %) string))

(defn basic-error [{:keys [request-method]}]
  {:zen.http.core/response
   (cond-> {:status 401 :headers {"Content-Type" "text/plain"}}
     (not= request-method :head) (assoc :body "access denied"))})

(defn verify-basic-auth
  [ztx {:keys [user password]} req]
  (if-let [auth (get-in req [:headers "authorization"])]
    (let [cred (and auth (decode-base64 (last (re-find #"^Basic (.*)$" auth))))
          [u p] (and cred (str/split (str cred) #":" 2))]
      (if (and (= user u) (= password p))
        {:basic-authentication {:u user :p password}}
        (basic-error req)))
    (basic-error req)))

(defn set-cors-headers [ztx cfg req resp]
  (when-let [origin (get-in req [:headers "origin"])]
    (update resp :headers merge
            {"Access-Control-Allow-Origin" origin
             "Access-Control-Allow-Credentials" "true"
             "Access-Control-Expose-Headers" "Location, Content-Location, Category, Content-Type, X-total-count"})))

(defn parse-params [ztx cfg {qs :query-string}]
  (when qs
    (let [parsed (walk/keywordize-keys (ring.util.codec/form-decode qs))
          params (if (string? parsed)
                   {(keyword parsed) nil}
                   parsed)]
      {:params params
       :query-params params})))

(def re-cookie-octet #"[!#$%&'()*+\-./0-9:<=>?@A-Z\[\]\^_`a-z\{\|\}~]")

(def re-cookie-value (re-pattern (str "\"" re-cookie-octet "*\"|" re-cookie-octet "*")))

(def re-cookie (re-pattern (str "\\s*(" re-token ")=(" re-cookie-value ")\\s*[;,]?")))

(defn parse-cookies [ztx cfg {:keys [headers] :as req}]
  (when-let [cookie (get headers "cookie")]
    {:cookies
     (->> (for [[_ name value] (re-seq re-cookie cookie)]
            [name value])
          (map (fn [[name value]]
                 (when-let [value (codec/form-decode-str
                                   (str/replace value #"^\"|\"$" ""))]
                   [name {:value value}])))
          (remove nil?)
          (into {}))}))

(def attr-map {:domain "Domain", :max-age "Max-Age", :path "Path"
               :secure "Secure", :expires "Expires", :http-only "HttpOnly"
               :same-site "SameSite"})

(def same-site-map {:strict "Strict"
                    :lax "Lax"
                    :none "None"})

(defn write-attr-map [attrs]
  (for [[key value] attrs]
    (let [attr-name (name (get attr-map key))]
      (cond
        (satisfies? cookies/CookieInterval value) (str ";" attr-name "=" (cookies/->seconds value))
        (satisfies? cookies/CookieDateTime value) (str ";" attr-name "=" (cookies/rfc822-format value))
        (true? value)  (str ";" attr-name)
        (false? value) ""
        (= :same-site key) (str ";" attr-name "=" (get same-site-map value))
        :else (str ";" attr-name "=" value)))))

(defn set-cookies
  [ztx cfg req resp]
  (when-let [cookies (:cookies resp)]
    (let [http-cookies
          (->> cookies
               (map (fn [[k v]]
                      (if (map? v)
                        (apply str
                               (codec/form-encode {k (:value v)})
                               (write-attr-map (dissoc v :value)))
                        (codec/form-encode {k v})))))]
      (assoc-in resp [:headers "Set-Cookie"] (vec http-cookies)))))


