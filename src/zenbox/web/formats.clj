(ns zenbox.web.formats
  (:require [ring.util.codec]
            [ring.middleware.multipart-params :as multi]
            [clojure.edn :as edn]
            [ring.middleware.multipart-params.byte-array :as ba]
            [cheshire.core :as json]
            [cheshire.generate :as json-gen]
            [clj-yaml.core :as yaml]
            [clojure.pprint :as pprint]
            [ring.util.io]
            [cognitect.transit :as transit]
            [clj-time.format :as tfmt]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.xml :as xml])
  (:import org.httpkit.server.AsyncChannel
           [java.io BufferedWriter OutputStreamWriter  ByteArrayInputStream ByteArrayOutputStream]))

(json-gen/add-encoder org.httpkit.server.AsyncChannel json-gen/encode-str)
(json-gen/add-encoder clojure.lang.Var json-gen/encode-str)
(json-gen/add-encoder java.lang.Object json-gen/encode-str)

(def date-to-json-formatter (tfmt/formatters :date-time))

(extend-protocol yaml/YAMLCodec
  org.joda.time.DateTime
  (encode [d]
    (tfmt/unparse date-to-json-formatter d))

  clojure.lang.Keyword
  (encode [data]
    (str/join "/" (remove nil? [(namespace data) (name data)]))))

(json-gen/add-encoder
 org.joda.time.DateTime
 (fn  [d json-generator]
   (.writeString json-generator (tfmt/unparse date-to-json-formatter d))))

(defn generate-stream
  ([data] (generate-stream data nil))
  ([data options]
   (ring.util.io/piped-input-stream
    (fn [out] (json/generate-stream
               data (-> out (OutputStreamWriter.) (BufferedWriter.)) options)))))


(defmulti do-format (fn [fmt _ pretty? ] fmt))

(defmethod do-format :json [_ body pretty?]
  (generate-stream body {:pretty pretty?}))

(defmethod do-format :js [_ body pretty?]
  (str "var AIDBOX_DATA = "
       (json/generate-string body {:pretty pretty?})
       ";"))

(defmethod do-format :yaml [_ body _] ;; (yaml always pretty)
  (yaml/generate-string body))

(defmethod do-format :text [_ body _] body)

(defmethod do-format :edn [_ body pretty? ]
  (if pretty?
    (with-out-str (pprint/pprint body))
    (with-out-str (pr body))))

(defmethod do-format :transit [_ body _] ;; (transit always ugly)
  (ring.util.io/piped-input-stream
   (fn [out] (transit/write (transit/writer out :json) body))))

(defmulti parse-format (fn [fmt _ _] fmt))

(defmethod parse-format :json [_ _ {b :body}]
  (when b
    {:resource (cond
                 (string? b) (json/parse-string b keyword)
                 (instance? java.io.InputStream b) (json/parse-stream (io/reader b) keyword)
                 :else b)}))

(defmethod parse-format :edn [_ _ {b :body}]
  (when b
    {:resource
     (cond
       (string? b) (edn/read-string b)
       (instance? java.io.InputStream b)
       (-> b
           io/reader
           java.io.PushbackReader.
           edn/read)
       :else b)}))

(defmethod parse-format :form-data [_ _ req]
  {:form-params (clojure.walk/keywordize-keys (:multipart-params (multi/multipart-params-request req {:store (ba/byte-array-store)})))})

(defmethod parse-format :text [ct _ {b :body}]
  (when b
    {:body b
     :resource b}))

(defmethod parse-format :ndjson [ct _ {b :body}]
  (when b
    {:body b}))

(defmethod do-format :xml [_ body pretty?]
  (xml/emit-str (xml/sexp-as-element body)))

(defmethod parse-format nil [ct _ {b :body}]
  {})

(defmethod parse-format :transit [_ _ {b :body}]
  (when b
    (let [r (transit/reader b :json)]
      {:resource (transit/read r)})))

(defmethod parse-format :default [ct _ {b :body}]
  (throw (RuntimeException. (str "Unknown Content-Type: " ct))))

(defmethod parse-format :yaml [_ _ {b :body}]
  (when b
    {:resource (cond
                 (string? b) (yaml/parse-string b)
                 (instance? java.io.InputStream b)
                 (let [b (slurp b)]
                   (yaml/parse-string b))
                 :else b)}))

(defn drop-unused-xml-elements [dom]
  (assoc dom :content
         (filter (complement nil?)
                 (mapv (fn [el]
                         (if (= (:tag el) :xmlns.http%3A%2F%2Fwww.w3.org%2F1999%2Fxhtml/div)
                           (xml/element :div {:value (xml/emit-str (:content el))} [])

                           (if (string? el)
                             nil

                             (let [el (if (= (namespace (:tag el)) "xmlns.http%3A%2F%2Fhl7.org%2Ffhir")
                                        (assoc el :tag (keyword (name (:tag el))))
                                        el)]
                               (drop-unused-xml-elements el)))))
                       (:content dom)))))

(defmethod parse-format :xml [_ _ {b :body}]
  (when b
    (let [dom (drop-unused-xml-elements (xml/parse b))]
      {:resource dom
       :body dom})))

(defn form-decode [s] (clojure.walk/keywordize-keys (ring.util.codec/form-decode s)))
(defmethod parse-format :query-string [_ _ {b :body}]
  (when-let [b (cond
                 (string? b) b
                 (instance? java.io.InputStream b)
                 (if (pos? (.available b))
                   (slurp (io/reader b))
                   nil)
                 :else nil)]
    {:form-string b
     :form-params (form-decode b)}))

(def ct-mappings
  {"application/json" :json
   "application/json+fhir" :json
   "application/fhir+json" :json
   "application/json-patch+json" :json
   "application/merge-patch+json" :json
   "application/fhir+ndjson" :ndjson
   "application/ndjson" :ndjson

   "application/transit+json" :transit
   "text/yaml" :yaml
   "text/edn" :edn
   "text/plain" :text
   "text/html" :json
   "*/*" :json
   "application/x-www-form-urlencoded" :query-string
   "multipart/form-data" :form-data
   "application/yaml" :yaml
   "application/edn" :edn

   "application/fhir+xml" :xml
   "application/xml" :xml})

(defn header-to-format [content-type]
  (if (str/blank? content-type)
    [nil ""]

    (let [[ct options] (->> (str/split (first (str/split content-type #",")) #"\s*;\s*")
                            (map str/trim))]
      [(get ct-mappings ct ct) options])))

(defn content-type [fmt]
  (get {:edn "text/edn"
        :json "application/json"
        :transit "application/transit+json"
        :js      "text/javascript"
        :xml "application/fhir+xml"
        :yaml "text/yaml"} fmt))

(declare parse-accept-header)

(defn accept-header-to-format [ct]
  (let [disable-xml? (System/getenv "AIDBOX_DISABLE_XML")]
    (when (and ct (string? ct))
      (some ct-mappings (filter (fn [x] (if disable-xml? (not (str/index-of x "xml")) true))
                                (parse-accept-header ct))))))

(def known-formats
  {"json"     :json
   "edn"      :edn
   "xml"      :xml
   "js"       :js
   "transit"  :transit
   "yaml"     :yaml})

(defn get-wanted-format [{{fmt :_format pretty? :_pretty} :params {ac "accept"} :headers :as request}]
  (cond
    fmt (or (get known-formats fmt)
            (get ct-mappings fmt))
    ac (accept-header-to-format ac)
    :else :json))

(defn parse-accept-header [ct]
  (map str/trim (str/split ct #"[,;]")))

(defn format-response
  [{body :body {ct "content-type" :as headers} :headers :as resp}
   {{fmt :_format pretty? :_pretty} :params {ac "accept"} :headers :as request}]
  (if-not ct
    (if-let [fmt (get-wanted-format request)]
      (if (and body (or (vector? body) (map? body) (coll? body)))
        (assoc resp
               :body (do-format fmt body (Boolean/valueOf pretty?))
               :headers (merge headers {"content-type" (content-type fmt)}))
        resp)
      {:body (str "Unknown request format - " (or fmt ac)". Use "
                  (cond fmt (str "_format = " (str/join " | " (keys known-formats)))
                        ac  (str "Accept: " (str/join " | " (keys ct-mappings)))))
       :status 422})
    resp))

(defn parse-body
  [{body :body {ct "content-type"} :headers {fmt :_format} :params :as req}]
  (let [[content-type options] (header-to-format ct)]
    (when body
      (parse-format content-type options req))))
