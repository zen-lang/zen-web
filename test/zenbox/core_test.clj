(ns zenbox.core-test
  (:require [zenbox.core :as zenbox]
            [zenbox.storage.core]
            [zen.core :as zen]
            [zen.store :as zen-extra]
            [clojure.test :refer [deftest is]]
            [matcho.core :as matcho]))


(deftest test-storage
  (def ctx (zen/new-context))

  (zen/read-ns ctx 'demo)

  (matcho/match
   (zenbox/rpc ctx {:method 'zen-ui/rpc-methods})

   ;; {:result sample-valid-patinet}
   )
  )
