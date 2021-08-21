(ns zenbox.storage-test
  (:require [zenbox.core :as zenbox]
            [zenbox.storage.core]
            [zen.core :as zen]
            [zen.store :as zen-extra]
            [clojure.test :refer [deftest is]]
            [matcho.core :as matcho]))


(deftest test-storage
  (def ctx (zen/new-context))
  (zen/read-ns ctx 'demo)

  (def sample-valid-patinet {:resourceType "Patient" :id "patient"})


  (matcho/match
   (zenbox/rpc ctx {:method 'demo/insert-patient :params sample-valid-patinet})
   {:result sample-valid-patinet})

  (matcho/match
   (zenbox/rpc ctx {:method 'demo/read-patient :params sample-valid-patinet})
   {:result sample-valid-patinet})

  (matcho/match
   (zenbox/rpc ctx {:method 'demo/delete-patient :params sample-valid-patinet})
   {:result sample-valid-patinet})

  (matcho/match
   (zenbox/rpc ctx {:method 'demo/read-patient :params sample-valid-patinet})
   {:result nil})

  (matcho/match
   (zenbox/rpc ctx {:method 'demo/insert-patient :params {}})
   {:error []})

  (matcho/match
   (zenbox/rpc ctx {:method 'demo/read-patient :params {}})
   {:error []})

  (matcho/match
   (zenbox/rpc ctx {:method 'demo/delete-patient :params sample-valid-patinet})
   {:error [{:message "resource doesn't exists"}]})

  (matcho/match
   (zenbox/rpc ctx {:method 'demo/read-patient :params sample-valid-patinet})
   {:error [{:message "resource doesn't exists"}]})


  (matcho/match
   (zenbox/rpc ctx {:method 'demo/delete-patient :params {}})
   {:error []}))
