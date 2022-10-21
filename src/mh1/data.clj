(ns mh1.data
  (:require
   [clojure.edn :as edn]))
;; records are defined here
(defrecord item [id                     ;probably useless
                 name
                 weight
                 value])
(let [main-data (-> "resources/data.edn" slurp edn/read-string)]
  (def total-weight (:weight main-data))
  (def items (map (partial apply ->item) (:items main-data)))
  (def len (count items)))
