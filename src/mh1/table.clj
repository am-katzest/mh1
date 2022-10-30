(ns mh1.table
  (:require
   [comb.template :as template]
   [mh1.alg :refer :all]
   [incanter.stats :refer :all]))

(def ^:dynamic interesting-rows [10 20 50 100 150 200 300 400 600 800 1000])
(def ^:dynamic format-entry #(format "%2.2f\\%%" (/ % 136928.87)))

(defn make-row [xs] (str (reduce #(str %1 " & " %2) xs) " \\\\\n"))

(defn make-table [rows] (reduce str (map make-row rows)))

(def textemplate (slurp "template.tex"))

(defn prepare-table [results graph-name]
  (let [maximums (map #(map (partial apply max) %) results)
        dataa (map (fn [x] (cons x (map format-entry (quantile (nth maximums (dec x)))))) (take-while #(<= % duration) interesting-rows))]
    (template/eval textemplate {:tabela (make-table dataa)
                                :graf graph-name})))
