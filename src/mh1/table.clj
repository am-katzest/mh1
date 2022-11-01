(ns mh1.table
  (:require
   [comb.template :as template]
   [mh1.alg :refer :all]
   [incanter.stats :refer :all]))

(def ^:dynamic interesting-rows [10 20 50 100 150 200 300 400 600 800 1000])
;; (def ^:dynamic interesting-rows [10 20 50 100 150 250 300 350 400 450 500 550 600 650 700 750 800 850 900 950 1000])
(defn to-percentile [x] (/ x 136928.87))
(def ^:dynamic format-entry #(format "%.2f\\%%" %))

(defn make-row [xs] (str (reduce #(str %1 " & " %2) xs) " \\\\\n"))

(defn make-table [rows] (reduce str (map make-row rows)))

(def textemplate (slurp "template.tex"))

(defn prepare-table [filename results graph-name]
  (let [maximums (map #(map (partial apply max) %) results)
        ;; ok, tutaj robimy trochę dziwną rzecz
        best-up-untils (reductions (partial map max) maximums)
        format-row (fn [x] (let [row (nth best-up-untils (dec x))
                                 [min _ median _ max] (map to-percentile (quantile row))
                                 row% (map to-percentile row)
                                 cnt (count row)
                                 pct-by #(* 100.0 (/  (count (filter % row%)) cnt))
                                 bests (pct-by #(== 100 %))
                                 top1% (pct-by #(<= 99 %))]
                             (cons x (map format-entry [min median max bests top1%]))))
        dataa (map format-row
                   (take-while #(<= % duration) interesting-rows))]
    (template/eval textemplate {:tabela (make-table dataa)
                                :graf graph-name
                                :label filename})))
