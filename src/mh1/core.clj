(ns mh1.core
  (:require [mh1.alg :refer :all]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.pdf :as pdf]
            [comb.template :as template]
            [mh1.utils :refer [make-wheel]]
            [clojure.core.matrix.stats :as ms]
            [clojure.core.matrix :as matrix]))
(def ^:dynamic graph-every-nth :auto)
(def ^:dynamic runs)
(def ^:dynamic interesting-rows [10 20 50 100 150 200 300 400 600 800 1000])
(def ^:dynamic format-entry #(format "%2.2f\\%%" (/ % 136928.87)))

(defn tran [x] (apply mapv vector x))
(defn extract-correct-scores [xs] (let [ans (->>  xs (filter :valid) (map :value))]
                                    (if (empty? ans) [0] ans)))
(defn run [_] (map extract-correct-scores (simulate)))

(defn make-up-data []
  (->> runs
       range
       (pmap run)
       (map-indexed (fn [idx x] (println idx) x))
       tran))

(defn meta-available? [x]
  (instance? clojure.lang.IMeta x))
(defmacro add-names [dict]
  (->> dict
       (map (fn [[k v]]
              [k `(let [e# ~v] (if (meta-available? e#)
                                 (with-meta e# {:name ~(str v)}) e#))]))
       (into {})))
(defmacro with-named-bindings [c & xs]
  (concat `(with-bindings (add-names ~c)) xs))

(defn pretty [x] (or (:name (meta x)) (str x)))

(def cfg (add-names {#'runs 10
                     #'population-size 100
                     #'duration 200
                     #'replacement-rate 10
                     #'merge-identical :pozwalaj
                     #'scoring-fn
                     (allowing 0.95 3)
                     ;; (comp #(Math/pow % 10) (allowing 1 3))
                     #'distribution-fn
                     ranked
                     ;; (fn [x] (let [x (sort-by scoring-fn > x)] (fn [n] (take n x))))
                     ;; #'good-enough? (fn [pop] (<= 13692887 (reduce max 0 (extract-correct-scores pop))))
                     #'choose-cross-method  (make-wheel
                                             {(mutate 1) 1 ;mało przydatne
                                              (mutate 2) 2 ;ma jakąś tam szanse na ulepszenie
                                              (mutate 3) 1
                                              one-point 3
                                              random-cross 3
                                              two-point 3
                                              flip-all 0.5
                                              entirely-new 0.5})}))

(defn graph [results filename]
  (let [plot (box-plot [])
        truncated (map first (partition (if (= :auto graph-every-nth) (max 1 (int (/ duration 100))) graph-every-nth) results))
        add-column (fn [column f num]
                     (add-box-plot
                      plot
                      (map f column)
                      :series-label num))]

    (doseq [column  truncated]
      (add-column column (partial apply max) 1)
      ;; (add-column column median 2)
      ;; (add-column column (partial apply min) 3)
      )

    (doto plot
      (set-y-range 12000000 13700000)
      (set-y-label (format "wartość"))
      ;; (set-x-label "pokolenie")
      (-> .getPlot .getDomainAxis (.setVisible false))
      (pdf/save-pdf (str "sprawko/" filename) :width 1000 :height 700))))

(defn make-row [xs] (str (reduce #(str %1 " & " %2) xs) " \\\\\n"))
(defn make-table [rows] (reduce str (map make-row rows)))
(def textemplate (slurp "template.tex"))
(defn prepare-table [results graph-name]
  (let [maximums (map #(map (partial apply max) %) results)
        dataa (map (fn [x] (cons x (map format-entry (quantile (nth maximums (dec x)))))) (take-while #(<= % duration) interesting-rows))]
    (template/eval textemplate {:tabela (make-table dataa)
                                :graf graph-name})))

(defn create-section [f] (let [results (make-up-data)
                               graph-filename (str (java.util.UUID/randomUUID) ".tmp.pdf")]
                           (graph results graph-filename)
                           (spit (str "sprawko/" f)
                                 (prepare-table results graph-filename))))
(defmacro file& [cfg filename]
  `(future (with-named-bindings ~cfg (create-section ~filename))))

(with-bindings cfg
  (file& {#'merge-identical :łącz} "1-mif.transient.tex")
  (file& {#'merge-identical :pozwalaj} "1-mit.transient.tex")
  ;; (file& {#'merge-identical :pozwalaj} "1-mit.transient.tex")
  )
