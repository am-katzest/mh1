(ns mh1.core
  (:gen-class)
  (:require [mh1.alg :refer :all]
            [mh1.graph :refer :all]
            [mh1.table :refer :all]
            [mh1.utils :refer [make-wheel]]))

(def ^:dynamic runs)

(defn tran [x] (apply mapv vector x))

(defn extract-correct-scores [xs] (let [ans (->>  xs (filter :valid) (map :value))]
                                    (if (empty? ans) [0] ans)))
(defn run [_] (map extract-correct-scores (simulate)))

(defn make-up-data []
  (->> runs
       range
       (pmap run)
       tran))

(defmacro add-names [dict]
  (->> dict
       (map (fn [[k v]]
              [k `(let [e# ~v]
                    (if (instance? clojure.lang.IMeta e#)
                      (with-meta e# {:name ~(str v)})
                      e#))]))
       (into {})))

(defmacro with-named-bindings [c & xs]
  (concat `(with-bindings (add-names ~c)) xs))

(defn pretty [x] (or (:name (meta x)) (str x)))

(defn create-section [f] (let [results (make-up-data)
                               graph-filename (str (java.util.UUID/randomUUID) ".tmp.pdf")]
                           (graph results graph-filename)
                           (spit (str "sprawko/" f)
                                 (prepare-table results graph-filename))))

(defmacro file& [cfg filename]
  `(future (with-named-bindings ~cfg (create-section (str ~filename ".transient.tex"))) (println ~filename) (flush)))

;; #'good-enough? (fn [pop] (<= 13692887 (reduce max 0 (extract-correct-scores pop))))
;; (fn [x] (let [x (sort-by scoring-fn > x)] (fn [n] (take n x))))
(def mutacja:0 (make-wheel {two-point 1}))
(def mutacja:10 (make-wheel {(mutate 3) 1 two-point 9}))
(def mutacja:25 (make-wheel {(mutate 3) 1 two-point 3}))
(def mutacja:50 (make-wheel {(mutate 3) 1 two-point 1}))
(def mutacja:75 (make-wheel {(mutate 3) 3 two-point 1}))
(def mutacja:100 (make-wheel {(mutate 3) 1}))
(pcalls)
(defn -main [])
(with-named-bindings
  {#'runs 10
   #'population-size 200
   #'duration 400
   #'replacement-rate 20
   #'merge-identical :łącz
   #'scoring-fn (allowing 0.95 3) ;; (comp #(Math/pow % 10) (allowing 1 3))
   #'distribution-fn ranked
   #'choose-cross-method  mutacja:10}
  (println "starting")
  ;; łączenie
  (file& {#'merge-identical :łącz} "1-mit")
  (file& {#'merge-identical :pozwalaj} "1-mif")
  ;; mutacja
  (comment)
  (file& {#'choose-cross-method mutacja:0} "2-m0")
  (file& {#'choose-cross-method mutacja:10} "2-m10")
  (file& {#'choose-cross-method mutacja:25} "2-m25")
  (file& {#'choose-cross-method mutacja:50} "2-m50")
  (file& {#'choose-cross-method mutacja:75} "2-m75")
  (file& {#'choose-cross-method mutacja:100} "2-m100"))
