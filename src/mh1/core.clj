(ns mh1.core
  (:require [mh1.alg :refer :all]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [incanter.pdf :as pdf]
            [mh1.utils :refer [make-wheel]]
            [clojure.core.matrix.stats :as ms]
            [clojure.core.matrix :as matrix]))
(defn extract-correct-scores [xs] (->>  xs (filter :valid) (map :value)))

(let [cfg {#'population-size 50
           #'duration 400
           #'replacement-rate 5
           #'merge-identical false
           #'scoring-fn
           (allowing 0 3)
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
                                    entirely-new 0.5})}
      run (fn [_] (->> cfg
                       simulate
                       (partition 1)     ; only graph every-nth generation
                       (map first)
                       (map extract-correct-scores)
                       time))]
  (let [plot (box-plot [])
        results (pmap run (range 40))
        maxs (matrix/transpose (map #(map (partial apply max) %) results))]
    (doseq [x maxs]
      (add-box-plot plot x))
    (doto plot
      (set-y-range 12000000 13700000)
      (set-y-label (format "wartość"))
      view)))
