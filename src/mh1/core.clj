(ns mh1.core
  (:require [mh1.alg :refer :all]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]
            [mh1.utils :refer [make-wheel]]))

(let [cfg {#'population-size 200
           #'duration 300
           #'replacement-rate 70
           #'merge-identical true
           #'scoring-fn
           (allowing 1 3)
           ;; (comp #(Math/pow % 15) (allowing 1 3))
           #'distribution-fn
           ranked
           ;; (fn [x] (let [x (sort-by scoring-fn > x)] (fn [n] (take n x))))
           #'choose-cross-method  (make-wheel
                                   {(mutate 1) 1 ;mało przydatne
                                    (mutate 2) 2 ;ma jakąś tam szanse na ulepszenie
                                    (mutate 3) 1
                                    one-point 3
                                    random-cross 3
                                    two-point 3
                                    flip-all 5
                                    entirely-new 0.5})}
      data (->> cfg
                simulate
                (partition 1)           ; only graph every-nth generation
                (map first)
                (map #(->>  % (filter :valid) (map :value)))
                time)]
  (println "max:" (apply  max (apply concat data)))
  (let [plot (box-plot [])]
    (doseq [x data]
      (add-box-plot plot x))
    (doto plot
      (set-y-range 12000000 13700000)
      view))
  :ok)
