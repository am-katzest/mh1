(ns mh1.core
  (:require [mh1.alg :refer :all]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]))

(let [cfg {#'population-size 300
           #'duration 1600
           #'replacement-rate 30
           #'merge-identical true
           #'scoring-fn  (allowing 0.9) ;; (comp  #(Math/pow % 5) (allowing 0.9))
           #'distribution-fn  ranked
           #'cross-fns  {(mutate 1) 3
                         (mutate 2) 2
                         (mutate 3) 5
                         one-point 3
                         random-cross 3
                         two-point 3
                         flip-all 0.5
                         entirely-new 0.5}}
      data (->> cfg
                simulate
                (partition 3)
                (map first)
                (map #(->>  % (filter :valid) (map :value)))
                time)]
  (println "max:" (apply  max (map (partial apply max) data)))
  (time (let [plot (box-plot [])]
          (doseq [x data]
            (add-box-plot plot x))
          (doto plot
            (set-y-range 12000000 13700000)
            view)))
  :ok)
