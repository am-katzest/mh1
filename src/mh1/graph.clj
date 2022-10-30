(ns mh1.graph
  (:require
   [incanter.core :refer :all]
   [incanter.stats :refer :all]
   [incanter.charts :refer :all]
   [incanter.io :refer :all]
   [incanter.pdf :as pdf]
   [mh1.alg :refer :all]))

(def ^:dynamic graph-every-nth :auto)

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
