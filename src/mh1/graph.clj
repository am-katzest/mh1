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
  (let [plot (xy-plot  [] [] :legend true :series-label "")
        xs (range duration)
        extract (fn [x]
                  (->> results
                       (map #(map x %))
                       (map quantile)
                       (apply mapv vector)))
        q (extract (partial apply max))
        m (extract median)
        addd-lines (fn [plot [q0 q1 q2 q3 q4] color start label]
                     (doto plot
                       (add-lines xs q0  :series-label "q0")
                       (add-lines xs q1  :series-label "q1")
                       (add-lines xs q2  :series-label "q2")
                       (add-lines xs q3  :series-label "q3")
                       (add-lines xs q4  :series-label (str "q4 " label "   ")))

                     (doseq [[i q] (map-indexed vector (take 5 (iterate #(.darker %) color)))]
                       (set-stroke-color plot q :dataset (+ start i))))]
    (doto plot
      (set-stroke-color java.awt.Color/white :dataset 0)
      (addd-lines  q java.awt.Color/blue 1 "maximum")
      (addd-lines m java.awt.Color/red 6 "median")
      (set-y-range 12000000 13700000)
      (set-y-label (format "total value"))
      (set-x-label "generation")
      (pdf/save-pdf (str "sprawko/" filename) :width 1000 :height 700))))
