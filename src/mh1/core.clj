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
                           (spit (str "sprawko/" f ".transient.tex")
                                 (prepare-table f results graph-filename))))
(def base-conf {#'runs 5
                #'population-size 150
                #'duration 400
                #'replacement-rate 30
                #'merge-identical :łącz
                #'scoring-fn (allowing 1 2)
                #'distribution-fn ranked
                #'choose-cross-method  (krzyżowanie random-cross 25)})

(def threads (atom #{}))
(defn show [cfg]
  (with-bindings base-conf (with-bindings cfg (graph (make-up-data) :show))))
(defmacro file& [cfg filename]
  `(println (time (do (with-named-bindings ~cfg (create-section ~filename)) ~filename))))

(defn wait []
  (doseq [th @threads]
    (println @th))
  (println "all finished"))

(defn -main [do not run me]
  (with-named-bindings
    {#'runs 500
     #'population-size 150
     #'duration 400
     #'replacement-rate 30
     #'merge-identical :łącz
     #'scoring-fn (allowing 1 2)
     #'distribution-fn ranked
     #'choose-cross-method  (krzyżowanie random-cross 25)}
    (println "starting")
    ;; łączenie
    ;; (with-named-bindings {#'scoring-fn (allowing 0.99 2)} ; to tak musi być, żeby je się dało bezpośrednio porównać
    ;;   (file& {#'merge-identical :łącz} "1-mit")
    ;;   (file& {#'merge-identical :pozwalaj} "1-mif"))
    ;;
    ;; (with-named-bindings {}
    ;;             ;; mutacja
    ;;             (file& {#'choose-cross-method (krzyżowanie random-cross 0)} "2-m0")
    ;;             (file& {#'choose-cross-method (krzyżowanie random-cross 10)} "2-m10")
    ;;             (file& {#'choose-cross-method (krzyżowanie random-cross 25)} "2-m25")
    ;;             (file& {#'choose-cross-method (krzyżowanie random-cross 50)} "2-m50")
    ;;             (file& {#'choose-cross-method (krzyżowanie random-cross 75)} "2-m75")
    ;;             (file& {#'choose-cross-method (krzyżowanie random-cross 100)} "2-m100"))
    ;; (with-named-bindings {#'distribution-fn top
    ;;                       #'merge-identical :pozwalaj
    ;;                       #'scoring-fn (allowing 0.95 3)}
    ;;   ;; mutacja w gorszych warunkach
    ;;   (file& {#'choose-cross-method (krzyżowanie random-cross 0)} "2-x0")
    ;;   (file& {#'choose-cross-method (krzyżowanie random-cross 25)} "2-x25")
    ;;   (file& {#'choose-cross-method (krzyżowanie random-cross 50)} "2-x50")
    ;;   (file& {#'choose-cross-method (krzyżowanie random-cross 75)} "2-x75")
    ;;   (file& {#'choose-cross-method (krzyżowanie random-cross 100)} "2-x100")
    ;;   (file& {#'choose-cross-method (krzyżowanie (mutate 2) 0)} "2-2100")
    ;;   (file& {#'choose-cross-method (krzyżowanie (mutate 3) 0)} "2-3100")
    ;;   (file& {#'choose-cross-method (krzyżowanie (mutate 1) 0)} "2-1100")
    ;;   (file& {#'choose-cross-method (krzyżowanie (mutate 4) 0)} "2-4100"))

    ;; wielkość populacji
    ;; (with-named-bindings {#'replacement-rate 30}
    ;;   (file& {#'population-size  60} "3-30-60")
    ;;   (file& {#'population-size  90} "3-30-90")
    ;;   (file& {#'population-size  120} "3-30-120")
    ;;   (file& {#'population-size  150} "3-30-150")
    ;;   (file& {#'population-size  180} "3-30-180")
    ;;   (file& {#'population-size  210} "3-30-210")
    ;;   (file& {#'population-size  240} "3-30-240")
    ;;   (file& {#'population-size  300} "3-30-300")
    ;;   (file& {#'population-size  360} "3-30-360"))

    ;; (with-named-bindings {#'replacement-rate 45}
    ;;   (file& {#'population-size 180} "3-45-180")
    ;;   (file& {#'population-size 225} "3-45-225")
    ;;   (file& {#'population-size  270} "3-45-270")
    ;;   (file& {#'population-size  315} "3-45-315"))

    ;; (with-named-bindings {#'replacement-rate 15}
    ;;   (file& {#'population-size  60} "3-15-60")
    ;;   (file& {#'population-size  75} "3-15-75")
    ;;   (file& {#'population-size  90} "3-15-90")
    ;;   (file& {#'population-size  105} "3-15-105"))

    ;; (with-named-bindings {#'replacement-rate 10}
    ;;   (file& {#'population-size  40} "3-10-40")
    ;;   (file& {#'population-size  50} "3-10-50")
    ;;   (file& {#'population-size  60} "3-10-60")
    ;;   (file& {#'population-size  70} "3-10-70"))

    ;; metoda selekcji
    ;; (with-named-bindings {}
    ;;   (file& {#'distribution-fn ranked} "4-ranked")
    ;;   (file& {#'distribution-fn roulette} "4-roulette1")
    ;;   (file& {#'distribution-fn top}
    ;;          "4-top")
    ;;   (file& {#'distribution-fn roulette
    ;;           #'scoring-fn (allowing-pow 1 2 5)}  "4-roulette5")
    ;;   (file& {#'distribution-fn roulette
    ;;           #'scoring-fn (allowing-pow 1 2 10)} "4-roulette10")
    ;;   (file& {#'distribution-fn roulette
    ;;           #'scoring-fn (allowing-pow 1 2 15)} "4-roulette15")
    ;;   (file& {#'distribution-fn roulette
    ;;           #'scoring-fn (allowing-pow 1 2 20)} "4-roulette20")
    ;;   (file& {#'distribution-fn roulette
    ;;           #'scoring-fn (allowing-pow 1 2 25)} "4-roulette25")
    ;;   (file& {#'distribution-fn roulette
    ;;           #'scoring-fn (allowing-pow 1 2 30)} "4-roulette30"))

    ;; (with-named-bindings {}
    ;;   ;;  metody krzyżowania
    ;;   (file& {#'choose-cross-method (krzyżowanie one-point 25)}  "5-1")
    ;;   (file& {#'choose-cross-method (krzyżowanie two-point 25)}  "5-2")
    ;;   (file& {#'choose-cross-method (krzyżowanie (stripe-cross 3) 25)}  "5-5")
    ;;   (file& {#'choose-cross-method (krzyżowanie random-cross 25)}  "5-6"))

    ;; (with-named-bindings {}
    ;;   funkcja przystosowania
    ;;   (file& {#'scoring-fn (allowing 0 3)}  "6-1")
    ;;   (file& {#'scoring-fn (allowing 0.95 3)}  "6-4")
    ;;   (file& {#'scoring-fn (allowing 1 3)}  "6-5")
    ;;   (file& {#'scoring-fn (allowing 1.05 3)}  "6-6")
    ;;   (file& {#'scoring-fn (allowing 1 2)}  "6-7")
    ;;   (file& {#'scoring-fn (allowing 1 1.5)}  "6-8")
    ;;   (file& {#'scoring-fn (allowing 1 1.2)}  "6-9")
    ;;   (file& {#'scoring-fn (allowing 1 1.1)}  "6-10")
    ;;   (file& {#'scoring-fn (allowing 1 1)}  "6-11")
    ;;   )
    )
  (println "finished"))

(comment (-main 'i 'don't 'fucking 'care))
(time (show {#'runs 6
             #'duration 400
             #'scoring-fn (allowing 1 5)}))
