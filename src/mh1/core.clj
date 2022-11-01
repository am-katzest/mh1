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
       (map run)
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

(def threads (atom #{}))

(defmacro file& [cfg filename]
  `(swap! threads conj (future (with-named-bindings ~cfg (create-section ~filename)) ~filename)))
(defn wait []
  (doseq [th @threads]
    (println @th))
  (println "all finished"))

(defn -main [do not run me]
  (with-named-bindings
    {#'runs 1
     #'population-size 200
     #'duration 400
     #'replacement-rate 30
     #'merge-identical :łącz
     #'scoring-fn (allowing 1 3) ;; (comp #(Math/pow % 10) (allowing 1 3))
     #'distribution-fn ranked
     #'choose-cross-method  (krzyżowanie random-cross 25)}
    (println "starting")
    ;; łączenie
    (comment)
    (with-named-bindings {#'scoring-fn (allowing 0.95 3)}
      (file& {#'merge-identical :łącz} "1-mit")
      (file& {#'merge-identical :pozwalaj} "1-mif")
      ;;
      (with-named-bindings {#'population-size 30
                            #'replacement-rate 5}
        ;; mutacja
        (file& {#'choose-cross-method (krzyżowanie random-cross 0)} "2-m0")
        (file& {#'choose-cross-method (krzyżowanie random-cross 10)} "2-m10")
        (file& {#'choose-cross-method (krzyżowanie random-cross 25)} "2-m25")
        (file& {#'choose-cross-method (krzyżowanie random-cross 50)} "2-m50")
        (file& {#'choose-cross-method (krzyżowanie random-cross 75)} "2-m75")
        (file& {#'choose-cross-method (krzyżowanie random-cross 100)} "2-m100"))

      ;; wielkość populacji
      (with-named-bindings {#'replacement-rate 20}
        (file& {#'population-size 25} "3-25")
        (file& {#'population-size 50} "3-50")
        (file& {#'population-size 100} "3-100")
        (file& {#'population-size 200} "3-200")
        (file& {#'population-size 400} "3-400")
        (file& {#'population-size 800} "3-800"))
      ;; metoda selekcji
      (with-named-bindings {}
        (file& {#'distribution-fn ranked} "4-ranked")
        (file& {#'distribution-fn roulette} "4-roulette1")
        (file& {#'distribution-fn roulette
                #'scoring-fn (allowing-pow 1 3 5)}  "4-roulette5")
        (file& {#'distribution-fn roulette
                #'scoring-fn (allowing-pow 1 3 10)} "4-roulette10")
        (file& {#'distribution-fn roulette
                #'scoring-fn (allowing-pow 1 3 15)} "4-roulette15")
        (file& {#'distribution-fn roulette
                #'scoring-fn (allowing-pow 1 3 20)} "4-roulette20")
        (file& {#'distribution-fn top}
               "4-top"))
      (with-named-bindings {}
        (file& {#'choose-cross-method (krzyżowanie one-point 25)}  "5-1")
        (file& {#'choose-cross-method (krzyżowanie two-point 25)}  "5-2")
        (file& {#'choose-cross-method (krzyżowanie (stripe-cross 3) 25)}  "5-5")
        (file& {#'choose-cross-method (krzyżowanie random-cross 25)}  "5-6")))

    (with-named-bindings {}
      (file& {#'scoring-fn (allowing 0 3)}  "6-1")
      (file& {#'scoring-fn (allowing 0.5 3)}  "6-2")
      (file& {#'scoring-fn (allowing 0.9 3)}  "6-3")
      (file& {#'scoring-fn (allowing 0.95 3)}  "6-4")
      (file& {#'scoring-fn (allowing 1 3)}  "6-5")
      (file& {#'scoring-fn (allowing 1.05 3)}  "6-6")
      (file& {#'scoring-fn (allowing 1 2)}  "6-7")
      (file& {#'scoring-fn (allowing 1 4)}  "6-8"))
    (wait)
    ;; (System/exit 0)
    ))
(-main 'i 'don't 'fucking 'care)
