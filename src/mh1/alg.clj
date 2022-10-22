(ns mh1.alg
  (:require [mh1.data :as d]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]))

(defrecord specimen [choices weight value valid id])                      ; the thing we are evolving

(def ^:dynamic population-size)
(def ^:dynamic duration)
(def ^:dynamic replacement-rate)
(def ^:dynamic merge-identical)
(def ^:dynamic scoring-fn)
(def ^:dynamic distribution-fn)
(def ^:dynamic cross-fns)

(defn create-specimen [choices]
  {:pre (= d/len (count choices))}
  (let [sum-by (fn [sel] (->> choices (map #(* (sel %1) %2) d/items) (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    (->specimen choices weight value (>= d/max-weight weight) (if merge-identical :merge (rand)))))

(defn spawn-orphan []
  (create-specimen (repeatedly d/len #(rand-int 2))))

(defn create-initial-population []
  (into #{} (repeatedly population-size spawn-orphan)))

(let [a (fn [a & _] a)
      b (fn [_ b] b)
      r #(rand-nth [a b])
      flip (comp {1 0, 0 1} a)
      sr (fn [& _] (rand-nth [0 1]))]
  ;; aaabbbbbbb
  ;; aaaaaaabbb
  (defn simple-cross []
    (let [x (rand-int d/len)
          y (- d/len x)]
      (concat (repeat x a)
              (repeat y b))))
  (defn cut-cross []
    (let [x (rand-int d/len)
          y (rand-int (- d/len x))
          z (- d/len x y)]
      (concat (repeat x a)
              (repeat y b)
              (repeat z a))))
  ;; ababbbabbb
  ;; babbbababa
  (defn random-cross []
    (repeatedly  d/len r))
  ;; 011110001100
  (defn entirely-new []
    (repeat d/len sr))
  ;; ababababab
  ;; aabbaabbaa
  ;; aaabbbaaab
  (defn stripe-cross [x]
    (let [pattern (concat (repeat x a) (repeat x b))]
      (->> pattern
           repeat
           (apply concat)
           (take d/len)
           constantly)))
  ;; aaaāaaaaāaa
  (defn mutate [x]
    (fn [] (shuffle (concat (repeat x flip)
                            (repeat (- d/len x) a)))))
  ;; āāāāāāāāāāā
  (defn flip-all []
    (repeat d/len (fn [a _] (flip a)))))

(defn cross [f a b]
  (let [ac (:choices a)
        bc (:choices b)
        fc (f)]
    (assert (= (count ac) (count bc) (count fc)))
    (create-specimen (mapv #(%1 %2 %3) fc ac bc))))

(defn sum-vals [vs] (->> vs (map second) (reduce +)))

(defn choose-weighted
  "takes {weight value} map, values should be non-negative"
  ([xs]
   {:post (some? %)}
   (let [sum (sum-vals xs)
         choice (* (rand) sum)]
     (loop [[[k v] & xs] xs
            rem choice]
       (let [rem (- rem v)]
         (if (pos? rem)
           (recur xs rem)
           k)))))
  ([n xs]
   {:post (= n (count %))}
   (if (>= n (count xs)) (keys xs)
       (loop [unchoosen (into {} xs)
              to-be-choosen n
              results []]
         (let [sum (sum-vals unchoosen)
               choices (sort (repeatedly to-be-choosen #(* sum (rand))))
               loop-results (loop [[[k v] & xs] unchoosen
                                   choices choices
                                   ctr 0
                                   acc []]
                              (if (nil? v) acc
                                  (let [ctr' (+ ctr v)
                                        this? #(> ctr' %)
                                        ;;  remove those below
                                        [this-one rest] (split-with this? choices)]
                                    (cond (empty? this-one) (recur xs rest ctr' acc)
                                          :else  (recur xs rest ctr' (conj acc k))))))
               to-be-choosen' (- to-be-choosen (count loop-results))
               results' (concat results loop-results)]
           (if (= to-be-choosen' 0) results'
               (recur (apply dissoc unchoosen loop-results) to-be-choosen' results')))))))

(defn make-specimen [ranked-pop]
  (apply cross
         (choose-weighted cross-fns)
         (choose-weighted 2 ranked-pop)))

(defn roulette [population]
  (->> population
       (map (juxt identity scoring-fn))))

(defn ranked [population]
  (->> population
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b a]))))

(defn allowing [x]
  (fn  [{:keys [value valid weight]}]
    (if valid value (max 0.001 (* value x (/ d/max-weight weight))))))

(defn advance [state]
  (let [roulette-wheel (distribution-fn state)
        survivors (choose-weighted  (- population-size replacement-rate) roulette-wheel)
        children (repeatedly replacement-rate #(make-specimen roulette-wheel))]
    (into survivors children)))

(defn simulate [conf]
  (with-bindings conf
    (let [initial-state (create-initial-population)]
      (doall (take duration (iterate advance initial-state))))))
;; (let [a (create-specimen (repeat 26 0))
;;       b (create-specimen (repeat 26 1))]
;;   (println "x")
;;   (println a)
;;   ;; (println b)
;;   (println (cross (mutate 3) a a)))
(let [cfg {#'population-size 70
           #'duration 500
           #'replacement-rate 5
           #'merge-identical true
           #'scoring-fn (comp  #(Math/pow % 5) (allowing 0.9))
           #'distribution-fn  ranked
           #'cross-fns  {(mutate 1) 3
                         (mutate 2) 2
                         (mutate 3) 1
                         simple-cross 3
                         random-cross 3
                         cut-cross 3
                         flip-all 0.5
                         entirely-new 0.5}}
      data (->> cfg
                simulate
                (partition 10)
                (map first)
                (map #(->>  % (filter :valid) (map :value)))
                doall
                time)]
  (println "max:" (apply  max (map (partial apply max) data)))
  (time (let [plot (box-plot [])]
          (doseq [x data]
            (add-box-plot plot x))
          (doto plot
            (set-y-range 12000000 13700000)
            view)))
  :ok)
;; (view (histogram (map  (fn [{:keys [weight value]}] (/ value weight)) d/items)))
