(ns mh1.alg
  (:require [mh1.data :as d]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]))

(defrecord specimen [choices weight value valid id])                      ; the thing we are evolving

(def ^:dynamic point-mutation)
(def ^:dynamic cross-chance)
(def ^:dynamic mutation-chance)

(defn calc-adaptation [choices])

(defn create-specimen [choices]
  {:pre (= d/len (count choices))}
  (let [sum-by (fn [sel] (->> choices (map #(* (sel %1) %2) d/items) (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    (->specimen choices weight value (>= d/max-weight weight) "uwu")))

;; (defn spawn-orphan []
;;   (create-specimen (repeatedly length #(rand-int 2))))
(defn spawn-orphan [] (->>
                       (lazy-cat
                        (repeat (rand-int  d/len) 1)
                        (repeat 0))
                       (take d/len)
                       shuffle
                       vec
                       create-specimen))

(defn create-initial-population [size]
  (into #{} (repeatedly size spawn-orphan)))

(defn select-n [elems n choice-fn]
  (loop [elems elems
         n n
         acc #{}]
    (if (= n 0) acc
        (let [choice (choice-fn elems)]
          (recur (disj elems choice)
                 (dec n)
                 (conj acc choice))))))

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

  ;; aaaaaaaāaaaaa
  (defn mutate []
    (->> a
         (repeat d/len)
         vec
         (#(assoc % (rand-int d/len) flip))))
  (defn flip-all []
    (repeat d/len (fn [a _] (flip a)))))

(defn cross [f a b]
  (let [ac (:choices a)
        bc (:choices b)
        fc (f)]
    (assert (= (count ac) (count bc) (count fc)))
    (create-specimen (mapv #(%1 %2 %3) fc ac bc))))

(defn choose-weighted [xs]
  {:post (some? %)}
  ;; takes {weight value} map, values should be non-negative
  (let [choice (->> xs (map second) (reduce +) (* (rand)))]
    (loop [[[k v] & xs] xs
           rem choice]
      (let [rem (- rem v)]
        (if (pos? rem)
          (recur xs rem)
          k)))))

(defn roulette [population scoring-fn]
  (->> population
       (map (juxt identity scoring-fn))
       choose-weighted))
(defn ranked [elements scoring-fn]
  (->> elements
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b a]))
       choose-weighted))
(sort-by first (frequencies (repeatedly 10000 #(ranked (range 5) identity))))
(sort-by first (frequencies (repeatedly 10000 #(choose-weighted {:a 1 :b 1 :c 1}))))

(defn advance [{:keys [size cross-fns selector step] :as conf} state]
  (println (count state))
  (into (select-n state (- size step) selector)
        (repeatedly step #(apply cross
                                 (choose-weighted cross-fns)
                                 (select-n state 2  selector)))))

(defn simulate [{:keys [size duration] :as conf}]
  (let [initial-state (create-initial-population size)]
    (take  duration (iterate (partial advance conf) initial-state))))

(defn dumb-score [{:keys [value valid]}]
  (if valid value 0))
(defn allowing [{:keys [value valid weight]}]
  (if valid value (* value 0.3 (/ d/max-weight weight))))
(defn  squared [{:keys [value valid weight]}]
  (* value (if valid value (* value 0.3 (/ d/max-weight weight)))))

(let [cfg {:size 30
           :duration 300
           :step 5
           :selector  #(roulette % squared)
           :cross-fns  {mutate 5
                        simple-cross 3
                        random-cross 3
                        (stripe-cross 1) 1
                        (stripe-cross 2) 1
                        (stripe-cross 3) 1
                        flip-all 1
                        entirely-new 1}}
      data (map #(->>  % (filter :valid) (map dumb-score))
                (simulate cfg))]
  (view (let [plot (box-plot [])]
          (doseq [x data]
            (add-box-plot plot x))
          plot))
  :ok)
