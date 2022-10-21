(ns mh1.alg
  (:require [mh1.data :as d]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]))

(defrecord specimen [choices weight value valid])                      ; the thing we are evolving

(def ^:dynamic point-mutation)
(def ^:dynamic cross-chance)
(def ^:dynamic mutation-chance)

(defn calc-adaptation [choices])

(defn create-specimen [choices]
  {:pre (= d/len (count choices))}
  (let [sum-by (fn [sel] (->> choices (map #(* (sel %1) %2) d/items) (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    (->specimen choices weight value (>= d/max-weight weight))))

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

(def tmp (create-initial-population 3))

(defn point-mutation [a]
  (let [loc (rand-int (count a))]
    (update a loc flip)))

;; (def possible-actions [{:prob 1 :fn point-mutation :arity 1}])

(defn select-n [elems n choice-fn]
  (loop [elems elems
         n n
         acc #{}]
    (if (= n 0) acc
        (let [choice (choice-fn elems)]
          (recur (disj elems choice)
                 (dec n)
                 (conj acc choice))))))

(let [a (fn [a _] a)
      b (fn [_ b] b)
      r #(rand-nth [a b])
      flip {1 0, 0 1}]
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

  (defn stripe-cross [x]
    (let [pattern (concat (repeat x a) (repeat x b))]
      (->> pattern
           repeat
           (apply concat)
           (take d/len)
           constantly)))

  ;; ababababab
  (def stripe-1 (stripe-cross 1))
  ;; aabbaabbaa
  (def stripe-2 (stripe-cross 2))
  ;; aaabbbaaab
  (def stripe-3 (stripe-cross 3))

  ;; aaaaaaaāaaaaa
  (defn mutate []
    (->> identity
         (repeat d/len)
         vec
         (#(assoc % (rand-int d/len) {0 1, 1 0}))
         (map #(fn [a _] (% a)))))
  (mutate))

(defn cross [f a b]
  (let [ac (:choices a)
        bc (:choices b)
        fc (f)]
    (assert (= (count ac) (count bc) (count fc)))
    (create-specimen (mapv #(%1 %2 %3) fc ac bc))))

(defn choose-weighted [xs]
  ;; takes {weight value} map, values should be non-negative
  (let [choice (->> xs (map second) (reduce +) (* (rand)))]
    (loop [[[k v] & xs] xs
           rem choice]
      (let [rem (- rem v)]
        (if (pos? rem)
          (recur xs (- rem v))
          k)))))

(defn roulette [population scoring-fn]
  (choose-weighted (map (juxt identity scoring-fn) population)))

(defn advance [{:keys [size cross-fns selector] :as conf} state]
  (into #{} (repeatedly size #(apply cross
                                     (choose-weighted cross-fns)
                                     (select-n state 2  selector)))))

(defn simulate [{:keys [size duration] :as conf}]
  (let [initial-state (create-initial-population size)]
    (take  duration (iterate (partial advance conf) initial-state))))

(defn dumb-score [{:keys [value valid]}]
  (if valid value 0))

(let [data (map #(map dumb-score (filter :valid %)) (simulate {:size 50
                                                               :duration 200
                                                               :selector #(roulette % dumb-score)
                                                               :cross-fns  {mutate 3
                                                                            simple-cross 3
                                                                            random-cross 3}}))]
  (view (let [plot (box-plot [])]
          (doseq [x data]
            (add-box-plot plot x))
          plot)))
