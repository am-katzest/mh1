(ns mh1.alg
  (:require [mh1.data :as d]))

(defrecord specimen [choices score])                      ; the thing we are evolving

(def ^:dynamic point-mutation)
(def ^:dynamic cross-chance)
(def ^:dynamic mutation-chance)

(defn calc-adaptation [choices] (let [sum-by (fn [sel] (->> choices (map #(* (sel %1) %2) d/items) (reduce +)))
                                      weight (sum-by :weight)]
                                  (if (> weight d/total-weight) 0 (sum-by :value))))

(defn create-specimen [choices]
  {:pre (= d/len (count choices))}
  (->specimen choices (calc-adaptation choices)))

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

(def flip {1 0, 0 1})

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
      r #(rand-nth [a b])]
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
  (def stripe-3 (stripe-cross 3)))

(defn cross [f a b]
  (let [ac (:choices a)
        bc (:choices b)
        fc (f)]
    (assert (= (count ac) (count bc) (count fc)))
    (create-specimen (mapv #(%1 %2 %3) fc ac bc))))

(defn choose-weighted [xs]
  ;; takes {weight value} map, values need to be ints, positive
  (let [choice (->> xs vals (reduce +) (* (rand)))]
    (loop [[[k v] & xs] xs
           rem choice]
      (let [rem (- rem v)]
        (if (pos? rem)
          (recur xs (- rem v))
          k)))))

(defn roulette [population])
(defn advance [{:keys [size] :as conf} state]
  (into #{} (repeatedly size #(apply cross random-cross  (select-n state 2  first)))))

(defn simulate [{:keys [size duration] :as conf}]
  (let [initial-state (create-initial-population size)]
    (take  duration (iterate (partial advance conf) initial-state))))

(defn stat [state] [(count state) (/ (reduce + (map :score state)) (count state))])

(map stat (simulate {:size 30 :duration 50}))
