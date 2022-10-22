(ns mh1.alg
  (:require [mh1.data :as d]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [incanter.io :refer :all]))

(defrecord specimen [choices weight value valid id])                      ; the thing we are evolving

(defn create-specimen [choices]
  {:pre (= d/len (count choices))}
  (let [sum-by (fn [sel] (->> choices (map #(* (sel %1) %2) d/items) (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    (->specimen choices weight value (>= d/max-weight weight) :bez-duplikatów)))

(defn spawn-orphan []
  (create-specimen (repeatedly d/len #(rand-int 2))))

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

(defn sum-vals [vs] (->> vs (map second) (reduce +)))
(defn choose-weighted ([xs]
                       {:post (some? %)}
                    ;; takes {weight value} map, values should be non-negative
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
   ;; takes {weight value} map, values should be non-negative
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

(defn make-specimen [avialable-methods ranked-pop]
  (apply cross
         (choose-weighted avialable-methods)
         (choose-weighted 2 ranked-pop)))

(defn roulette [population scoring-fn]
  (->> population
       (map (juxt identity scoring-fn))))

(defn ranked [elements scoring-fn]
  (->> elements
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b a]))))

(defn sqranked [elements scoring-fn]
  (->> elements
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b (* (inc a) (inc a))]))))

(defn dumb-score [{:keys [value valid]}]
  (if valid value 0))

(defn allowing [x]
  (fn  [{:keys [value valid weight]}]
    (if valid value (* value x (/ d/max-weight weight)))))

(defn advance [{:keys [size cross-fns distribution-fn scoring-fn step] :as conf} state]
  (let [ranked (distribution-fn state scoring-fn)
        survivors (choose-weighted  (- size step) ranked)
        children (repeatedly step #(make-specimen cross-fns ranked))]
    (into survivors children)))

(defn simulate [{:keys [size duration] :as conf}]
  (let [initial-state (create-initial-population size)]
    (take  duration (iterate (partial advance conf) initial-state))))

(let [cfg {:size 70
           :duration 500
           :step 5
           :scoring-fn (allowing 0.9)
           :distribution-fn  ranked
           :cross-fns  {mutate 5
                        simple-cross 3
                        random-cross 3
                        flip-all 0.5
                        cut-cross 5
                        entirely-new 0.5}}
      data (->> cfg
                simulate
                (partition 2)
                (map first)
                (map #(->>  % (filter :valid) (map dumb-score))))]
  (time (last data))
  (println "max:" (apply  max (map (partial apply max) data)))
  (time (let [plot (box-plot [])]
          (doseq [x data]
            (add-box-plot plot x))
          (doto plot
            (set-y-range 12000000 13700000)
            view)))
  :ok)
;; (view (histogram (map  (fn [{:keys [weight value]}] (/ value weight)) d/items)))
