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
   {:pre (<= n (count xs))
    :post (= n (count %))}
   ;; takes {weight value} map, values should be non-negative
   (let [sum (sum-vals xs)]
     (loop [unchoosen (into {} xs)
            to-be-choosen n
            results []]
       (let [choices (sort (repeatedly to-be-choosen #(* (rand) sum)))
             loop-results (loop [[[k v] & xs] unchoosen
                                 choices choices
                                 ctr 0
                                 acc []]
                            (if (nil? v) acc
                                (let [ctr' (+ ctr v)
                                      this? #(> ctr' %)
                                      ;;  remove those below
                                      this-one (filter this? choices)
                                      rest (remove this? choices)]
                                  (cond (empty? this-one) (recur xs rest ctr' acc)
                                        :else  (recur xs rest ctr' (conj acc k))))))
             to-be-choosen' (- to-be-choosen (count loop-results))
             results' (concat results loop-results)]
         (if (= to-be-choosen' 0) results'
             (recur (apply dissoc unchoosen loop-results) to-be-choosen' results')))))))

(defn roulette [population scoring-fn]
  (->> population
       (map (juxt identity scoring-fn))
       choose-weighted))

(defn ranked [elements scoring-fn]
  (->> elements
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b a]))
       choose-weighted))

(defn sqranked [elements scoring-fn]
  (->> elements
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b (* (inc a) (inc a))]))
       choose-weighted))

(defn advance [{:keys [size cross-fns selector step] :as conf} state]
  (let [survivors (select-n state (- size step) selector)
        children (repeatedly step #(apply cross
                                          (choose-weighted cross-fns)
                                          (select-n state 2  selector)))]
    (into survivors children)))

(defn simulate [{:keys [size duration] :as conf}]
  (let [initial-state (create-initial-population size)]
    (take  duration (iterate (partial advance conf) initial-state))))

(defn dumb-score [{:keys [value valid]}]
  (if valid value 0))

(defn allowing [x]
  (fn  [{:keys [value valid weight]}]
    (if valid value (* value x (/ d/max-weight weight)))))

(let [cfg {:size 200
           :duration 50
           :step 10
           :selector  #(ranked % dumb-score)
           :cross-fns  {mutate 5
                        simple-cross 3
                        random-cross 3
                        flip-all 1
                        entirely-new 1}}
      data (map #(->>  % (filter :valid) (map dumb-score))
                (simulate cfg))
      _ (time (last data))]
  (view (let [plot (box-plot [])]
          (doseq [x data]
            (add-box-plot plot x))
          plot))
  :ok)
;; (view (histogram (map  (fn [{:keys [weight value]}] (/ value weight)) d/items)))
