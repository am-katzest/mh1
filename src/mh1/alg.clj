(ns mh1.alg
  (:require [mh1.data :as d]
            [mh1.utils :refer [make-wheel up-until]]))

(defrecord specimen [choices weight value valid id]) ; the thing we are evolving

(def ^:dynamic population-size)
(def ^:dynamic duration)
(def ^:dynamic replacement-rate)
(def ^:dynamic merge-identical)
(def ^:dynamic scoring-fn)
(def ^:dynamic distribution-fn)
(def ^:dynamic choose-cross-method)
(def ^:dynamic choices-len d/len)
(def ^:dynamic valid? #(<= % d/max-weight))
(def ^:dynamic good-enough? (constantly false))

(defn create-specimen [choices]
  {:pre (= choices-len (count choices))}
  (let [sum-by (fn [sel] (->> choices
                              (map #(* (sel %1) %2) d/items)
                              (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    (->specimen choices
                weight
                value
                (valid? weight)
                (if merge-identical :merge (rand)))))

(let [a (fn [a & _] a)
      b (fn [_ b] b)
      r #(rand-nth [a b])
      don't-flip a
      flip (comp {1 0, 0 1} a)
      sr (fn [& _] (rand-nth [0 1]))
      rng-without-extremes #(inc (rand-int (- % 2)))]

  ;; one-point --
  ;; aaabbbbbbb
  ;; aaaaaaabbb
  (defn one-point []
    (let [x (rng-without-extremes choices-len)
          y (- choices-len x)]
      (concat (repeat x a)
              (repeat y b))))
  ;; --

  ;; aaabbbbaa
  ;; abaaaaaaa
  ;; aaaaabbba
  (defn two-point []
    (let [x (rng-without-extremes choices-len)
          y (rng-without-extremes (- choices-len x))
          z (- choices-len x y)]
      (concat (repeat x a)
              (repeat y b)
              (repeat z a))))
  ;; ababbbabbb
  ;; babbbababa
  (defn random-cross []
    (repeatedly  choices-len r))
  ;; 011110001100
  (defn entirely-new []
    (repeat choices-len sr))

  ;; stripe
  ;; ababababab
  ;; aabbaabbaa
  ;; aaabbbaaab
  (defn stripe-cross [x]
    (let [pattern (concat (repeat x a) (repeat x b))]
      (->> pattern
           cycle
           (take choices-len)
           constantly)))
  ;; --

  ;; mutate --
  ;; aaaāaaaaāaa
  (defn mutate [x]
    (fn [] (shuffle
            (concat (repeat x flip)
                    (repeat (- choices-len x) don't-flip)))))
  ;; --

  ;; āāāāāāāāāāā
  (defn flip-all []
    ((mutate choices-len))))

(defn cross [f a b]
  (let [ac (:choices a)
        bc (:choices b)
        fc (f)]
    (assert (= (count ac) (count bc) (count fc)))
    (create-specimen (mapv #(%1 %2 %3) fc ac bc))))

(defn make-specimen [wheel]
  (let [[a b] (shuffle (wheel 2))
        [f] (choose-cross-method 1)]
    (cross f a b)))

(defn roulette [population]
  (->> population
       (map (juxt identity scoring-fn))
       make-wheel))

(defn ranked [population]
  (->> population
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b (inc a)]))
       make-wheel))

;; scoring --
(defn allowing [scale power]
  (fn  [{:keys [value valid weight]}]
    (if valid
      value
      (let [weight-factor (/ d/max-weight weight)
            weight-comp (Math/pow weight-factor power)
            result (* value scale weight-comp)]
        (max 0.001 result)))))
;; --

;; advance --
(defn advance [state]
  (let [chooser (distribution-fn state)
        survivors (chooser  (- population-size replacement-rate))
        children (repeatedly replacement-rate #(make-specimen chooser))]
    (into survivors children)))
;; --

(defn spawn-orphan []
  (create-specimen (repeatedly choices-len #(rand-int 2))))

(defn create-initial-population []
  (into #{} (repeatedly population-size spawn-orphan)))

(defn simulate [conf]
  (with-bindings conf
    (let [initial-state (create-initial-population)]
      (->> initial-state
           (iterate advance)
           (up-until good-enough?)
           (take duration)
           doall))))
;; example
(comment (let [a (create-specimen (repeat choices-len 0))
               b (create-specimen (repeat choices-len 1))
               x (cross two-point a b)]
           (println a)
           (println b)
           (println x)
           x))

(comment (reduce #(map + %1 %2)
                 (for [_ (range 1000)]
                   (let [a (create-specimen (repeat choices-len 0))
                         b (create-specimen (repeat choices-len 1))
                         x (cross (mutate choices-len) a b)]
                     (:choices x)))))
