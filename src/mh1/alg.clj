(ns mh1.alg
  (:require [mh1.data :as d]
            [mh1.utils :refer [make-wheel]]))

(defrecord specimen [choices weight value valid id])                      ; the thing we are evolving

(def ^:dynamic population-size)
(def ^:dynamic duration)
(def ^:dynamic replacement-rate)
(def ^:dynamic merge-identical)
(def ^:dynamic scoring-fn)
(def ^:dynamic distribution-fn)
(def ^:dynamic choose-cross-method)
(def ^:dynamic choices-len d/len)
(def ^:dynamic valid? #(<= % d/max-weight))

(defn create-specimen [choices]
  {:pre (= choices-len (count choices))}
  (let [sum-by (fn [sel] (->> choices (map #(* (sel %1) %2) d/items) (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    (->specimen choices weight value (valid? weight) (if merge-identical :merge (rand)))))

(defn spawn-orphan []
  (create-specimen (repeatedly choices-len #(rand-int 2))))

(let [a (fn [a & _] a)
      b (fn [_ b] b)
      r #(rand-nth [a b])
      flip (comp {1 0, 0 1} a)
      sr (fn [& _] (rand-nth [0 1]))]
  ;; aaabbbbbbb
  ;; aaaaaaabbb
  (defn one-point []
    (let [x (rand-int choices-len)
          y (- choices-len x)]
      (concat (repeat x a)
              (repeat y b))))
  ;; aaabbbbaa
  ;; abaaaaaaa
  ;; aaaaabbba
  (defn two-point []
    (let [x (inc (rand-int (dec choices-len))) ; at least one
          y (rand-int (- choices-len x))
          z (- choices-len x y)]
      (concat (repeat y a)
              (repeat x b)
              (repeat z a))))
  ;; ababbbabbb
  ;; babbbababa
  (defn random-cross []
    (repeatedly  choices-len r))
  ;; 011110001100
  (defn entirely-new []
    (repeat choices-len sr))
  ;; ababababab
  ;; aabbaabbaa
  ;; aaabbbaaab
  (defn stripe-cross [x]
    (let [pattern (concat (repeat x a) (repeat x b))]
      (->> pattern
           cycle
           (take choices-len)
           constantly)))
  ;; aaaāaaaaāaa
  (defn mutate [x]
    (fn [] (shuffle (concat (repeat x flip)
                            (repeat (- choices-len x) a)))))
  ;; āāāāāāāāāāā
  (defn flip-all []
    (repeat choices-len (fn [a _] (flip a)))))

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
       (map-indexed (fn [a b] [b a]))
       make-wheel))

(defn allowing [scale power]
  (fn  [{:keys [value valid weight]}]
    (if valid value (max 0.001
                         (* value
                            scale
                            (Math/pow (/ d/max-weight weight)
                                      power))))))

(defn advance [state]
  (let [chooser (distribution-fn state)
        survivors (chooser  (- population-size replacement-rate))
        children (repeatedly replacement-rate #(make-specimen chooser))]
    (into survivors children)))

(defn create-initial-population []
  (into #{} (repeatedly population-size spawn-orphan)))

(defn simulate [conf]
  (with-bindings conf
    (let [initial-state (create-initial-population)]
      (doall (take duration (iterate advance initial-state))))))

;; example
(comment (let [a (create-specimen (repeat choices-len 0))
               b (create-specimen (repeat choices-len 1))
               x (cross two-point a b)]
           (println a)
           (println b)
           (println x)
           x))
