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

(defn create-specimen "tworzy osobnika na podstawie jego genomu"
  [choices]
  {:pre (= choices-len (count choices))}
  ;;łączna wartość cechy względem danych
  (let [sum-by (fn [sel] (->> choices
                              (map #(* (sel %1) %2) d/items)
                              (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    (->specimen choices
                weight
                value
                (valid? weight)
                ;; ponieważ są one przechowywane w hash-setach,
                ;; osobniki zupełnie jednakowe są łączone
                ;; dodanie losowej liczby temu zapobiega
                (if merge-identical :merge (rand)))))

;; krzyżowanie --
(let [a (fn [a & _] a)
      b (fn [_ b] b)
      r #(rand-nth [a b])
      don't-flip a
      flip (comp {1 0, 0 1} a)
      sr (fn [& _] (rand-nth [0 1]))
      rng-without-extremes #(inc (rand-int (- % 2)))]

  ;; aaabbbbbbb
  ;; aaaaaaabbb
  (defn one-point []
    (let [x (rng-without-extremes choices-len)
          y (- choices-len x)]
      (concat (repeat x a)
              (repeat y b))))

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
    (fn [] (shuffle
            (concat (repeat x flip)
                    (repeat (- choices-len x) don't-flip)))))

  ;; āāāāāāāāāāā
  (defn flip-all []
    ((mutate choices-len)))

  ;; 011110001100
  (defn entirely-new []
    (repeat choices-len sr)))
;; --

(defn cross "dokonuje krzyżowania" [f a b]
  (let [ac (:choices a)
        bc (:choices b)
        fc (f)]
    (assert (= (count ac) (count bc) (count fc)))
    (create-specimen (mapv #(%1 %2 %3) fc ac bc))))

(defn make-child "tworzy dziecko z populacji" [wheel]
  ;; wybieramy dwoje rodziców
  (let [[a b] (shuffle (wheel 2))
        ;; i metodę krzyżowania
        [f] (choose-cross-method 1)]
    (cross f a b)))

(defn roulette "zwraca `koło` w którym każdemu z osobników
  przypada obszar równy jego przystosowaniu" [population]
  (->> population
       (map (juxt identity scoring-fn))
       make-wheel))

(defn ranked "zwraca `koło` w którym każdemu osobnikowi przypada obszar
  zależny od miejsca w rankingu" [population]
  (->> population
       (sort-by  scoring-fn <)
       (map-indexed (fn [a b] [b (inc a)]))
       make-wheel))

;; scoring --
(defn allowing [scale power]
  (fn  [{:keys [value valid weight]}]
    (if valid
      ;; jeżeli waga nie przekracza maksymalnej zwracamy wartość
      value
      (let [weight-factor (/ d/max-weight weight)
            weight-comp (Math/pow weight-factor power)
            ;; w przeciwnym wypadku mnożymy ją przez `scale`
            ;; i `power` potęgę nadmiaru wagi
            result (* value scale weight-comp)]
        ;; nie chcemy tu zera, niektóre metody wybierania przy nim szwankują
        (max 0.001 result)))))
;; --

;; advance --
(defn advance  "tworzy stan świata `n+1` ze stanu `n`"
  [state]
  (let [;; chooser to f(x) która zwraca x niejednakowych wyników
        chooser (distribution-fn state)
        ;; wybieramy osobniki które przetrwają
        survivors (chooser  (- population-size replacement-rate))
        ;; tworzymy dzieci
        children (repeatedly replacement-rate #(make-child chooser))]
    ;; łączymy je
    (into survivors children)))
;; --

(defn spawn-orphan "creates a child with no parents" []
  (create-specimen (repeatedly choices-len #(rand-int 2))))

(defn create-initial-population []
  (into #{} (repeatedly population-size spawn-orphan)))

;; simulate --
(defn simulate "przeprowadza symulację" [conf]
  (with-bindings conf
    ;; losujemy początkową populję
    (let [initial-state (create-initial-population)]
      (->> initial-state
           ;; (to zwraca leniwą sekwencję x, f(x), f(f(x))...
           (iterate advance)
           ;; progress
           (map-indexed (fn [idx x] (print (format "\r%.2f" (* 100.0 (inc idx) (/  duration)))) (flush) x))
           ;;ucinamy po przekroczeniu maksymalnej ilości
           (take duration)
           ;; albo po znalezieniu wystarczająco dobrego rozwiązania
           (up-until good-enough?)
           ;; sekwencja niestety musi być zrealizowana lokalnie
           doall))))
;; --

;; tutaj są przykłady działania niektórych funkcji
(comment (let [a (create-specimen (repeat choices-len 0))
               b (create-specimen (repeat choices-len 1))
               x (cross (mutate 3) a b)]
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
