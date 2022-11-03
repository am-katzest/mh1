(ns mh1.alg
  (:require [mh1.data :as d]
            [mh1.utils :refer [make-wheel up-until]]))

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

;;specimen --
(defrecord specimen [choices weight value valid id])
(defn create-specimen "tworzy osobnik na podstawie listy cech"
  [choices]
  ;;sumuje wartość pól danych dla których cecha jest równa 1
  (let [sum-by (fn [sel] (->> choices
                              (map #(* (sel %1) %2) d/items)
                              (reduce +)))
        weight (sum-by :weight)
        value (sum-by :value)]
    ;; wywołuje konstruktor
    (->specimen choices
                weight
                value
                (valid? weight)
                ;; ponieważ są one przechowywane w hash-setach,
                ;; osobniki zupełnie jednakowe są łączone
                ;; dodanie losowej liczby temu zapobiega
                (if (= :łącz merge-identical) :merge (rand)))))
;; --

;; krzyżowanie --
(let [a (fn [a & _] a)
      b (fn [_ b] b)
      r #(rand-nth [a b])
      don't-flip a
      flip (comp {1 0, 0 1} a)
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
    (->> (concat (repeat x a) (repeat x b))
         cycle
         (take choices-len)
         constantly))

  ;; aaaāaaaaāaa
  (defn mutate "zmienia `n` wyborów" [n]
    (fn [] (shuffle
            (concat (repeat n flip)
                    (repeat (- choices-len n) don't-flip))))))
;; --

(defn cross "dokonuje krzyżowania" [f a b]
  (let [ac (:choices a)
        bc (:choices b)
        fc (f)]
    (assert (= (count ac) (count bc) (count fc)))
    (create-specimen (mapv #(%1 %2 %3) fc ac bc))))

;; wybór-mutacji --
(defn krzyżowanie [metoda prawdopodobieństwo-mutacji]
  (let [q (- 100 prawdopodobieństwo-mutacji)
        p (/ prawdopodobieństwo-mutacji 3)]
    (make-wheel {(mutate 2) p
                 (mutate 3) p
                 (mutate 4) p
                 metoda q})))
;; --

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

;; top --
(defn top "zawsze wybiera najbardziej przystosowane osobniki" [x]
  (let [sorted (sort-by scoring-fn > x)]
    (fn [n] (take n sorted))))
;; --

;; scoring --
(defn allowing "ocenia jak bardzo przystosowany jest osobnik" [scale power]
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

(defn allowing-pow "allowing, z wynikiem podniesionym do `power` potęgi"
  [a b power] (comp #(Math/pow % power) (allowing a b)))
;; --

;; advance --
(defn advance  "tworzy stan świata `n+1` ze stanu `n`"
  [state]
  (let [;; chooser to f(x) która zwraca x niejednakowych wyników
        chooser (distribution-fn state)
        ;; wybieramy osobniki które przetrwają
        survivors (chooser  (- population-size replacement-rate))
        ;; tworzymy dzieci, każda para jest niezależna
        children (repeatedly replacement-rate #(make-child chooser))]
    ;; łączymy je
    (into survivors children)))
;; --

;; orphan --
(defn spawn-orphan "creates a child with no parents" []
  (create-specimen (repeatedly choices-len #(rand-int 2))))
;; --

(defn create-initial-population []
  (into #{} (repeatedly population-size spawn-orphan)))

;; simulate --
(defn simulate "przeprowadza symulację" []
  ;; losujemy początkową populję
  (let [initial-state (create-initial-population)]
    (->> initial-state
         ;; (to zwraca leniwą sekwencję x, f(x), f(f(x))...
         (iterate advance)
         ;;ucinamy po przekroczeniu maksymalnej ilości
         (take duration)
         ;; albo po znalezieniu wystarczająco dobrego rozwiązania
         (up-until good-enough?)
         ;; realizujemy  całą sekwencję
         doall)))
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
