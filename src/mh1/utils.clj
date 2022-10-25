(ns mh1.utils)

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
               (recur (apply dissoc unchoosen loop-results) to-be-choosen' results'))))))
  ([n sum xs]
   {:post (= n (count %))}
   (case (>= n (count xs)) (map second xs)
         (= n 0) []
         :else (let [choices (sort (repeatedly n #(* sum (rand))))
                     results (loop [[[k v] & xs] xs
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
                     to-be-choosen' (- n (count results))]
                 (if (= to-be-choosen' 0) results
                     (into results
                           (choose-weighted to-be-choosen'
                                            (apply dissoc (into {} xs) results))))))))
(defn make-wheel [kvs]
  (let [elems (into () kvs)
        sum (sum-vals kvs)]
    #(choose-weighted % sum elems)))
