(ns mh1.core-test
  (:require [clojure.test :refer :all]
            [mh1.alg :refer :all]))

(deftest select-n-test
  (is (= #{:a} (select-n #{:a :b :c :d} 1 (comp first sort))))
  (is (= #{:a :b} (select-n #{:a :b :c :d} 2 (comp first sort))))
  (is (= #{:d} (select-n #{:a :b :c :d} 1 (comp last sort)))))

(deftest choose-weighted-test
  (is (= :a (choose-weighted {:a 1})))
  (is (= :a (choose-weighted {:a 1 :b 0})))
  (is (#{:a :b} (choose-weighted {:a 1 :b 1})))
  (is (>= 70
          (->> #(choose-weighted {:a 1 :b 1})
               repeatedly
               (take 100)
               (filter #{:a})
               count)
          30))
  (is (>= 70
          (->> #(choose-weighted {:a 1 :b 1 :c 0 :d 0 :e 0})
               repeatedly
               (take 100)
               (filter #{:a})
               count)
          30))
  (is (>= 70
          (->> #(choose-weighted {:a 100 :b 100})
               repeatedly
               (take 100)
               (filter #{:a})
               count)
          30))
  (is (>= 70
          (->> #(choose-weighted {:a 0.1 :b 0.1})
               repeatedly
               (take 100)
               (filter #{:a})
               count)
          30)))
