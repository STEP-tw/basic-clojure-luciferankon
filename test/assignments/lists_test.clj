(ns assignments.lists-test
  (:require [clojure.test :refer :all]
            [assignments.lists :refer :all]))

(deftest map
  (testing "identity with single coll"
    (is (= [1 2 3] (map' identity [1 2 3])))))

(deftest filter-test
  (testing "even? predicate"
    (is (= [2 4 6] (filter' even? [1 2 3 4 5 6 9])))))

(deftest reduce-test
  (testing "arity 2"
    (is (= 15 (reduce' + [1 2 3 4 5]))))
  (testing "arity 3"
    (is (= 15 (reduce' + 1 [2 3 4 5])))))

(deftest count-test
  (testing "sequence length"
    (is (= 5 (count' [1 2 3 4 5]))))
  (testing "nil"
    (is (= 0 (count' nil))))
  (testing "[]"
    (is (= 0 (count' []))))
  (testing "map"
    (is (= 2 (count {:one 1 :two 2}))))
  (testing "string"
    (is (= 6 (count "abcdef")))))

(deftest reverse-test
  (testing "seqable collection"
    (is (= [5 4 3 2 1] (reverse' [1 2 3 4 5]))))
  (testing "non-seqable collection"
    (is (nil? (reverse' 1)))))

(deftest every-test
  (testing "true condition"
    (is (true? (every?' even? [2 4 6 8]))))
  (testing "false condition"
    (is (false? (every?' even? [2 4 9 6 8])))))

(deftest some-test
  (testing "true condition"
    (is (true? (some?' even? [1 5 3 4]))))
  (testing "false condition"
    (is (false? (some?' even? [1 5 7 9])))))

(deftest ascending-test
  (testing "true condition"
    (is (true? (ascending? [1 2 3 4 5 6]))))
  (testing "false condition"
    (is (false? (ascending? [1 2 3 2 1 0])))))

(deftest index-of-test
  (testing "element present in collection"
    (is (= 4 (index-of [1 2 3 4 5 6] 5))))
  (testing "element not-present in collection"
    (is (= -1 (index-of [1 2 3 4 5 6] 9)))))

(deftest palindrome?-test
  (testing "true condition"
    (is (true? (palindrome? ["n" "a" "m" "a" "n"]))))
  (testing "false condition"
    (is (false? (palindrome? ["k" "a" "n" "n" "u"])))))

(deftest sqr-of-the-first-test
  (testing "non-empty collection"
    (is (= [16 16 16] (sqr-of-the-first [4 5 6])))))

(deftest double-up-test
  (testing "single dimensional collection"
    (is (= [1 1 2 2 3 3] (double-up [1 2 3]))))
  (testing "two dimensional collection"
    (is (= [[1 2] [1 2] [3 4] [3 4]] (double-up [[1 2] [3 4]])))))

(deftest distinct-test
  (testing "non-empty collection"
    (is (= [1 2 4 5 3] (distinct' [1 2 1 2 4 5 3 5])))))

(deftest dedupe'-test
  (testing "non-empty collection"
    (is (= [1 2 1 4 5 3 5] (dedupe' [1 1 2 2 1 4 5 3 5])))))

(deftest sum-of-adjacent-digits-test
  (testing "sum of adjacent digits of [1 2 3]"
    (is (= [3 5] (sum-of-adjacent-digits [1 2 3])))))

(deftest difference-test
  (testing "non-empty collection"
    (is (= [2 3] (difference [1 2 3] [1])))))

(deftest union-test
  (testing "non-empty collection"
    (is (= [1 2 3] (union [1 2] [2 3])))))

(deftest transpose-test
  (testing "non-empty collection"
    (is (= [[:a :d] [:b :e] [:c :f]] (transpose [[:a :b :c] [:d :e :f]])))))

(deftest cross-product-test
  (testing "non-empty collection"
    (is (= [[1 4] [1 3] [1 5] [2 4] [2 3] [2 5] [3 4]] (cross-product [1 2 3] [4 3 5])))))

(deftest third-or-fifth-test
  (testing "non-empty collection"
    (is (= [1 4] (third-or-fifth [1 2 3 4])))))

(deftest russian-dolls-test
  (testing "[1 2 3] 3 => [[[1]] [[2]] [[3]]]"
    (is (= [[[1]] [[2]] [[3]]]  (russian-dolls [1 2 3] 3)))))

(deftest muted-thirds-test
  (testing "[1 2 8 4 15 2 7] => [1 2 0 4 15 0 7]"
    (is (= [1 2 0 4 15 0 7] (muted-thirds [1 2 8 4 15 2 7])))))

(deftest split-comb-test
  (testing "odd length collection"
    (is (= '(1 3 2 4 5) (split-comb [1 2 3 4 5]))))
  (testing "even length collection"
    (is (= '(1 4 2 5 3 6) (split-comb [1 2 3 4 5 6])))))

(deftest max-three-digit-sequence-test
  ;(testing "less than 3 elements"
  ;  (is (= [1 2] (max-three-digit-sequence [1 2]))))
  (testing "three elements or more"
    (are [x y] (= x y)
               [4 5 6] (max-three-digit-sequence [1 2 4 5 6])
               [5 2 4] (max-three-digit-sequence [1 3 5 2 4 1]))))
