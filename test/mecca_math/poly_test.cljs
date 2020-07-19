(ns mecca-math.poly-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [mecca-math.poly :as SUT]))

(deftest add-and-subtract-test
  (testing "Add polynomials"
    (is (= [-1 4 -7 0 -1] (SUT/add-poly [-1 -5 -10 0 0] [9 3 0 -1])))
    (is (= [-2 -1 8 0] (SUT/add-poly [-2 -7 5 0] [6 3 0])))
    (is (= [-1 0 -2 3] (SUT/add-poly [-1 8 -3 0] [-8 1 3]))))
  (testing "Subtract polynomials."
    (is (= (SUT/sub-poly [-9 0 0 0 8] [-9 2 5 0 0])
           '(0 -2 -5 0 8)))
    (is (= (SUT/sub-poly [6 2 5] [5 -6 -11])
           '(1 8 16) ))
    (is (= (SUT/sub-poly [-7 3 -6] [3 4 4])
           '(-10 -1 -10)))
    (is (= (SUT/sub-poly [1 8 -9] [11 -4 7])
           '(-10 12 -16)))))

(deftest mult-poly-test
  (testing "Multiply binomials by polynomials"
    (is (= [1 9 23 15 0 0 0] (SUT/mult-poly [1 4 3 0] [1 5 0 0])))
    (is (= [3 0 1 12 0 4] (SUT/mult-poly [3 0 1] [1 0 0 4])))
    (is (= [1 9 23 15 0 0 0] (SUT/mult-poly [1 4 3 0] [1 5 0 0])))
    (is (= [1 7 14 8] (SUT/mult-poly [1 4] [1 3 2])))
    (is (= [2 0 12 5 0 30 0] (SUT/mult-poly [2 0 0 5] [1 0 6 0])))
    (is (= [1 6 3 6 2 0] (SUT/mult-poly [1 6 2 0] [1 0 1])))
    (is (= [2 17 11 24 0] (SUT/mult-poly [1 8] [2 1 3 0]))))
  (testing "Polynomial special products: difference of squares"
    (is (= [49 0 0 0 0 0 0 0 0 0 0 0 -36] (SUT/mult-poly [7 0 0 0 0 0 6] [7 0 0 0 0 0 -6])))
    (is (= [25 0 -36 0 0 0 0] (SUT/mult-poly [5 -6 0 0] [5 6 0 0])))
    (is (= [-1 0 0 0 0 0 0 0 0 0 0 0 0 0 64] (SUT/mult-poly [-1 0 0 0 0 0 0 8] [1 0 0 0 0 0 0 8])))
    (is (= [-1 0 0 0 0 0 0 0 81 0 0 0 0] (SUT/mult-poly [1 0 0 0 9 0 0] [-1 0 0 0 9 0 0])))
    (is (= [-4 0 0 0 0 0 0 0 25] (SUT/mult-poly [-2 0 0 0 5] [2 0 0 0 5])))
    (is (= [-9 0 0 0 0 0 0 0 0 0 64 0 0 0 0] (SUT/mult-poly [3 0 0 0 0 8 0 0] [-3 0 0 0 0 8 0 0])))
    (is (= [25 0 0 0 0 0 -4] (SUT/mult-poly [5 0 0 -2] [5 0 0 2])))
    (is (= [4 0 -9 0 0 0 0 0 0] (SUT/mult-poly [2 3 0 0 0] [2 -3 0 0 0])))))

(deftest div-poly-test
  (testing "Divide polynomial by x, no remainder"
    (is (= (SUT/divide-poly (SUT/poly "x" [1 0 0 -3 2 0]) (SUT/poly "x" [1 0]))
           {:variable "x", :term-list [1 0 0 -3 2], :remainder nil})))
  (testing "Divide polynomial by x, with remainder"
    (is (= (SUT/divide-poly (SUT/poly "x" [5 0 0 0 9]) (SUT/poly "x" [1 0]))
           {:variable "x", :term-list [5 0 0 0], :remainder 9}))
    (is (= (SUT/divide-poly (SUT/poly "x" [2 0 0 5 4]) (SUT/poly "x" [1 0]))
           {:variable "x", :term-list [2 0 0 5], :remainder 4}))
    (is (= (SUT/divide-poly (SUT/poly "x" [6 -2 0 0 0 -1]) (SUT/poly "x" [1 0]))
           {:variable "x", :term-list [6 -2 0 0 0], :remainder -1}))))

(run-tests)