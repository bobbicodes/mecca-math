(ns mecca-math.poly-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [mecca-math.poly :refer [add-poly mult-poly sub-poly div-poly]]))

(deftest add-and-subtract-test
  (testing "Add polynomials"
    (is (= [-1 4 -7 0 -1] (add-poly [-1 -5 -10 0 0] [9 3 0 -1])))
    (is (= [-2 -1 8 0] (add-poly [-2 -7 5 0] [6 3 0])))
    (is (= [-1 0 -2 3] (add-poly [-1 8 -3 0] [-8 1 3]))))
  (testing "Subtract polynomials."
    (is (= (sub-poly [-9 0 0 0 8] [-9 2 5 0 0])
           '(0 -2 -5 0 8)))
    (is (= (sub-poly [6 2 5] [5 -6 -11])
           '(1 8 16) ))
    (is (= (sub-poly [-7 3 -6] [3 4 4])
           '(-10 -1 -10)))
    (is (= (sub-poly [1 8 -9] [11 -4 7])
           '(-10 12 -16)))))

(deftest mult-poly-test
  (testing "Multiply binomials by polynomials"
    (is (= [1 9 23 15 0 0 0] (mult-poly [1 4 3 0] [1 5 0 0])))
    (is (= [3 0 1 12 0 4] (mult-poly [3 0 1] [1 0 0 4])))
    (is (= [1 9 23 15 0 0 0] (mult-poly [1 4 3 0] [1 5 0 0])))
    (is (= [1 7 14 8] (mult-poly [1 4] [1 3 2])))
    (is (= [2 0 12 5 0 30 0] (mult-poly [2 0 0 5] [1 0 6 0])))
    (is (= [1 6 3 6 2 0] (mult-poly [1 6 2 0] [1 0 1])))
    (is (= [2 17 11 24 0] (mult-poly [1 8] [2 1 3 0]))))
  (testing "Polynomial special products: difference of squares"
    (is (= [49 0 0 0 0 0 0 0 0 0 0 0 -36] (mult-poly [7 0 0 0 0 0 6] [7 0 0 0 0 0 -6])))
    (is (= [25 0 -36 0 0 0 0] (mult-poly [5 -6 0 0] [5 6 0 0])))
    (is (= [-1 0 0 0 0 0 0 0 0 0 0 0 0 0 64] (mult-poly [-1 0 0 0 0 0 0 8] [1 0 0 0 0 0 0 8])))
    (is (= [-1 0 0 0 0 0 0 0 81 0 0 0 0] (mult-poly [1 0 0 0 9 0 0] [-1 0 0 0 9 0 0])))
    (is (= [-4 0 0 0 0 0 0 0 25] (mult-poly [-2 0 0 0 5] [2 0 0 0 5])))
    (is (= [-9 0 0 0 0 0 0 0 0 0 64 0 0 0 0] (mult-poly [3 0 0 0 0 8 0 0] [-3 0 0 0 0 8 0 0])))
    (is (= [25 0 0 0 0 0 -4] (mult-poly [5 0 0 -2] [5 0 0 2])))
    (is (= [4 0 -9 0 0 0 0 0 0] (mult-poly [2 3 0 0 0] [2 -3 0 0 0])))))

(deftest div-poly-test
  (testing "Divide polynomial by x"
    (is (= [1 0 0 -3 2] (div-poly [1 0 0 -3 2 0] [1 0])))))

(run-tests)