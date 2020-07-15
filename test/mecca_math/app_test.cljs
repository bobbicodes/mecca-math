(ns mecca-math.app-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [mecca-math.app :as SUT]))

(deftest poly->latex-test
  (testing "All coefficients greater than 1"
    (is (= "2x^2+7x+6" (SUT/poly->latex [2 7 6] "x"))))
  (testing "All negative coefficients"
    (is (= "-2x^2-7x-6" (SUT/poly->latex [-2 -7 -6] "x"))))
  (testing "All coefficients 1"
    (is (= "x^2+x+1" (SUT/poly->latex [1 1 1] "x"))))
  (testing "Coefficient of 0"
    (is (= "x+1" (SUT/poly->latex [0 1 1] "x")))))

(run-tests)