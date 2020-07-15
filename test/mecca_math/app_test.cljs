(ns mecca-math.app-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [mecca-math.app :as SUT]))

(deftest poly->latex-test
  (testing "All coefficients greater than 1"
    (is (= "2x^2+7x+6" (SUT/poly->latex [2 7 6] "x"))))
  (testing "All negative coefficients"
    (is (= "-2x^2-7x-6" (SUT/poly->latex [-2 -7 -6] "x")))))

(run-tests)