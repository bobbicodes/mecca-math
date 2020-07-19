(ns mecca-math.latex-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [mecca-math.latex :refer [div]]))

(deftest div-test
  (testing "Render result of polynomial division with no remainder"
    (is (= (div {:variable "x"
                 :term-list [5 0 0 0]})
           "5x^3")))
  (testing "Render result of polynomial division with positive remainder"
    (is (= (div {:variable "x"
                 :term-list [5 0 0 0]
                 :remainder 9})
           "5x^3+\\dfrac{9}{x}")))
  (testing "Render result of polynomial division with negative remainder"
    (is (= (div {:variable "x"
                 :term-list [5 0 0 0]
                 :remainder -9})
           "5x^3-\\dfrac{9}{x}"))))

(run-tests)