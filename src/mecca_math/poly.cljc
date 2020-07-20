(ns mecca-math.poly)

(defn poly [var coeffs]
  {:variable var :term-list coeffs})

(defn sparse-to-dense [p]
  (let [poly (reverse (:term-list p))
        diff-terms (map #(vector (dec (- (first %1) (first %2))) (second %1))
                        (next poly) poly)]
    {:variable (:variable p)
      :term-list (->> diff-terms
         (cons (first poly))
         (mapcat #(concat (repeat (first %) 0) [(second %)]))
         (reverse)
         vec)}))

(defn dense-to-sparse [p]
  {:variable (:variable p)
    :term-list
    (->> (:term-list p)
       (reverse)
       (map-indexed #(when (not= %2 0)  [%1 %2]))
       (filter some?)
       (reverse)
       vec)})

(defn negate-terms [termlist]
  (map
   (fn [t]
     [(first t)
     (- 0 (last t))]))
   termlist)

(defn add-terms [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) '()
    (empty? l1) l2
    (empty? l2) l1
    :else
    (let [t1 (first l1)
          t2 (first l2)]
      (cond
        (> (first t1) (first t2)) (cons t1 (add-terms (rest l1) l2))
        (< (first t1) (first t2)) (cons t2 (add-terms l1 (rest l2)))
        :else
        (cons [(first t1)
                      (+ (last t1) (last t2))]
                     (add-terms (rest l1)
                                (rest l2)))))))

(defn mul-term-by-all-terms [t1 l]
  (if (empty? l)
    l
    (let [t2 (first l)]
      (cons
       [(+ (first t1) (first t2))
                  (* (last t1) (last t2))]
       (mul-term-by-all-terms t1 (rest l))))))

(defn mul-terms [l1 l2]
  (if (empty? l1)
    l1
    (add-terms (mul-term-by-all-terms (first l1) l2)
               (mul-terms (rest l1) l2))))

(defn add-poly [poly1 poly2]
  (:term-list
   (sparse-to-dense
    (poly 'x
          (add-terms
           (:term-list
            (dense-to-sparse (poly 'x poly1)))
           (:term-list (dense-to-sparse (poly 'x poly2))))))))

(comment

  (:term-list (sparse-to-dense (poly 'x (add-terms (:term-list (dense-to-sparse (poly 'x [-1 -5 -10 0 0])))
                                                   (:term-list (dense-to-sparse (poly 'x [9 3 0 -1])))))))
  
  (add-poly [-1 -5 -10 0 0] [9 3 0 -1])
  (add-poly [-2 -7 5 0] [6 3 0])
  (add-poly [-1 8 -3 0] [-8 1 3])
  
  )

(defn sub-poly [poly1 poly2]
  (map #(- % %2) poly1 poly2))

; https://www.khanacademy.org/math/algebra2/x2ec2f6f830c9fb89:poly-div/x2ec2f6f830c9fb89:poly-div-by-x/e/poly-by-x-no-remainders
; Divide the polynomials.
; Your answer should be a polynomial.

; (x^5 - 3x^2 + 2x) / x
; 
; Usually, there are many different ways to divide polynomials. Here, we will use the method of factoring and canceling common factors.
; Try to factor the numerator, and see if you end up with a common factor to cancel with the denominator.
; 
; x(x^4 - 3x + 2)





; Another way to perform this division is by splitting the quotient into multiple quotients:

; (x^5 / x) - (3x^2 / x) + (2x / x)

; = x^4 - 3x + 2

(dense-to-sparse (poly 'x [5 0 0 0 9]))
(dense-to-sparse (poly 'x [1 0]))

(dense-to-sparse (poly 'x [5 0 0 0]))

(defn div-term
  "Takes 2 vectors containing the order and coefficient (sparse-form)
   of 2 polynomial terms, and divides the first by the second."
  [t1 t2]
  (if (> (first t2) (first t1))
    {:error (str "Cannot divide order " (first t1) " by order " (first t2))
     :remainder (last t1)}
    [(- (first t1) (first t2))
     (/ (last t1) (last t2))]))

(comment
  (div-term [4 5] [1 1]))

; Divide polynomials by x (with remainders)
; https://www.khanacademy.org/math/algebra2/x2ec2f6f830c9fb89:poly-div/x2ec2f6f830c9fb89:poly-div-by-x/e/poly-by-x-remainders

; Divide the polynomials:
; (5x^4+9) / x	
; The answer must be in the form:
; p(x)+\dfrac{k}{x}
; where p is a polynomial and k is an integer.
; LaTeX rendering is handled by the latex/div function,
; which takes a map containing a variable, term-list
; and optional integer remainder.
; so here our division function needs to output:

; {:variable "x"
;  :term-list [[3 5]]
;  :remainder 9}

(dense-to-sparse (poly "x" [5 0 0 0 9]))

; 1st polynomial's term-list:
; [[4 5] [0 9]]
; 
; Divide this by x:
; [[1 1]]

(div-term [4 5] [1 1])
; 5x^4 divided by x is 5x^3

(div-term [0 9] [1 1])

#_(defn div-terms [l1 l2]
  (if (empty? l1)
    l1
    (let [t1 (first l1)
          t2 (first l2)]
      (if (> (first t2) (first t1))
        l1
        (let [new-c (/ (last t1) (last t2))
              new-o (- (first t1) (first t2))
              rest-of-result (div-terms
                              (add-terms l1
                                         (negate-terms
                                          (mul-terms l2
                                                     [[new-o new-c]])))
                              l2)]
          [(cons [new-o new-c]
                 (first rest-of-result))
           (fnext rest-of-result)])))))

(defn div-terms [l1 l2]
  (mapv #(div-term % (first l2)) l1))

(comment

  (div-terms [[4 5] [0 9]] [[1 1]]))

; Cool so dividing polynomials by x with remainders works.
; Next is Divide quadratics by linear expressions (no remainders).
; The answer is a polynomial.
; (x^2-16) / (x+4) 

;           x-4
;    ----------
; x+4|x^2+0x−16
;  - (x^2+4x)
;     --------
;        −4x−16
;     - (−4x−16)
;       -------
;            0

; However, we are not equipped to divide by more than a single term.
; 
; Here is the transcript for a similar problem,
; (x^2 + 7x +10) / x + 2 = x + 5

; First look at the highest degree terms.
; So then, you have an x in the divisor and an x squared in the dividend.
; x goes into x squared x times.
; Now write that in the first degree column.
; And then you take that x
; and you multiply it times the entire expression, x + 2.
; So x times two is 2x.
; Put that in the first degree column.
; X times x is x squared.
; Then subtract x^2 and 2x
; from the original x^2 and 7x
; And then we will be left with 7x minus 2x is 5x.
; And then x squared minus x squared is just a zero.
; And then we can bring down this plus 10.
; And once again, we look at the highest degree term.
; X goes into 5x five times.
; That's a zero degree.
; It's a constant, so I'll write it in the constant column.
; Five times two is 10.
; Five times x is five.
; And then I'll subtract these from what we have up here.
; And notice, we have no remainder.

(defn mul-poly [a b]
  {:variable (when (= (:variable a) (:variable b))
                   (:variable a))
   :term-list (mul-terms (:term-list a)
                         (:term-list b))})

(defn divide-poly [a b]
  (let [div (div-terms (:term-list (dense-to-sparse a))
                      (:term-list (dense-to-sparse b)))]
    {:variable (when (= (:variable a) (:variable b))
                 (:variable a))
     :term-list (:term-list (sparse-to-dense (poly "x" (vec (if (:remainder (last div))
                                                              (butlast div)
                                                              div)))))
     :remainder (when (:remainder (last div))
                  (:remainder (last div)))}))

(comment

  (last (div-terms (:term-list (dense-to-sparse (poly "x" [5 0 0 0 9])))
                   (:term-list (dense-to-sparse (poly "x" [1 0])))))

  (sparse-to-dense (poly "x" [[3 5]]))
  (divide-poly (poly "x" [1 0 0 -3 2 0]) (poly "x" [1 0]))
  (divide-poly (poly "x" [5 0 0 0 9]) (poly "x" [1 0]))
  (divide-poly (poly "x" [2 0 0 5 4]) (poly "x" [1 0]))
  )

(defn mult-poly [poly1 poly2]
  (:term-list (sparse-to-dense (mul-poly (dense-to-sparse (poly 'x poly1)) (dense-to-sparse (poly 'x poly2))))))

(defn div-poly [poly1 poly2]
  (:term-list (sparse-to-dense (divide-poly (dense-to-sparse (poly 'x poly1)) (dense-to-sparse (poly 'x poly2))))))

(comment

  (poly "x" [5 0 0 0 9])
  (poly "x" [1 0])

  (div-poly [1 0 0 -3 2 0] [1 0])

  (:term-list (dense-to-sparse (poly 'x [1 0 0 -3 2 0])))
  (:term-list (dense-to-sparse (poly 'x [1 0])))

  (:term-list (sparse-to-dense (divide-poly (dense-to-sparse (poly 'x [1 0 0 -3 2 0])) (dense-to-sparse (poly 'x [1 0])))))
  (sub-poly [-9 0 0 0 8] [-9 2 5 0 0]))

(defn prime-factors
  ([n] (prime-factors 2 n))
  ([f n]
   (when (> n 1)
     (if (zero? (mod n f))
       (cons f (prime-factors f (/ n f)))
       (recur (inc f) n)))))

(defn perfect-squares [s]
  (loop [items (sort s) pairs []]
    (if (empty? items) pairs
        (if (= (first items) (second items))
          (recur (drop 2 items) (conj pairs (first items)))
          (recur (rest items) pairs)))))

(defn simplify-sqrt [sqrt]
  (let [sq (reduce * (perfect-squares (prime-factors sqrt)))]
    [sq (/ sqrt (* sq sq))]))
