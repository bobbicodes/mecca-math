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
      (- 0 (last t))])
   termlist))

(defn add-terms [l1 l2]
  (vec (cond
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
                              (rest l2))))))))

(defn sub-terms [l1 l2]
  (add-terms l1 (negate-terms l2)))

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

  #_(div-terms [[4 5] [0 9]] [[1 1]]))

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

; However, we are not yet equipped to divide by more than a single term.
; What we need is the recursive evaluator.
; 
; Here are the steps:
; 
; Dividend:
; x^2-16
(dense-to-sparse (poly "x" [1 0 -16]))
; {:variable "x", :term-list [[2 1] [0 -16]]}
; 
; Divisor:
; x+4
(dense-to-sparse (poly "x" [1 4]))
; {:variable "x", :term-list [[1 1] [0 4]]}
; 
; 
; 
; Here is the transcript for a similar problem,
; which is particularly important because it is the first time
; we have needed to do full long-division
; including the multiplication and subtraction.
; (x^2 + 7x + 10) / x + 2 = x + 5

(dense-to-sparse (poly "x" [1 7 10]))
; {:variable "x", :term-list [[2 1] [1 7] [0 10]]}

(dense-to-sparse (poly "x" [1 2]))
; {:variable "x", :term-list [[1 1] [0 2]]}

(def dividend [[2 1] [1 7] [0 10]])
(def divisor [[1 1] [0 2]])

; First look at the highest degree terms.

(defn highest-degree-term
  "Returns the term in the list with the highest order with a non-zero coefficient."
  [terms]
  (last (sort-by first (filter #(not= 0 (last %)) terms))))


(highest-degree-term dividend)
; [2 1]

(highest-degree-term divisor)
; [1 1]
; 
; So then, you have an x in the divisor and an x squared in the dividend.
; x goes into x squared x times.
(div-term [2 1] [1 1]) 
; [1 1] 
; 
; Now write that in the first degree column.
; And then you take that x
; and you multiply it times the entire expression, x + 2.
(mul-terms [[1 1]] [[1 1] [0 2]])
; [[2 1] [1 2]]
; 
; So x times two is 2x.
; Put that in the first degree column.
; X times x is x squared.
; Then subtract x^2 and 2x
; from the original x^2 and 7x
; And then we will be left with 7x minus 2x is 5x.
; And then x squared minus x squared is just a zero.
; 
(sub-terms [[2 1] [1 7]] [[2 1] [1 2]])
; [[2 0] [1 5]]
; 
; And then we can bring down this plus 10.

(drop 2 dividend)
; ([0 10])
; And once again, we look at the highest degree term.
(highest-degree-term (sub-terms [[2 1] [1 7]] [[2 1] [1 2]]))
; [1 5]
(div-term [1 5] [1 1])
; [0 5]
; X goes into 5x five times.
; That's a zero degree.
; It's a constant, so I'll write it in the constant column.
; Five times two is 10.
; Five times x is five.
(mul-terms [[0 5]] [[1 1] [0 2]])
; [[1 5] [0 10]]
; And then I'll subtract these from what we have up here.
(sub-terms [[1 5] [0 10]] [[1 5] [0 10]])
; And notice, we have no remainder.
;
; So as we see, our answer is produced by 
; collecting together the results of our calls to div-term:
; 
; [[1 1] [0 5]]
; 
; Eventually we want a recursive solution,
; but for now this works:

(defn div-quad-by-linear [dividend divisor]
  (let [q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
        m1 (mul-terms [q1] divisor)
        s1 (sub-terms (take 2 dividend) m1)
        q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
        m2 (mul-terms [q2] divisor)
        s2 (sub-terms (into s1 (drop 2 dividend)) m2)]
  {:quotient [q1 q2]
   :remainder (last (last s2))}))

; (x^2-16) / (x+4) = x-4

(comment
  (div-quad-by-linear [[2 1] [1 7] [0 10]] [[1 1] [0 2]])
  (div-quad-by-linear [[2 1] [0 -16]] [[1 1] [0 4]])
  (div-quad-by-linear [[2 1] [1 -3] [0 -10]] [[1 1] [0 -5]])
  (div-quad-by-linear [[2 1] [1 6] [0 9]] [[1 1] [0 3]])
  (div-quad-by-linear [[2 1] [1 1] [0 -6]] [[1 1] [0 3]])
  (div-quad-by-linear [[2 1] [0 -9]] [[1 1] [0 -3]])
  )


; Now with remainders - 
; (x^2 + 5x + 8) / x+2
; So we're trying to take x+2 and divide it into
; x^2 + 5x + 8
; Look at the highest degree terms, the x and the x squared.
; x goes into x squared x times,
; put it in the first degree column.
; x times two is two x, x times x is x squared,
; subtract these from x squared plus five x
; and we get five x minus two x is three x.
; x squared minus x squared, that's just zero.
; Bring down that eight.
; Look at the highest-degree term.
; And we get x goes into three x three times.
; Put that in the constant column,
; or the zeroth degree column, so plus three.
; Three times two is six, three times x is three x.
; Subtract ds, and we are left with,
; let me scroll down a little bit,
; you're left with, those cancel out,
; and you're left with eight minus six,
; which is, indeed, equal to two.
; And we could say, hey, we don't really know how to divide
; x plus two into two for an arbitrary x,
; so we will say, hey, this is going to be equal to
; x plus three with a remainder of two.
; Now once again, if you wanted to rewrite that original
; expression, and you wanted it to be completely the same,
; including the domain, you would have to constrain,
; you would have to constrain the domain,
; just like that.

(div-quad-by-linear [[2 1] [1 -3] [0 9]] [[1 1] [0 -2]])
(div-quad-by-linear [[2 1] [0 -5]] [[1 1] [0 -2]])
(div-quad-by-linear [[2 1] [1 -9] [0 14]] [[1 1] [0 -5]])
(div-quad-by-linear [[2 1] [0 2]] [[1 1] [0 -1]])

; Next: Divide polynomials by linear expressions
; 
(defn div-poly-by-linear [dividend divisor]
  (let [q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
        m1 (mul-terms [q1] divisor)
        s1 (sub-terms (take 2 dividend) m1)
        q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
        m2 (mul-terms [q2] divisor)
        s2 (sub-terms (into s1 (drop 2 dividend)) m2)
        q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
        m3 (mul-terms [q3] divisor)
        s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
    {:quotient [q1 q2 q3]
     :remainder (last (last s3))}))

(div-poly-by-linear [[3 3] [1 1] [0 -11]] [[1 1] [0 1]])
(div-poly-by-linear [[3 3] [2 4] [1 -3] [0 7]] [[1 1] [0 2]])

(let [dividend [[3 3] [2 4] [1 -3] [0 7]]
      divisor [[1 1] [0 2]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (drop 2 dividend)) m2)
      q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      m3 (mul-terms [q3] divisor)
      s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
  {:quotient [q1 q2 q3]
   :remainder (last (last s3))})


; All right, now let's work through it together.
; And what we're trying to do is divide x plus two,
; into three x to the third power
; plus four x squared minus three x plus seven.
; And so, like always,
; we focus on the highest degree terms first.
; x goes into three x to the third power how many times?
; Three x squared times.
; We'd want to put that in the second degree column.
; Three x squared.
; Three x squared times two is six x squared.
; Three x squared times x is three x to the third power.
; There's something very meditative
; about algebraic long division.
; Anyway, we'd want to subtract what we just wrote
; from what we have up here.
; So let's subtract.
; And these characters cancel out.
; And then four x squared minus six x squared
; is negative two x squared.
; Bring down that negative three x.
; And now we would wanna say,
; hey, how many times does x go into negative two x squared?
; Well, it would go negative two x times.
; Put that in our first degree column.
; Negative two x times two is negative four x.
; Negative two x times x is negative two x squared.
; Now we wanna subtract what we have here in orange
; from what we have up here in teal.
; So we either put a negative around the whole thing
; or we distribute that negative and that becomes a positive,
; that becomes a positive.
; And so this is equal to, the x squared terms cancel out.
; Negative three x plus four x
; is just going to be a straight up x.
; Bring down that seven, x plus seven.
; How many times does x go into x?
; Well, one time.
; Actually, let me do a, use a new color here.
; So, how many times does x go into x?
; It goes one time.
; Put that in the constant column.
; One times two is two.
; One times x is x.
; We wanna subtract these characters.
; And we're left with seven minus two is five.
; And so, we can rewrite this whole thing as,
; we deserve I guess a little bit of a drum roll.
; Three x squared minus two x plus one,
; plus the remainder,
; five, over x plus two.
; One way to think about it is,
; hey, I have this remainder,
; I'd have to keep dividing it by x plus two
; if I really wanted to figure out exactly what this is.
; Now if I wanted these expressions to be completely identical
; I would put a condition on the domain
; that x cannot be equal to negative two
; because if x was equal to negative two,
; we'd be dividing by zero here.
; But for the purposes of this exercise,
; you just have to input this part right over here,
; you'd have to type it in,
; which I guess isn't the easiest thing to do in the world.
; But it's worth doing.
; All right, see you in the next video.

(let [dividend [[3 3] [2 10] [1 7]]
      divisor [[1 1] [0 1]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (drop 2 dividend)) m2)
      ;q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      ;m3 (mul-terms [q3] divisor)
      ;s3 (sub-terms (into s2 (drop 3 dividend)) m3)
      ]
  {:quotient [q1 q2]
   :remainder (last (last s2))})

; \begin{array}{r} 3x^2-\phantom{1}5x+\phantom{1}6 \\ x+2|\overline{3x^3+\phantom{6}x^2-\phantom{1}4x+12} \\ \mathllap{-(}\underline{3x^3+6x^2\phantom{-14x+12}\rlap )} \\ {}-5x^2-\phantom{1}4x+12 \\ \mathllap{-(}\underline{{}-5x^2-10x\phantom{+12}\rlap )} \\ 6x+12 \\ \mathllap{-(}\underline{6x+12\rlap )} \\ 0 \end{array}

(let [dividend [[3 3] [2 1] [1 -4] [0 12]]
      divisor [[1 1] [0 2]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (take 1 (drop 2 dividend))) m2)
      q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      m3 (mul-terms [q3] divisor)
      s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
  {:quotient [q1 q2 q3]
   :remainder (last (last s3))})

; \dfrac{x^3+5x^2-9x+30}{x+7}=

(let [dividend [[3 1] [2 5] [1 -9] [0 30]]
      divisor [[1 1] [0 7]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (take 1 (drop 2 dividend))) m2)
      q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      m3 (mul-terms [q3] divisor)
      s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
  {:quotient [q1 q2 q3]
   :remainder (last (last s3))})

; \begin{array}{r} x^2-\phantom{1}2x+\phantom{1}5 \\ x+7|\overline{x^3+5x^2-\phantom{1}9x+30} \\ \mathllap{-(}\underline{x^3+7x^2\phantom{-19x+30}\rlap )} \\ {}-2x^2-\phantom{1}9x+30 \\ \mathllap{-(}\underline{{}-2x^2-14x\phantom{+30}\rlap )} \\ 5x+30 \\ \mathllap{-(}\underline{5x+35\rlap )} \\ -5 \end{array}

; \dfrac{2x^3-x^2-12}{x+3}=

(let [dividend [[3 2] [2 -1] [0 -12]]
      divisor [[1 1] [0 3]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (take 1 (drop 2 dividend))) m2)
      q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      m3 (mul-terms [q3] divisor)
      s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
  {:quotient [q1 q2 q3]
   :remainder (last (last s3))})

(let [dividend [[3 1] [1 -4] [0 -15]]
      divisor [[1 1] [0 -3]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (take 1 (drop 2 dividend))) m2)
      q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      m3 (mul-terms [q3] divisor)
      s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
  {:quotient [q1 q2 q3]
   :remainder (last (last s3))})

(let [dividend [[3 4] [2 -14] [1 -7] [0 -4]]
      divisor [[1 1] [0 -4]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (take 1 (drop 2 dividend))) m2)
      q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      m3 (mul-terms [q3] divisor)
      s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
  {:quotient [q1 q2 q3]
   :remainder (last (last s3))})

(let [dividend [[3 5] [2 -22] [1 -17] [0 11]]
      divisor [[1 1] [0 -5]]
      q1 (div-term (highest-degree-term dividend) (highest-degree-term divisor))
      m1 (mul-terms [q1] divisor)
      s1 (sub-terms (take 2 dividend) m1)
      q2 (div-term (highest-degree-term s1) (highest-degree-term divisor))
      m2 (mul-terms [q2] divisor)
      s2 (sub-terms (into s1 (take 1 (drop 2 dividend))) m2)
      q3 (div-term (highest-degree-term s2) (highest-degree-term divisor))
      m3 (mul-terms [q3] divisor)
      s3 (sub-terms (into s2 (drop 3 dividend)) m3)]
  {:quotient [q1 q2 q3]
   :remainder (last (last s3))})

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
