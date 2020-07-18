(ns mecca-math.latex)

(defn poly->latex
  "Takes a sequence of polynomial terms in dense-form
   and a variable letter, returns a string of LaTeX output."
  [coeffs v]
  (let [s (apply str
                 (loop [terms coeffs tex []]
                   (if (empty? terms)
                     tex
                     (recur (rest terms)
                            (conj tex
                                  (when-not (zero? (first terms))
                                    (str
                                     (if (pos? (first terms))
                                       "+"
                                       "-")
                                     (when (or
                                            (> (Math/abs (first terms)) 1)
                                            (= 1 (count terms)))
                                       (Math/abs (first terms)))
                                     (when (> (count terms) 1)
                                       v)
                                     (when (> (count terms) 2)
                                       (str "^" (dec (count terms)))))))))))]
    (if (= "+" (first s))
      (subs s 1)
      s)))

(defn frac
  "Takes a numerator and a denominator and outputs a LaTex string."
  [numer denom]
  (str "\\dfrac{" (str numer) "}"
             "{" (str denom) "}"))

(defn trim-sign [s]
  (if (= \+ (first s))
    (subs s 1)
    s))

(defn poly-with-remainder
  "Takes a vector of coefficients and variable representing a univariate polynomial,
   and a remainder produced from a previous division operation.
   Renders a string of LaTeX representing a polynomial
   plus or minus the remainder over the variable."
  [coeffs var remainder]
  (trim-sign (str (poly->latex coeffs var)
                  (frac remainder var))))

(comment
 (poly-with-remainder [5 0 0 0] "x" 9)
  
  )