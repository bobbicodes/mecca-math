(ns mecca-math.latex)

(defn trim-sign [s]
  (if (= \+ (first s))
    (subs s 1)
    s))

(defn poly->latex
  "Takes a sequence of polynomial terms in dense-form
   and a variable letter, returns a string of LaTeX output."
  [coeffs v]
  (trim-sign (let [s (apply str
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
                 s))))

(defn frac
  "Takes a numerator and a denominator and outputs a LaTex string."
  [numer denom]
  (str "\\dfrac{" numer "}"
       "{" denom "}"))

(defn poly-with-remainder
  "Takes a vector of coefficients and variable representing a univariate polynomial,
   and an integer remainder produced from a previous division operation.
   Renders a string of LaTeX representing a polynomial
   plus or minus a fraction made up of the remainder over the variable."
  [coeffs var remainder]
  (trim-sign (str (poly->latex coeffs var)
                  (if (pos? remainder) "+" "-")
                  (frac (Math/abs remainder) var))))

(defn div
  "Renders result of polynomial division."
  [{:keys [term-list variable remainder]}]
  (if remainder
    (poly-with-remainder term-list variable remainder)
    (poly->latex term-list variable)))

(comment

  (div {:variable "x"
        :term-list [5 0 0 0]
        :remainder -9})

  (div {:variable "x"
        :term-list [5 0 0 0]
        :remainder 9})
  
  (div {:variable "x"
        :term-list [5 0 0 0]}))
