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
       (map-indexed #(if (not= %2 0)  [%1 %2]))
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

(defn div-terms [l1 l2]
  (if (empty? l1)
    l1
    (let [t1 (first l1)
          t2 (first l2)]
      (if (> (first t2) (first t1))
        l1
        (let [new-c (/ (last t1) (last t2))
              new-o (- (first t1) (first t2))]
          (let [rest-of-result
                (div-terms
                 (add-terms l1
                            (negate-terms
                             (mul-terms l2
                                        (list
                                         [new-o new-c]))))
                 l2)]
            (list (cons [new-o new-c]
                               (first rest-of-result))
                  (fnext rest-of-result))))))))

(defn mul-poly [a b]
  {:variable (when (= (:variable a) (:variable b))
                   (:variable a))
   :term-list (mul-terms (:term-list a)
                         (:term-list b))})

(defn divide-poly [a b]
  {:variable
   (when (= (:variable a) (:variable b))
     (:variable a))
   :term-list (div-terms (:term-list a)
                         (:term-list b))})

(defn mult-poly [poly1 poly2]
  (:term-list (sparse-to-dense (mul-poly (dense-to-sparse (poly 'x poly1)) (dense-to-sparse (poly 'x poly2))))))

(defn div-poly [poly1 poly2]
  (:term-list (sparse-to-dense (divide-poly (dense-to-sparse (poly 'x poly1)) (dense-to-sparse (poly 'x poly2))))))

(comment
  (divide-poly (poly 'x [1 0 0 -3 2 0]) (poly 'x [1 0]))
  (sub-poly [-9 0 0 0 8] [-9 2 5 0 0]))
