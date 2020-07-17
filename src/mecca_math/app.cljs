(ns mecca-math.app
  (:require [reagent.core :as r]
            ["katex" :as katex]
            [clojure.edn :as edn]
            [mecca-math.poly :as poly]))

(defonce var (r/atom "x"))
(defonce coeffs (r/atom "[1 4 3 0]"))
(defonce coeffs2 (r/atom "[1 5 0 0]"))
(defonce latex (r/atom "x^3+4x^2+3x"))
(defonce latex2 (r/atom "x^3+5x^2"))
(defonce output (r/atom nil))

(defn latex->html [latex]
  (.renderToString katex latex (clj->js {:throwOnError false})))

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

(comment
  (poly->latex [2 7 -6] "x")
  )

(defn button [label onclick]
  [:button
   {:on-click onclick}
   label])

(defn add [poly1 poly2]
  (reset! output (poly/add-poly poly1 poly2)))

(defn subtract [poly1 poly2]
  (reset! output (poly/sub-poly poly1 poly2)))

(defn multiply [poly1 poly2]
  (reset! output (poly/mult-poly poly1 poly2)))

(defn div [poly1 poly2]
  (reset! output (poly/div-poly poly1 poly2)))

(defn app []
  [:div#app
   [:h2 "Enter 2 polynomials:"]
   [:div "Variable: "
    [:textarea
     {:on-change #(reset! var (-> % .-target .-value))
      :value @var
      :style {:resize "none"
              :height "16px"
              :width "4%"}}]]
   [:p]
   [:textarea
    {:on-change
     #(do (reset! coeffs (-> % .-target .-value))
          (reset! latex (poly->latex (edn/read-string (-> % .-target .-value)) @var)))
     :value @coeffs
     :style {:resize "none"
             :height "20px"
             :width "34%"}}]
   #_[:div [:textarea
            {:on-change #(reset! latex (-> % .-target .-value))
             :value @latex
             :style {:resize "none"
                     :height "20px"
                     :width "34%"}}]]
   [:div {:dangerouslySetInnerHTML {:__html (latex->html @latex)}}]
   [:p]
   [:textarea
    {:on-change
     #(do (reset! coeffs2 (-> % .-target .-value))
          (reset! latex2 (poly->latex (edn/read-string (-> % .-target .-value)) @var)))
     :value @coeffs2
     :style {:resize "none"
             :height "20px"
             :width "34%"}}]
   [:div {:dangerouslySetInnerHTML {:__html (latex->html @latex2)}}]
   [button "Add" #(add (edn/read-string @coeffs) (edn/read-string @coeffs2))]
   [button "Subtract" #(subtract (edn/read-string @coeffs) (edn/read-string @coeffs2))]
   [button "Multiply" #(multiply (edn/read-string @coeffs) (edn/read-string @coeffs2))]
   [button "Divide" #(div (edn/read-string @coeffs) (edn/read-string @coeffs2))]
   [:p]
   (when @output
     [:div {:dangerouslySetInnerHTML
            {:__html (latex->html
                      (str "\\huge{"
                           (poly->latex @output @var)
                           "}"))}}])])

(defn render []
  (r/render [app]
            (.getElementById js/document "root")))

(defn ^:dev/after-load start []
  (render)
  (js/console.log "start"))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
