(ns mecca-math.app
  (:require [reagent.core :as r]
            ["katex" :as katex]
            [clojure.edn :as edn]))

(defonce var (r/atom "x"))
(defonce coeffs (r/atom "[2 7 -6]"))
(defonce latex (r/atom "2x^2+7x-6"))

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

(defn app []
  [:div#app
   [:h1 "Polynomial playground"]
   [:h2 "Coefficients:"]
   [:div [:textarea
          {:on-change 
           #(do (reset! coeffs (-> % .-target .-value))
                (reset! latex (poly->latex (edn/read-string (-> % .-target .-value)) @var)))
           :value @coeffs
           :style {:resize "none"
                   :height "20px"
                   :width "34%"}}]]
   [:h2 "Variable:"]
   [:textarea
    {:on-change #(reset! var (-> % .-target .-value))
     :value @var
     :style {:resize "none"
             :height "20px"
             :width "5%"}}]
   [:h2 "LaTeX:"]
   [:div [:textarea
          {:on-change #(reset! latex (-> % .-target .-value))
           :value @latex
           :style {:resize "none"
                   :height "50px"
                   :width "67%"}}]]
   [:h2 "Rendered output:"]
   [:div {:dangerouslySetInnerHTML {:__html (latex->html @latex)}}]])

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
