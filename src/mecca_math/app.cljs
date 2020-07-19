(ns mecca-math.app
  (:require [reagent.core :as r]
            ["katex" :as katex]
            [clojure.edn :as edn]
            [mecca-math.poly :as poly]
            [mecca-math.latex :as latex]))

(defonce var (r/atom "x"))
(defonce coeffs (r/atom "[1 4 3 0]"))
(defonce coeffs2 (r/atom "[1 5 0 0]"))
(defonce latex (r/atom "x^3+4x^2+3x"))
(defonce latex2 (r/atom "x^3+5x^2"))
(defonce output (r/atom nil))

(defn latex->html [latex]
  (.renderToString katex latex (clj->js {:throwOnError false})))

(comment
  (latex/poly->latex [2 7 -6] "x")
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
  (reset! output (latex/div (poly/div-poly poly1 poly2))))

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
          (reset! latex (latex/poly->latex (edn/read-string (-> % .-target .-value)) @var)))
     :value @coeffs
     :style {:resize "none"
             :height "20px"
             :width "34%"}}]
   [:div#latex [:textarea
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
          (reset! latex2 (latex/poly->latex (edn/read-string (-> % .-target .-value)) @var)))
     :value @coeffs2
     :style {:resize "none"
             :height "20px"
             :width "34%"}}]
   [:div#latex2 [:textarea
                {:on-change #(reset! latex2 (-> % .-target .-value))
                 :value @latex2
                 :style {:resize "none"
                         :height "20px"
                         :width "34%"}}]]
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
                           (latex/poly->latex @output @var)
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
