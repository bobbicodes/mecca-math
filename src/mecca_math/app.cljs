(ns mecca-math.app
  (:require [reagent.core :as r]
            [mecca-math.letters :as letters]
            [mecca-math.numbers :as numbers]
            [mecca-math.symbols :as symbols]
            ["katex" :as katex]))

(defonce latex (r/atom ""))

(defn latex->html [latex]
  (.renderToString katex latex (clj->js {:throwOnError false})))

(defn svg-paths
  ([paths]
   (svg-paths nil paths 0 0 1))
  ([attrs paths]
   (svg-paths attrs paths 0 0 1))
  ([paths x y]
   (svg-paths nil paths x y 1))
  ([paths x y scale]
   (svg-paths nil paths x y scale))
  ([attrs paths x y scale]
   (into [:g (merge attrs
                    {:transform (str "scale(" scale ") translate(" x "," y ")")})]
         (for [[color path] paths]
           ^{:key [color path]}
           [:path {:stroke color
                   :d      path}]))))

(def selected-cell (atom nil))

(def mouse-over-cell (atom nil))

(defn letter-row-1 []
  (into [:g]
        (for [x (range 10)]
          ^{:key [x]}
          [:path {:d ((get [:q :w :e :r :t :y :u :i :o :p] x) letters/paths) :stroke "black"
                  :transform (str "matrix(1 0 0 -1 0 0) scale(0.05) translate(" (+ 170 (* 1060 x)) "," 2150 ")")}])))

(defn letter-row-2 []
  (into [:g]
        (for [x (range 9)]
          ^{:key [x]}
          [:path {:d ((get [:a :s :d :f :g :h :j :k :l] x) letters/paths) :stroke "black"
                  :transform (str "matrix(1 0 0 -1 0 0) scale(0.05) translate(" (+ 170 (* 1060 x)) "," 1060 ")")}])))

(defn letter-row-3 []
  (into [:g]
        (for [x (range 7)]
          ^{:key [x]}
          [:path {:d ((get [:z :x :c :v :b :n :m] x) letters/paths) :stroke "black"
                  :transform (str "matrix(1 0 0 -1 0 0) scale(0.05) translate(" (+ 150 (* 1060 x)) "," -20 ")")}])))

(defn letter-cells []
  (into [:g]
        (for [x (range 10)
              y (range 3)]
          ^{:key [x y]}
          [:path {:d (:equals symbols/paths) :stroke "black"
                  :transform (str "matrix(1 0 0 -1 0 0) scale(0.05) translate(" (+ 100 (* 1060 x)) "," (* 1060 y) ")")}])))

(defn cells []
  (into [:g]
        (for [x (range 10)
              y (range 3)]
          ^{:key [x y]}
          [:rect {:width 50.5 :height 50.5 :x (* 53.1 x) :y (+ -145 (* 53.1 y)) 
                  :stroke-width 1 :stroke "black" :fill "white" :rx "10"}])))

(defn app []
  [:div#app
   [:h1 "MECCA-Math"]
   [:svg
    {:width    "480px"
     :view-box "0 -241 529 451"}
   [cells]
    [letter-row-1]
    [letter-row-2]
    [letter-row-3]]
[:div [:textarea
      {:on-change #(reset! latex (-> % .-target .-value))
       :value @latex
       :style {:resize "none"
               :height "50px"
               :width "100%"}}]]
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
