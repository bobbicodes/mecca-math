(ns mecca-math.app
  (:require [reagent.core :as r]))

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

(def pixels (r/atom {}))

(def selected-cell (atom nil))

(def mouse-over-cell (atom nil))

(def letter-paths
  {:a "M33 157Q33 258 109 349T280 441Q331 441 370 392Q386 422 416 422Q429 422 439 414T449 394Q449 381 412 234T374 68Q374 43 381 35T402 26Q411 27 422 35Q443 55 463 131Q469 151 473 152Q475 153 483 153H487Q506 153 506 144Q506 138 501 117T481 63T449 13Q436 0 417 -8Q409 -10 393 -10Q359 -10 336 5T306 36L300 51Q299 52 296 50Q294 48 292 46Q233 -10 172 -10Q117 -10 75 30T33 157ZM351 328Q351 334 346 350T323 385T277 405Q242 405 210 374T160 293Q131 214 119 129Q119 126 119 118T118 106Q118 61 136 44T179 26Q217 26 254 59T298 110Q300 114 325 217T351 328Z"
   :b "M73 647Q73 657 77 670T89 683Q90 683 161 688T234 694Q246 694 246 685T212 542Q204 508 195 472T180 418L176 399Q176 396 182 402Q231 442 283 442Q345 442 383 396T422 280Q422 169 343 79T173 -11Q123 -11 82 27T40 150V159Q40 180 48 217T97 414Q147 611 147 623T109 637Q104 637 101 637H96Q86 637 83 637T76 640T73 647ZM336 325V331Q336 405 275 405Q258 405 240 397T207 376T181 352T163 330L157 322L136 236Q114 150 114 114Q114 66 138 42Q154 26 178 26Q211 26 245 58Q270 81 285 114T318 219Q336 291 336 325Z"
   :c "M34 159Q34 268 120 355T306 442Q362 442 394 418T427 355Q427 326 408 306T360 285Q341 285 330 295T319 325T330 359T352 380T366 386H367Q367 388 361 392T340 400T306 404Q276 404 249 390Q228 381 206 359Q162 315 142 235T121 119Q121 73 147 50Q169 26 205 26H209Q321 26 394 111Q403 121 406 121Q410 121 419 112T429 98T420 83T391 55T346 25T282 0T202 -11Q127 -11 81 37T34 159Z"})

(defn cells []
  (into [:g]
        (for [x (range 10)
              y (range 3)]
          ^{:key [x y]}
          [:rect {:width 50.5 :height 50.5 :x (* 50.5 x) :y (+ -140 (* 50.5 y)) 
                  :stroke-width 1 :stroke "black" :fill "white" :rx "10"}])))

(defn letter-cells []
  (into [:g]
        (for [x (range 10)
              y (range 3)]
          ^{:key [x y]}
          [:path {:d (:c letter-paths) :stroke "black" 
                  :transform (str "matrix(1 0 0 -1 0 0) scale(0.07) translate(" (+ 80 (* 720 x)) "," (* 720 y) ")" )}])))


(:a letter-paths)
(defn app []
  [:div#app
   [:h1 "MECCA-Math"]
   [:svg
    {:width    "480px"
     :view-box "0 -241 529 451"}
   [cells]
    [letter-cells]]])

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
