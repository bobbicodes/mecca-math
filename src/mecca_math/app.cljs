(ns mecca-math.app
  (:require [reagent.core :as r]))

(defn app []
  [:div#app
   [:h1 "MECCA-Math"]])

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
