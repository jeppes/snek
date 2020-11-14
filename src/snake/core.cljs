(ns snake.core
  (:require
   [snake.game :as game]
   [snake.view :as view]
   [reagent.dom :as d]))

;; -------------------------
;; Initialize app
;; 
(defn home-page []
  [:div
   [view/game game/game-state]])

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
