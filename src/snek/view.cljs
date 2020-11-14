(ns snek.view
  (:require
   [snek.game :as game]
   [reagent.core :as r]))

(def square
  {:snake [:div {:class ["square" "snake"]}]
   :treat [:div {:class ["square" "treat"]}]
   :empty [:div {:class ["square" "empty"]}]})

(defn- board [board]
  [:div
   (for [row (range (count board))]
     ^{:key (str "Row-" row)}
     [:div.row
      (for [col (range (count (get board row)))]
        ^{:key (str row "-" col)}
        [square (get (get board row) col)])])])

(def keyboard
  {\i :up
   \k :down
   \j :left
   \l :right})

(defn- handle-input! [state key-code]
  (let [direction (keyboard (char key-code))]
    (when direction
      (swap! state game/change-direction direction))))

(defn game [game-state]
  (let [state (r/atom game-state)]
    (js/setInterval #(swap! state game/step) 50)
    (fn [_]
      [:div
       [board (@state :board)]
       #_[:button {:on-click #(swap! state game/step)} "step"]

       [:p (@state :score)]
       [:input {:on-key-press #(handle-input! state (.-charCode %))}]

       [:button {:on-click #(swap! state game/change-direction :up)} "^"]
       [:button {:on-click #(swap! state game/change-direction :down)} "v"]
       [:button {:on-click #(swap! state game/change-direction :left)} "<"]
       [:button {:on-click #(swap! state game/change-direction :right)} ">"]])))