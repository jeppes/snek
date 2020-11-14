(ns snake.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]))

;; -------------------------
;; Game
;;

(def snake-length 5)
(defn place-snake-on-board
  [{snake-position :snake-position 
    treats :treats 
    rows :rows
    cols :cols
    :as game-state}]
  (let [board (vec (repeat rows (vec (repeat cols :empty))))
        board-with-snake
        (reduce (fn [game [row col]] (assoc-in game [row col] :snake))
                board
                snake-position)
        board-with-treats
        (reduce (fn [game [row col]] (assoc-in game [row col] :treat))
                board-with-snake
                treats)]
    (assoc game-state :board board-with-treats)))

(def default-snake-position (vec (take snake-length [[0 0] [1 0] [2 0] [3 0] [4 0] [5 0]])))
(def default-treats [[0 1] [0 5] [0 6]])
(def game-state
  (place-snake-on-board
    {:rows 50
     :cols 50
     :score 0
     :snake-position default-snake-position
     :treats default-treats
     :snake-direction :right ;; options: :up :down :left :right
     }))

(defn recover-state [game-state]
  (-> game-state
      (assoc :snake-position default-snake-position)
      (assoc :treats default-treats)
      (assoc :score 0)))

(defn verify-snake-does-not-self-intersect [game-state]
  (let [[head & tail] (game-state :snake-position)
        intersects-self? (->> tail
                              (drop-while #(= head %))
                              (some #(= head %)))]
    (when (not intersects-self?) game-state)))

(-> game-state
    (assoc :snake-position [[1 2] [2 1] [1 2]])
    (verify-snake-does-not-self-intersect))

(defn add-treats! [{rows :rows cols :cols :as game-state}]
  (let [random-treats (repeatedly (fn [] [(rand-int rows) (rand-int cols)]))
        treat-count (if (> 1 (rand-int 25)) 1 0)
        new-treats (first (take treat-count random-treats))
        intersects-snake? (some #(= new-treats %) (game-state :snake-position))]
    (if (and new-treats (not intersects-snake?))
      (update game-state :treats conj new-treats)
      game-state)))

(defn update-score [{snake-position :snake-position :as game-state}]
  (assoc game-state :score (* (- (count snake-position) snake-length) 10)))

(defn change-direction
  [{direction :snake-direction :as game-state} new-direction]
  (let [new-direction
        (case [direction new-direction]
          [:up :down] :up
          [:down :up] :down
          [:left :right] :left
          [:right :left] :right
          new-direction)]
    (assoc game-state :snake-direction new-direction)))

(defn step-in-direction [[rows cols] [row col] direction]
  (case direction
    :up [(mod (dec row) rows) col]
    :down [(mod (inc row) rows) col]
    :left [row (mod (dec col) cols)]
    :right [row (mod (inc col) cols)]))

(defn step-snake-forward
  [{snake-position :snake-position
    direction :snake-direction
    rows :rows
    cols :cols
    :as game-state}]
  (let [head (step-in-direction [rows cols] (first snake-position) direction)
        new-position (cons head (butlast snake-position))
        [new-row new-col] (first new-position)
        game-state (assoc game-state :snake-position new-position)]
    (when
     (and (<= 0 new-row (dec rows))
          (<= 0 new-col (dec cols)))
      game-state)))

(defn consume-treats
  [{snake-positions :snake-position treats :treats :as game-state}]
  (let [head (first snake-positions)
        new-treats (filter #(not= head %) treats)
        new-snake (if (not= new-treats treats)
                    (cons head snake-positions)
                    snake-positions)]
    (-> game-state
        (assoc :treats new-treats)
        (assoc :snake-position new-snake))))

(defn step [game-state]
  (try
    (or (some-> game-state
                (step-snake-forward)
                (verify-snake-does-not-self-intersect)
                (consume-treats)
                (add-treats!)
                (update-score)
                (place-snake-on-board))
        (recover-state game-state))
    (catch :default _ (recover-state game-state))))
  

;; -------------------------
;; Views
;;

(def square
  {:snake [:div {:class ["square" "snake"]}]
   :treat [:div {:class ["square" "treat"]}]
   :empty [:div {:class ["square" "empty"]}]})

(defn board [board]
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

(defn handle-input! [state key-code]
  (let [direction (keyboard (char key-code))]
    (when direction
      (swap! state change-direction direction))))

(defn game [game-state]
  (let [state (r/atom game-state)]
    (js/setInterval #(swap! state step) 50)
    (fn [_]
      [:div
       [board (@state :board)]
       [:button {:on-click #(swap! state step)} "step"]

       ;; These should also set the next state
       [:p (@state :score)]
       [:input {:on-key-press #(handle-input! state (.-charCode %))}]

       [:button {:on-click #(swap! state change-direction :up)} "^"]
       [:button {:on-click #(swap! state change-direction :down)} "v"]
       [:button {:on-click #(swap! state change-direction :left)} "<"]
       [:button {:on-click #(swap! state change-direction :right)} ">"]])))

(defn home-page []
  [:div
   [game game-state]])






























;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
