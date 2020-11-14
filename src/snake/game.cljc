(ns snake.game)

(defn- place-snake-on-board
  [{snake :snake
    treats :treats
    rows :rows
    cols :cols
    :as game-state}]
  (let [board (vec (repeat rows (vec (repeat cols :empty))))
        board-with-snake
        (reduce (fn [game [row col]] (assoc-in game [row col] :snake)) board snake)
        board-with-treats
        (reduce (fn [game [row col]] (assoc-in game [row col] :treat)) board-with-snake treats)]
    (assoc game-state :board board-with-treats)))

(def snake-length 5)
(def default-snake (vec (take snake-length [[0 0] [1 0] [2 0] [3 0] [4 0] [5 0]])))
(def default-treats [[0 1] [0 5] [0 6]])
(def game-state
  (place-snake-on-board
   {:rows 25
    :cols 25
    :score 0
    :snake default-snake
    :treats default-treats
    :snake-direction :right ;; options: :up :down :left :right
    }))

(defn- recover-state [game-state]
  (-> game-state
      (assoc :snake default-snake)
      (assoc :treats default-treats)
      (assoc :score 0)))

(defn- verify-snake-does-not-self-intersect [game-state]
  (let [[head & tail] (game-state :snake)
        intersects-self? (->> tail
                              (drop-while #(= head %))
                              (some #(= head %)))]
    (when (not intersects-self?) game-state)))

(defn- add-treats! [{rows :rows cols :cols :as game-state}]
  (let [random-treats (repeatedly (fn [] [(rand-int rows) (rand-int cols)]))
        new-treat? (if (> 1 (rand-int 25)) 1 0)
        new-treat (first (take new-treat? random-treats))
        intersects-snake? (some #(= new-treat %) (game-state :snake))]
    (if (and new-treat (not intersects-snake?))
      (update game-state :treats conj new-treat)
      game-state)))

(defn update-score [{snake :snake :as game-state}]
  (assoc game-state :score (-> (count snake)
                               (- snake-length)
                               (* 10))))

(defn- step-in-direction [[rows cols] [row col] direction]
  (case direction
    :up [(mod (dec row) rows) col]
    :down [(mod (inc row) rows) col]
    :left [row (mod (dec col) cols)]
    :right [row (mod (inc col) cols)]))

(defn- step-snake-forward
  [{snake :snake
    direction :snake-direction
    rows :rows
    cols :cols
    :as game-state}]
  (let [head (step-in-direction [rows cols] (first snake) direction)
        new-position (cons head (butlast snake))
        [new-row new-col] (first new-position)
        game-state (assoc game-state :snake new-position)]
    (and (<= 0 new-row (dec rows))
         (<= 0 new-col (dec cols))
         game-state)))

(defn- consume-treats
  [{snakes :snake treats :treats :as game-state}]
  (let [head (first snakes)
        new-treats (filter #(not= head %) treats)
        new-snake (if (not= new-treats treats)
                    (cons head snakes)
                    snakes)]
    (-> game-state
        (assoc :treats new-treats)
        (assoc :snake new-snake))))

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
