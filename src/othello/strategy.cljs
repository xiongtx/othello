(ns othello.strategy
  (:require [clojure.set :as set]
            [othello.game :as game]))

(defn random-strategy
  [player board]
  (rand-nth (game/legal-moves player board)))

(defn maximizer
  "Return a strategy that:

  - Considers every legal move
  - Applies eval-f to each resulting board, and
  - Chooses the move for which eval-fn returns the best score.

  eval-fn takes two parameters: player and board."
  [eval-fn]
  (fn [player board]
    (let [moves (game/legal-moves player board)
          scores (->> moves
                      (map #(game/make-move % player board))
                      (map #(eval-fn player %)))
          best (apply max scores)]
      (nth moves (.indexOf scores best)))))

(defn maximize-difference
  "Strategy that maximizes difference in pieces."
  [player board]
  ((maximizer game/count-difference) player board))

(def weights
  [0   0   0  0  0  0  0   0   0 0
   0 120 -20 20  5  5 20 -20 120 0
   0 -20 -40 -5 -5 -5 -5 -40 -20 0
   0  20  -5 15  3  3 15  -5  20 0
   0   5  -5  3  3  3  3   -5  5 0
   0   5  -5  3  3  3  3   -5  5 0
   0  20  -5 15  3  3 15  -5  20 0
   0 -20 -40 -5 -5 -5 -5 -40 -20 0
   0 120 -20 20  5  5 20 -20 120 0
   0   0   0  0  0  0  0   0   0 0])

(defn weighted-squares
  "Sum of weights of player's squares vs. opponent's."
  [player board]
  (loop [w 0
         squares game/all-squares]
    (if-let [square (first squares)]
      (let [piece (nth board square)
            mult (condp = piece
                   player 1
                   (game/opponent player) -1
                   0)]
        (recur (* mult (nth weights square))
               (rest squares)))
      w)))

(defn modified-weighted-squares
  "Like `weighted-squares`, but doesn't penalize for moving near an occupied
  corner."
  [player board]
  (let [w (weighted-squares player board)
        modifiers (for [corner [11 18 81 88]
                        :when (not= game/blank (nth board corner))
                        c (game/neighbors corner)
                        :when (not= game/blank (nth board c))]
                    (* (- 5 (nth weights c))
                       (if (= (nth board c) player) 1 -1)))]
    (apply + w modifiers)))

(defn maximize-weights
  "Strategy that maximizes sum of weights of player's squares vs. opponent's.

  See: `weighted-squares`."
  [player board]
  ((maximizer weighted-squares) player board))

(defn maximize-modified-weights
  "Strategy that maximizes sum of modified weights of player's squares vs. opponent's.

  See: `modified-weighted-squares`."
  [player board]
  ((maximizer modified-weighted-squares) player board))

(def winning-value ##Inf)

(def losing-value ##-Inf)

(defn final-value
  "Is this a win, loss, or draw for player?"
  [player board]
  (case (Math/sign (game/count-difference player board))
    -1 losing-value
    0 0
    1 winning-value))

(defn minimax
  "Return the best move and its value for player according to eval-fn,
  searching ply levels deep and backing up values."
  [player board ply eval-fn]
  (if (zero? ply)
    {:best-val (eval-fn player board)}
    (let [opp (game/opponent player)
          moves (game/legal-moves player board)]
      (if (empty? moves)
        (if (game/any-legal-move? opp board)
          {:best-val (- (:best-val (minimax opp board (dec ply) eval-fn)))}
          {:best-val (final-value player board)})
        (let [vals (->> moves
                        (map #(game/make-move % player board))
                        (map #(- (:best-val (minimax opp % (dec ply) eval-fn)))))]
          (let [best-val (apply max vals)
                best-move (nth moves (.indexOf vals best-val))]
            {:best-val best-val
             :best-move best-move}))))))

(defn minimax-searcher
  "Return strategy that searches ply levels, then uses eval-fn."
  [ply eval-fn]
  (fn [player board]
    (let [{:keys [best-val best-move]} (minimax player board ply eval-fn)]
      best-move)))

(defn alpha-beta
  "Find the best move for player according to eval-fn.

  Searches ply levels deep and backing up values, using cutoffs whenever
  possible."
  [player board achievable cutoff ply eval-fn]
  (if (zero? ply)
    {:best-val (eval-fn player board)}
    (let [opp (game/opponent player)
          moves (game/legal-moves player board)]
      (if (empty? moves)
        (if (game/any-legal-move? opp board)
          (-> (alpha-beta opp board
                          (- cutoff) (- achievable)
                          (dec ply) eval-fn)
              (update :best-val -))
          {:best-val (final-value player board)})
        (loop [achievable achievable
               best-move (first moves)
               moves moves]
          (if (or (empty? moves)
                  (>= achievable cutoff))
            {:best-val achievable
             :best-move best-move}
            (let [move (first moves)
                  new-board (game/make-move move player board)
                  val (- (:best-val (alpha-beta opp new-board
                                                (- cutoff) (- achievable)
                                                (dec ply) eval-fn)))
                  [achievable best-move] (if (> val achievable)
                                           [val move]
                                           [achievable best-move])]
              (recur achievable best-move (rest moves)))))))))

(defn alpha-beta-searcher
  "Return strategy that searches to depth, then uses eval-fn."
  [depth eval-fn]
  (fn [player board]
    (:best-move (alpha-beta player board
                            losing-value winning-value
                            depth eval-fn))))

(defrecord Node [square board value])

(defn legal-nodes
  "Return list of legal moves, each packed into a `Node`.

  Nodes are sorted by value given by eval-fn."
  [player board eval-fn]
  (->> (game/legal-moves player board)
       (map #(let [new-board (game/make-move % player board)]
               (->Node % new-board (eval-fn player new-board))))
       (sort-by :value >)))

(defn alpha-beta-2
  "α-β search, sorting moves by eval-fn."
  [player node achievable cutoff ply eval-fn]
  (if (zero? ply)
    {:best-val (:value node)
     :best-node node}
    (let [opp (game/opponent player)
          board (:board node)
          nodes (legal-nodes player board eval-fn)]
      (if (empty? nodes)
        (if (game/any-legal-move? opp board)
          {:best-val (- (:best-val (alpha-beta-2 opp (update node :value -)
                                                 (- cutoff) (- achievable)
                                                 (dec ply) eval-fn)))}
          {:best-val (final-value player board)})
        (loop [achievable achievable
               best-node (first nodes)
               nodes nodes]
          (if (or (empty? nodes)
                  (>= achievable cutoff))
            {:best-val achievable
             :best-node best-node}
            (let [node (first nodes)
                  new-node (update node :value -)
                  val (- (:best-val (alpha-beta-2 opp new-node
                                                  (- cutoff) (- achievable)
                                                  (dec ply) eval-fn)))]
              (recur (if (> val achievable) val achievable)
                     (if (> val achievable) node best-node)
                     (rest nodes)))))))))

(defn alpha-beta-searcher-2
  "Return a strategy that does α-β search with sorted moves."
  [depth eval-fn]
  (fn [player board]
    (let [result (alpha-beta-2 player (->Node nil board (eval-fn player board))
                               losing-value winning-value
                               depth eval-fn)]
      (get-in result [:best-node :square]))))

(defn neighboring-opp?
  "Does square have an opponent's piece as a neighbor?"
  [player board square]
  (->> (game/neighbors square)
       (map #(nth board %))
       (some #(= (game/opponent player) %))
       boolean))

(defn mobility
  "Return [current potential] mobility for a player.

   Current mobility is number of legal moves.

   Potential mobility is number of blank squares adjacent to opponent."
  [player board]
  (let [opp (game/opponent player)]
    [(count (game/legal-moves player board))
     (->> (game/blank-squares board)
          (filter #(neighboring-opp? player board %))
          count)]))

;; Values to player-to-move for edge positions
(defonce edge-table
  (make-array (Math/pow 3 10)))

(def edge-and-x-lists
  "Four edges (with their x-squares)."
  [[22 11 12 13 14 15 16 17 18 27]
   [72 81 82 83 84 85 86 87 88 77]
   [22 11 21 31 41 51 61 71 81 72]
   [27 18 28 38 48 58 68 78 88 77]])

(defn edge-index
  "The index counts 1 for player, 2 for opponent on each square--summed as a
  base 3 number."
  [player board squares]
  (reduce (fn [index square]
            (+ (* 3 index)
               (condp = (nth board square)
                 game/blank 0
                 player 1
                 2)))
          0
          squares))

(def top-edge (first edge-and-x-lists))

(def corner-xs
  "Map of {corner x-square} values."
  {11 22
   18 27
   81 72
   88 77})

(defn corner?
  [square]
  (contains? corner-xs square))

(defn x-square?
  [square]
  (boolean (some #{square} (vals corner-xs))))

(defn x-square-for
  [corner]
  (get corner-xs corner))

(defn corner-for
  [x-square]
  (get (set/map-invert corner-xs) x-square))

(defn count-edge-neighbors
  "Number of two neighbors, left and right of square, occupied by player."
  [player board square]
  (->> [(dec square) (inc square)]
       (map #(nth board %))
       (filter #(= player %))
       count))

(defn edge-move-probability
  "Return probability player can move to square.

  There are four cases.

  1. Since we don't know anything about the interior of the board, we assume
  each player has a 50% chance of being able to play an x-square.

  2. If we can show that a more is legal (because it flips opponent pieces on
  the edge), then it has 100% probability.

  3. For corner squares, assign

     - 90% chance if the opponent occupies the x-square

     - 10% if it is blank

     - 0.1% if we occupy it

  4. Otherwise, probability is determined by the two neighboring squares

     - If a square is next to one or more opponents it is more likely we can
  move there; if it is next to our pieces it is less likely.

     - If it is legal for the opponent to move into the square, then the
  chances are cut in half (though we may still be able to move there, since we
  move first.)"
  [player board square]
  (cond
    (x-square? square) .5
    (game/legal? square player board) 1
    (corner? square) (condp = (nth board (x-square-for square))
                       game/blank .1
                       player .001
                       .9)
    :else (let [opp (game/opponent player)
                probs [[.1 .4 .7]
                       [.05 .3]
                       [.01]]
                neighbor-player-squares (count-edge-neighbors player board square)
                neighbor-opp-squares (count-edge-neighbors opp board square)]
            (/ (get-in probs [neighbor-player-squares neighbor-opp-squares])
               (if (game/legal? square opp board) 2 1)))))

(defn possible-edge-move
  "Return a [prob val] for a possible edge move."
  ([player board square]
   (possible-edge-move player board square edge-table))
  ([player board square table]
   (let [board (game/make-move square player board)]
     [(edge-move-probability player board square)
      (- (aget table (edge-index (game/opponent player) board top-edge)))])))

(defn combine-edge-moves
  "Combine the best moves."
  [possibilities player]
  (let [f (if (= game/black player) > <)
        [_ final-value] (reduce (fn [[prob value] [p v]]
                                  (if (pos? prob)
                                    [(- prob (* prob p))
                                     (+ value (* prob p v))]
                                    (reduced [prob value])))
                                [1 0]
                                (sort-by second f possibilities))]
    final-value))

(defn possible-edge-moves-value
  "Consider all possible edge moves and combine their values into a single
  number."
  ([player board index]
   (possible-edge-moves-value player board index edge-table))
  ([player board index table]
   (combine-edge-moves
    (cons [1.0 (aget table index)]
          (for [square top-edge
                :when (= game/blank (nth board square))]
            (possible-edge-move player board square)))
    player)))

(def static-edge-table
  "Table of weights for each edge square and stability."
  ;;stab semi    un
  [[nil     0 -2000]                      ; X
   [700   nil   nil]                      ; corner
   [1200  200   -25]                      ; C
   [1000  200    75]                      ; A
   [1000  200    50]                      ; B
   [1000  200    50]                      ; B
   [1000  200    75]                      ; A
   [1200  200   -25]                      ; C
   [700   nil   nil]                      ; corner
   [nil     0 -2000]                      ; X
   ])

(defn piece-stability
  "Find two pieces, p1 and p2, which lay on either side of the piece in
  question, and which are not of the same color as the piece. These \"pieces\"
  may be empty or off the board.

  A piece is:

  - Unstable if one of the two is empty and the other is the opponent

  - Semistable if there are opponents on both sides and at least one empty
  square to play on, or if it's surrounded by empty pieces

  - Stable if either p1 or p2 is nil, since it must be connected by a solid
  wall of pieces to the corner."
  [board square]
  (let [stable 0
        semistable 1
        unstable 2]
    (cond
      (corner? square) stable
      (x-square? square) (if (= game/blank (nth board (corner-for square)))
                           unstable
                           semistable)
      :else (let [player (nth board square)
                  opp (game/opponent player)
                  p1 (->> board
                          (drop (dec square))
                          (take (- 19 square))
                          (filter #(not= player))
                          first)
                  p2 (->> board
                          (drop 10)
                          (take (- square 11))
                          (filter #(not= player))
                          first)]
              (cond
                ;; Unstable pieces can be captured immediately by playing in
                ;; the empty square
                (or (and (= game/blank p1)
                         (= opp p2))
                    (and (= game/blank p2)
                         (= opp p1))) unstable

                ;; semistable pieces may be captured
                (and (= opp p1)
                     (= opp p2)
                     (->> board
                          (drop 10)
                          (take 8)
                          (filter #(= game/blank))
                          first)) semistable

                (and (= game/blank p1)
                     (= game/blank p2)) semistable

                ;; stable pieces can never be captured
                :else stable)))))

(defn static-edge-stability
  "Compute edge's static stability.

  A piece is stable if it cannot be captured, unstable if it is in immediate
  danger of being captured, and semistable otherwise.

  Note that corner squares are always stable, and x-squares are semistable if
  the adjacent corner is taken, and unstable otherwise."
  [player board]
  (->> top-edge
       (map-indexed (fn [i square]
                      (condp = (nth board square)
                        game/blank 0
                        player (get-in static-edge-table [i (piece-stability board square)])
                        (- (get-in static-edge-table [i (piece-stability board square)])))))
       (apply +)))

(defn set-edge-table-static!
  ([board index]
   (set-edge-table-static! board index edge-table))
  ([board index table]
   (aset table index (static-edge-stability game/black board))))

(defn set-edge-table-possible!
  ([board index]
   (set-edge-table-possible! board index edge-table))
  ([board index table]
   (aset table index (possible-edge-moves-value game/black board index))))

(defn map-edge-n-pieces!
  "Call f on all edges with n pieces."
  [f player board n squares index]
  (cond
    (< (count squares) n) nil
    (empty? squares) (f board index)
    :else (let [index3 (* 3 index)
                square (first squares)]
            (map-edge-n-pieces! f player board n (rest squares) index3)
            (when (and (pos? n)
                       (= game/blank (nth board square)))
              (map-edge-n-pieces! f player (assoc board square player)
                                  (dec n) (rest squares) (inc index3))
              (map-edge-n-pieces! f player (assoc board square (game/opponent player))
                                  (dec n) (rest squares) (+ 2 index3))))))

(defn init-edge-table!
  "Initialize an edge table (default `edge-table`), starting from empty board."
  ([]
   (init-edge-table! edge-table))
  ([table]
   ;; Initialize static values
   (dotimes [n (inc 10)]
     (map-edge-n-pieces! set-edge-table-static! game/black game/initial-board
                         n top-edge 0))
   ;; Iterate 5 times, trying to improve
   (dotimes [_ 5]
     ;; Do indices with most pieces first
     (doseq [n (range 9 (dec 1) -1)]
       (map-edge-n-pieces! set-edge-table-possible! game/black game/initial-board
                           n top-edge 0)))))

(defonce _ (init-edge-table! edge-table))

(defn edge-stability
  "Total edge evaluation for player to move on board."
  [player board]
  (->> edge-and-x-lists
       (map #(aget edge-table (edge-index player board %)))
       (apply +)))

(defn Iago-eval
  "Combine edge-stability, current mobility, and potential mobility to arrive at
  an evaluation.

  The three factors are multiplied by coefficients that vary by move number."
  [player board]
  (let [move-number @game/move-number
        c-edg (+ 312000 (* 6240 move-number))
        c-cur (if (< move-number 25)
                (+ 50000 (* 2000 move-number))
                (+ 75000 (* 1000 move-number)))
        c-pot 20000
        [p-cur p-pot] (mobility player board)
        [o-cur o-pot] (mobility (game/opponent player) board)]
    (+ (quot (* c-edg (edge-stability player board)) 32000)
       (quot (* c-cur (- p-cur o-cur)) (+ p-cur o-cur 2))
       (quot (* c-pot (- p-pot o-pot)) (+ p-pot o-pot 2)))))

(defn Iago
  [depth]
  (alpha-beta-searcher-2 depth Iago-eval))

(defn batch-test
  [b-strat w-strat]
  (let [state (atom {:b 0 :w 0 :t 0})]
    (dotimes [_ 100]
      (case (Math/sign (game/othello b-strat w-strat false))
        -1 (swap! state update :w inc)
        0 (swap! state update :t inc)
        1 (swap! state update :b inc)))
    @state))

(defn othello-series
  "Play a series of 2*n-pairs games, swapping sides."
  [strat-1 strat-2 n-pairs]
  (let [scores (mapcat #(vector (game/othello strat-1 strat-2 false)
                                (- (game/othello strat-2 strat-1 false)))
                       (range n-pairs))]
    {:wins (+ (count (filter pos? scores))
              (/ (count (filter zero? scores)) 2))
     :point-diff (apply + scores)
     :scores scores}))
