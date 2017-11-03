(ns othello.game
  (:require [cljs.pprint :refer [cl-format]]
            [cljs.reader :as reader]))

(defonce move-number (atom 1))

(def black :b)

(def white :w)

(def blank nil)

(def outer :o)

(def all-squares
  "Indices of all playing squares."
  (for [i (range 11 (inc 88))
        :when (<= 1 (mod i 10) 8)]
    i))

(def all-directions
  [-11 -10 -9 -1 1 9 10 11])

(def initial-board
  "Initial board"
  (let [board (vec (repeat 100 outer))
        blanks (interleave all-squares (repeat (count all-squares) blank))]
    (as-> board board
      (apply assoc board blanks)
      (assoc board
             44 white
             45 black
             54 black
             55 white))))

(def neighbor-table
  "Table of neighbors to each square."
  (for [square (range 0 100)]
    (for [dir all-directions
          :let [neighbor (+ square dir)]
          :when (valid? square)
          :when (valid? neighbor)]
      neighbor)))

(defn blank-squares
  "List of all blank squares on board."
  [board]
  (filter #(= blank (nth board %)) all-squares))

(defn neighbors
  "Return list of squares adjacent to a square."
  [square]
  (nth neighbor-table square))

(defn opponent
  [player]
  (if (= black player)
    white
    black))

(defn valid?
  "A valid move is \"syntactically correct\", i.e. moves to a playing square.

  A valid move is not necessarily `legal?`."
  [move]
  (boolean (some #{move} all-squares)))

(defn find-bracketing-piece
  "Return square number of the bracketing piece, or nil."
  [square player board dir]
  (let [piece (nth board square)]
    (cond
      (= piece player) square
      (= piece (opponent player)) (recur (+ square dir) player board dir)
      :else nil)))

(defn would-flip?
  "Would this move result in flips in this direction?

  If so, return the square number of the bracketing piece.

  A flip occurs if, starting at the adjacent square in dir dir, there is
  a sequence of at least one opponent pieces, bracketed by one of the player's
  pieces."
  [move player board dir]
  (let [c (+ move dir)
        piece (nth board c)]
    (and (= piece (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(defn make-flips
  "Return board after making flips in the given direction."
  [move player board dir]
  (if-let [bracketer (would-flip? move player board dir)]
    (let [positions (range (+ move dir) bracketer dir)]
      (apply assoc board (interleave positions (repeat (count positions) player))))
    board))

(defn legal?
  "A legal move must be into an blank square and flip an opponent's piece."
  [move player board]
  (and (valid? move)
       (= blank (nth board move))
       (some #(would-flip? move player board %)
             all-directions)))

(defn make-move
  "Update board to reflect player's move.

  This does not verify that a move is `valid?`, let alone `legal?`."
  [move player board]
  (loop [board (assoc board move player)
         dirs all-directions]
    (if (seq dirs)
      (recur (make-flips move player board (first dirs))
             (rest dirs))
      board)))

(defn count-player
  "Number of player's pieces on the board."
  [player board]
  (count (filter #(= player %) board)))

(defn count-difference
  "Number of player's pieces minus opponent's pieces."
  [player board]
  (- (count-player player board)
     (count-player (opponent player) board)))

(defn name-of
  "Print representation of a piece, empty, or outer."
  [piece]
  ({:b "@"
    :w "0"
    nil "."
    :o "?"} piece))

(defn print-board
  "Print board, along with some statistics"
  [board]
  (cl-format true "~2%    1 2 3 4 5 6 7 8  [~c=~2a ~c=~2a (~@d)]"
             (name-of black) (count-player black board)
             (name-of white) (count-player white board)
             (count-difference black board))
  (doseq [row (range 1 (inc 8))]
    (cl-format true " ~d " (* 10 row))
    (doseq [col (range 1 (inc 8))]
      (let [piece (nth board (+ col (* 10 row)))]
        (cl-format true "~c " (name-of piece)))))
  (cl-format true "~2%"))

(defn legal-moves
  [player board]
  (for [move all-squares
        :when (legal? move player board)]
    move))

(defn any-legal-move?
  "Return non-nil if player has any legal moves in this position."
  [player board]
  (seq (legal-moves player board)))

(defn get-move
  "Return board after applying player's strategy to get a move.

  Keep calling until a legal move is made."
  [strategy player board print?]
  (when print? (print-board board))
  (let [move (strategy player board)]
    (cond
      (and (legal? move player board))
      (do
        (when print?
          (cl-format true "~&~A moves to ~d" (name-of player) move))
        (make-move move player board))
      :else (do
              (cl-format true "~&Illegal move: ~d" move)
              (recur strategy player board print?)))))

(defn next-to-play
  "Return the player to play next, or nil if neither can move."
  ([board prev-player]
   (next-to-play board prev-player true))
  ([board prev-player print?]
   (let [opp (opponent prev-player)]
     (cond
       (any-legal-move? opp board) opp
       (any-legal-move? prev-player board)
       (do
         (when print?
           (cl-format true "~&~A has no moves and must pass." (name-of opp)))
         prev-player)
       :else nil))))

(defn othello
  "Play a game of Othello.

  Returns the score, where a positive difference means black (the first
  player) wins."
  ([b-strat w-strat]
   (othello b-strat w-strat true))
  ([b-strat w-strat print?]
   (loop [board initial-board
          player black]
     (if player
       (let [strategy (if (= black player) b-strat w-strat)
             new-board (get-move strategy player board print?)]
         (recur new-board (next-to-play new-board player print?)))
       (do
         (when print?
           (cl-format true "~&Game over. Final result:")
           (print-board board))
         (count-difference black board))))))
