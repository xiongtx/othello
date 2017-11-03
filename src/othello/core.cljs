(ns othello.core
  (:require [cljs.reader :as reader]
            [othello.game :as game]
            [othello.strategy :as strategy]
            [reagent.core :as r]))

;; -------------------------
;; State

(defonce strategy-map
  {"Random strategy" strategy/random-strategy
   "Maximize difference" strategy/maximize-difference
   "Maximize weights" strategy/maximize-weights
   "Modified maximize weights" strategy/maximize-modified-weights
   "α-β maximize difference, depth=3" (strategy/alpha-beta-searcher 3 game/count-difference)
   "α-β maximize modified weights, depth=6" (strategy/alpha-beta-searcher 6 strategy/modified-weighted-squares)
   "α-β-2 maximize modified weights, depth=6" (strategy/alpha-beta-searcher-2 6 strategy/modified-weighted-squares)
   "Iago-3" (strategy/Iago 3)})

(defonce app-state (r/atom (let [board game/initial-board
                                 player game/black
                                 strat-name "Random strategy"]
                             {:board board
                              :player player
                              :move-number @game/move-number
                              :opp-strat {:name strat-name
                                          :strat (strategy-map strat-name)}
                              :status :starting})))

;; -------------------------
;; Views

(defonce piece-class-map {game/black "black-piece"
                          game/white "white-piece"
                          game/blank "blank"})

(defonce computer-player game/white)

(defn square
  [state]
  (fn [{:keys [board player status pos piece]
       {:keys [strat]} :opp-strat}]
    [:div {:class (piece-class-map piece)
           :on-click #(if (and (or (= :starting status)
                                   (= :in-progress status))
                               (some #{pos} (game/legal-moves player board)))
                        (loop [board (game/make-move pos player board)
                               player (game/next-to-play board player)]
                          (swap! app-state assoc
                                 :board board
                                 :player player
                                 :move-number (swap! game/move-number inc)
                                 :status (if player :in-progress :finished))
                          (when (= player computer-player)
                            (let [board (game/get-move strat player board false)
                                  player (game/next-to-play board player)]
                              (recur board player)))))}]))

(defn board
  [app-state]
  (let [{:keys [player board]
         :as app-state} @app-state]
    [:div
     (for [i (range 1 (inc 8))]
       ^{:key (str i)}
       [:div
        (for [j (range 1 (inc 8))]
          (let [id (str i j)
                pos (+ (* 10 i) j)]
            ^{:key id}
            [:div {:class (str "square "
                               (when (some #{pos} (game/legal-moves player board))
                                 "legal"))}
             [square (merge app-state
                            {:pos pos
                             :piece (nth board pos)})]]))])]))

(defn opp-strat []
  (let [{:keys [status]
         {:keys [name]} :opp-strat} @app-state]
    [:div.btn-group
     [:button.btn.btn-secondary.dropdown-toggle
      {:aria-expanded "false"
       :aria-haspopup "true"
       :data-toggle "dropdown"
       :disabled (let [{:keys [status]} @app-state]
                   (when-not (= :starting status) true))
       :type "button"}
      (str "Strategy: " name)]
     [:div.dropdown-menu.dropdown-menu-right
      (for [strat (keys strategy-map)]
        ^{:key strat}
        [:button.dropdown-item
         {:on-click #(swap! app-state assoc
                            :opp-strat {:name strat
                                        :strat (strategy-map strat)})
          :type "button"}
         strat])]]))

(defn home-page []
  (let [{:keys [status]
         brd :board} @app-state
        black-count (game/count-player game/black brd)
        white-count (game/count-player game/white brd)]
    [:div
     [:center [:h1 "Othello"]]
     (if (= :finished status)
       [:div.alert.alert-success.show
        {:role "alert"}
        (cond
          (< black-count white-count) "White wins!"
          (> black-count white-count) "Black wins!"
          :else "Tie")])
     [:h2 (str "Black: " black-count)]
     [:h2 (str "White: " white-count)]
     [opp-strat]
     [:button.btn.btn-primary
      {:on-click #(swap! app-state assoc
                         :board game/initial-board
                         :move-number (reset! game/move-number 1)
                         :player game/black
                         :status :starting)
       :type "button"}
      "New game"]
     [board app-state]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  #_(strategy/init-edge-table!)
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
