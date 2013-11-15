(ns blackjack.stats
  (:require [blackjack.core :as bj]
            [blackjack.strategies :as strat]
            [clojure.stacktrace :as st]
            [clojure.pprint :as pp]
            [clojure.walk :as w]
            [incanter.core :as i]
            [incanter.stats :as stats]
            [incanter.charts :as charts]))

(defn average-hand-winnings [starting-money packs hands & [handler bet-handler result-hook]]
  (-> (bj/play-game [{:money starting-money 
                      :handler (or handler bj/default-handler) 
                      :bet-handler (or bet-handler bj/default-bet-handler)
                      :result-hook (or result-hook bj/default-result-hook)}]
                    packs
                    hands)
      :players
      first
      :money
      (- starting-money)
      (/ hands)
      float))
