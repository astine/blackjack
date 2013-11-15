(ns blackjack.strategies
  (:require [blackjack.core :as bj]))

(def hand-totals 
  {8 {2 :hit 3 :hit 4 :hit 5 :hit 6 :hit 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   9 {2 :hit 3 :double-down 4 :double-down 5 :double-down 6 :double-down 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   10 {2 :double-down 3 :double-down 4 :double-down 5 :double-down 6 :double-down 7 :double-down 8 :double-down 9 :double-down 10 :hit 1 :hit}
   11 {2 :double-down 3 :double-down 4 :double-down 5 :double-down 6 :double-down 7 :double-down 8 :double-down 9 :double-down 10 :double-down 1 :hit}
   12 {2 :hit 3 :hit 4 :stand 5 :stand 6 :stand 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   13 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   14 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   15 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   16 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   17 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :stand 8 :stand 9 :stand 10 :stand 1 :stand}})

(def soft-totals
  {2 {2 :hit 3 :hit 4 :hit 5 :double-down 6 :double-down 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   3 {2 :hit 3 :hit 4 :double-down 5 :double-down 6 :double-down 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   4 {2 :hit 3 :hit 4 :double-down 5 :double-down 6 :double-down 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   5 {2 :hit 3 :double-down 4 :double-down 5 :double-down 6 :double-down 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   6 {2 :hit 3 :double-down 4 :double-down 5 :double-down 6 :double-down 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   7 {2 :stand 3 :double-down 4 :double-down 5 :double-down 6 :double-down 7 :stand 8 :stand 9 :hit 10 :hit 1 :hit}
   8 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :stand 8 :stand 9 :stand 10 :stand 1 :stand}
   9 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :stand 8 :stand 9 :stand 10 :stand 1 :stand}})

(def pairs
  {2 {2 :split 3 :split 4 :split 5 :split 6 :split 7 :split 8 :hit 9 :hit 10 :hit 1 :hit}
   3 {2 :split 3 :split 4 :split 5 :split 6 :split 7 :split 8 :hit 9 :hit 10 :hit 1 :hit}
   4 {2 :hit 3 :hit 4 :hit 5 :split 6 :split 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   5 {2 :double-down 3 :double-down 4 :double-down 5 :double-down 6 :double-down 7 :double-down 8 :double-down 9 :double-down 10 :hit 1 :hit}
   6 {2 :split 3 :split 4 :split 5 :split 6 :split 7 :hit 8 :hit 9 :hit 10 :hit 1 :hit}
   7 {2 :split 3 :split 4 :split 5 :split 6 :split 7 :split 8 :stand 9 :hit 10 :hit 1 :hit}
   8 {2 :split 3 :split 4 :split 5 :split 6 :split 7 :split 8 :split 9 :split 10 :split 1 :split}
   9 {2 :split 3 :split 4 :split 5 :split 6 :split 7 :stand 8 :split 9 :split 10 :stand 1 :stand}
   10 {2 :stand 3 :stand 4 :stand 5 :stand 6 :stand 7 :stand 8 :stand 9 :stand 10 :stand 1 :stand}
   1 {2 :split 3 :split 4 :split 5 :split 6 :split 7 :split 8 :split 9 :split 10 :split 1 :split}})

(defn basic-strategy [hand bet money dealer]
  (if (= (count hand) 2)
    (cond (bj/blackjack? hand)
          :stand
          (= (second (first hand)) (second (second hand)))
          (get (get pairs (bj/card-value (first hand))) 
               (bj/card-value (first dealer)))
          (= (bj/card-value (first hand)) 1)
          (get (get soft-totals (bj/card-value (second hand)))
               (bj/card-value (first dealer)))
          (= (bj/card-value (second hand)) 1)
          (get (get soft-totals (bj/card-value (first hand)))
               (bj/card-value (first dealer)))
          (>= (bj/hand-value hand) 17)
          :stand
          (<= (bj/hand-value hand) 8)
          :hit
          :else
          (get (get hand-totals (bj/hand-value hand))
               (bj/card-value (first dealer))))
    (cond (>= (bj/hand-value hand) 17)
          :stand
          (<= (bj/hand-value hand) 8)
          :hit
          :else
          (get (get hand-totals (bj/hand-value hand))
               (bj/card-value (first dealer))))))
          

(defn hi-low-bet [{money :money count :count :as player}]
  (let [base-bet 10
        bet (-> (* base-bet (Math/pow 2 (/ (or count 0) 2)))
                (bj/greater (/ money 2) <))]
    (assoc player :money (- money bet)
           :bets [bet])))


(defn hi-low-count [{hands :hands counts :count last-deck :last-deck :as player} 
                   {deck :deck dealer-hand :dealer-hand}]
  (assoc player
    :count (+ (if (< (or last-deck 0) (count deck)) 0 (or counts 0))
              (reduce + (for [card (apply concat (cons dealer-hand hands))]
                          (case (bj/card-value card)
                            (2 3 4 5 6) 1
                            (7 8 9) 0
                            (10 1) -1))))))
