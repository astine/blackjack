(ns blackjack.core
  (:require [clojure.stacktrace :as st]
            [clojure.pprint :as pp]
            [clojure.walk :as w]))

(def ^:dynamic logging false)

(defmacro a-assoc 
  "Anaphoric assoc"
  [mapn & bindings]
  `(assoc ~mapn 
     ~@(let [bindings (partition 2 bindings)
             symbols (set (map first bindings))]
         (mapcat (fn [[key binding]]
                   [key
                    (w/postwalk #(if (and (symbol? %) (symbols (keyword %))) 
                                   `(~(keyword %) ~mapn) 
                                   %) 
                                binding)])
                 bindings))))

(defn greater [x y & [comp]]
  (if ((or comp >) x y)
    x y))

(def ^:dynamic ^java.util.Random *rnd* (java.util.Random. 26))

(defn random
  ([] (.nextDouble *rnd*))
  ([high] (* high (random)))
  ([low high] (+ low (random (- high low)))))

(defn shuffle-cards [deck]
  (loop [old-deck deck new-deck []]
    (let [index (int (random (count old-deck)))]
      (if (not-empty old-deck)
        (recur (concat (drop (inc index) old-deck) (take index old-deck))
               (conj new-deck (nth old-deck index)))
        new-deck))))

(def suits [:h :d :c :s])

(def ranks [:a :2 :3 :4 :5 :6 :7 :8 :9 :10 :j :q :k])

(def pack (for [suit suits rank ranks] [suit rank]))

(defn deck 
  ([] (deck 1))
  ([packs]
     (shuffle-cards (apply concat (repeat packs pack)))))

(defn deal [cards deck]
  (if (>= (count deck) cards)
    (take cards deck)
    (throw (ex-info "Not enough cards left in deck" {:type :deck-exception :cause :too-few-cards :number-of-requested-cards cards :deck deck}))))

(defn deal-rest [cards deck]
  (if (>= (count deck) cards)
    (drop cards deck)
    (throw (ex-info "Not enough cards left in deck" {:type :deck-exception :cause :too-few-cards :number-of-requested-cards cards :deck deck}))))

(def table
  {:deck []
   :deck-info {:packs 0 :redeal-threshold 0}
   :dealer-hand []
   :players []
   :shoe? false})

(defn card-value 
  [card]
  (case (second card)
    (:j :q :k) 10
    (:a) 1
    (read-string (name (second card)))))

(defn hand-value
  [hand]
  (try
    (let [sum (reduce + (map card-value hand))]
      (if (and (some #(= :a (second %)) hand)
               (>= (- 21 sum) 10))
        (+ sum 10)
        sum))
    (catch NullPointerException e
      (print (str hand))
      (throw e))))

(defn hand-score 
  [hand]
  (let [hand-value (hand-value hand)]
    (if (> hand-value 21)
      0
      hand-value)))

(defn blackjack? [hand]
  (and (= (hand-score hand) 21)
       (= (count hand) 2)))

(defn stand-on-17 [hand bet money dealer]
  (if (> 17 (hand-value hand))
    :hit
    :stand))

(def default-handler stand-on-17)

(defn interactive-handler [hand bet money dealer]
  (println (str "Hand: " hand ", Bet: " bet " Money: " money " Dealer Card: " (first dealer)))
  (println (str "What do you want to do? - (hit, stand, double down, split, surrender): "))
  (let [action (keyword (read))]
    (if (#{:hit :stand :double-down :split :surrender} action)
      action
      :stand)))

(defn default-bet-handler [{money :money :as player}]
  (let [bet (greater money 10 <)]
    (assoc player :money (- money bet)
           :bets [bet])))

(defn default-result-hook [player table] 
  player)

(def player {:bets [0]
             :money 0 
             :hands [[]] 
             :handler default-handler 
             :bet-handler default-bet-handler
             :result-hook default-result-hook})

(defn new-players 
  ([player-count player-features]
     (map merge (repeat player-count player) (repeat player-features)))
  ([players]
     (map merge (repeat player) players)))

(defn hit [hand bet money deck]
  [[(conj hand (first (deal 1 deck)))]
   [bet]
   money
   (deal-rest 1 deck)
   :hit])

(defn stand [hand bet money deck]
  [[hand] [bet] money deck :stand])

(defn double-down [hand bet money deck]
  [[(conj hand (first (deal 1 deck)))]
   [(+ bet (greater bet money <))]
   (- money (greater bet money <))
   (deal-rest 1 deck) 
   :double-down])

(defn split [hand bet money deck]
  [(map vector [(first hand) (second hand)] (deal 2 deck))
   [bet bet]
   (- money bet)
   (deal-rest 2 deck)
   :split])
   
(defn surrender [hand bet money deck]
  [[hand]
   [(/ bet 2)]
   (+ money (/ bet 2))
   deck
   :surrender])

(defn dispatch-action [action]
  (case action
    :hit hit
    :stand stand
    :double-down double-down
    :split split
    :surrender surrender
    (throw (Exception. (str "Bad action: " action)))))

(defn run-player [player deck dealer]
  (let [{hands :hands bets :bets money :money handler :handler} player]
    (loop [[hand & hands] hands [bet & bets] bets money money deck deck new [[][]]]
      (if (not (nil? hand))
        (let [[new-hands new-bets money deck action] 
              ((dispatch-action (handler hand bet money dealer))
               hand bet money deck)]
          (case action
            :hit (recur (cons (first new-hands) hands)
                        (cons (first new-bets) bets) 
                        money deck new)
            :split (recur (concat new-hands hands)
                          (concat new-bets bets) 
                          money deck new)
            (recur hands
                   bets money deck
                   (map concat [new-hands new-bets] new))))
        [(assoc player :hands (first new) :bets (second new) :money money)
         deck]))))

(defn update-players [table]
  (let [{players :players deck :deck dealer :dealer-hand} table]
    (loop [[player & players] players deck deck new []]
      (if (not (nil? player))
        (let [[player deck] (run-player player deck dealer)]
          (recur players deck (cons player new)))
        (assoc table :players new :deck deck)))))

(defn update-dealer [table]
  (loop [dealer-hand (:dealer-hand table)
         deck (:deck table)]
    (case (stand-on-17 dealer-hand nil nil nil)
      :hit (recur (cons (first (deal 1 deck)) dealer-hand)
                  (deal-rest 1 deck))
      :stand (assoc table :dealer-hand dealer-hand :deck deck))))

(defn initial-player-bets
  [table]
  (assoc table :players
         (map #((:bet-handler %) %) (:players table))))

(defn deal-cards
  [table]
  (as-> table table
        (a-assoc table :players (map #(a-assoc %1 :hands [(conj (first hands) %2)])
                                    players deck)
                 :deck (deal-rest (count players) deck))
        (a-assoc table :dealer-hand (conj dealer-hand (first (deal 1 deck)))
                 :deck (deal-rest 1 deck))
        (a-assoc table :players (map #(a-assoc %1 :hands [(conj (first hands) %2)])
                                    players deck)
                 :deck (deal-rest (count players) deck))
        (a-assoc table :dealer-hand (conj dealer-hand (first (deal 1 deck)))
                 :deck (deal-rest 1 deck))))

(defn deal-first-hand 
  ([players] (deal-first-hand players 2))
  ([player-bets packs] (deal-first-hand player-bets (repeat default-handler) packs))
  ([player-bets player-handlers packs]
     (->  table
          (assoc :deck (deck packs))
          (assoc :players (map #(assoc %1 :bets [%2] :handler %3) 
                               (repeat player) player-bets player-handlers))
          (deal-cards))))

(defn generate-deck
  ([table]
     (generate-deck table 2))
  ([table packs]
     (assoc table :deck (deck packs)
            :deck-info {:packs packs :redeal-threshold (* (count (deck packs)) 0.25)})))

(defn generate-players 
  ([table players]
     (assoc table :players (new-players players)))
  ([table player-count default-player]
     (assoc table :players (new-players player-count default-player))))

(defn start-game 
  ([players pack shoe?]
     (if (vector? players)
       (-> table
           (generate-deck pack)
           (generate-players players)
           (assoc :shoe? shoe?))
       (-> table
           (generate-deck pack)
           (generate-players (:player-count players) (:player-features players))
           (assoc :shoe? shoe?))))
  ([players pack]
     (start-game players pack false))
  ([players]
     (start-game players 2 false)))

(defn result [table]
  (let [dealer-score (hand-score (:dealer-hand table))]
    (assoc table :players
           (for [player (:players table)]
             ((:result-hook player)
              (cond (= (:bets player) [0])
                    (assoc player :last-hand :no-bet)
                    (and (= (count (:hands player)) 1)
                         (blackjack? (first (:hands player))))
                    (assoc player 
                      :money (+ (:money player) (* 3/2 (first (:bets player))))
                      :last-hand :blackjack)
                    (blackjack? (:dealer-hand table))
                    (assoc player :last-hand :dealer-blackjack)
                    :else
                    (let [winnings 
                          (reduce + (for [[score bet] (zipmap (map hand-score (:hands player)) (:bets player))]
                                      (cond
                                       (or (> dealer-score score) (= score 0))
                                       0
                                       (< dealer-score score)
                                       (* 2 bet)
                                       (= dealer-score score)
                                       bet)))]
                      (assoc player :money (+ (:money player) winnings))))
              table)))))
  
(defn clear-hands [table]
  (assoc table
    :dealer-hand []
    :players (map #(assoc % :hands [[]] :bets []) (:players table))
    :deck (if (< (count (:deck table)) (:redeal-threshold (:deck-info table)))
            (deck (:packs (:deck-info table)))
            (:deck table))))

(defn print-table-state [table]
  (when logging
    (let [{:keys [deck dealer-hand players]} table]
      (pp/cl-format true "~%Deck: ~A cards - ~A~%" (count deck) (take 3 deck))
      (pp/cl-format true "Dealer: ~A~%" dealer-hand)
      (pp/cl-format true "Players:~%")
      (doseq [{:keys [hands bets money last-hand]} players]
        (pp/cl-format true "~tHands: ~A, Bets: ~A, Money: ~A, Win?: ~A~%"
                      hands bets money last-hand))))
  table)

(defn seperate-non-betting-players [table]
  (assoc table :players (remove #(= (:bets %) [0]) (:players table))
         :out-players (filter #(= (:bets %) [0]) (:players table))))

(defn join-players [table]
  (assoc table :players (concat (:players table) (:out-players table))
         :out-players []))

(defn play-hand
  [table]
  (-> table
      (clear-hands)
      (initial-player-bets)
      (seperate-non-betting-players)
      (deal-cards)
      (update-players)
      (update-dealer)
      (join-players)
      (result)
      (print-table-state)))

(defn play-game [players packs hands]
  (try
    (-> (start-game players packs)
        ((apply comp (repeat hands play-hand))))
    (catch Exception e
      (pp/pprint (ex-data e))
      (st/print-cause-trace e))))

(def current-game-state (atom table))
