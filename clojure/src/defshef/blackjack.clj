(ns defshef.blackjack
  (:require [expectations :refer [expect]]))

;; Modelling the base data

(def suits [:H :C :D :S])
(def numbers [2 3 4 5 6 7 8 9 10 :J :Q :K :A])

(defn card
  "build a card representation"
  [number suit]
  {:pre [(some #{number} numbers)
         (some #{suit} suits)]}
  {:number number
   :suit suit})

(expect 2 (:number (card 2 :H)))
(expect :A (:number (card :A :H)))
(expect :H (:suit (card 2 :H)))
(expect AssertionError (card 11 :H))
(expect AssertionError (card 7 :T))

(defn hand
  "Helper function for building a hand

  Usage: (hand 2 :H, 2 :C, 3 :S)
  "
  [& numbers-and-suits]
  (->> numbers-and-suits
       (partition 2)
       (mapv #(apply card %))))

(expect [(card 2 :H) (card :K :S)] (hand 2 :H, :K :S))
(expect [(card 2 :H) (card :K :S) (card :A :H)] (hand 2 :H, :K :S, :A :H))

(defn- card-value
  "How much is a card worth?

  In this function, aces are always hard"
  [{:keys [number] :as card}]
  (condp #(%1 %2) number
    number? number
    #{:J :Q :K} 10
    #{:A} 11
    (throw (ex-info "Unknown number" {:card card}))))

(expect 5 (card-value (card 5 :C)))
(expect 10 (card-value (card 10 :H)))
(expect 10 (card-value (card :K :S)))
(expect 10 (card-value (card :Q :C)))
(expect 10 (card-value (card :J :D)))
(expect 11 (card-value (card :A :S)))
(expect Throwable (card-value {:number :x}))

(def ^:private sum (partial reduce +))
(defn- has-ace [hand]
  (some #(= :A (:number %)) hand))

(defn value
  "How much is a hand worth?"
  [hand]
  (let [total (sum (map card-value hand))
        ace (has-ace hand)
        qualifier (cond (> total 21) :bust
                        (and (= 21 total) (= 2 (count hand))) :blackjack
                        ace :soft
                        :else nil)]
    ; bust with an ace? Use lower value instead
    (if (and ace (= qualifier :bust))
      [(- total 10) :hard]
      [total qualifier])))

(expect [4 nil] (value (hand 2 :H, 2 :C)))
(expect [8 nil] (value (hand 2 :H, 2 :C, 2 :D, 2 :S)))
(expect [19 nil] (value (hand 2 :H, 2 :C, 7 :D, 8 :S)))
(expect [21 nil] (value (hand :K :H, 5 :C, 6 :S)))
(expect [23 :bust] (value (hand :K :H, :Q :C, 3 :S)))
(expect [27 :bust] (value (hand :J :H, 8 :C, 9 :S)))
(expect [21 :blackjack] (value (hand :K :H, :A :C)))
(expect [21 :blackjack] (value (hand :A :H, 10 :S)))
(expect [16 :soft] (value (hand :A :S, 5 :H)))
(expect [13 :hard] (value (hand :A :S, 5 :H, 7 :H)))
(expect [12 :hard] (value (hand :A :S, :A :H)))
