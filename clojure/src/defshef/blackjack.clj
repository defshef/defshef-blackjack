(ns defshef.blackjack)

;; Modelling the base data

; (def suits [:♣ :♦ :♠])
(def suits [:H :C :D :S])
(def numbers [2 3 4 5 6 7 8 9 10 :J :Q :K :A])

(defn card
  "build a card representation"
  [number suit]
  {:pre [(some #{number} numbers)
         (some #{suit} suits)]}
  {:number number
   :suit suit})

