(ns main.2022.day2
 (:require
   [main.util :as util]))

;; 1 for Rock, 2 for Paper, and 3 for Scissors
;; A for Rock, B for Paper, and C for Scissors.
;; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.


(def get-score { "A X" 4
                 "A Y" 8
                 "A Z" 3
                 "B X" 1
                 "B Y" 5
                 "B Z" 9
                 "C X" 7
                 "C Y" 2
                 "C Z" 6})

(def strategy { "A X" "A Z"
                "A Y" "A X"
                "A Z" "A Y"
                "B X" "B X"
                "B Y" "B Y"
                "B Z" "B Z"
                "C X" "C Y"
                "C Y" "C Z"
                "C Z" "C X"})

;; part 1
(->> (util/load-data "2022/data/day2.txt")
     (map #(get-score %))
     (apply +))

;part 2
(->> (util/load-data "2022/data/day2.txt")
     (map (fn[x] (->> (strategy x)
                      get-score)))
     (apply +))
