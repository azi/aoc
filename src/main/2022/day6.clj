(ns main.2022.day6
 (:require
   [main.util :as util]))

(defn run [n]
 (->> (util/load-data "2022/data/day6.txt")
      first
      (partition n 1)
      (keep-indexed #(if (= (count (set %2)) n) (+ %1 n)))
      first))

;; part 1
(run 4) ; 1929

;; part2
(run 14) ; 3298

