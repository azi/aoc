(ns main.2022.day4
 (:require
   [main.util :as util]))

;; [N]     [C]                 [Q]
;; [W]     [J] [L]             [J] [V]
;; [F]     [N] [D]     [L]     [S] [W]
;; [R] [S] [F] [G]     [R]     [V] [Z]
;; [Z] [G] [Q] [C]     [W] [C] [F] [G]
;; [S] [Q] [V] [P] [S] [F] [D] [R] [S]
;; [M] [P] [R] [Z] [P] [D] [N] [N] [M]
;; [D] [W] [W] [F] [T] [H] [Z] [W] [R]
;;  1   2   3   4   5   6   7   8   9

(def stack-list [["D" "M" "S" "Z" "R" "F" "W" "N"]
                 ["W" "P" "Q" "G" "S"]
                 ["W" "R" "V" "Q" "F" "N" "J" "C"]
                 ["F" "Z" "P" "C" "G" "D" "L"]
                 ["T" "P" "S"]
                 ["H" "D" "F" "W" "R" "L"]
                 ["Z" "N" "D" "C"]
                 ["W" "N" "R" "F" "V" "S" "J" "Q"]
                 ["R" "M" "S" "G" "Z" "W" "V"]])

(def stack-sample [["Z" "N"]
                   ["M" "C" "D"]
                   ["P"]])

(defn prepare-data [s]
  (let [parse (->>
               (re-find (re-matcher #"move (\d+) from (\d+) to (\d+)" s))
               rest
               (map #(util/str->int %)))]
    (into {} [
              [:move (nth parse 0)]
              [:from (- (nth parse 1) 1)]
              [:to   (- (nth parse 2) 1)]])))


(defn go [s commend]
  (let [from-v             (nth s (:from commend))
        [rest-from move-v] (split-at (- (count from-v) (:move commend)) from-v)]

     (-> s
         (assoc-in [(:from commend)] rest-from)
         (assoc-in [(:to commend)] (concat (nth s (:to commend)) move-v)))))

;; part 1
(let [commends (->> (util/load-data "2022/data/day5.txt")
                    (mapv #(prepare-data %)))]

   (->> (reduce go stack-list commends)
        (map last)
        (clojure.string/join #"")))

(let [commends (->> (util/load-data "2022/data/sample.txt")
                    (mapv #(prepare-data %)))]

   (->> (reduce go stack-sample commends)
        (map last)
        (clojure.string/join #"")))
