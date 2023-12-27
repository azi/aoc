(ns main.2023.day4
 (:require
   [main.util :as util]
   [clojure.string :as str]))

;; https://adventofcode.com/2023/day/4

(def sample-data
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def pattern #"Card\s+(\d+): (\d*(?: \d*)*) \| (\d*(?: \d*)*)")

(comment
  (re-matches pattern "Card  21:  7 42 25 84 54 11 88  6 55 73 | 86  5 82 70 49 80 21 36 16 34 17 77 44 74 61  1  4 39 45 47  3 81 57 60 24")
  (re-matches pattern "Card 221:  7 42 25 84 54 11 88  6 55 73 | 86  5 82 70 49 80 21 36 16 34 17 77 44 74 61  1  4 39 45 47  3 81 57 60 24"))

(defn transfer-to-set [v]
  (->> (str/split v #" ")
       (remove empty?)
       (map #(util/str->int %))
       set))

(comment
  (transfer-to-set " 1 21 53 59 44"))

(defn cal [b p]
  (clojure.set/intersection b p))

(comment
 (cal #{86 48 41 17 83}  #{86 48 31 6 17 9 83 53})
 (Math/pow 2 3))

(defn parse-data [data]
 (->> data
      (map #(re-matches pattern %))
      (map #(into {:card (util/str->int (get-in % [1]))
                   :banker (transfer-to-set (get-in % [2]))
                   :player (transfer-to-set (get-in % [3]))}))
      (map #(assoc-in % [:intersection] (cal (:banker %) (:player %)))) ; for day1 use
      (map #(assoc-in % [:copy-cards] (take (count (:intersection %)) (range (+ 1 (:card %)) (java.lang.Integer/MAX_VALUE))))))) ; for day2 use

(comment
  (parse-data (util/load-sample-data sample-data))
  (parse-data (util/load-data "2023/data/day4.txt")))



;; ===============Day1======================
(defn day1 [data]
  (let [parsed-data (parse-data data)
        result (->> parsed-data
                    (map #(assoc-in % [:point] (if (empty? (:intersection %))
                                                 0
                                                 (Math/pow 2 (- (count (:intersection %)) 1))))))]

    (->> result
         (map #(:point %))
         (apply +))))

;;ans: 13, 28538
(day1 (util/load-sample-data sample-data))
(day1 (util/load-data "2023/data/day4.txt"))

;; ===============Day2======================
(defn reduce-cards [card pool]
  (let [[n & r] (:copy-cards card)
        next-card (first (filter #(= (:card %) n) pool))]
   (cond
    (nil? n) 1
    :else (+
            (reduce-cards next-card pool)
            (reduce-cards (assoc-in card [:copy-cards] r) pool)))))


(defn day2 [data]
  (let [parse-data (->> data
                        parse-data)]
    (->> (map #(reduce-cards % parse-data) parse-data)
         (apply +))))

;; ans: 30, 9425061 (跑的有點久 QQ)
(day2 (util/load-sample-data sample-data))
(day2 (util/load-data "2023/data/day4.txt"))
