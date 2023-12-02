(ns main.2023.day2
 (:require
   [main.util :as util]
   [clojure.string :as str]))

;; https://adventofcode.com/2023/day/2

;; ==== murmur ====
;; 還沒整理
;; 一開始看到這樣的 sample Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; 就在想 regexp 直接就解掉, 大概像這類的去matched {1-12}red / {1-13}green / {1-14}blue
;; 不過偶寫不出來，攤手～
;; 只好用跑迴圈來轉格式, 大概是這方向吧
;; {:1 [[[3 "blue"] [4 "red"]]
;;      [[1 "red"]  [2 "green"]]
;;      [[2 "green"]]]}

;; 這題轉資料轉很久啊～～～心有不甘,
;; 寫的真的很落漆
;; 不知道是否有不是轉資料的寫法

(defn r2 [arr]
 (->> (map #(re-seq #"\d+ \w+" %) arr)
      (map #(str/split (first %) #" "))
      (map (fn [[_n color]]
             (let [n (util/str->int _n)]
              (or
                (and (= "red" color) (<= n 12))
                (and (= "green" color) (<= n 13))
                (and (= "blue" color) (<= n 14))))))
      (every? true?)))

;; 261 -> too low
;; 2685 -> correct result
;; Day2-1
(->> (for [x (util/read-file-by-line "2023/data/day2.txt")
           :let [s1 (str/split x #":")
                 t  (util/str->int (first (re-seq #"\d+" (first s1))))
                 r  (->> (str/split (last s1) #";")
                         (map #(str/split % #","))
                         (map #(r2 %))
                         (every? true?))]
           :when (true? r)]
       t)
     (apply +))

;; Day2-2
;; result: 83707
(->> (for [x (util/read-file-by-line "2023/data/day2.txt")
           :let [s1 (str/split x #":")
                 t  (util/str->int (first (re-seq #"\d+" (first s1))))
                 r  (->> (str/split (last s1) #";")
                         (map #(str/split % #","))
                         flatten
                         (map #(re-seq #"\d+ \w+" %))
                         (map #(str/split (first %) #" "))
                         (map (fn [[n c]] [(util/str->int n) c]))
                         (group-by last)
                         (map (fn [[_c items]]
                                  (first (last (sort-by first items)))))
                         (apply *))]]
      r)
    (apply +))
