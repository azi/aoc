(ns main.2022.day1
 (:require
   [main.util :as util]
   [clojure.string :as str]))

;; part 1
(->> (util/read-chunks "2022/data/day1.txt")
     (map #(str/split % #"\n"))
     (map (fn [coll] (map read-string coll)))
     (map #(apply + %))
     sort
     last)


;; part 2
(->> (util/read-chunks "2022/data/day1.txt")
     (map #(str/split % #"\n"))
     (map (fn [coll] (map read-string coll)))
     (map #(apply + %))
     sort
     reverse
     (take 3)
     (apply +))
