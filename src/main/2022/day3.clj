(ns main.2022.day3
 (:require
   [main.util :as util]))

(def val-index (->> (map-indexed (fn [idx itm] [(str itm) (+ 1 idx) ]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
                    (into {})))

;; part 1
(defn find-intersection [s]
 (->> (partition (/ (count s) 2) s)
      (map set)
      (apply clojure.set/intersection)
      (apply str)))

(->> (util/load-data "2022/data/day3.txt")
     (map #(find-intersection %))
     (map #(val-index %))
     (apply +))

;part 2
(defn find-intersection-from-coll [coll]
 (->> (map set coll)
      (apply clojure.set/intersection)
      (apply str)))

(->> (util/load-data "2022/data/day3.txt")
     (partition 3)
     (map #(find-intersection-from-coll %))
     (map #(val-index %))
     (apply +))

