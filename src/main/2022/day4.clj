(ns main.2022.day4
 (:require
   [main.util :as util]))

(defn prepare-data [s]
  (->> (re-find (re-matcher #"(\d+)-(\d+),(\d+)-(\d+)" s))
       rest
       (map #(util/str->int %))
       (partition 2)))

(defn include? [t, [c1 c2]]
  (let [s1 (set (range (first c1) (+ 1 (last c1))))
        s2 (set (range (first c2) (+ 1 (last c2))))]
    (case t
      :all (or (clojure.set/superset? s1 s2)
               (clojure.set/superset? s2 s1))

      :any (->> (clojure.set/intersection s1 s2)
                util/present?))))

(defn run [t]
 (->> (util/load-data "2022/data/day4.txt")
      (map #(prepare-data %))
      (map #(include? t %))
      (filter true?)
      count))

;; part 1
(run :all)

;part 2
(run :any)
