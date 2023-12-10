(ns main.2023.day3
 (:require
   [main.util :as util]
   [clojure.string :as str]))

;; https://adventofcode.com/2023/day/3

;; ==== murmur ====
;; 一看到題目就想到去年也差不多是停在這類題型上面, 大魔王～
;;

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; (def tblr
;;   (let [source (->> (util/load-data "2023/data/sample.txt")
;;                     (mapv #(str/split % #"")))
;;         data  (for [[y vals] (map-indexed vector source)
;;                     [x val]  (map-indexed vector vals)]
;;                 {:x x :y y :val val})]
;;     data))

(def sample
  (->> (util/load-data "2023/data/sample.txt")
       (mapv #(str/split % #""))))

(def source
  (->> (util/load-data "2023/data/day3.txt")
       (mapv #(str/split % #""))))

(defn wrapped-data [source]
  (for [
        [x vals] (map-indexed vector source)
        [y _val]  (map-indexed vector vals)
        :let [
              v  (get-in source [x y])
              t  (get-in source [(- x 1) y])
              d  (get-in source [(+ x 1) y])
              r  (get-in source [x (+ y 1)])
              l  (get-in source [x (- y 1)])
              lt (get-in source [(- x 1) (- y 1)])
              ld (get-in source [(- x 1) (+ y 1)])
              rt (get-in source [(+ x 1) (- y 1)])
              rd (get-in source [(+ x 1) (+ y 1)])
              digit? (number? (util/str->int v))
              star-sym? (clojure.set/intersection (set ["*"]) (set [t d r l lt ld rt rd]))
              sym? (clojure.set/intersection (set ["%" "&" "#" "*" "+" "$" "@" "=" "-" "_" "*" "\\" "/"]) (set [t d r l lt ld rt rd]))]]
    {:x x :y y :v v :sym? sym? :star-sym? star-sym? :digit? digit?}))

(wrapped-data sample)
(wrapped-data source)

(defn all-digit? [items]
   (every? true? items))

(defn valid-items [items]
 (let [digit (util/str->int (str/join (map #(:v %) items)))
       all-digit? (every? true? (map #(:digit? %) items))
       no-any-sym?   (every? empty? (map #(:sym? %) items))]
  [digit (and all-digit? no-any-sym?)]))

(valid-items '({:x 9, :y 5, :v "5", :sym? #{}, :digit? false}
               {:x 9, :y 6, :v "9", :sym? #{}, :digit? true}
               {:x 9, :y 7, :v "8", :sym? #{}, :digit? true}))

;; Day1: 554003
(defn part-one [parse]
 (let [dada (->> parse
                 (partition-by :x)
                 (map #(partition-by :digit? %)))
       items (for [row dada
                   :let [
                         _col (map #(map :v %) row)
                         valid-item (map #(valid-items %) row)]]
                valid-item)]
   (->> items
        flatten
        (partition 2)
        (filter #(and (false? (last %)) (number? (first %))))
        (map #(first %))
        (apply +))))

(part-one (wrapped-data sample)) ;; 4361
(part-one (wrapped-data source)) ;; 55003


; ========part 2=============================

(defn format-items [items]
 (let [digit (util/str->int (str/join (map #(:v %) items)))
       grid  (map #(select-keys % [:x :y]) items)
       all-digit? (every? true? (map #(:digit? %) items))
       _no-any-sym?   (every? empty? (map #(:sym? %) items))]
  [digit grid (and all-digit?)]))

;; (format-items '({:x 9, :y 5, :v "5", :sym? #{}, :star-sym? #{} :digit? true}
;;                 {:x 9, :y 6, :v "9", :sym? #{""}, :star-sym? #{} :digit? true}
;;                 {:x 9, :y 7, :v "8", :sym? #{"%"}, :star-sym? #{} :digit? true}))


(defn valid-star-items [x y items]
 (let [_v  (filter #(and (= (:x %) x)       (= (:y %) y)) items)
       t  (filter #(and (= (:x %) (- x 1)) (= (:y %) y)) items)
       d  (filter #(and (= (:x %) (+ x 1)) (= (:y %) y)) items)
       r  (filter #(and (= (:x %) x)       (= (:y %) (+ y 1))) items)
       l  (filter #(and (= (:x %) x)       (= (:y %) (- y 1))) items)
       lt (filter #(and (= (:x %) (- x 1)) (= (:y %) (- y 1))) items)
       ld (filter #(and (= (:x %) (- x 1)) (= (:y %) (+ y 1))) items)
       rt (filter #(and (= (:x %) (+ x 1)) (= (:y %) (- y 1))) items)
       rd (filter #(and (= (:x %) (+ x 1)) (= (:y %) (+ y 1))) items)]
       ;; _ (prn "--" [t d r l lt ld rt rd])]

   (some? (some #(not-empty %) [t d r l lt ld rt rd]))))

(valid-star-items 1 3 '({:x 0, :y 0} {:x 0, :y 1} {:x 0, :y 2}))


;; Day2:
(defn part-two [source]
 (let [dada (->> source
                 (partition-by :x)
                 (map #(partition-by :digit? %)))
       star-list (->> source
                      (filter #(= "*" (:v %))))
       items (->> (for [row dada
                        :let [
                              _col (map #(map :v %) row)
                              formated-items (->> row
                                                  (map #(format-items %))
                                                  (filter #(true? (last %))))]]
                   formated-items)
                  (remove #(empty? %))
                  (reduce concat []))]


      (->> (for [x star-list
                 [y1 y _] items]
            ;; :when (valid-star-items (:x x) (:y x) y)]
            [(select-keys x [:x :y]) y1 (valid-star-items (:x x) (:y x) y)])
           (filter #(true? (last %)))
           (partition-by #(first %))
           (filter #(= 2 (count %)))
           (map (fn[coll] (->> (map #(% 1) coll)
                               (apply *))))
           (apply +))))

(part-two (wrapped-data source)) ;; 87263515


;; 1. [數字,   (數字的x, y)]
;; 2. [sym *,  (star-sym 的x , y)]
;; 3. 比較 star-sym x y 有沒有被包含在數字的xy array裡

;; ([467 ({:x 0, :y 0} {:x 0, :y 1} {:x 0, :y 2}) true]
;;  [114 ({:x 0, :y 5} {:x 0, :y 6} {:x 0, :y 7}) true]
;;  [35 ({:x 2, :y 2} {:x 2, :y 3}) true]
;;  [633 ({:x 2, :y 6} {:x 2, :y 7} {:x 2, :y 8}) true]
;;  [617 ({:x 4, :y 0} {:x 4, :y 1} {:x 4, :y 2}) true]
;;  [58 ({:x 5, :y 7} {:x 5, :y 8}) true]
;;  [592 ({:x 6, :y 2} {:x 6, :y 3} {:x 6, :y 4}) true]
;;  [755 ({:x 7, :y 6} {:x 7, :y 7} {:x 7, :y 8}) true]
;;  [664 ({:x 9, :y 1} {:x 9, :y 2} {:x 9, :y 3}) true]
;;  [598 ({:x 9, :y 5} {:x 9, :y 6} {:x 9, :y 7}) true])
;; ; --------------------------------------------------------------------------------
;; ; eval (current-form): (let [dada (->> (wrapped-data sample) (partition-by :x) (map #(parti...
;; ({:x 1, :y 3, :v "*", :sym? #{}, :star-sym? #{}, :digit? false}
;;  {:x 4, :y 3, :v "*", :sym? #{}, :star-sym? #{}, :digit? false}
;;  {:x 8, :y 5, :v "*", :sym? #{}, :star-sym? #{}, :digit? false})


;; ([{:x 1, :y 3} 467 true]
;;  [{:x 1, :y 3} 35 true]
;;  [{:x 4, :y 3} 617 true]
;;  [{:x 8, :y 5} 755 true]
;;  [{:x 8, :y 5} 598 true])
