(ns main.2023.day1
 (:require
   [main.util :as util]
   [clojure.string :as str]))

;; https://adventofcode.com/2023/day/1
;; 題1, 單純把數字抓出來後，再抓頭抓尾出來計算就好

;; result: 54450
;; part 1
(->> (util/read-file-by-line "2023/data/day1.txt")
     (map #(re-seq #"\d" %))
     (map (fn [item] [(first item) (last item)]))
     (map #(str/join "" %))
     (map #(util/str->int %))
     (apply +))


(def mapping
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"
   "1"     "1"
   "2"     "2"
   "3"     "3"
   "4"     "4"
   "5"     "5"
   "6"     "6"
   "7"     "7"
   "8"     "8"
   "9"     "9"})


;; "ninefourone1" => 9
;; "inefourone1"  => 4
;; "nefourone1"   => 4
;; "efourone1"    => 4
;; "fourone1"     => 4
;; "ourone1"      => 1
;; "urone1"       => 1
;; "rone1"        => 1
;; "one1"         => 1
;; "ne1"          => nil
;; "e1"           => nil
;; "1"            => nil

(defn matched-nums [txt]
 (for [y (range (count txt))
       :let [sub-t (subs txt y)
             matched (re-find (re-pattern (str/join "|" (keys mapping))) sub-t)]
       :when (some? matched)]
   [(mapping matched)]))


(comment
 (matched-nums "ninefourone1"))

;; 題2, 想了半天試圖想用replace的方法換掉找到的英文數字，但後來發現英文數字的字母
;; 會有前後重覆到的狀況，ex: nineight 若我先把nine 換成了9 那更後面的8就不會被抓出來換掉了
;; 想了半天用迴圈去掃一輪換掉英文數字後，再跟題1一樣抓頭抓尾來處理。

;; part 2
;; 54258 -> too low
;; 54265 -> correct result
;; 54289 -> too heigt

(->> (util/read-file-by-line "2023/data/day1.txt")
     (map #(flatten (matched-nums %)))
     (map (fn [item] [(first item) (last item)]))
     (map #(str/join "" %))
     (map #(util/str->int %))
     (apply +))


;; ==== murmur ====
;; re-pattern
;; re-sep
;; re-matcher
;; re-find
;; 上面這組有夠複雜的，研究了半天結果也沒用到太多
