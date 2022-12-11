(ns main.2022.day8
 (:require
   [main.util :as util]
   [clojure.string :as str]))

(def tblr
  (let [source (->> (util/load-data "2022/data/day8.txt")
                    (mapv #(str/split % #""))
                    (mapv (fn [c] (mapv #(util/str->int %) c))))
        max-x (->> source first count)
        max-y (->> source count)
        data  (for [[y vals] (map-indexed vector source)
                    [x val]  (map-indexed vector vals)]
                {:x x :y y :val val})]

   (for [x (range max-x)
         y (range max-y)
         :let [node (->> data
                         (filter #(and (= y (:y %)) (= x (:x %))))
                         first)
               left (->> data
                         (filter #(and (> x (:x %)) (= y (:y %))))
                         reverse)
               right (->> data
                          (filter #(and (< x (:x %)) (= y (:y %)))))
               top   (->> data
                          (filter #(and (< (:y %) y) (= x (:x %))))
                          reverse)
               bottom (->> data
                           (filter #(and (> (:y %) y) (= x (:x %)))))
               outer? (or (contains? (set [0 max-x]) (:x node))
                          (contains? (set [0 max-y]) (:y node)))]]

      {:x x :y y :node node :inner [top bottom left right] :outer? outer?})))


; part I 1647
(->> tblr
     (map (fn [{:keys [outer? inner node]}]
              (or outer?
                  (some true? (->> inner
                                   (map (fn[item]
                                          (every? #(< (:val %) (:val node)) item))))))))
     (filter #(true? %))
     count)

; part II 392080
(defn count-see
  [t [l & rest-item]]
  (cond
    (nil? l) nil
    (>= l t) [l]
    :else (concat [l]  (count-see t rest-item))))

(->> tblr
     (map (fn [{:keys [inner node]}]
            (->> inner
                 (map #(map :val %))
                 (map #(count-see (:val node) %))
                 (map count)
                 (apply *))))
     (apply max))
