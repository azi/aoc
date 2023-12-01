(ns main.2022.day7
 (:require
   [main.util :as util]
   [clojure.string :as str]))


;; id parent-id record-type size  name
    ;; current-node-id 0  ; cd /

    ;; 0    0         D         0      /
    ;; 1    0         D         0     a
    ;; 2    0         F        3434   b.txt
    ;; 3    0         F        333    c.dat
    ;; 4    0         D         0     d

    ;; current-node-id 1 ; cd a

    ;; 5   1          D         0     e
    ;; 6   1          F         3333   f
    ;; 7   1          F         3454   g
    ;; 8   1          F         3333   h.lst

    ;; current-node-id 5 ; cd e

    ;; 9   5         F          584   i

    ;; current-node-id 1 ; cd ..

    ;; current-node-id 0 ; cd ..

    ;; current-node-id 4 ; cd d

    ;; 10  4         F         333   d.log
    ;; 11  4         F         333   d.ext
    ;; 12  4         F         333   k]))

;; $ cd /
;; $ ls
;; dir a
;; 14848514 b.txt
;; 8504156 c.dat
;; dir d
;; $ cd a -> setup current-node-id
;; $ ls
;; dir e
;; 29116 f
;; 2557 g
;; 62596 h.lst
;; $ cd e  -> setup current-node-id
;; $ ls
;; 584 i
;; $ cd ..
;; $ cd ..
;; $ cd d
;; $ ls
;; 4060174 j
;; 8033020 d.log
;; 5626152 d.ext
;; 7214296 k

;; [{:id 0, :parent-id 0, :size 0, :name "/", :record-type "D"}
;;  {:id 1, :parent-id 0, :size 0, :name "a", :record-type "D"}
;;  {:id 2, :parent-id 0, :size 14848514, :name "b.txt", :record-type "F"}
;;  {:id 3, :parent-id 0, :size 8504156, :name "c.dat", :record-type "F"}
;;  {:id 4, :parent-id 0, :size 0, :name "d", :record-type "D"}
;;  {:id 5, :parent-id 1, :size 0, :name "e", :record-type "D"}
;;  {:id 6, :parent-id 1, :size 29116, :name "f", :record-type "F"}
;;  {:id 7, :parent-id 1, :size 2557, :name "g", :record-type "F"}
;;  {:id 8, :parent-id 1, :size 62596, :name "h.lst", :record-type "F"}
;;  {:id 9, :parent-id 5, :size 584, :name "i", :record-type "F"}
;;  {:id 10, :parent-id 4, :size 4060174, :name "j", :record-type "F"}
;;  {:id 11, :parent-id 4, :size 8033020, :name "d.log", :record-type "F"}
;;  {:id 12, :parent-id 4, :size 5626152, :name "d.ext", :record-type "F"}
;;  {:id 13, :parent-id 4, :size 7214296, :name "k", :record-type "F"}]


(let [[a b] (re-find (re-matcher #"\$ cd (\S+)" "$ cd /"))]
   (prn b))

(let [[a b] (re-find (re-matcher #"(\S+) (\S+.+\S+)" "4234234 dld.log"))]
   (prn a b))

(defn cd? [input] (str/includes? input "$ cd"))
(defn ls? [input] (str/includes? input "$ ls"))
(defn dir? [input] (str/includes? input "dir"))

; / , cd , ..
(defn commend-process [{:keys [current-node-id nodes] :as ctx} s]
  (let [[_ commend-or-name] (re-find (re-matcher #"\$ cd (\S+)" s))
        node (->> nodes
                  (filter #(and (= current-node-id (:parent-id %))
                                (= commend-or-name (:name %))))
                  first)
        parent-node (->> nodes
                         (filter #(and (= current-node-id (:id %))))
                         first)]
         ;; _ (prn "folder-process:" current-node-id commend-or-name node)]
   (cond
     (= "/" commend-or-name)  (if node
                                (-> ctx
                                    (assoc-in [:current-node-id] (:id node)))

                                (-> ctx
                                    (assoc-in [:nodes] (conj nodes {:id           (count nodes)
                                                                    :parent-id    current-node-id
                                                                    :size        0
                                                                    :name         commend-or-name
                                                                    :record-type "D"}))))
     (= ".." commend-or-name) (-> ctx
                                  (assoc-in [:current-node-id] (:parent-id parent-node)))
     :else (-> ctx
               (assoc-in [:current-node-id] (:id node))))))

(defn dir-process [{:keys [current-node-id nodes] :as ctx} s]
  (let [[_ folder-name] (re-find (re-matcher #"dir (\S+)" s))
        node (->> nodes
                  (filter #(and (= current-node-id (:parent-id %))
                                (= folder-name (:name %))))
                  first)]
         ;; _ (prn "dir-process" ctx folder-name node)]

     (-> ctx
         (assoc-in [:nodes] (conj nodes {:id           (count nodes)
                                         :parent-id    current-node-id
                                         :size        0
                                         :name         folder-name
                                         :record-type "D"})))))

(defn file-process [{:keys [current-node-id nodes] :as ctx} s]
  (let [[size file-name] (str/split s #" ")]
         ;; _ (prn "file-process" ctx)]

     (-> ctx
         (assoc-in [:nodes] (conj nodes {:id           (count nodes)
                                         :parent-id    current-node-id
                                         :size         (util/str->int size)
                                         :name         file-name
                                         :record-type "F"})))))


(defn init-nodes [ctx input]
  ;; (let [_ (prn "evey-time" ctx)]
   (->> (cond
          (cd? input)  (commend-process ctx input)
          (ls? input)  ctx
          (dir? input) (dir-process ctx input)
          :else (file-process ctx input))
        (merge ctx)))

(def dd
  [{:id 0, :parent-id 0, :size 0, :name "/", :record-type "D"}
   {:id 1, :parent-id 0, :size 0, :name "a", :record-type "D"}
   {:id 2,
    :parent-id 0,
    :size 14848514,
    :name "b.txt",
    :record-type "F"}
   {:id 3,
    :parent-id 0,
    :size 8504156,
    :name "c.dat",
    :record-type "F"}
   {:id 4, :parent-id 0, :size 0, :name "d", :record-type "D"}])

(defn get-folder-size [id data]
  (->> (filter #(= id (:parent-id %)) data)
       (map :size)
       (apply +)))

(get-folder-size 0 dd)

(+ 4060174 8033020 5626152 7214296)
(+ 8504156 14848514)
;; (-> {5 584, 4 24933642, 1 94269, 0 23352670}
;;     (assoc-in [6] 333))

{5 584, 4 24933642, 1 94269, 0 23352670}
(+ 584 94269 23352670 24933642)

(defn run []
 (let [data (->> (util/load-data "2022/data/day7.txt")
                (reduce init-nodes {:current-node-id 0,
                                    :nodes []})
                :nodes)
       folder (->> data
                   (filter #(= "D" (:record-type %)))
                   (map :id)
                   (sort >)
                   (map (fn[a] [a (get-folder-size a data)]))
                   (into {}))

        final (reduce
                (fn[acc n]
                  (let [node (first (filter #(= n (:id %)) data))
                        parent-id (:parent-id node)]
                    (if (not= 0 n)
                     (assoc-in acc [parent-id] (+ (get acc parent-id)
                                                  (get acc n)))

                     acc)))

                folder
                (sort > (keys folder)))]
     ;; data))
     ; partI
     ;; (->> final
     ;;      (filter (fn [[_a b]] (<= b 100000)))
     ;;      ;; (filter (fn [[_a b]] (<= b 3562874)))
     ;;      (map last)
     ;;      ;; (apply max))
     ;;      ;; sort
     ;;      (apply +))

     (->> final
          ;; (filter (fn [[_a b]] (<= 8381165 b)))
          (filter (fn [[_a b]] (<= 3562874 b)))
          (map last)
          (apply min))))
          ;; sort)))
          ;; (apply +))))
     ;; final))
    ;; (->> data
    ;;      (filter #(contains? folder (:parent-id %)))
    ;;      (group-by :parent-id))))
        ;; (map (fn[v] (map :size apply +) v)))))
        ;; vals)))
        ;; first)))
        ;; (map :size)
        ;; (apply +))))
        ;; (map #(%)))))
     ;; folder))
      ;; first
      ;; (partition n 1)
      ;; (keep-indexed #(if (= (count (set %2)) n) (+ %1 n)))
      ;; first))

;; part 1
;; (run 4) ; 1929

;; part2
;; (run 14) ; 3298

;; 70000000
;; >= 30-000-000

(- 30000000 (- 70000000 48381165))
(- 30000000 (- 70000000 43562874))
