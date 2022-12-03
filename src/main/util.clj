(ns main.util
 (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn load-data [path]
  (with-open [txt (io/reader (str "src/main/" path))]
   (->> (line-seq txt)
        (into []))))

(defn blank? [v] (if (seqable? v) (empty? v) (not v)))
(def present? (complement blank?))

(def str->int #(try (Integer/parseInt %) (catch Exception _ex %)))

;; reference https://github.com/tschady/advent-of-code/blob/main/src/aoc/file_util.clj
(defn read-file
  "Return full file contents from `path`."
  [path]
  (-> (str "../src/main/" path) io/resource slurp))

(defn read-chunks
  "Return file contents as collection of chunks, where chunks are separated by a
  full blank line."
  [path]
  (-> path read-file (str/split #"\n\n")))

(defn read-file-by-line
  [path]
  (-> path
      read-file
      str/split-lines))

(comment
  (load-data "2022/data/day1.txt")
  (read-file "2022/data/day1.txt")
  (read-chunks "2022/data/day1.txt")
  (read-file-by-line "2022/data/day1.txt"))
