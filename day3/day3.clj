(ns day3
  (:gen-class))

(require '[clojure.string :as str])

;; process-file returns a seq of strings in the given file separated by line
(defn process-file [file-name]
   (with-open [rdr (clojure.java.io/reader file-name)]
   (reduce conj [] (line-seq rdr))))

(defn parse-int [n-str & base]
  (if base (Integer/parseInt n-str base) (Integer/parseInt n-str)))

(defn sum-one-bits [bit-counts line]
    (map + bit-counts (map parse-int (str/split line #""))))

(defn get-bit [len comp-op bit-count]
  (if (comp-op bit-count (* len 1/2)) 1 0))

(let [lines    (process-file "day3/input.txt")
      len      (count lines)
      sum-bits (reduce sum-one-bits (repeat (count (first lines)) 0) lines)]
  (println (*
    (Integer/parseInt (reduce str (map (partial get-bit len >) sum-bits)) 2)
    (Integer/parseInt (reduce str (map (partial get-bit len <) sum-bits)) 2)
  ))
)
