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

(defn x-common-bit [comp-op pos count-one count-zero lines]
  (cond
    (= 0 (count lines)) (if (comp-op count-one count-zero) \1 \0)
    (= \1 (get (first lines) pos)) (x-common-bit comp-op pos (+ count-one 1) count-zero (rest lines))
    (= \0 (get (first lines) pos)) (x-common-bit comp-op pos count-one (+ count-zero 1) (rest lines))))

(defn most-common-bit [pos count-one count-zero lines]
  (x-common-bit >= pos count-one count-zero lines))

(defn least-common-bit [pos count-one count-zero lines]
  (x-common-bit < pos count-one count-zero lines))

(defn filter-bits [common-bit pos all-lines line]
  (=
    common-bit
    (get line pos)))

(defn filter-all [lines pos bit-matcher]
  (loop [pos 0
         rem-lines lines]
    (if (= 1 (count rem-lines)) (first rem-lines)
      (recur (+ 1 pos) (filter (partial filter-bits (bit-matcher pos 0 0 rem-lines) pos rem-lines) rem-lines))
    )
  ))

(let [lines    (process-file "day3/input.txt")
      len      (count lines)
      sum-bits (reduce sum-one-bits (repeat (count (first lines)) 0) lines)]
  (println (*
    (Integer/parseInt (reduce str (map (partial get-bit len >) sum-bits)) 2)
    (Integer/parseInt (reduce str (map (partial get-bit len <) sum-bits)) 2)
  ))
  (println (*
    (Integer/parseInt (filter-all lines 0 most-common-bit) 2)
    (Integer/parseInt (filter-all lines 0 least-common-bit) 2)
  ))
)
