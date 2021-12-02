(ns day1
  (:gen-class))

;; process-file returns a seq of strings in the given file separated by line
(defn process-file [file-name]
   (with-open [rdr (clojure.java.io/reader file-name)]
   (reduce conj [] (line-seq rdr))))

(defn parse-int [n-str]
  (Integer/parseInt n-str))

(defn sum-increases [ints]
  (if (<= (count ints) 3)
    0
    (+ (sum-increases (rest ints))
      (if (< (+ (first ints) (second ints) (nth ints 2))
             (+ (second ints) (nth ints 2) (nth ints 3)))
        1
        0))))

(println (sum-increases (map parse-int (process-file "day1/input.txt"))))
