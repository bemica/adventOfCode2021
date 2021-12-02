(ns day2
  (:gen-class))

(use '[clojure.string :only [index-of]])

;; process-file returns a seq of strings in the given file separated by line
(defn process-file [file-name]
   (with-open [rdr (clojure.java.io/reader file-name)]
   (reduce conj [] (line-seq rdr))))

(defn parse-int [n-str]
  (Integer/parseInt n-str))

(defn pilot [directions x y]
  (if (= (count directions) 0) [x y]
  (let [dir (subs (first directions) 0 (index-of (first directions) " "))
        n   (parse-int (subs (first directions) (+ 1 (index-of (first directions) " "))))]
    (cond
      (= dir "forward") (pilot (rest directions) (+ x n) y)
      (= dir "up") (pilot (rest directions) x (- y n))
      (= dir "down") (pilot (rest directions) x (+ y n))))
))

(defn pilot-aim [directions x y a]
  (if (= (count directions) 0) [x y]
  (let [dir (subs (first directions) 0 (index-of (first directions) " "))
        n   (parse-int (subs (first directions) (+ 1 (index-of (first directions) " "))))]
    (cond
      (= dir "forward") (pilot-aim (rest directions) (+ x n) (+ y (* a n)) a)
      (= dir "up") (pilot-aim (rest directions) x y (- a n))
      (= dir "down") (pilot-aim (rest directions) x y (+ a n)))
)))

(println (pilot (process-file "day2/input.txt") 0 0))
(println (reduce * (pilot (process-file "day2/input.txt") 0 0)))

(println (pilot-aim (process-file "day2/input.txt") 0 0 0))
(println (reduce * (pilot-aim (process-file "day2/input.txt") 0 0 0)))
