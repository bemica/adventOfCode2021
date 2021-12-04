(ns day3
  (:gen-class))

(require '[clojure.string :as str])

;; process-file returns a seq of strings in the given file separated by line
(defn process-file [file-name]
   (with-open [rdr (clojure.java.io/reader file-name)]
   (reduce conj [] (line-seq rdr))))

(defn parse-int [n-str]
  (Integer/parseInt n-str))

(defn get-moves [moves]
  (map parse-int (str/split moves #",")))

(defn make-bingo-card [unprocessed-rows processed-rows]
  (if (= 0 (count unprocessed-rows)) processed-rows
    (make-bingo-card (rest unprocessed-rows)
      (conj processed-rows (map (fn [x] [(parse-int x) false])
                                (str/split (str/triml (first unprocessed-rows)) #"\s+"))))))

(defn get-bingo-cards [bingo-cards]
  (if (<= (count bingo-cards) 1)
    bingo-cards
    (do
      ; (println bingo-cards)
      (conj (get-bingo-cards (nthrest bingo-cards 6)) (make-bingo-card (take 5 bingo-cards) (list))))))

(defn mark-row [move row]
  (pmap (fn [n] (if (= (first n) move) [(first n) true] n))
    row))

(defn mark-card [move bingo-card]
  (pmap (partial mark-row move) bingo-card))

(defn marked? [n]
  (last n))

(defn row-column-won? [row]
  (every? marked? row))

(defn get-column [card n]
  (loop [column `()
         rem-rows card]
    (if (= 0 (count rem-rows)) column
      (recur (conj column (nth (first rem-rows) n)) (rest rem-rows)))))

(defn card-won? [card]
  (or (some row-column-won? card)
    (some row-column-won? (map (partial get-column card) (range 5)))))

(defn sum-row [y row]
  (reduce (fn [x n]
    (if (marked? n) x (+ (first n) x)))
    y row))

(defn sum-unmarked [card]
    (reduce sum-row 0 card))

(defn mark-all-cards [bingo-cards all-moves]
  (loop [cards bingo-cards
         moves all-moves
         prev-move -1]
    (if (or (= 0 (count moves))
            (some card-won? cards))
      (* prev-move (sum-unmarked (first (filter card-won? cards))))
      (recur (pmap (partial mark-card (first moves)) cards) (rest moves) (first moves)))))

(let [lines       (process-file "day4/input.txt")
      moves       (get-moves (first lines))
      bingo-cards (get-bingo-cards (rest (rest lines)))]
  (println moves)
  (println (mark-all-cards bingo-cards moves))
)
