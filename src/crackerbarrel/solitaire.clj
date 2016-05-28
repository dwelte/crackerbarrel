(ns crackerbarrel.solitaire)

(def base-board
  [[false]
   [true true]
   [true true true]
   [true true true true]
   [true true true true true]])

(defn set-peg
  [board x y value]
  (assoc-in board [y x] value))

(defn get-peg
  [board x y]
  (get-in board [y x]))

(defn valid-peg?
  [board x y]
  (and (<= 0 y (-> board count dec))
       (<= 0 x y)))

(def moves
  [[0 1] [0 -1]
   [1 0] [-1 0]
   [1 1] [-1 -1]])

(defn all-positions
  [board]
  (for [x (-> board count range)
        y (-> board count range)
        :when (<= x y)]
    [x y]))

(defn valid-move?
  [board x y dx dy]
  (and (valid-peg? board (+ x (* 2 dx)) (+ y (* 2 dy)))
       (not (get-peg board (+ x (* 2 dx)) (+ y (* 2 dy))))
       (get-peg board (+ x dx) (+ y dy))
       (get-peg board x y)))

(defn apply-move
  [board x y dx dy]
  (-> board
      (set-peg x y false)
      (set-peg (+ x dx) (+ y dy) false)
      (set-peg (+ x (* 2 dx)) (+ y (* 2 dy)) true)))

(defn board-line-str
  [board y]
  (println "brock " y)
  (str (apply str (repeat (- (count board) y 1) " "))
       (apply str (butlast (flatten (map #(vector (if (get-peg board % y) "*" "_") " ")
                                         (range (inc y))))))
       "\n"))

(defn board-str
  [board]
  (apply str (map #(board-line-str board %) (-> board count range))))

(defn print-board
  [board]
  (print (board-str board)))


(defn valid-moves
  [board]
  (for [[x y]    (all-positions board)
        [dx dy]  moves
        :when    (valid-move? board x y dx dy)]
    [x y dx dy]))

(defn make-move
  [board previous-moves [x y dx dy]]
  [(apply-move board x y dx dy) (concat previous-moves [[x y dx dy]])])


(defn solved-board?
  [board]
  (->> board
       flatten
       (filter #(do %))
       count
       (= 1)))

(defn solve-board
  [board previous-moves]
  (if (solved-board? board)
    [previous-moves]
    (apply concat
           (map #(apply solve-board  %)
                (map #(make-move board previous-moves %)
                     (valid-moves board))))))

(defn -main
  []
  (print-board base-board)
  (println (first (solve-board base-board []))))

