(ns chess.board
  (:use [chess etc pieces])
  (:import [chess.pieces ChessPiece]))

(defn make-new-board
  "Generates a new board"
  []
  (let [back-row [:Rook :Horse :Bishop :Queen :King :Bishop :Horse :Rook]
        black-back-row (map #(ChessPiece. :black %) back-row)
        black-pawns (repeat 8 (ChessPiece. :black :Pawn))
        white-back-row (map #(ChessPiece. :white %) back-row)
        white-pawns (repeat 8 (ChessPiece. :white :Pawn))
        empty-rows (repeat (* 8 4) (ChessPiece. :nil :Empty))] ; 4 empty rows 
    (vec (concat black-back-row black-pawns empty-rows white-pawns white-back-row))))

(defn print-board
  "Prints the chess board. Nothing but side effects"
  [board]
  (let [idx-row (apply str (map #(str % "   ") (range 8)))
        rr (partition 8 board)
        rows (map #(str % " " (apply str (interpose " " (map terse-str %2)))) (range 8) rr)]
    (println "  " idx-row)
    (loop [coll rows]
      (when (not (empty? coll))
        (println (first coll))
        (recur (rest coll))))))

(defn move
  [board from to]
  (let [piece (nth board from)]
    (assoc (assoc board to piece)
      from (ChessPiece. :nil :Empty))))

(defn get-piece
  ([board x y] (get-piece board (point->index x y)))
  ([board n]
     (nth board n)))

(defn get-king-coords
  [board king-color]
  "Answers with a map of row, col, index keys for king location"
  (let [king-match? (fn [{:keys [color role]} ]
                      (and (= color king-color)
                           (= :King role)))
        index (first (where (map king-match? board)))]
    (if (not (nil? index))
      (let [[r c] (index->point index)]
        {:row r :col c :index index}))))

(defn all-piece-indices-for-color
  [board color]
  (where (map #(= (:color %) color) board))) 