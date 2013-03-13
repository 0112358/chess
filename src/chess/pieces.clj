(ns chess.pieces
  (:use [chess etc]))

(defrecord ChessPiece [color role])

(defn nil-color?
  [x]
  (= :nil (:color x)))
  

(defn diff-color?
  [cp1 cp2]
  (not= (:color cp1) (:color cp2)))

(defn empty-piece?
  [chess-piece]
  (= (:role chess-piece) :Empty))

(defn terse-str
  [chess-piece]
  (if (empty-piece? chess-piece)
    "   "
    (str ":"
         (clojure.string/capitalize (first (name (:color chess-piece))))
         (clojure.string/capitalize (first (name (:role chess-piece)))))))

(defn white?
  [chess-piece]
  (= :white (:color chess-piece)))

(defn black?
  [chess-piece]
  (not (white? chess-piece)))

(defn occupied?
  ([board x y] (occupied? board (point->index x y)))
  ([board n]
     (not= :Empty (:role (nth board n)))))

        