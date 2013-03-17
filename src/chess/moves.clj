(ns chess.moves
  (require [clojure.tools.logging :as log])
  (:use [chess etc pieces board]))

(def rows 8)
(def cols 8)

(defn- bounded?
  ([point-map] (bounded? (:row point-map) (:col point-map)))
  ([row col]
     "Answers if the indices are bounded by the board"
     (and (within 0 rows row)
          (within 0 cols col))))

(defn- add-index
  [point-maps]
  (map #(assoc % :index (point->index (:row %) (:col %))) point-maps))

(defn- gen-coords
  "Generates new coordinates. row-op and col-op are diadic functions. row and col are integers"
  [row col delta-range row-op col-op]
  (let [ not-input? (fn [r c]  ;  answer true if the coords are NOT the same as input row/col
                      (not (and (= r row)
                                (= c col))))]
    (for [delta (range delta-range)
          :let [r (row-op row delta)
                c (col-op col delta)]
          :while (bounded? r c)
          :when (not-input? r c)]
      {:row r :col c :index (point->index r c)})))

(defn- filter-allowed-moves
  "Keep only items to which a piece can move to. ie empty spot or a piece of a diff color."
  [board n point-maps]
  (let [piece (get-piece board n)]
    (log/debug "filter-allowed-moves point-maps" point-maps)
    (filter #(diff-color? piece (get-piece board (:index %))) point-maps)))

(defn- filter-seq-moves
  "Keep only items until a piece can move to.
Stop where the color is the same. Not including that one.
Stop at first location where the color is diff including that one."
  [board n point-maps]
  (let [piece (get-piece board n)
        not-nil-color? (fn [m]
                         (let [cp (get-piece board (:index m))]
                           (not= :nil (:color cp))))
        first-piece-index (first (indices not-nil-color? point-maps))]
    (if (nil? first-piece-index)
      point-maps
      (let [first-piece-loc (nth point-maps first-piece-index)
            first-piece (get-piece board (:index first-piece-loc))
            item-count (if (diff-color? first-piece piece)
                         (inc first-piece-index)  ; different color, include everything up to and including first-piece-index
                         first-piece-index)]
        (take item-count point-maps)))))
          
(defn- gen-moves-straight
  [board n delta-range]
  (let [[row col] (index->point n)
        moves-fn (partial gen-coords row col delta-range)
        filter-fn (partial filter-seq-moves board n)
        moves-right (moves-fn (fn[r d] r) +)  ; keep the row fixed, add deltas to the column
        moves-left (moves-fn (fn[r d] r) -)   ; keep the row fixed, sub deltas from the column
        moves-down (moves-fn + (fn[c d] c))   ; add to the row, keep the column fixed
        moves-up (moves-fn - (fn[c d] c))     ; sub from the row, keep column fixed
        amr (filter-fn moves-right)
        aml (filter-fn moves-left)
        amd (filter-fn moves-down)
        amu (filter-fn moves-up)
        moves (filter #(not (empty? %)) [amr aml amd amu])]
    (flatten moves)))   ; FIXME: not sure why this is a list of lists

(defn- gen-moves-diagonal
  [board n delta-range]
  (let [[row col] (index->point n)
        moves-fn (partial gen-coords row col delta-range)
        filter-fn (partial filter-seq-moves board n)
        down-right (moves-fn + +)
        down-left (moves-fn + -)
        up-right (moves-fn - +)
        up-left (moves-fn - -)
        adr (filter-fn down-right)
        adl (filter-fn down-left)
        aur (filter-fn up-right)
        aul (filter-fn up-left)
        moves (filter #(not (empty? %)) [adr adl aur aul])]
    (flatten moves)))

(defmulti gen-moves
  (fn [board n]
    (:role (nth board n))))

(defmethod gen-moves :Pawn
  [board n]
  (let [[row col] (index->point n)
        piece (nth board n)
        is-white? (white? piece)
        op (if is-white? - +)
        new-row (op row 1)
        pmm [ {:row new-row :col col :type :straight}
              {:row new-row :col (dec col) :type :diagonal}
              {:row new-row :col (inc col) :type :diagonal}]
        mm (filter bounded? (add-index pmm))
        moves (if (or (and is-white?
                           (= 6 row))
                      (and (not is-white?)
                           (= 1 row)))
                (conj mm {:row (op new-row 1) :col col :type :straight :index (point->index (op new-row 1) col)})
                mm)]
    (log/debug "pawn pmm: " pmm " mm: " mm " moves: " moves)
   (filter-allowed-moves board n moves)))

(defmethod gen-moves :Horse
  [board n]
  (let [[row col] (index->point n)
        pmm [ {:row (+ row 2) :col (inc col) :type :na}
              {:row (+ row 2) :col (dec col) :type :na}
              {:row (- row 2) :col (inc col) :type :na}
              {:row (- row 2) :col (dec col) :type :na}
              {:row (inc row) :col (+ col 2) :type :na}
              {:row (dec row) :col (+ col 2) :type :na}
              {:row (inc row) :col (- col 2) :type :na}
              {:row (dec row) :col (- col 2) :type :na} ]
        mm (add-index (filter bounded? pmm)) ]
    (filter-allowed-moves board n mm)))

(defmethod gen-moves :Rook
  [board n]
  (gen-moves-straight board n rows))
  
(defmethod gen-moves :Bishop
  [board n]
  (gen-moves-diagonal board n rows))

(defmethod gen-moves :Queen
  [board n]
  (let [straight-moves (gen-moves-straight board n rows)
        diagonal-moves (gen-moves-diagonal board n rows)]
    (into straight-moves diagonal-moves)))
        
(defmethod gen-moves :King
  [board n]
  (let [straight-moves (gen-moves-straight board n 2)   ; 2 because this is delta, (range 2) --> (0 1) delta at 0 is the same spot, 1 is next move
        diagonal-moves (gen-moves-diagonal board n 2)]
    (into straight-moves diagonal-moves)))

(defmethod gen-moves :Empty
  [board n]
  [])
