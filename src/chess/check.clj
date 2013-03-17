(ns chess.check
  (require [clojure.tools.logging :as log])
  (:use [chess etc board moves]))

(defn- is-check?
  [board indx king-index]
  (let [moves (gen-moves board indx)]
    (log/info "is-check? indx=" indx ", king-index " king-index " moves " moves)
    (not (nil? (some #(= (:index %) king-index) moves)))))
        

(defn- find-indices-which-check
  [board indices king-index]
  (for [i indices
        :when (is-check? board i king-index)]
    i))

(defn- find-check-indices
  [board player-color other-player-color]
  (let [indices (all-piece-indices-for-color board player-color)            ; get all locations of player-color pieces
        king-index (:index (get-king-coords board other-player-color))]      ; get location of other player's king
    (log/info "find-check-indices: indices=" indices)
    (find-indices-which-check board indices king-index)))

(defn- is-checked?
  [board player-color other-player-color]
  (let [indices (find-check-indices board player-color other-player-color)]
    (not (zero? (count indices)))))

(defn- still-checked?
  [board player-color other-player-color]
  (let [indices (all-piece-indices-for-color board other-player-color)
        checked (for [idx indices
                      :let [mm (gen-moves board idx)
                            new-boards (map #(move board idx (:index %)) mm)
                            boards-status (map #(is-checked? % player-color other-player-color) new-boards)
                            checked-count (count (where boards-status))]
                      :while (= (count boards-status) checked-count)] ; while all boards are checked keep looking
                  true)]
    (= (count indices) (count checked))))

  

(defn check-status
  "Answers with :check or :check-mate or nil"
  [board player-color other-player-color]
  (let [indices (find-check-indices board player-color other-player-color)
        has-checks? (not (zero? (count indices)))]
    (if has-checks?
      (let [second-check (still-checked? board player-color other-player-color)]
        (if second-check
          :check-make
          :check)))))
  
    
                                        ; is there a move that other-player can make which yields a board which does not result in a check situation
                                        ; get all moves for each piece belonging to other-player
                                        ; apply each move to the current board
                                        ; evaluate ischecked? if still checked then check mate
  