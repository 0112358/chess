(ns chess.core
  (:use [chess etc pieces moves board check]))

(defn chess-repl
  [board player]
  (print-board board)
  (println "Player is " player)
  (let [pieceloc (prompt-read "Enter piece location (row col): ")
        newloc (prompt-read "Enter new location (row col): ")
        piece-idx (point->index (input-str-2-int pieceloc))
        selected-piece (get-piece board piece-idx)
        new-idx (point->index (input-str-2-int newloc))
        moves (gen-moves board piece-idx)]
        
    #_(println "moves=" moves)

    (if (not (= player (:color selected-piece)))
      (do
        (println "Please select your own piece.")
        (recur board player))
      
      (if (empty? moves)
        (do
          (println "No valid moves for your selection.")
          (recur board player))
      
        (let [the-move (first (filter #(= (:index %) new-idx) moves))]
          (if (nil? the-move)
            (do
              (println "Invalid move for the selected piece")
              (recur board player))
            
            (let [new-board (move board piece-idx new-idx)
                  other-player (if (= :white player) :black :white)
                  check-status (check-status new-board player other-player)]

              (if (= check-status :check-mate)
                (println "Check & mate. " player " wins.")
                (do
                  (if (= check-status :check)
                    (println other-player " is checked."))
                  (recur new-board other-player))))))))))

(defn -main [& args]
  (chess-repl (make-new-board) :white))
