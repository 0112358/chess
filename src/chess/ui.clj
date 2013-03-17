(ns chess.ui
  (:use seesaw.core)
  (:use [chess etc pieces moves board check])  
  (require [clojure.tools.logging :as log]
           [clj-logging-config.log4j :as log-conf]))

(declare when-mouse-clicked)

(defonce chess-board (atom (make-new-board)))
(defonce selected-index (atom -1))
(defonce dest-index (atom -1))
(defonce current-player (atom :white))

(defonce chess-grid-panel (grid-panel :rows 8 :columns 8 :hgap 0 :vgap 0))

(defonce main-frame (frame :title "Chess"))
(defonce msg-area (label))

(defn- change-player!
  []
  (reset! current-player (if (= :white @current-player) :black :white)))

(defn- reset-chess-board!
  "Reset the binding for chess-board"
  [new-board]
  (reset! chess-board new-board))

(defn- reset-selected-index!
  [n]
  (reset! selected-index n))

(defn- reset-dest-index!
  [n]
  (reset! dest-index n))

(defn- update-msg-area
  [msg]
  (config! msg-area :text msg))

(defn- clear-msg-area
  []
  (update-msg-area ""))

(defn- make-square
  "Makes a single square which is a label"
  [index bg-color]
  (let [lbl (label :id (-> index str keyword)
                   :background bg-color
                   :halign :center
                   :h-text-position :center
                   :v-text-position :center
                   :user-data {:index index})]
    (listen lbl :mouse-clicked when-mouse-clicked)
    lbl))
             

(defn- make-chess-grid
  "Makes a grid of alternating colors for the chess board.
Returns a sequences of labels."
  []
  (let [colors1 (interleave (repeat 4 :green) (repeat 4 :blue))
        colors2 (rotate 1 colors1)
        colors (flatten (repeat 4 (concat colors1 colors2)))]
    (for [i (range (count colors))
          :let [color (nth colors i)]]
      (make-square i color))))


(defn- update-square!
  "Update the lbl with data from the chess piece"
  [lbl piece]
  (log/info "update-square! piece=" piece ", lbl=" lbl)
  (let [empty-px? (empty-piece? piece)
        text (if empty-px? "" (terse-str piece))
        color (if empty-px? :white (:color piece))]
    (config! lbl
             :text text
             :foreground color)))


  
(defn- update-board!
  "Updates the existing ui with the board specified"
  [board]
  (let [lbls (for [i (range (count board))]
               (select chess-grid-panel [(keyword (str "#" i))]))]
    (log/debug "before calling map lbls " (count lbls) ", board " (count board))
    (map #(update-square! % %2) lbls board)
    #_(log/debug "after calling map in update-board!")))

(defn- apply-user-move
  []
  (log/debug "in apply-user-move selected-index=" @selected-index ", dest-index=" @dest-index)
  (let [moves (gen-moves @chess-board @selected-index)]
    (log/info "moves: " moves)
    (if (empty? moves)
      (update-msg-area "No valid moves for your selection.")
      (let [the-move (first (filter #(= (:index %) @dest-index) moves))]
        (if (nil? the-move)
          (update-msg-area "Invalid move for the selected piece")
          (let [player @current-player
                other-player (change-player!)
                new-board (move @chess-board @selected-index @dest-index)
                check-status (check-status new-board player other-player)]
            (log/debug "the move is " the-move)
            (reset-chess-board! new-board)
            (log/debug "reset the chess board")
            (invoke-now (update-board! new-board))
            (log/debug "updated the board")))))
    (reset-selected-index! -1)
    (reset-dest-index! -1)))
          
            

(defn- when-mouse-clicked
  [lbl]
  (let [{:keys [index]} (user-data lbl)]
    (if (and (= -1 @selected-index)
             (= -1 @dest-index))
      (do
        (reset-selected-index! index)
        (update-msg-area (str "Selected index: " index)))
      (when (and (not= -1 @selected-index)
                 (= -1 @dest-index))
        (reset-dest-index! index)
        (update-msg-area (str "Dest index: " index))
        (apply-user-move)))))
  


(defn- build-ui
  []
  (let [border-pnl (border-panel :south msg-area
                                 :center chess-grid-panel)]
  (config! chess-grid-panel :items (make-chess-grid))
  (config! main-frame :content border-pnl)
  (update-board! @chess-board)
  (-> main-frame pack! show!)))

(defn- configure-logging
  []
  (log-conf/set-loggers!
   ["chess.ui" "chess.moves" "chess.check"]
   {:level :debug
    :out (org.apache.log4j.FileAppender.
          (org.apache.log4j.EnhancedPatternLayout.
           org.apache.log4j.EnhancedPatternLayout/TTCC_CONVERSION_PATTERN)
          "logs/chess.log"
          true) }))


(defn -main [& args]
  (configure-logging)
  (log/info "Logging initialized")
  (invoke-later
   (build-ui)))