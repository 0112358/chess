(ns chess.etc)

(defn cross
  "Generates cross product"
  [xs ys]
  (for [x xs
        y ys]
    [x y]))

(defn replace-items
  "Replace target with replacement within coll"
  [coll target replacement]
  (for [x coll
        :let [y (if (= target x) replacement x)]]
    y))

(defn prompt-read
  [prompt]
  (print (format "%s" prompt))
  (flush)
  (read-line))

(defn point->index
  "Takes an (x y) and transaltes to location in vector"
  ([p] (point->index (first p) (last p)))
  ([x y]
     (+ (* x 8) y)))

(defn index->point
  "Takes an index and translates to x y point"
  [n]
  (let [x (int (/ n 8.0))
        y (mod n 8)]
    [x y]))


(defn within
  [start end x]
  (and (>= x start)
       (< x end)))

(def within-board-bounds (partial within 0 8))

(defn input-str-2-int
  [s]
  (let [trimmed (clojure.string/trim s)]
    (map #(Integer/parseInt %) (clojure.string/split trimmed #" "))))


(defn where
  "Takes a sequence of boolean values and answers with the indices where true"
  [coll]
  (for [x (range (count coll))
        :when (nth coll x)]
    x))

(defn assoc-all
  "Takes a map and a seq of keys and a seq of vals and assoc the head of keys and vals in to the map"
  [map keys vals]
  (if (empty? keys)
    map
    (recur (assoc map (first keys) (first vals)) (rest keys) (rest vals))))

(defn curry
  "Currys a function eliding args via :_"
  [fname & args-curry]
  (let [elided (where (map #(= % :_) args-curry))]
    (fn [& args]
      (assert (= (count args) (count elided)))
      (apply fname (assoc-all (vec args-curry) elided (vec args))))))

(defn indices
  [pred coll]
  (for [idx (range (count coll))
        :let [item (nth coll idx)]
        :when (pred item)]
    idx))
        
  