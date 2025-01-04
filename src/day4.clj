(ns day4
  (:require [clojure.string :as str]))

(def input (slurp "input/day4.txt"))

(def grid (str/split-lines input))

(defn- dimensions [grid]
  {:length (count (first grid))
   :height (count grid)})

(defn- get-letter [x y grid]
  (let [{:keys [length height]} (dimensions grid)]
    (when (and (and (>= x 0) (>= y 0))
               (and (< x length) (< y height)))
      (nth (nth grid y) x))))

(defn translate-x [x y fx]
  (mapv (fn [x] [(fx x) y]) (range (dec x) (+ 3 x))))

(defn translate-y [x y fy]
  (mapv (fn [y] [x (fy y)]) (range (dec y) (+ 3 y))))

(defn- translate [x y fx fy]
  (let [x' (translate-x x y fx)
        y' (translate-y x y fy)]
    (map (fn [[x _] [_ b]] [x b]) x' y')))

(defn- flip [coords]
  (let [xs (map first coords)
        ys (map second coords)]
    (map vector xs (reverse ys))))

(defn search-indices [x y]
  (let [forward (translate-x x y inc)
        backward (reverse (translate-x (dec x) y dec))
        upward (reverse (translate-y x (dec y) dec))
        downward (translate-y x y inc)
        up-left (reverse (translate (dec x) (dec y) dec dec))
        up-right (flip (translate x (dec y) inc dec))
        down-left (flip (reverse (translate (dec x) y dec inc)))
        down-right (translate x y inc inc)]
    [forward backward upward downward up-left up-right down-left down-right]))

(defn get-words [grid idxs]
  (map (fn [coords]
         (reduce (fn [acc [x y]] (str acc (get-letter x y grid))) "" coords))
       idxs))

(defn solve-a [grid]
  (let [words (apply concat
                     (map-indexed
                       (fn [x line]
                         (let [letters (map-indexed (fn [y _] (get-words grid (search-indices x y))) line)]
                           (flatten letters))
                         ) grid))]
    (count (filter #(= "XMAS" %) words))))

(defn- diag-idxs [x y]
  (let [diag-right [[-1 -1] [0 0] [1 1]]
        diag-left [[1 -1] [0 0] [-1 1]]
        directions [diag-left diag-right (reverse diag-left) (reverse diag-right)]]
    (map (fn [coords]
           (map (fn [[x' y']] [(+ x x') (+ y y')]) coords))
         directions)))

(defn solve-b [grid]
  (let [words (map-indexed (fn [y line]
                             (map-indexed (fn [x c]
                                            (when (= \A c)
                                              (get-words grid (diag-idxs x y)))) line)) grid)]
    (->> (mapcat frequencies (apply concat words))
         (filter (fn [[word n]] (and (= "MAS" word) (= 2 n))))
         count)))

(comment
  (solve-a grid)
  (solve-b grid)
  )
