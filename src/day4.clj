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

(defn get-words [x y grid]
  (map (fn [coords]
         (reduce (fn [acc [x y]] (str acc (get-letter x y grid))) "" coords))
       (search-indices x y)))

(defn solve-a [grid]
  (let [words (apply concat
         (map-indexed
           (fn [x line]
             (let [letters (map-indexed (fn [y _] (get-words x y grid)) line)]
               (flatten letters))
             ) grid))]
    (count (filter #(= "XMAS" %) words))))

(comment
  (solve-a grid)
  )
