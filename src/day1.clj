(ns day1
  (:require [clojure.string :as str]))

(def input (slurp "input/day1.txt"))

(defn make-pair [s]
  (->> (str/split s #" +")
       (map ^[String] Integer/parseInt)))

(defn make-pairs [input]
  (->> (str/split-lines input)
       (map make-pair)))

(defn distance [[x y]]
  (abs (- x y)))

(defn- similarity-score [x coll]
  (-> (filter #(= x %) coll)
      count
      (* x)))

(defn solve-a [input]
  (let [pairs (make-pairs input)
        left (sort (map first pairs))
        right (sort (map second pairs))]
    (transduce (map distance) + (map vector left right))))

(defn solve-b [input]
  (let [pairs (make-pairs input)
        left (map first pairs)
        right (map second pairs)]
    (->> (map #(similarity-score % right) left)
         (reduce +))))

(comment
  (time (solve-a input))                                    ;;1258579
  (time (solve-b input))                                    ;;23981443
  )