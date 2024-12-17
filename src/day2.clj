(ns day2
  (:require [clojure.string :as str]))

(def input (slurp "input/day2.txt"))

(defn- remove-n [n coll]
  (keep-indexed (fn [idx i] (when (not= idx n) i)) coll))

(defn- dampen [report]
  (loop [i 1 combos [(rest report)]]
    (if (= i (count report))
      combos
      (recur (inc i) (conj combos (remove-n i report))))))

(defn decreasing? [report]
  (apply > report))

(defn increasing? [report]
  (apply < report))

(defn gradual? [report]
  (let [adj-lvls (partition 2 1 report)]
    (every? (fn [[a b]] (< 0 (abs (- a b)) 4)) adj-lvls)))

(defn safe? [report]
  (and (gradual? report)
       (or (increasing? report)
           (decreasing? report))))

(defn dampened? [levels]
  (let [dampened (dampen levels)]
    (pos? (count (remove (complement safe?) dampened)))))

(defn ->levels [s]
  (map Integer/parseInt (str/split s #" ")))

(defn parse-reports [input]
  (map ->levels (str/split-lines input)))

(defn solve-a [input]
  (->> (parse-reports input)
       (filter safe?)
       count))

(defn solve-b [input]
  (->> (parse-reports input)
       (filter #(or (safe? %) (dampened? %)))
       count))

(comment
  (solve-a input)
  (solve-b input))
