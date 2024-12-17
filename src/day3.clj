(ns day3)

(def input (slurp "input/day3.txt"))

(defn solve-a [input]
  (->> input
       (re-seq #"mul\(\d{1,3},\d{1,3}\)")
       (map #(subs % 3))
       (map clojure.edn/read-string)
       (map (partial apply *))
       (reduce +)))

(defn solve-b [input]
  (->> input
       (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)")
       (partition-by #(or (= "don't()" %) (= "do()" %)))
       (reduce (fn [[keep? saved], i]
                 (cond (= "don't()" (first i)) [false saved]
                       (= "do()" (first i)) [true saved]
                       :else [keep? (if keep? (conj saved i) saved)])) [true []])
       (flatten)
       rest
       (map #(subs % 3))
       (map clojure.edn/read-string)
       (map (partial apply *))
       (reduce +)))

(comment
  (solve-a input)
  (solve-b input))