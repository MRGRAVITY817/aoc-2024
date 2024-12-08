(ns aoc-clojure.day-1)

(comment
  (require '[clojure.string :as string])

  (def day-1-input
    (memoize
     #(let [input    (-> "resources/day-1-input.txt"
                         (slurp)
                         (string/split-lines))
            parsed   (->> input
                          (map (fn [element] (string/split element #"   ")))
                          (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)])))
            left     (->> parsed
                          (map first)
                          (sort))
            right    (->> parsed
                          (map second)
                          (sort))]
        {:left  left
         :right right})))

  (let [part-1-answer (->> (map (fn [left right] (abs (- left right))) (:left (day-1-input)) (:right (day-1-input)))
                           (apply +))]
    part-1-answer)

  (let [right-freq (frequencies (:right (day-1-input)))
        part-2-answer (->> (:left (day-1-input))
                           (map (fn [val] (* val (get right-freq val 0))))
                           (apply +))]

    part-2-answer)

  :rcf)

