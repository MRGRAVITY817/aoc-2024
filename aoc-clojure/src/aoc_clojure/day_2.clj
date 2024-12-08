(ns aoc-clojure.day-2)

(comment
  (require '[clojure.string :as string])

  (def day-2-input
    (memoize
     #(let [input (-> "resources/day-2-input.txt"
                      (slurp)
                      (string/split-lines))]
        (->> input
             (map (fn [x] (->> (string/split x #" ")
                               (map parse-long))))))))

  (defn safe-report? [report]
    (let [diffs (->> report
                     (partition 2 1)
                     (map (fn [[x y]] (- y x))))]
      (and (or (every? pos? diffs)
               (every? neg? diffs))
           (every? (fn [x] (contains? #{1 2 3} (abs x))) diffs))))

  (let [part-1-answer (->> (day-2-input)
                           (filter safe-report?)
                           (count))]
    part-1-answer)

  (defn get-comb-vec [input-vec]
    (let [n (count input-vec)]
      (vec
       (for [i (range n)]
         (vec
          (concat
           (subvec input-vec 0 i)
           (subvec input-vec (inc i) n)))))))

  (defn dampened-safe-report? [report]
    (let [report (vec report)]
      (->> (get-comb-vec report)
           (map safe-report?)
           (some true?))))

  (let [part-2-answer  (->> (day-2-input)
                            (filter dampened-safe-report?)
                            (count))]
    part-2-answer)

  :rcf)


