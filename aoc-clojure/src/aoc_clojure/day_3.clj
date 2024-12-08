(ns aoc-clojure.day-3)

(comment
  (require '[clojure.string :as string])

  (def day-3-input
    (memoize
     #(-> "resources/day-3-input.txt"
          (slurp)
          (string/replace #"\n" ""))))

  (defn mult-and-sum [input-str]
    (->> input-str
         (re-seq #"mul\(\d+,\d+\)")
         (map (fn [mul-str]
                (let [pair (->> mul-str
                                (re-find #"\d+,\d+"))
                      mult (->> (string/split pair #",")
                                (map parse-long)
                                (apply *))]

                  mult)))
         (apply +)))

  ;; part 1 answer
  (mult-and-sum (day-3-input))

  ;; part 2 answer
  (-> (day-3-input)
      (string/replace #"don't\(\).*?do\(\)|don't\(\).*" "")
      (mult-and-sum))

  :rcf)
