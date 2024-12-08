(ns aoc-clojure.day-4)

(comment
  (require '[clojure.string :as string])

  (def day-4-input
    (memoize
     #(->> "resources/day-4-input.txt"
           (slurp)
           (string/split-lines))))

  (defn find-xmas-or-samx [input]
    (->> input
         (map #(count (concat (re-seq #"XMAS" %)
                              (re-seq #"SAMX" %))))
         (apply +)))

  (defn get-0-deg-slices [input]
    input)

  (defn get-90-deg-slices
    [input]
    (->> input
         (apply map vector)
         (map string/join)))

  (defn create-diagonal-slice [{:keys [row col]} input slice reverse?]
    (let [new-slice (conj slice (get-in input [row col]))]
      (if (get-in input [(inc row) (if reverse? (dec col) (inc col))])
        (create-diagonal-slice {:row (inc row) :col (if reverse? (dec col) (inc col))}
                               input new-slice reverse?)
        (string/join new-slice))))

  (defn get-45-deg-slices
    [input]
    (let [col-count   (count (first input))
          row-count   (count input)
          top-coords  (map (fn [x] {:row 0 :col x}) (range col-count))
          left-coords (map (fn [x] {:row x :col 0}) (range row-count))
          coords      (concat top-coords left-coords)]
      (->> (rest coords)
           (map #(create-diagonal-slice % input [] false)))))

  (defn get-minus-45-deg-slices
    [input]
    (let [col-count    (count (first input))
          row-count    (count input)
          top-coords   (map (fn [x] {:row 0 :col x}) (range col-count))
          right-coords (map (fn [x] {:row x :col col-count}) (range row-count))
          coords       (concat top-coords right-coords)]
      (->> (rest coords)
           (map #(create-diagonal-slice % input [] true)))))

  ;; Part 1 answer
  (let [input  (day-4-input)
        slices (concat (get-0-deg-slices input)
                       (get-90-deg-slices input)
                       (get-45-deg-slices input)
                       (get-minus-45-deg-slices input))]
    (find-xmas-or-samx slices))

  (defn get-3-by-3-windows [input]
    (->> input
         (partition 3 1)
         (map (fn [chunk]
                (->> chunk
                     (map #(->> %
                                (partition 3 1)
                                (map string/join)))
                     (apply map vector))))
         (apply concat)))

  (defn find-mas-or-sam [input]
    (->> input
         (map #(count (concat (re-seq #"MAS" %)
                              (re-seq #"SAM" %))))
         (apply +)))

  ;; Part 2 answer
  (->> (day-4-input)
       (get-3-by-3-windows)
       (filter #(->> (concat (get-45-deg-slices %)
                             (get-minus-45-deg-slices %))
                     (find-mas-or-sam)
                     (= 2)))
       (count))

  :rcf)
