(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-calories-by-elf [calorie-list]
  (for [elf (str/split calorie-list #"\n\n")]
    (->> (str/split elf #"\n")
         (map parse-long)
         (reduce + 0))))

(defn get-most-calorific-elf
  "Day 1, Part 1"
  [calorie-list]
  (apply max (get-calories-by-elf calorie-list)))

(defn get-most-calorific-elves
  "Day 1, Part 2"
  [n calorie-list]
  (->> (get-calories-by-elf calorie-list)
       sort
       reverse
       (take n)
       (reduce + 0)))

(comment
  (def puzzle-input (slurp (io/resource "01.txt")))

  (time (get-most-calorific-elf puzzle-input))

  (time
   (get-most-calorific-elves 3 puzzle-input))

  (time
   (->> (get-calories-by-elf puzzle-input)
        cycle
        (take 1000000)

        sort
        ;;reverse
        (take-last 3)
        (reduce + 0)))

  (def calorie-list "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

  (get-most-calorific-elves 3 calorie-list)

  (get-calories-by-elf calorie-list)

)
