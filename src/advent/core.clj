(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-most-calorific-elf [calorie-list]
  (->> (for [elf (str/split calorie-list #"\n\n")]
         (->> (str/split elf #"\n")
              (map parse-long)
              (reduce + 0)))
       (apply max)))

(comment
  (get-most-calorific-elf (slurp (io/resource "01.txt")))

  (get-most-calorific-elf
    "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

)
