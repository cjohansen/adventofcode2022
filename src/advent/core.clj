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

(def plays
  {:A :rock
   :B :paper
   :C :scissors
   :X :rock
   :Y :paper
   :Z :scissors})

(def beaten-by
  {:scissors :rock
   :rock :paper
   :paper :scissors})

(def looses-to
  (->> beaten-by
       (map (juxt second first))
       (into {})))

(def scores
  {:rock 1
   :paper 2
   :scissors 3})

(defn determine-outcome [opponent player]
  (if (= player opponent)
    {:draw? true}
    {:winner
     (case [player opponent]
       [:rock :paper] :opponent
       [:rock :scissors] :player

       [:paper :scissors] :opponent
       [:paper :rock] :player

       [:scissors :paper] :player
       [:scissors :rock] :opponent)}))

(defn score-outcome [outcome]
  (cond
    (:draw? outcome) 3
    (= :player (:winner outcome)) 6
    :default 0))

(defn score-play [[opponent player]]
  (+ (-> (determine-outcome opponent player)
         score-outcome)
     (scores player)))

(defn interpret-strategy-1
  "Day 2, Part 1"
  [[opponent player]]
  [(plays opponent) (plays player)])

(defn interpret-strategy-2
  "Day 2, Part 2"
  [[opponent outcome]]
  (let [opponent-play (plays opponent)]
    [opponent-play
     (case outcome
       :X (looses-to opponent-play)
       :Y opponent-play
       :Z (beaten-by opponent-play))]))

(defn parse-rps-strategy [s]
  (->> (str/split s #"\n")
       (map #(map keyword (str/split % #" ")))))

(comment
  ;; Day 2
  (def test-round (parse-rps-strategy "A Y\nB X\nC Z"))
  (def rps-input (parse-rps-strategy (slurp (io/resource "02.txt"))))

  (->> test-round
       (map interpret-strategy-1)
       (map score-play)
       (reduce + 0))

  (->> rps-input
       (map interpret-strategy-1)
       (map score-play)
       (reduce + 0))

  (->> test-round
       (map interpret-strategy-2)
       (map (juxt identity score-play)))

  (->> test-round
       (map interpret-strategy-2)
       (map score-play)
       (reduce + 0))

  (->> rps-input
       (map interpret-strategy-2)
       (map score-play)
       (reduce + 0))

  ;; Day 1
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

  (def calorie-list "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

  (get-most-calorific-elves 3 calorie-list)

  (get-calories-by-elf calorie-list)

)
