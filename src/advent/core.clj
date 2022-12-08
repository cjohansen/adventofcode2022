(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
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

;; Rucksacks, day 3

(defn get-compartments [s]
  (partition (/ (count s) 2) s))

(defn parse-rucksacks [s]
  (map get-compartments (str/split-lines s)))

(defn find-shared-items [compartments]
  (apply set/intersection (map set compartments)))

(defn get-item-priority [item]
  (let [ascii-val (int item)]
    (- ascii-val
       (if (<= 97 ascii-val)
         96 ;; Lower case letters have higher ascii values
         38))))

(defn get-shared-item-priority [rucksacks]
  (->> rucksacks
       (map find-shared-items)
       (mapcat #(map get-item-priority %))
       (reduce + 0)))

(defn find-badge-item-types [rucksacks]
  (->> rucksacks
       (partition 3)
       (map (fn [group]
              (->> (map (comp set #(apply concat %)) group)
                   (apply set/intersection)
                   first)))))

;; Area assignments, day 4

(defn parse-area-assignments [s]
  (for [pair (str/split-lines s)]
    (for [assignment (str/split pair #",")]
      (let [[from to] (->> #"-"
                           (str/split assignment)
                           (map parse-long))]
        (set (range from (inc to)))))))

(defn assignments-overlap? [[elf1 elf2]]
  (or (not-empty (set/intersection elf1 elf2))
      (not-empty (set/intersection elf2 elf1))))

(defn assignments-fully-overlap? [[elf1 elf2]]
  (or (set/subset? elf1 elf2)
      (set/subset? elf2 elf1)))

;; Crate movement, day 5

(defn get-crate-name [n]
  (str (second n)))

(defn parse-stack-line [line]
  (->> (re-seq #"(.{2,3}) ?" line)
       (map (comp not-empty str/trim second))))

(defn parse-crate-stacks [s]
  (let [[labels & rows]
        (->> (str/split s #"\n\n")
             first
             str/split-lines
             (map parse-stack-line)
             reverse)]
    (loop [stacks (->> labels
                       (map (fn [label] [label []]))
                       (into {}))
           rows rows]
      (if (empty? rows)
        stacks
        (recur
         (->> (map vector labels (first rows))
              (reduce (fn [stacks [label crate]]
                        (cond-> stacks
                          crate (update label conj (get-crate-name crate))))
                      stacks))
         (rest rows))))))

(defn parse-crate-movements [s]
  (->> (str/split s #"\n\n")
       second
       str/split-lines
       (map (fn [instructions]
              (let [[_ n from to] (re-find #"move (\d+) from (\d+) to (\d+)" instructions)]
                {:n (parse-long n)
                 :from from
                 :to to})))))

(defn rearrange-stacks [stacks movements]
  (loop [stacks stacks
         movements (seq movements)]
    (if-not movements
      stacks
      (let [{:keys [n from to]} (first movements)]
        (recur
         (reduce (fn [stacks _]
                   (-> stacks
                       (update from #(drop-last 1 %))
                       (update to concat (take-last 1 (get stacks from)))))
                 stacks
                 (range n))
         (next movements))))))

(defn rearrange-stacks-9001 [stacks movements]
  (loop [stacks stacks
         movements (seq movements)]
    (if-not movements
      stacks
      (let [{:keys [n from to]} (first movements)]
        (recur
         (-> stacks
             (update from #(drop-last n %))
             (update to concat (take-last n (get stacks from))))
         (next movements))))))

(defn get-top-crates [stack]
  (->> stack
       (map (fn [[label stack]]
              [label (last stack)]))
       (sort-by first)
       (map second)
       str/join))

(comment
  ;; Day 5
  (def test-crate-arrangements "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")
  (def crate-arrangements (slurp (io/resource "05.txt")))

  ;; Part 1
  (get-top-crates
   (rearrange-stacks
    (parse-crate-stacks test-crate-arrangements)
    (parse-crate-movements test-crate-arrangements)
    ))

  (get-top-crates
   (rearrange-stacks
    (parse-crate-stacks crate-arrangements)
    (parse-crate-movements crate-arrangements)
    ))

  ;; Part 2
  (get-top-crates
   (rearrange-stacks-9001
    (parse-crate-stacks test-crate-arrangements)
    (parse-crate-movements test-crate-arrangements)
    ))

  (get-top-crates
   (rearrange-stacks-9001
    (parse-crate-stacks crate-arrangements)
    (parse-crate-movements crate-arrangements)
    ))

  ;; Day 4
  (def test-area-assignments (parse-area-assignments "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"))
  (def area-assignments (parse-area-assignments (slurp (io/resource "04.txt"))))

  ;; Part 1
  (->> area-assignments
       (filter assignments-fully-overlap?)
       count)

  ;; Part 2
  (->> area-assignments
       (filter assignments-overlap?)
       count)

  ;; Day 3
  (get-compartments "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
  (def test-sacks (parse-rucksacks "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n"))
  (def sacks (parse-rucksacks (slurp (io/resource "03.txt"))))

  (get-item-priority \a)
  (get-item-priority \z)
  (get-item-priority \A)
  (get-item-priority \Z)

  (get-shared-item-priority test-sacks)

  ;; Part 1
  (get-shared-item-priority sacks)

  ;; Part 2
  (->> sacks
       find-badge-item-types
       (map get-item-priority)
       (reduce + 0))

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
