(ns mastermind.core
  (:require [clojure.math.combinatorics :refer [selections]]
            [clojure.set :as set]
            [clojure.test :refer :all]
            ))

(def colors
  {
   :r "[31m"
   :y "[33m"
   :g "[32m"
   :b "[34m"
   :i "[36m"
   :v "[35m"
  })

(def keypeg-colors
  {
   :black "[7m"
   :white "[1m"
  })

(def all-codes
  (selections (vec (keys colors)) 4))

(defn color-wrap [color s]
  (str \u001b color (str s) (str \u001b "[0m")))

(defn colorized-code [code]
  (apply str (map #(color-wrap (% colors) "*") code)))

(defn white-score [code guess]
  (loop [c (frequencies code), g (frequencies guess), score 0]
    (if c
      (let [[color freq] (first c)]
        (recur (next c), (dissoc g color), (+ score (min freq (or (color g) 0)))))
      score)))

(defn score [guess code]
  (let [black (count (filter true? (map = guess code)))]
  {
   :black black
   :white (max 0 (- (white-score guess code) black))
  }
))

(defn score-characters [score]
 (apply concat (map (fn [[color count]] (repeat count (color-wrap (color keypeg-colors) "."))) score)))

(defn next-guess [last-guess s possible-guesses]
  (let [ng (rand-nth possible-guesses)]
    [ng (filter #(= (score % last-guess) s) possible-guesses)]
    ))

(defn -main
  []
  (let [secret (rand-nth all-codes)]
    (println (format "  | %s |  \n--| ---- |--" (colorized-code secret)))
    (loop [guess [:r :r :y :y], possible-guesses (remove #{guess} all-codes), guess-count 1]
      (let [score (score guess secret)]
        (let [[left right] (partition 2 2 (repeat " ") (score-characters score))]
        (println (format "%2s| %s |%2s"
                         (apply str left)
                         (colorized-code guess)
                         (apply str right)))
        (if (< (:black score ) 4)
          (let [[ng pgs] (next-guess guess score possible-guesses)]
            (recur ng, pgs, (inc guess-count)))
          (println (format "Success! %s guesses. %s untried possibilities."
                           guess-count
                           (count possible-guesses)))))))))

(deftest test-score
  (is (= {:black 4, :white 0} (score [ :r :r :y :y ] [ :r :r :y :y ])))
  (is (= {:black 0, :white 0} (score [ :b :b :b :b ] [ :r :r :y :y ])))
  (is (= {:black 1, :white 0} (score [ :r :b :b :b ] [ :r :r :y :y ])))
  (is (= {:black 0, :white 4} (score [ :y :y :r :r ] [ :r :r :y :y ])))
  (is (= {:black 1, :white 2} (score [ :r :y :r :r ] [ :r :r :y :y ])))
  )
