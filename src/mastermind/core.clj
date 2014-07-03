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

(def all-codes
  (selections (vec (keys colors)) 4))

(defn color-wrap [color s]
  (str \u001b (get colors color) (str s) (str \u001b "[0m")))

(defn colorized-code [code]
  (apply str (map #(color-wrap % "*") code)))

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
   :white (#(if (< % 0) 0 %) (- (white-score guess code) black))
  }
))

(defn -main
  []
  (let [secret (rand-nth all-codes)]
    (println "--|" (colorized-code secret) "|--\n--| ---- |--")
    (println (score [:r :r :y :y] secret))
    (apply println (map colorized-code all-codes))
    ))

(deftest test-score
  (is (= {:black 4, :white 0} (score [ :r :r :y :y ] [ :r :r :y :y ])))
  (is (= {:black 0, :white 0} (score [ :b :b :b :b ] [ :r :r :y :y ])))
  (is (= {:black 1, :white 0} (score [ :r :b :b :b ] [ :r :r :y :y ])))
  (is (= {:black 0, :white 4} (score [ :y :y :r :r ] [ :r :r :y :y ])))
  (is (= {:black 1, :white 2} (score [ :r :y :r :r ] [ :r :r :y :y ])))
  )
