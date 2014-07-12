(ns mastermind.core
  (:require [clojure.math.combinatorics :refer [selections]]
            [clojure.set :as set :refer [intersection]]
            [clojure.test :refer :all]
            ))

(def colors
  {
   :r 31
   :y 33
   :g 32
   :b 34
   :i 36
   :v 35
  })

(def all-codes
  (selections (set (keys colors)) 4))

(def all-scores
  (set (map (fn [f]
              {:black (get f :black 0)
               :white (get f :white 0)}) (map frequencies (selections [:black :white :empty] 4)))))

(defn white-score [code guess]
  (let [shared (intersection (set code) (set guess))
        [code* guess*] (map #(frequencies (filter shared %)) [code guess])]
    (apply + (vals (merge-with min code* guess*)))))

(defn score [guess code]
  (let [black (count (filter true? (map = guess code)))]
    {:black black
     :white (max 0 (- (white-score guess code) black))}))

(defn mini-max [guesses]
  (let [min-ranks (map (fn [guess]
                         (apply min-key :rank (map (fn [s]
                                                     {:rank (- (count guesses) (count (filter #(= (score % guess) s) guesses)))
                                                      :guess guess
                                                     }) all-scores))) guesses)]
  (map :guess (val (apply max-key key (group-by :rank min-ranks))))))

(mini-max (take 100 all-codes))

(defn next-guess [last-guess s unplayed-guesses possible-guesses]
  (let [pg (filter #(= (score % last-guess) s) possible-guesses)
        mini-max (mini-max possible-guesses)
        shared (intersection (set mini-max) (set pg))
        next-guess (if (empty? shared) (rand-nth mini-max) (first shared))]
    [next-guess pg]))

(defn sink [[topic message]]
  (let [keypeg-colors { :black 7, :white 1 }
        color-wrap (fn [color s]
                     (str \u001b "[" color "m" s \u001b "[0m"))
        colorized-code (fn [code]
                          (apply str (map #(color-wrap (% colors) "*") code)))
        score-characters (fn [score]
                           (apply concat (map (fn [[color count]]
                                                (repeat count (color-wrap (color keypeg-colors) "."))) score)))]
    (case topic
      :secret (println (format "  | %s |  \n--| ---- |--" (colorized-code message)))
      :row    (let [{:keys [guess possible score]} message
                    [left right] (partition 2 2 (repeat " ") (score-characters score))]
                (println (format "%2s| %s |%2s\t\t%-4s"
                                (apply str left)
                                (colorized-code guess)
                                (apply str right)
                                possible)))
      :success (println (format "Success! %s guesses." message)))))

(defn play []
  (let [secret (rand-nth all-codes)]
    (sink [:secret secret])
    (loop [guess [:r :r :y :y]
           unplayed-guesses (remove #{guess} all-codes)
           possible-guesses unplayed-guesses
           guess-count 1]
      (let [score (score guess secret)]
        (sink [:row {:guess guess :possible (count possible-guesses) :score score}])
        (if (< (:black score ) 4)
          (let [[ng pgs] (next-guess guess score unplayed-guesses possible-guesses)]
            (recur ng (remove #{guess} unplayed-guesses) pgs (inc guess-count)))
          (do (sink [:success guess-count]) guess-count))))))

(defn -main
  []
  (time (let [games (map (fn [_] (play)) (range 1))]
    (println (format "games:\t%d\nworst:\t%d\nbest:\t%d\navg:\t%f"
                      (count games)
                      (apply max games)
                      (apply min games)
                      (float (#(/ (reduce + games) (count games)))))))))

(deftest test-score
  (is (= {:black 4, :white 0} (score [ :r :r :y :y ] [ :r :r :y :y ])))
  (is (= {:black 0, :white 0} (score [ :b :b :b :b ] [ :r :r :y :y ])))
  (is (= {:black 1, :white 0} (score [ :r :b :b :b ] [ :r :r :y :y ])))
  (is (= {:black 0, :white 4} (score [ :y :y :r :r ] [ :r :r :y :y ])))
  (is (= {:black 1, :white 2} (score [ :r :y :r :r ] [ :r :r :y :y ])))
  )
