(ns mastermind.core
  (:require [clojure.math.combinatorics :as combo]))

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
  (combo/selections (keys colors) 4))

(defn color-wrap [color s]
  (str \u001b (get colors color) (str s) (str \u001b "[0m")))

(defn print-code [code]
  (println (apply str (map #(color-wrap % "*") code))))

(comment
  (print-code (rand-nth all-codes))
)

(defn -main
  []
  (print-code (rand-nth all-codes)))
