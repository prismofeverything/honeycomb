(ns honeycomb.adapt
  (:require [clojure.core.async :as async :refer (chan go >! <! >!! <!! close! alts!)]))

(defn control-state
  [gain expected delta]
  {:gain gain
   :expected expected
   :integral 0
   :delta delta
   :output 0})

(defn integrate-feedback
  [state signal]
  (let [output (- (* signal (:gain state)) (:integral state))
        error (- output (:expected state))
        integral (+ (:integral state) (* (:delta state) error))]
    (assoc state
      :integral integral
      :output output)))

(defn adaptation-channel
  [state input]
  (let [output (chan)]
    (go
     (loop [state state]
       (let [signal (<! input)
             state (integrate-feedback state signal)]
         (println state)
         (>! output (:output state))
         (recur state))))
    output))

