(ns honeycomb.sync
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.matrix.operators :as op]))

(def twopi (* 2 Math/PI))

(defn random-oscillators
  [n]
  {:oscillators (m/matrix (take n (repeatedly rand)))
   :frequencies (m/matrix (take n (repeatedly rand)))
   :connections (m/matrix (take n (repeatedly (comp (partial take n) (partial repeatedly rand)))))})

(defn delta-oscillators
  [{:keys [oscillators frequencies connections]}]
  (m/matrix 
   (map
    (fn [theta frequency row]
      (let [orientations (map
                          (fn [other connection]
                            (* connection (Math/sin (- other theta))))
                          oscillators row)]
        (+ frequency (reduce + 0 orientations))))
    oscillators frequencies connections)))

(defn advance
  [kuramoto]
  (update-in
   kuramoto
   [:oscillators]
   (partial m/add (delta-oscillators kuramoto))))

