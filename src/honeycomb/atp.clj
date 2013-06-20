(ns honeycomb.atp)

(defn variable
  [sym val rate]
  {:sym sym :val val :rate rate})

(defn adenosine-balance
  []
  {:ATP 0 :ADP 0 :AMP 0})

(defn parameters
  []
  {:kuse 0
   :kform 0
   :kdistr 0
   :kratio 1})

(defn adenosine-flux
  [{:keys [ATP ADP AMP] :as balance} 
   {:keys [kuse kform kdistr kratio] :as params}]
  (let [vuse (* kuse ATP)
        vform (* kform ADP)
        vdistr (* kdistr (- (* ADP ADP)
                            (* ATP AMP kratio)))]
    {:dATP (+ (* -1 vuse) vform vdistr)
     :dADP (+ vuse (* -1 vform) (* -2 vdistr))
     :dAMP vdistr}))

(defn apply-flux
  [balance {:keys [dATP dADP dAMP] :as flux} dt]
  (-> balance
      (update-in [:ATP] #(+ % (* dt dATP)))
      (update-in [:ADP] #(+ % (* dt dADP)))
      (update-in [:AMP] #(+ % (* dt dAMP)))))

(defn drive-balance
  [balance params dt]
  (iterate 
   (fn [balance]
     (let [flux (adenosine-flux balance params)]
       (println flux)
       (apply-flux balance flux dt)))
   balance))
