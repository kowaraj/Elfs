(ns elfs.elfs-locus)
(in-ns 'elfs.elfs-main)

;;;---------------------------------------------
;;; Locus : a place (as reference) in the world,
;;;       : contains: being, food, damage, ...

(defprotocol LocusProtocol
  (get-being [this])
  (get-food [this])
  (get-poison [this])
  (set-being [this being])
  (set-food [this range])
  )
(defrecord Locus [being food poison])

(defn locus-get-being [this] (:being this))
(defn locus-get-food [this] (if-let [f (:food this)] f 0))
(defn locus-get-poison [this] (if-let [p (:poison this)] p 0))

(defn locus-set-being [this being] (assoc this :being being))
(defn locus-set-food [this range] (assoc this :food range))


;; interfaces --------------------------------------------;
(def locus-getters  {
		     :get-being locus-get-being
		     :get-food locus-get-food
		     :get-poison locus-get-poison
		     :set-being locus-set-being
		     :set-food locus-set-food
		     })

(extend Locus
  LocusProtocol
  locus-getters)
