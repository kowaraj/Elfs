(ns elfs.elfs-third-variety)
(in-ns 'elfs.elfs-main)

;;;----------------------------------------------------
;;; SecondVariety : type II, the evolution of the Being
(defrecord Thiva [genome inst-data int-cond ext-cond memory]
  Object (toString [this]
		   (str "thiva: " (being-to-string this))))

(defn being-child-assert [this]
  true)

(defn thiva-ctor [loc dir]
  {:pre [(vector? loc) (= 2 (count loc))]} 
  (let [my-name (str "thiva-" (loc 0) "." (loc 1))]
    (Thiva.
     (Genome. 1 ;type 0=dead, 1=alive
	      my-name ;name
	      ()) ;;(make-map conditions-map primitive-actions-list)) ;behavioral-model
     (InstData. loc ;location
		dir) ;direction
     (IntCond2. kk-being-ic-satiety ;satiety
		kk-being-ic-energy ;energy
		0) ;age
     (ExtCond. false ;damage
	       false ;damage-around
	       false ;food
	       false ;food-around
	       )
     ;;memory: 
     (Memory-ctor loc))))

(defn thv-behave [this]
  (println "thv-behave")
  (being-do-behave this))

(defn thv-head-color [this]
  ;(println "thv-head-color")
  (if (alive? this) (. Color blue) (. Color magenta)))

(defn thv-body-color [this]
  (. Color black))

(def being-actions-thv  {
			:do-behave thv-behave
			:die being-die
			:alive? being-alive?
			:look-around being-look-around
			:print-me being-print-me
			:calc-outcome being-calc-outcome
			:learn being-learn
			})

(def thv-gui-getters {
		     :head-color thv-head-color
		     :body-color thv-body-color
		     })

(extend Thiva
  BeingGettersProtocol
  being-getters
  BeingSettersProtocol
  being-setters
  BeingIntCondProtocol
  being-intcond-getters
  BeingActionsProtocol
  being-actions-thv
  BeingPrimeActionsProtocol
  being-prime-actions
  BeingGUIProtocol
  thv-gui-getters
  )

(extend Thiva
  BeingFunctionsProtocol
  being-functions)

