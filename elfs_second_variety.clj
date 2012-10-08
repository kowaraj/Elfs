(ns elfs.elfs-second-variety)
(in-ns 'elfs.elfs-main)

;;;----------------------------------------------------
;;; SecondVariety : type II, the evolution of the Being
(defrecord Seva [genome inst-data int-cond ext-cond memory]
  Object (toString [this]
		   (str "sv: " (being-to-string this))))

(defn being-child-assert [this]
  ;(println "sv=" (instance? elfs.elfs-main.SecondVariety this))
  true)

(defn seva-ctor [loc dir]
  {:pre [(vector? loc) (= 2 (count loc))]} 
  (let [my-name (str "seva-" (loc 0) "." (loc 1))]
    (Seva.
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

(defn sv-behave [this]
  (println "sv-behave")
  (being-do-behave this))

(defn sv-head-color [this]
  ;(println "sv-head-color")
  (if (alive? this) (. Color blue) (. Color magenta)))

(defn sv-body-color [this]
  (. Color black))

(def being-actions-sv  {
			:do-behave sv-behave
			:die being-die
			:alive? being-alive?
			:look-around being-look-around
			:print-me being-print-me
			:calc-outcome being-calc-outcome
			:learn being-learn
			})

(def sv-gui-getters {
		     :head-color sv-head-color
		     :body-color sv-body-color
		     })

(extend Seva
  BeingGettersProtocol
  being-getters
  BeingSettersProtocol
  being-setters
  BeingIntCondProtocol
  being-intcond-getters
  BeingActionsProtocol
  being-actions-sv
  BeingPrimeActionsProtocol
  being-prime-actions
  BeingGUIProtocol
  sv-gui-getters
  )

(extend Seva
  BeingFunctionsProtocol
  being-functions)

