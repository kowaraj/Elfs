(ns elfs.elfs-being)
(in-ns 'elfs.elfs-main)

;;; classes --------------------------------

;;; Genome : inheritance of a Being;
(defrecord Genome [type
		   name
		   behavioral-model ;;actions-map 
		   ])

;;; InstData : instant Being's data;
(defrecord InstData [loc dir]) 

;;; IntCond : internal Being's conditions;
(defrecord IntCond2 [satiety energy age])


;;;---------------------------------------
;;; ExtCond : external markers of a Being;
;;(defrecord ExtCond [type name loc dir])
;; FIXME: ExtCond must not be a part of a Being. It has nothing to do with one.
(defrecord ExtCond [
		    damage
		    danger
		    food
		    food-around
		    ])

;;;-------------------------------------------------
;;; Being : an alive entity (as agent) in the world;
(declare being-to-string get-inst-data get-int-params get-ext-params)
(defrecord Being [genome inst-data int-cond ext-cond memory]
  Object
  (toString [this]
	    (being-to-string this)))


(declare get-name get-loc get-inst-data)
(defn being-to-string
  ([this]
     (let [g (:genome this)]
       (str "\n ID= " (get-inst-data this)
	    "\n IC= " (get-int-params this)
	    "\n EC= " (get-ext-params this)
	    "\n GENOME= {:type " (:type g) ", :name " (:name g)"}"
	    "\n MEMORY= \n"
	    (:memory this))))
  ([this degree]
     (str "> " (get-name this) ", @ " (get-loc this) "\n"
	  " id= " (get-inst-data this))))
  

(defn being-ctor [loc dir]
  {:pre [(vector? loc) (= 2 (count loc))]}
  (let [my-name (str "being-" (loc 0) "." (loc 1))]
    (Being.
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
;; !!! create the second variety which would satisfy the need of the first:
;;  - to immitate a simbiosis: they should survive only together

(defn being-assert [someone]
  (or
   (instance? elfs.elfs-main.Being someone)
   (being-child-assert someone)))

(defprotocol BeingGettersProtocol
  (get-genome [this])
  (get-inst-data [this])
  (get-int-params [this])
  (get-ext-params [this])
  (get-actions-map [this])
  (get-mem-contacts [this])
  (get-mem-actions [this])
  (get-type [this])
  (get-name [this])
  (get-loc [this])
  (get-dir [this])
  )
(defprotocol BeingSettersProtocol
  (set-type [this type])
  (set-loc [this loc])
  (set-dir [this dir])
  (age [this dawn])
  )
(defprotocol BeingIntCondProtocol
  (get-satiety [this]) ;FIXME: OBSOLETE? or move to BeingGettersProtocol
  (get-energy [this]) ;FIXME: OBSOLETE? or move to BeingGettersProtocol
  (energy-loss [this amount])
  (food-to-energy [this])
  )
(defprotocol BeingPrimeActionsProtocol
  (turn-to [this new-dir])
  (turn-to-avoid [this])
  (go-forward [this new-loc])
  (appease [this food-available])
  )
(defprotocol BeingActionsProtocol
  (do-behave [this])
  (die [this]) ;FIXME: must be in Being's getters
  (alive? [this]) ;FIXME: must be in Being's getters
  (look-around [this])
  (print-me [this label])
  (to-string [this degree])
  (calc-outcome [this-old this-new])
  (learn [this stm])
  )
(defprotocol BeingGUIProtocol
  (head-color [this])
  (body-color [this])
  )

;; getters

;;FIXME: find a proper way to get a map from a record
(defn being-get-genome [this] (let [g (:genome this)]
				    (zipmap (keys g) (vals g))))
;;FIXME: find a proper way to get a map from a record
(defn being-get-inst-data [this] (let [g (:inst-data this)]
				    (zipmap (keys g) (vals g))))
;;FIXME: find a proper way to get a map from a record
(defn being-get-int-params [this] (let [ic (:int-cond this)]
				    (zipmap (keys ic) (vals ic))))
;;FIXME: find a proper way to get a map from a record
(defn being-get-ext-params [this] (let [ic (:ext-cond this)]
				    (zipmap (keys ic) (vals ic))))
(defn being-get-actions-map [this]
  {:pre [(not (nil? (:behavioral-model (:genome this))))]}
  (:behavioral-model (:genome this)))
;;FIXME: find a proper way to get a map from a record
(defn being-get-mem-contacts [this] (let [m (:memory this)
					  m2 (:contacts m)]
				      (for [e m2]
					(zipmap (keys e) (vals e)))))
(defn being-get-mem-actions [this] (let [vec (memory-get-actions (:memory this))]
				     (for [el vec]
				       el)))

(defn being-get-type [this] {:pre [(not (nil? (:type (:genome this))))]}
  (:type (:genome this)))
(defn being-get-name [this] {:pre [(not (nil? (:name (:genome this))))]}
  (:name (:genome this)))
(defn being-get-loc [this] {:pre [(not (nil? (:loc (:inst-data this))))]}
  (:loc (:inst-data this)))
(defn being-get-dir [this] {:pre [(not (nil? (:dir (:inst-data this))))]}
  (:dir (:inst-data this)))
(defn being-get-satiety [this] (:satiety (:int-cond this)))
(defn being-get-energy [this] (:energy (:int-cond this)))  

;;; setters

(defn being-set-type [this type] (assoc this :genome
					(assoc (:genome this) :type type)))
(defn being-set-loc [this loc] (assoc this :inst-data
				      (assoc (:inst-data this) :loc loc)))
(defn being-set-dir [this dir] (assoc this :inst-data
				      (assoc (:inst-data this) :dir dir)))
(defn being-age [this dawn]
  (let [ic (:int-cond this)
	age (:age ic)
	dusk (get-current-time)
	new-age (+ age (- dusk dawn))
	]
    (assoc this :int-cond (assoc ic :age new-age))))

;;; setters: Being's Memory access

(defn being-learn [this stm]
  "After each behaving slot. Takes an STMemory instance and updates the memory."
  (let [memory (:memory this)
	memory-upd-stm (assoc memory :stm stm) ;update memory with stm
	this-updated-stm (assoc this :memory memory-upd-stm)] ;update being with stm
    ;; update memory  ;;FIXME! complete memory-update must be done at once
    (assoc this-updated-stm :memory (update-actions memory-upd-stm (:action stm)))))

;;; prime actions:

(defn being-go-forward [this new-loc]
  ;; (println "$ being-go-forward," (get-loc this) " =>" new-loc)
  ;; (assoc this :ext-cond (assoc (:ext-cond this) :loc new-loc))
  (set-loc this new-loc))

(defn being-turn-to [this new-dir]
  ;; (println "$ being-turn-to," (get-dir this) " =>" new-dir)
  ;; (assoc this :ext-cond (assoc (:ext-cond this) :dir new-dir))
  (set-dir this new-dir))

(defn being-turn-to-avoid [this]
  (let [cur-d (get-dir this)]
    (if (> (rand) 0.5)
      (set-dir this (bound 8 (inc cur-d)))
      (set-dir this (bound 8 (dec cur-d))))))

(defn being-appease [this food-available]
  "Returns a vector [updated-this delta-food]"
  (let [int-cond (:int-cond this)
	curr-s (:satiety int-cond)
	can-be-eaten (if (> food-available kk-being-appeased-by)
		       kk-being-appeased-by food-available) 
	can-eat (- kk-being-max-satiety curr-s)
	delta-food (if (>= can-eat can-be-eaten) can-be-eaten can-eat)
	new-s (+ curr-s delta-food)]
    ;; (println "$ being-appease, delta-food =" delta-food)
    [(assoc this :int-cond (assoc (:int-cond this) :satiety new-s)) delta-food]))

;;called every step (energy losses) 
(defn being-energy-loss [this amount]
  "One of the steps in the undergo-fn"
  (let [int-cond (:int-cond this)
	curr-e (:energy int-cond)
	new-e (- curr-e amount)]
    ;; (println (str "$ bel for " (get-name this) ": " curr-e "/" amount "/" new-e))
    (if (pos? new-e)
      (assoc this :int-cond (assoc (:int-cond this) :energy new-e))
      (let [dead-this (die this)]     
	(assoc dead-this :int-cond (assoc (:int-cond dead-this) :energy 0))))))

;;called every step (food to energy transformation) 
(defn being-food-to-energy [this]
  "One of the steps in the undergo-fn"
  (let [int-cond (:int-cond this)
	curr-s (:satiety int-cond)
	new-s (- curr-s kk-f2e)]
    (if (>= new-s 0)
      (let [curr-e (:energy int-cond)
	    _e (+ curr-e kk-f2e)
	    new-e (if (> _e kk-being-max-energy) kk-being-max-energy _e)]
	(assoc this :int-cond (assoc (:int-cond this)
				:satiety new-s :energy new-e)))
      this)))

;; actions


(defn being-look-around 
  [this]
  "Updates the parameters. Supposed to be called before a 'select-action'."
  (let [loc (get-loc this)
	;d (get-dir this) ;;UNUSED - remove it
	p (place loc)
	;; environment
	fov (get-fov loc)
	food-dir (get-food-dir fov)	
	p-food (:food @p)
	;; params
	food (if (and p-food (pos? p-food)) p-food false)	
	food-around (if food-dir food-dir false)
	damage false
	danger false ;;should be: (get-danger-dir fov)
	
	ext-conds (:ext-cond this)
	upd-ext-conds (assoc ext-conds
			:damage damage
			:danger danger
			:food food
			:food-around food-around)
	]
    ;; (println "@ look-around, food-dir =" food-dir)
    ;; (println "@ look-around, food =" food) 
    (assoc this :ext-cond upd-ext-conds)))


(defn being-calc-outcome [this-old this-new]
  (let [sat-old (get-satiety this-old)
	sat-new (get-satiety this-new)
	eng-old (get-energy this-old)
	eng-new (get-energy this-new)]
    (+ (- sat-new sat-old) (- eng-new eng-old))))

(defn being-do-behave
  "Agent's function. Takes and returns an instance of (Being.)"
  [this]
  (if (alive? this) ; even a dead one could be pushed
    (let [dawn (get-current-time)]
      (. Thread (sleep (get-gui-ant-sleep-ms)))
      (dosync
       ;; A new circle of live
       (println (str "*** " (get-name this) ", @ " (get-loc this)))
       ;; passive life
       (let [upd-this (look-around this)
	     undergone-this (undergo-fn upd-this)]
	 ;; active life
	 (if (alive? undergone-this) ;still alive? give him one more chance...
	   ;; a survived one
	   (let [[react-fn conds] (get-action-fn undergone-this)
;		 _ (println (str "ug-being = " (to-string undergone-this 2) ))
		 behaved-this (react-fn undergone-this)
;		 _ (print-me behaved-this "[b'ed]")
		 ;; memory update
		 outcome (calc-outcome undergone-this behaved-this)
;		 _ (println "outcome = " outcome)
		 _action-info (ActionInfo. react-fn outcome :self 1) ;;FIXME: replace by a c-tor
		 _vol-params (VolParams-ctor conds)
		 _memory-action (MemoryAction-ctor _vol-params _action-info)
		 _stm (STMemory. nil _memory-action nil nil)
		 learnt-this (learn behaved-this _stm)
		 ]
	     (when running
	       (send-off *agent* #'do-behave))
	     (let [aged-this (age learnt-this dawn)]
	       ;;;(if (not lighting) (push-the-light)) - make the agent to repaint the world
	       aged-this))
	   ;; didn't survive
	   (let [dead-this (age undergone-this dawn)]
	     dead-this))
	 )))
    (do
      (println (str "zombi:" (get-name this) ", @ " (get-loc this)))
      this)))
     
(defn being-alive?
  [this]
;  (print "$ being-alive?..")
  (let [alive (not (= (get-type this) 0))]
;    (println alive)
    alive))

(defn being-die
  "Kills the being."
  [this] 
  (println (str "$ mark as dead:" (get-name this) ", died @ " (get-loc this)))
 ;;; (assoc this :ext-cond (assoc (:ext-cond this) :type 0))
  (set-type this 0))

(defn being-print-me [this label]
  (let [g (:genome this)]
    (println "@" label
	     (str ": g={:type " (:type g) ", :name " (:name g)"},")
	     "id=" (get-inst-data this)
	     ", ic=" (get-int-params this)
	     ", ec=" (get-ext-params this))))

(defn string-me [this label]
  (print-str (str label this )))

;(defn make-string [this]
;  "@overridden - Being.toString()";
;  (let [g (:genome this)]
;    (str "..." 
;	 "\n id= " (get-inst-data this)
;	 "\n ic= " (get-int-params this)
;	 "\n ec= " (get-ext-params this)
;	 "\n g=  {:type " (:type g) ", :name " (:name g)"}"
;	 "\n memory:\n"
;	 (:memory this)
;	 )))

(defn being-head-color [this]
  (if (alive? this) (gui-take-color 'green) (gui-take-color 'magenta)))

(defn being-body-color [this]
  (gui-take-color 'black))

;; interfaces --------------------------------------------;
(def being-getters {
		    :get-genome being-get-genome
		    :get-inst-data being-get-inst-data
		    :get-int-params being-get-int-params
		    :get-ext-params being-get-ext-params
		    :get-actions-map being-get-actions-map
		    :get-mem-contacts being-get-mem-contacts
		    :get-mem-actions being-get-mem-actions
		    :get-type being-get-type
		    :get-name being-get-name
		    :get-loc being-get-loc
		    :get-dir being-get-dir
		    })
(def being-setters {
		    :set-type being-set-type
		    :set-loc being-set-loc
		    :set-dir being-set-dir
		    :age being-age
		    })
(def being-intcond-getters  {
			     :get-satiety being-get-satiety
			     :get-energy being-get-energy
			     :energy-loss being-energy-loss
			     :food-to-energy being-food-to-energy
			     })
(def being-actions  {
		     :do-behave being-do-behave
		     :die being-die
		     :alive? being-alive?
		     :look-around being-look-around
		     :print-me being-print-me
		     :to-string being-to-string
		     :calc-outcome being-calc-outcome
		     :learn being-learn
		     })
(def being-prime-actions  {
			   :go-forward being-go-forward
			   :turn-to being-turn-to
			   :turn-to-avoid being-turn-to-avoid
			   :appease being-appease
			   })
(def being-gui-getters {
			:head-color being-head-color
			:body-color being-body-color
			})

;;FIXME: find a way for a child to keep _ALL_ the methods of the parent
;; and DO NOT modify any if a parent's one changes....
;; as a way, it could be added into an existing Protocol (protocol's map).
;; In this case any child has it 'by default'

(extend Being
  BeingGettersProtocol
  being-getters
  BeingSettersProtocol
  being-setters
  BeingIntCondProtocol
  being-intcond-getters
  BeingActionsProtocol
  being-actions
  BeingPrimeActionsProtocol
  being-prime-actions
  BeingGUIProtocol
  being-gui-getters
  )

