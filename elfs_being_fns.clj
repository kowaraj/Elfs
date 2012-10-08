(ns elfs.elfs-being-fns)
(in-ns 'elfs.elfs-main)

;; <elfs-being-fns>
;; 1. primitive-actions-list
;;    - a list of all possible actions of a being to behave.
;; 2. conditions-map
;;    - a list of ranges per condition tag
;; 3. actions-*

(defprotocol BeingFunctionsProtocol
  (action-wander [this])
  )

(defn being-action-wander ;;FIXME: replace wander by move.. jump... step... 
  "Agent's action - updated the place.
   Go straight forward.
   Takes: Being (this), World (p)
   Affects: Being (loc = loc+dir), World (p=p-Being, p+dir=p+Being)"
  [this]
  {:post [(being-assert %)]}
  ;; (println "action-wander")
  (let [loc (get-loc this)
	p (place loc)
	new-loc (delta-loc loc (get-dir this))
	new-p (place new-loc)]
    (if (not (get-being @new-p)) ;access granted?
      ;; move
      (let [moved-this (go-forward this new-loc)] ;changes the beings' loc
	(alter new-p set-being moved-this) ;fills a new place with the being
	(alter p set-being nil) ;;FIXME: clean the place by dissoc, example is (move)
	moved-this) ;clears the current place      
      ;; punish the beast, update p's being
      (let [turned-this (turn-to-avoid this)]
	(alter p set-being turned-this)
	turned-this))))

(def being-functions  {
		       :action-wander being-action-wander
		       })

(extend Being
  BeingFunctionsProtocol
  being-functions)

;; actions selector -------------------------------------------------------------;
(def conditions-map {
   :satiety {:full [50 100] :hungry [20 49] :starving [1 19] :dead [0 0]}
   :energy {:strong [40 100] :weak [10 39] :dying [1 9] :dead [0 0]}
   :danger {:true [0 7] :false false}
;   :danger-around {:true [0 7] :false false}
   :food {:true [1 food-range] :false false}
   :food-around {:true [0 7] :false false}		     
   })

(def primitive-actions-list [
			     #'action-wander
			     #'action-graze
			     #'action-forage
			     ])

(defn key-to-string [a-keyword]
  (apply str (drop 1 (seq (str a-keyword)))))
;;testit: (map key-to-string params-map)

(defn key-to-symbol [a-keyword]
  (symbol (apply str (drop 1 (seq (str a-keyword))))))

(defn pick-action-random []
  (let [i (int (mod (* (rand) 10) (count primitive-actions-list)))]
    (primitive-actions-list i)))

(defn pick-action [m-conds actions-map]
  ;;;DEBUG_ONLY
  (cond
   (= m-conds {:satiety :starving, :energy :dying, :danger :false, :food :false, :food-around :false})
   #'action-forage
   (= m-conds {:satiety :starving, :energy :dying, :danger :false, :food :false, :food-around :true})
   #'action-forage
   (= m-conds {:satiety :starving, :energy :dying, :danger :false, :food :true, :food-around :false})
   #'action-graze
   (= m-conds {:satiety :starving, :energy :dying, :danger :false, :food :true, :food-around :true})
   #'action-graze    
   (= m-conds {:satiety :dead, :energy :dying, :danger :false, :food :false, :food-around :false})
   #'action-forage
   (= m-conds {:satiety :dead, :energy :dying, :danger :false, :food :false, :food-around :true})
   #'action-forage
   (= m-conds {:satiety :dead, :energy :dying, :danger :false, :food :true, :food-around :false})
   #'action-graze
   (= m-conds {:satiety :dead, :energy :dying, :danger :false, :food :true, :food-around :true})
   #'action-graze    
   :else
   (pick-action-random)
   )
  )
   
;; actions ----------------------------------------------------------------------;

;(defn action-wander ;;FIXME: replace wander by move.. jump... step... 
;  "Agent's action - updated the place.
;   Go straight forward.
;   Takes: Being (this), World (p)
;   Affects: Being (loc = loc+dir), World (p=p-Being, p+dir=p+Being)"
;  [this]
;  {:post [(being-assert %)]}
;  ;; (println "action-wander")
;  (let [loc (get-loc this)
;	p (place loc)
;	new-loc (delta-loc loc (get-dir this))
;	new-p (place new-loc)]
;    (if (not (get-being @new-p)) ;access granted?
;      ;; move
;      (let [moved-this (go-forward this new-loc)] ;changes the beings' loc
;	(alter new-p set-being moved-this) ;fills a new place with the being
;	(alter p set-being nil) ;;FIXME: clean the place by dissoc, example is (move)
;	moved-this) ;clears the current place      
;      ;; punish the beast, update p's being
;      (let [turned-this (turn-to-avoid this)]
;	(alter p set-being turned-this)
;	turned-this))))

(defn action-forage
  ;;FIXME: if there's no food around => go forward, don't turn!
  "Agent's action - updates the place.
   Turns to the max-food direction. If already, goes there.
   Takes:
    Being: this
    World: p
   Affects as 'action-wander' or:
    Being: dir=dir-max-food
    World: p=new-Being"
  [this]
  {:post [(being-assert %)]}
  (let [dir (get-dir this)
	dir-max-food (:food-around (:ext-cond this))]
    ;; (println "action-forage, dir =" dir ", dir-max-food =" dir-max-food)
    (if (or (= dir dir-max-food)
	    (not dir-max-food))
      ;; go forward
      (action-wander this)
      ;; turn to the max-food direction
      (let [loc (get-loc this)
	    p (place loc)
	    turned-this (turn-to this dir-max-food)]
	(alter p set-being turned-this)
	turned-this))))

(defn penalty-sleep [this]
  ;; (println "penalty-sleep")
  ;;FIXME: idle for some time to be punished for a uneffective action-selection
  this)

(defn action-graze
  "Agent's actions - updates the place.
   Decrements an amount of food at the place. Increments his own one.
   Takes:
    Being: this
    World: p
   Affects:
    Being: this::satiety+=1
    World: p::food-=1, p::being=new-Being "
  [this]
  {:post [(being-assert %)]}
  ;; (println "action-graze")
  (let [loc (get-loc this)
	p (place loc)
	food-available (:food (get-ext-params this))]   
    (if food-available
      ;; eat, update p's being & p's food
      (let [[grazed-this delta-food] (appease this food-available)]
	(alter p set-food (- food-available delta-food))
	(alter p set-being grazed-this)
	grazed-this)
      ;; punish the beast, update p's being
      (let [dumb-this (penalty-sleep this)]
	(alter p set-being dumb-this)
	dumb-this))))

(defn belongs? [a-range a-value]
  " Arguments: a-range must be a vector of two elements [min max]"
   ;;{:pre [(vector? a-range) (= 2 (count a-range))]}
;  (println "belongs?, a-range =" a-range)
;  (println "belongs?, a-value =" a-value)
;  (println "belongs?, type of a-range =" (type a-range))
  (cond
   (vector? a-range)
   ;; a-range is a vector
   (cond
    ;; a-value is an Integer
    (instance? java.lang.Integer a-value)
    (let [[min max] a-range] 
      (and (>= a-value min)
	   (<= a-value max)))
    ;; a-value is a Boolean
    (instance? java.lang.Boolean a-value)
    false)
   ;; a-range is a Boolean
   (instance? java.lang.Boolean a-range)
   (= a-value a-range)
   ;; Type not found
   :else (throw (Exception. "unknown type"))))
   
(defn value-to-label [a-value a-map]
  " Input: a-value: int = a value of a parameter
    Input: a-map: map = a record of the conditions-map
    Output: a keyword - the name of the range"
  ;;{:post [(keyword? %)]}
  (loop [ks (keys a-map)]
    (if (not (empty? ks))
      (if (belongs? ((first ks) a-map) a-value)
	(first ks)
	(recur (rest ks)))
      (throw (Exception. (str "The value {" a-value "} is not found in the map {" a-map "}."))))))
;;testit: (value-to-label 50 ((first (keys conditions-map)) conditions-map))
;;=> :full
;;TODO: put some limitations on the parameters of a Being

(defn params-to-conds [a-params]
  "Takes a Being's params and maps them upon the 'conditions-map'"
  {:pre [(map? a-params)]}
  (let [conds-ks (keys conditions-map)]
    (loop [ks conds-ks ret {}]
      (if-let [kw (first ks)]
	(let [_value (kw a-params)
	      _map (kw conditions-map)]
;	  (println "params-to-conds, kw=" kw)
;	  (println "params-to-conds, value=" _value)
;	  (println "params-to-conds, map=" _map) 
	  (recur (rest ks) (merge ret {kw (value-to-label _value _map)})))
	ret
	))))
;;testit: (params-to-conds-macro {:satiety 50 :energy 43 :danger 0})
;;=> {:danger :false, :energy :strong, :satiety :full}


(def dir-delta {0 [0 -1]
		1 [1 -1]
		2 [1 0]
		3 [1 1]
		4 [0 1]
		5 [-1 1]
		6 [-1 0]
		7 [-1 -1]})

(defn delta-loc
  [[x y] dir]
  (let [[dx dy] (dir-delta (bound 8 dir))]
    [(bound dim (+ x dx)) (bound dim (+ y dy))]))

(defn bound
  [b n]
  (let [n (rem n b)]
    (if (neg? n)
      (+ n b)
      n)))

(defn get-fov [[x y]]
  "Get a field of vision (8 adjancent cells)"
  (let [x-1 (bound dim (- x 1))
	x+1 (bound dim (+ x 1))
	y-1 (bound dim (- y 1))
	y+1 (bound dim (+ y 1))
	cells [[x-1 y-1] [x y-1] [x+1 y-1]
	       [x-1   y]         [x+1   y]
	       [x-1 y+1] [x y+1] [x+1 y+1]]
	fov (map place cells)
	]
    ;; (println "get-fov, cells = " cells)
    ;; (println "get-fov, FOV = " fov)
    fov))

(defn get-food-dir [fov-places]
  (let [food-values (map #(:food @%) fov-places)
	directions-vector [7 0 1 6 2 5 4 3]
	place-dir-pairs (map vector food-values directions-vector)]
    ;; (println "get-food-dir, food-values =" food-values)
    ;; (println "get-food-dir, place-dir-pairs =" place-dir-pairs)
    (let [max-food-dir-pair (last (sort place-dir-pairs))
	  food-value (first max-food-dir-pair)]
      (if (and food-value (pos? food-value))
	(second max-food-dir-pair)
	nil))))

(defn get-action-fn [this]
  (try
    (let [params (merge (get-int-params this) (get-ext-params this))
;	  _ (println "* params =" params)
	  conds (params-to-conds params)
;	  _ (println "* conds =" conds)
	  actions-map (get-actions-map this)]
      (let [a (pick-action conds actions-map)]	
	[a conds]))
     
     (catch Exception e
       (println (str "[EH].get-action-fn for: " (get-name this)))
       (time-off)
       (print-e-backtrace e 5)) ))

(defn undergo-fn
  "Agent's undergo."
  ;;FIXME: it's called for an already updated-ext-conds-this!
  ;; so, food-to-energy and energy-loss needn't get ext-conds again!
  ;; and they don't ;)
  ;; but 'being-attacked' and others will do!
  [this]
  (let [new-this-f2e (food-to-energy this)
	new-this-eloss (energy-loss new-this-f2e 1)
	loc (get-loc this)
	p (place loc)]
    (alter p set-being new-this-eloss)
    new-this-eloss))

