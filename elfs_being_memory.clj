(ns elfs.elfs-being-memory)
(in-ns 'elfs.elfs-main)

;;;-------------------------------------------------
;;; class: MemoryContacts
;;; - holds a list of beings one has once met:
;;; = 1.name, 2.outcome, 3.timestamp

(defrecord MemoryContacts [name outcome ts])


;;;--------------------------------------------------------------------------
;;; class: VolParams   >> MemoryAction
;;; - holds a set of Volatile (every step) parameter for a Being
;;; = keys of the MemoryActions map it's created from IncCond and ExtCond

(defrecord VolParams [energy satiety food food-around danger]
  Object (toString [this] (print-str (map str (vals this)))))

(defn VolParams-ctor [params]
  (VolParams. (:energy params) (:satiety params) (:food params) (:food-around params) (:danger params)))


;;;------------------------------------------------------------------
;;; class: ActionInfo   >> MemoryAction
;;; - holds a list of actions once executed
;;; = vals of the MemoryActions map

(defrecord ActionInfo [action-fn outcome source counter]
  Object (toString [this] (str (:name (meta (:action-fn this)))
			       " = $" (:outcome this) " " (:source this) " #" (:counter this))))

;;;-----------------------------------------------------------
;;; class: MemoryLexicon
;;; - list of words one has ever heard: 1.word 2.counter
;;; = [:verbs {verb/count pairs} :nouns {noun/count pair}])

(defrecord MemoryLexicon [verbs ; {"verb" int "verb2" int}
			  nouns ; {"noun" int "noun2" int}
			  ])


;;;-------------------------------------------------------------------------
;;; class: MemoryAction   >> Memory

(defrecord MemoryAction [#^VolParams vol-params #^ActionInfo action-info]
  Object (toString [this]
		   (str (:vol-params this) "\t>> " (:action-info this) "\n")))

(defn MemoryAction-ctor [#^VolParams vol-params #^ActionInfo action-info]
  (MemoryAction. vol-params action-info))

;;; methods:

(defn ma-get-vol-params [memory-action] 
  (:vol-params memory-action))

(defn ma-get-action-info [memory-action] 
  (:action-info memory-action))

(defn ma-search-action [memory-action actions-vec] 
  memory-action)

(defn ma-inc-counter [memory-action]
  (let [a-info (:action-info memory-action)
	cur-c (:counter a-info)
	new-a-info (:assoc a-info :counter (+ 1 cur-c))]
    (assoc memory-action :action-info new-a-info)))

;;; protocol:

(def memory-action  {
		     :get-vol-params ma-get-vol-params
		     :search-action ma-search-action
		     :inc-counter ma-inc-counter
		     })

(defprotocol MemoryActionProtocol
  (get-vol-params [#^MemoryAction memory-action])
  (search-action [el vec])
  (inc-counter [memory-action]))

(extend MemoryAction
  MemoryActionProtocol
  memory-action)


;;;--------------------------------------------------------
;;; class: STMemory 
;;; - short-term memory = result of the last behaving slot

(defrecord STMemory [contact #^MemoryAction action coords word]
  Object (toString [this] (str " _memory.stm.action: \n"
			       (:action this))))

(defn STMemory-ctor [contact action coords word]
  (STMemory. contact action coords word))


;;;---------------------------------------------------------
;;; class: Memory
;;; 

(defrecord Memory [contacts #^MemoryAction actions stm coords lexicon]
  Object (toString [this] (str
		   " _memory.stm:\n"
		   stm
		   " _memory.actions:\n"
		   (print-str (map str (let [vec (:actions this)]
					 (for [el vec]
					   el))
				   )))))

(defn Memory-ctor [loc]
  (let [ts (get-current-time)]
    (Memory. 
     ;;contacts: name, outcome, ts	      
     [(MemoryContacts. "my-name" 0 ts)
      (MemoryContacts. "my-name" 1 ts)] 
     ;;actions: vector of MemoryAction's
     []
     ;;last-action:
     nil
     ;;coords: MemoryCoords - vector of [loc]
     [loc]
     ;;lexicon: 
     [(MemoryLexicon. {"go" 0} ;verbs
		      {"mama" 0} ;nouns
		      )])))


;;; methods:

(defn memory-get-actions [memory]
  (:actions memory))

(defn ma-equal? [#^MemoryAction ma1 #^MemoryAction ma2]
  (if (and (= (:vol-params ma1) (:vol-params ma2))
	   (= (:action-fn (:action-info ma1)) (:action-fn (:action-info ma2))))
    true
    false))
  
(defn memory-search-action [el vec]
  (first (seq (filter #(ma-equal? el %) vec))))
  
(defn memory-inc-action-counter [#^MemoryAction this-ma]
  (let [ai (:action-info this-ma)
	ai-c (:counter ai)
	outcome (:outcome ai)]
    (assoc this-ma :action-info (assoc ai :counter (+ 1 ai-c) :outcome (+ outcome 1) ))))

(defn memory-update-actions [#^Memory memory #^MemoryAction action-to-add]
  ;; (println "update-actions: conds = " (:vol-params action-to-add))
  ;; (println "update-actions: action-fn = " (:action-fn (:action-info action-to-add)))
  (let [actions (memory-get-actions memory)] ; vector of MemoryAction's
   ;; (println "actions = " actions)
    (if-let [action-from-mem (memory-search-action action-to-add actions)]
      (do
	;;(println "action-from-mem = " action-from-mem)
	(let [updated-action (memory-inc-action-counter action-from-mem)]
	  (let [actions-new (replace {action-from-mem updated-action} actions)]
	    (assoc memory :actions actions-new))))
      (let [actions-new (conj actions action-to-add)]
	;;(println "actions-new = " actions-new)
	(assoc memory :actions actions-new)))))

;;; protocol 

(defprotocol MemoryProtocol
  (update-actions [#^Memory memory #^MemoryAction memory-action])
  (get-actions [mem]))

(def memory-actions  {
		      :update-actions memory-update-actions
		      :get-actions memory-get-actions
		      })

(extend Memory
  MemoryProtocol
  memory-actions)
