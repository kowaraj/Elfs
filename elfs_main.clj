(ns elfs.elfs-main
  (:use :reload elfs.elfs-defs)
  (:use :reload elfs.elfs-locus)
  (:use :reload elfs.elfs-being-memory)
  (:use :reload elfs.elfs-being)
  (:use :reload elfs.elfs-utils)  
  (:use :reload elfs.elfs-being-fns)
  (:use :reload elfs.elfs-gui)
  (:use :reload elfs.elfs-second-variety)
  (:use :reload elfs.elfs-third-variety)
  (:gen-class)
  )


(defn create-a-being
  "create a being at the location, returning a being agent on the location"
  [loc dir]
  (sync nil
	(let [p (place loc)
	      ;;TRY_SECOND_VARIETY
;	      b (if (> (rand) 0.5)
;		  (being-ctor loc dir)
;		  (seva-ctor loc dir))
;;;	      b (seva-ctor loc dir)
	      b (thiva-ctor loc dir)
	      ]
;(println "Being's action map: ")
;(print-an-action {:satiety :starving :energy :dying :danger :false :food :false :food-around :false})
;(print-an-action {:satiety :starving :energy :dying :danger :false :food :false :food-around :true})
;(print-an-action {:satiety :starving :energy :dying :danger :false :food :true :food-around :false})
;(print-an-action {:satiety :starving :energy :dying :danger :false :food :true :food-around :true})

	  ;(println "type = " (type b))
	  (alter p set-being b)
	  (agent b))))

;; LIVE
(defn let-there-be-live 
  "places initial food and beings, returns seq of ant agents"
  []
  (sync nil
    (dotimes [i food-places]
      (let [p (place [(rand-int dim) (rand-int dim)])]
	(alter p set-food (rand-int food-range))))
    (doall
     (for [i (range beings-number)]
      (let [x (rand-int dim) y (rand-int dim)]
	(create-a-being [x y] (rand-int 8)))))))


(defn time-on []
  (def running true))

(defn time-off []
  (def running false))

(defn light-on []
  (def lighting true))

(defn light-off []
  (def lighting false))

;; ---------------------------------- world creation

(defn create-the-world [] ;;Start the application
  "Global entities creation"
  
  ;; SPACE
  (defn let-there-be-space []
    (let [world (apply vector (map (fn [_] (apply vector (map
		 (fn [_] (ref (Locus. nil nil nil))) (range dim)))) (range dim)))]
      (defn place [[x y]]
	(-> world (nth x) (nth y))))
    nil)

  ;; LAND & GROUND
  (defn let-there-be-land []
    (create-main-frame))

  ;; LIGHT
  (defn let-there-be-light []
    (agent nil))
    
  ;; Start the creation!
  (let [the-void (let-there-be-space) ;place (and the 'world' in a closure)
	[the-ground the-control the-text] (let-there-be-land) ;frame
	the-beings (let-there-be-live) ;beings
	the-light (let-there-be-light) ;animator agent
	]

    ;; ---------------------------- world destruction
    (defn destroy-the-world []
      (light-off)
      (time-off)
      (println ">> Apathetic bloody planet, I've no sympathy at all.")
      ;; destroy-the-land
      (if the-ground
	(do
	  (.dispose the-ground)
	  (.dispose the-control)
	  (.dispose the-text))		
	(println ">> nothing to delete")))

    (defn follow-target-being []
      (when @target-being
	(let [target-agent (first (filter #(= (get-name @target-being) (get-name @%))
					  the-beings))]
	  (if target-agent
	    (let [target-b @target-agent]
	      ;; display
	      (upd-s-pane-text (string-me target-b "[target]"))
	      )))))

    ;; a kind of 'behave' for 'the-light' agent
    (defn keep-the-light-on [_]
      (try
	(when lighting
	  (send-off *agent* keep-the-light-on))
	(repaint-the-land)
	(when (gui-following-mode?)
	  (follow-target-being)) ;;observe a target
	(. Thread (sleep (get-gui-animation-sleep-ms)))	
	(catch Exception e
	  (println "[EXCEPTION HANDLER].keep-the-light-on")
	  (print-e-backtrace e 15)
	  (light-off)))
      nil)

    ;; setup life-agents handlers
    (defn agent-handler [a e]
      (println "[LIFE HANDLER]")
      (print-e-backtrace e 10)
      (time-off))
    (dorun (map #(set-error-mode! % :continue) the-beings))
    (dorun (map #(set-error-handler! % agent-handler) the-beings))

    ;; setup light-agent handlers
    (defn light-agent-handler [a e]
      (println "[LIGHT HANDLER]")
      (print-e-backtrace e 10)
      (time-off))
    (set-error-mode! the-light :continue)
    (set-error-handler! the-light light-agent-handler)

    
    ;; ----------------------------------- LIFE functions
    (defn push-the-life []
      (try
	(println "push the time")
	(dorun (map #(send-off % do-behave) the-beings))
	(catch Exception e
	  (println "[EXCEPTION HANDLER].push-the-life")
	  (print-e-backtrace e 5)
	  (time-off))))

    (defn switch-the-time []
      (println "switch the time -" (if running "OFF" "ON"))
      (if running
	(do (gui-set-chb-time false) (time-off))
	(do (gui-set-chb-time true) (time-on))))

    ;; ---------------------------------- LIGTH functions 
    (defn push-the-light []
      (try
	(println "push the light")
	(send-off the-light keep-the-light-on)
	(catch Exception e
	  (println "[EXCEPTION HANDLER].push-the-light")
	  (print-e-backtrace e 5)
	  (light-off))))

    (defn switch-the-light []
      (println "switch the light -" (if lighting "OFF" "ON"))
      (if lighting
	(do (gui-set-chb-light false) (light-off))
	(do (gui-set-chb-light true) (light-on))))

    (light-off)
    (time-off)
    ))

(defn re-create-the-world []
  (println "re-creation...")
  (destroy-the-world)
  (require :reload-all 'elfs.elfs-main) ;WARNING: the code is reloaded!
  (create-the-world))
 
(defn -main []
  (create-the-world))

;; usage------------------------------------------------

;(defn unuse [ns]
;  (doseq [[n v] (ns-refers *ns*)]
;    (if (= (.. v ns name) ns)
;      (ns-unmap *ns* n))))
;
;(defn reuse [ns]
;  (unuse ns)
;  (remove-ns ns)
;  (use :reload-all ns))

(comment
  (create-the-world)
  (re-create-the-world)
  (destroy-the-world)
  ;; from REPL || run.clj:
  (load "elfs/elfs_main")
  (elfs.elfs-main/create-the-world)

  ;; from here:
  *ns*
  (defn run []
    (load "elfs_main")
    (create-the-world))
  (run)
  
  (use 'clojure.stacktrace)
  (print-stack-trace *e 3)
  )





;;; It gets the source code, but can only print it to the REPL output!
;(with-out-str (println (clojure.repl/source-fn 'create-the-world)))
;(with-out-str (source create-the-world))
;(read-string (clojure.repl/source-fn 'create-the-world))
;(elf-source create-the-world)
;(defmacro elf-source [n]
;  `(let [~'out (clojure.repl/source-fn '~n)]
;     ~'out))

     

;;;FIXME: how to grab the output and redirect it to the JTextField?
;(defn get-source-as-text []
;  (elf-source panel))

