(ns elfs.elfs-utils)
(in-ns 'elfs.elfs-main)

;;; Printing out

(declare pick-action)
(defmacro print-an-action [conds]
  `(let []
     (println "conds =" ~conds)
     (println "   --->" ~(pick-action conds))
     ))

;;; Exception's handling

(defn print-e-backtrace 
  ([exception]
  (println "e.class = " (.getClass exception))
  (println "e.msg = " (.getMessage exception))
  (println "e.backtrace:")
  (loop [e-seq (seq (.getStackTrace exception))]
    (when (first e-seq)
      (println "   >>" (first e-seq))
      (recur (rest e-seq)))))
  
  ([exception count]
  (println "e.class = " (.getClass exception))
  (println "e.msg = " (.getMessage exception))
  (println "e.backtrace:")
  (loop [e-seq (seq (.getStackTrace exception))
	 i count]
    (when (and (first e-seq)
		 (> i 0))
      (println "   >>" (first e-seq))
      (recur (rest e-seq) (dec i))))))


;;; Multi-dimentional maps parsing

(defn get-random-action [a-list] (nth a-list (rand (count a-list))))

(defn make-map-from-keys [keys a-list]
  "Creates a map. Keys are passed as an argument.
  Values are taken from a sequence of random actions"
  (let [max-number-of-keys 100]
    (zipmap keys (for [i (range max-number-of-keys)]
		   (get-random-action a-list)))))

(defn get-current-time-ms []
  (.getTime (java.util.Date.)))
(def get-current-time get-current-time-ms)













(comment

;(defn embed-map [map updating-map a-list]
;  "Embeds an updating-map to the map"
;  (loop [ks (keys map) m {}]
;    ;; If there are still some keys, go further.
;    (if-let [k (first ks)]
;      (let [v (k map)] ;take the first value (it could be again a map).
;	;; go deeper?
;	(if (map? v)
;	  (recur (rest ks) (assoc m k (embed-map v updating-map a-list)))
;	  ;; If the map is 1-dimensional....ooouuuuuhhh.
;	  (recur (rest ks) (assoc m k (make-map-from-keys (keys updating-map) a-list)))))
;      m)))
;
;(defn make-map [_conditions-map _actions-list]
;  "Parses the conditions-map passed as an arg line by line. Calls
;  'embed-map' to create an internal N-Dimentional conditions space"
;  (let [cm-keys (keys _conditions-map)]
;    ;; Takes the first record of _conditions-map.
;    (loop [record-N ((first cm-keys) _conditions-map)
;	   conds (rest cm-keys)]
;      ;; If there are more records, go further.
;      (if-let [c (first conds)]
;	;; record-N+1 is a next record of _conditions-map
;	(let [record-N+1 (c _conditions-map)]
;	  ;; Embed record-N+1 -> record-N, and recur.
;	  (recur (embed-map record-N record-N+1 _actions-list) (rest conds)))
;	record-N))))

  (defn embed-map [map updating-map name a-list]
  "Embeds an updating-map to the map"
  (loop [ks (keys map) m {}]
    ;; If there are still some keys, go further.
    (if-let [k (first ks)]
      (let [v (k map)] ;take the first value (it could be again a map).
	;; go deeper?
	(if (map? v)
	  (recur (rest ks) (assoc m k (embed-map v updating-map name a-list)))
	  ;; If the map is 1-dimensional....ooouuuuuhhh.
	    (recur (rest ks) (assoc m k {name (make-map-from-keys (keys updating-map) a-list)}))))
      m)))

(defn make-map [_conditions-map _actions-list]
  "Parses the conditions-map passed as an arg line by line. Calls
  'embed-map' to create an internal N-Dimentional conditions space"
  (let [cm-keys (keys _conditions-map)]
    ;; Takes the first record of _conditions-map.
    (loop [record-N ((first cm-keys) _conditions-map)
	   conds (rest cm-keys)]
      ;; If there are more records, go further.
      (if-let [c (first conds)]
	;; record-N+1 is a next record of _conditions-map
	(let [record-N+1 (c _conditions-map)
	      name c]
	  ;; Embed record-N+1 -> record-N, and recur.
	  (recur (embed-map record-N record-N+1 name _actions-list) (rest conds)))
	{(first cm-keys) record-N}))))


(defn embed-predefined-map [up-name path map updating-map name a-list]
  "Embeds an updating-map to the map"
  (loop [ks (keys map) m {}]
    ;; If there are still some keys, go further.
    (if-let [k (first ks)]
      (let [v (k map)] ;take the first value (it could be again a map).
	(println "v =" v)
;	(println "k =" k)
	;; go deeper?
	(if (map? v)
	  (recur (rest ks) (assoc m k (embed-predefined-map k path v updating-map name a-list)))
	  ;; If the map is 1-dimensional....ooouuuuuhhh.
	  (do
	   ; (println "assoc: m =" m)
	    (println "assoc: k =" k)
	    (println "assoc: name = " name)
	    ;(println "assoc: updating-map =" updating-map)
	    (println "assoc: up-name =" up-name)	    

	    (let [up-name-val (name pattern-map)]
	      (println "up-name-val =" up-name-val)
	      (if (not (= up-name-val k))
		(recur (rest ks) (assoc m k {name (make-map-from-keys (keys updating-map) a-list)}))
		(recur (rest ks) (assoc m k {name (make-map-from-keys (keys updating-map) debug-actions-list-2)}))))
	    )))
      m)))

(def pattern-map {:satiety :hungry :energy :weak :danger :false})
(def debug-actions-map (make-predefined-map debug-conditions-map debug-actions-list))

(defn make-predefined-map [_conditions-map _actions-list]
  "Parses the conditions-map passed as an arg line by line. Calls
  'embed-map' to create an internal N-Dimentional conditions space"
  (println "mpm")
  (let [cm-keys (keys _conditions-map)
	up-name (first cm-keys)
	path (zipmap cm-keys (repeat nil))]
    ;; Takes the first record of _conditions-map.
    (loop [record-N ((first cm-keys) _conditions-map)
	   conds (rest cm-keys)]
      ;; If there are more records, go further.
      (if-let [c (first conds)]
	;; record-N+1 is a next record of _conditions-map
	(let [record-N+1 (c _conditions-map)
	      name c]
	  ;; Embed record-N+1 -> record-N, and recur.
	  (recur (embed-predefined-map up-name path record-N record-N+1 name _actions-list) (rest conds)))
	{(first cm-keys) record-N}))))




  (def debug-actions-list '(a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
  (def debug-actions-list-2 '(a666 a661 a662 a663 a664 a665 a666 a667 a668 a669))
  
  (def debug-conditions-map {
   :satiety {:full [50 100] :hungry [20 49] :starving [1 19] :dead [0 0]}
   :energy {:strong [40 100] :weak [10 39] :dying [1 9] :dead [0 0]}
   :danger {:true [1 1] :false [0 0]}
   })

  (def debug-actions-map (make-map debug-conditions-map debug-actions-list))
  debug-actions-map

  (defn pick-action [m-conds]
  (loop [actions-map-level debug-actions-map]
    ;; should be an only top-key (name of a cond)
    (let [first-cond-name (first (keys actions-map-level))
	  am-record (first-cond-name actions-map-level)
	  first-cond-value (first-cond-name m-conds)
	  am-record-value (first-cond-value am-record)]
      (if (map? am-record-value)
	  (recur am-record-value)
	  am-record-value))))

  (pick-action {:satiety :full :energy :weak :danger :true})
  (pick-action {:satiety :hungry :energy :strong :danger :false})

)