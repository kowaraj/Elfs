(ns elfs.elfs-gui)
(in-ns 'elfs.elfs-main)


;; GUI--------------------------------------------------------------------
(import 
 '(java.awt Color Graphics Dimension BorderLayout FlowLayout GridBagLayout GridBagConstraints)
 '(java.awt.image BufferedImage)
 '(javax.swing BorderFactory border.Border JPanel JFrame JButton BoxLayout JTextField JTextArea JScrollPane JSplitPane JSpinner SpinnerNumberModel JCheckBox)
 '(java.awt.event WindowAdapter ActionListener MouseListener))

(use 'clojure.repl)

;; text field -------------------------------------------------------
(def gui-tf01-fn (JTextField. "elfs.elfs-main/create-the-world"))

;; edit field -------------------------------------------------------
(def gui-spin01-model (SpinnerNumberModel. ant-sleep-ms 0 10000 100))
(def gui-spin01-ant-int (JSpinner. gui-spin01-model))

(defn get-gui-ant-sleep-ms [] (.. gui-spin01-model  getNumber intValue))
(defn get-gui-animation-sleep-ms [] animation-sleep-ms)
;;(defn get-gui-animation-sleep-ms [] (int (/ (get-gui-ant-sleep-ms) 3)))


;; ------------------------------------- buttons
(defn btn-ctor [btn-name btn-action]
  "A Button constructor"
  (proxy [JButton ActionListener] [btn-name]    
    (actionPerformed [e]
		     (btn-action) )))

;; buttons actions:

;; push
(defn button-start-action [] (push-the-life))
(defn button-refresh-action [] (push-the-light))
;; switch
(defn switch-light-action [] (switch-the-light))
(defn switch-time-action [] (switch-the-time))
;; create
(defn button-re-create-action [] (re-create-the-world))
(defn button-destroy-action [] (destroy-the-world))
;; other
(defn button-update-action []
  (let [tf-value (.getText gui-tf01-fn)
    	f-name (symbol (.getText gui-tf01-fn))]
    (upd-s-pane-text (clojure.repl/source-fn f-name))))
(defn dummy [] (println "a dummy action"))

;; buttons:

(def button-start (btn-ctor "start" button-start-action))
(def button-refresh (btn-ctor "refresh" button-refresh-action))
(def button-light (btn-ctor "dummy-light" dummy)) ;;OBSOLETE - replaced by checkboxes
(def button-time (btn-ctor "dummy-time" dummy)) ;;OBSOLETE - replaced by checkboxes
(def button-re-create (btn-ctor "re-create" button-re-create-action))
(def button-destroy (btn-ctor "destroy" button-destroy-action))
(def button-update (btn-ctor "update" button-update-action))

;; listeners:

(.addActionListener button-start button-start)
(.addActionListener button-refresh button-refresh)
(.addActionListener button-time button-time)
(.addActionListener button-light button-light)
(.addActionListener button-re-create button-re-create)
(.addActionListener button-destroy button-destroy)
(.addActionListener button-update button-update)


;; ------------------------------------- checkboxes
(defn chbox-ctor [chbox-name chbox-action]
  "A Button constructor"
  (proxy [JCheckBox ActionListener] [chbox-name]    
    (actionPerformed [e]
		     (chbox-action) )))

;; ------------------------------------ control panel
(defn get-control-panel []
  "control-panel"
  (let [chb-time (chbox-ctor "Time" switch-time-action)
	chb-light  (chbox-ctor "Light" switch-light-action)]
    ;; xetters
    (defn gui-get-chb-time [] (.isSelected chb-time))
    (defn gui-set-chb-time [check] (.setSelected chb-time check))
    (defn gui-get-chb-light [] (.isSelected chb-light))
    (defn gui-set-chb-light [check] (.setSelected chb-light check))
    (.addActionListener chb-time chb-time)
    (.addActionListener chb-light chb-light)

    (let [control-panel (JPanel.)
	  time-panel (JPanel.)
	  light-panel (JPanel.)
	  b (BorderFactory/createTitledBorder "control")]
      (doto time-panel
	(.add chb-time)
	(.add button-time))
      (doto light-panel
	(.add chb-light)
	(.add button-light))
      (doto control-panel
	(.setLayout (BoxLayout. control-panel BoxLayout/Y_AXIS))
	(.add time-panel)
	(.add light-panel)
	(.setBorder b))
      control-panel)))

;; ------------------------------------ info-panel
(def gui-tf-bx (JTextField. "x"))
(def gui-tf-by (JTextField. "y"))
(def gui-tf-p (JTextField. "p"))

(defn button-info-action []
  ;; get-coordinates
  (let [tf-x (Integer/parseInt (.getText gui-tf-bx))
	tf-y (Integer/parseInt (.getText gui-tf-by))
	loc [tf-x tf-y]
	p (place loc)
	being (get-being @p)]
    ;; print the info
    (if (and being (being-assert being))
;;;;      (upd-s-pane-text (string-me being (str "being at" tf-x tf-y ":")))
      (upd-s-pane-text (string-me being (str "being at" tf-x tf-y ":")))
      (upd-s-pane-text "there's no being here"))))

(def button-info (btn-ctor "info" button-info-action))
(.addActionListener button-info button-info)

(defn get-info-panel []
  "info-panel"
  (defn make-checkbox-panel []
    "checkbox"
    (let [chbox (JCheckBox.)]
      (defn gui-following-mode? []
	(.isSelected chbox))
      (.setSelected chbox true)
      chbox))
  ;;panel
  (let [i-panel (JPanel.)
	b (BorderFactory/createTitledBorder "info")]
    (doto i-panel
      (.setLayout (BoxLayout. i-panel BoxLayout/X_AXIS))
      (.add (make-checkbox-panel))
      (.add gui-tf-p)
      (.add gui-tf-bx)
      (.add gui-tf-by)
      (.add button-info)
      (.setBorder b))
    i-panel))

;; ------------------------------------- buttons panel
(defn make-tools-panel []
  (let [tools-panel (JPanel.)
	run-panel (JPanel.)
	create-panel (JPanel.)
	run-create-panel (JPanel.)
	therest-panel (JPanel.)
	button-spin-panel (JPanel.)
	run-create-border (BorderFactory/createTitledBorder "run-create")
	therest-border (BorderFactory/createTitledBorder "...")
	]

    (doto run-panel
      (.setLayout (BoxLayout. run-panel BoxLayout/Y_AXIS)) 
      (.add button-start)
      (.add button-refresh))
    (doto create-panel
      (.setLayout (BoxLayout. create-panel BoxLayout/Y_AXIS)) 
      (.add button-re-create)
      (.add button-destroy))
    (doto run-create-panel
      (.setLayout (BoxLayout. run-create-panel BoxLayout/X_AXIS)) 
      (.add run-panel)
      (.add create-panel)
      (.setBorder run-create-border))

    (doto button-spin-panel
      (.setLayout (BoxLayout. button-spin-panel BoxLayout/X_AXIS)) 
      (.add button-update)
      (.add gui-spin01-ant-int))
    
    (doto therest-panel
      (.setLayout (BoxLayout. therest-panel BoxLayout/Y_AXIS)) 
      (.add gui-tf01-fn)
      (.add button-spin-panel)
      (.add (get-control-panel))
      (.add (get-info-panel))
      (.setBorder therest-border))

    (doto tools-panel
      (.setLayout (BoxLayout. tools-panel BoxLayout/Y_AXIS)) 
      (.add run-create-panel)
      (.add therest-panel))
    tools-panel))

;; text field -----------------------------------------------------------
(defn get-text-pane []
  (let [text-area-1 (JTextArea.)
	text-area-2 (JTextArea.)]
    (doto text-area-1
      (.setLineWrap true)	     
      (.setColumns 10)
      (.setRows 10)
      ;;(.setText (str (read-string (clojure.repl/source-fn 'create-the-world))))
      (.setText "there you can see a fn source code"))
    (doto text-area-2
      (.setText "there will be a console output"))

    (let [s-pane (JScrollPane. text-area-1)]
      (defn upd-s-pane-text [_str]
	(.setText text-area-1 _str)
	(.validate s-pane))
      (defn upd-s-pane-text-2 [_str]
	(.setText text-area-2 _str)
	(.validate s-pane))
      (let [s-pane-console (JScrollPane. text-area-2)]
	(let [split-pane (JSplitPane. JSplitPane/VERTICAL_SPLIT s-pane, s-pane-console)]
	  (doto s-pane
	    (.setPreferredSize (Dimension. text-panel-width (* dim 4 ants-size)))
	    )
	  split-pane)))))
 
;; panel --------------------------------------------------------------

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn gui-take-color [color-name]
  (eval `(. Color ~color-name)))
;;;(gui-take-color 'black)
;;(defmacro mgui-take-color [color-name]
;; `(. Color ~color-name))
;;;(mgui-take-color black)

(defn render-being [#^Being b #^Graphics g x y]
  (let [black (. (new Color 0 0 0 255) (getRGB))
	gray  (. (new Color 100 100 100 255) (getRGB))
	red   (. (new Color 255 0 0 255) (getRGB))
	;head x.y, tail x.y, head offset x.y
	[hx hy tx ty ox oy] ({0 [2 0 2 4 -1  0] ;  |
			      1 [4 0 0 4 -2  0] ;  /`
			      2 [4 2 0 2 -2 -1] ;  -
			      3 [4 4 0 0 -2 -2] ;  \.
			      4 [2 4 2 0 -1 -2] ;  |
			      5 [0 4 4 0  0 -2] ; ./
			      6 [0 2 4 2  0 -1] ;  -
			      7 [0 0 4 4  0  0]}; `\
			     (get-dir b))
	head-color (head-color b)
	body-color (body-color b)]
    (doto g
      ;; body
      (.setColor body-color)
      (.drawLine (+ (* hx ants-size) (* x scale)) (+ (* hy ants-size) (* y scale))
		 (+ (* tx ants-size) (* x scale)) (+ (* ty ants-size) (* y scale)))
      ;; head
      (.setColor head-color)
      (.fillRect (+ (* x scale) (+ (* hx ants-size) ox))
		 (+ (* y scale) (+ (* hy ants-size) oy))
		 ants-head ants-head))))

(defn render-place [g p x y]  
  [(try
     ;;(println "in: render-place: p=" p)
     (when (pos? 0);(get-poison p))
       (fill-cell g x y (new Color 0 255 0
			     (int (min 255 (* 255 (/ (get-poison p) pher-scale)))))))
     (when (pos? (get-food p))
       (fill-cell g x y (new Color 255 0 0
			     (int (min 255 (* 255 (/ (get-food p) food-scale)))))))
     (let [b (get-being p)]
       ;;(println "in: render-place: b=" b)
       (when b
	 (render-being b g x y)))
     
     (catch Exception e
       (println "[EXCEPTION HANDLER].render-place")
       (print-e-backtrace e 5)
       (time-off) ))])

(defn render-place-light-off [g p x y]
  (fill-cell g x y (. Color gray)))

(defn render-base [g])

(defn render-h [g]
  (let [img (new BufferedImage (+ (* scale dim) 20) 20
		 (. BufferedImage TYPE_INT_ARGB))
	bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 20 20)
      (.setColor (. Color LIGHT_GRAY))
      (.fillRect 20 0 (- (. img (getWidth)) 20) (. img (getHeight)))
      )
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(defn render-v [g]
  (let [img (new BufferedImage 20 (* scale dim)
		 (. BufferedImage TYPE_INT_ARGB))
	bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color LIGHT_GRAY))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(defn render [g]
  ;;FIXME: why do I need a 'dosync' for 'read-only' reference access?
  (let [v (dosync (apply vector (for [x (range dim) y (range dim)]
				  @(place [x y]))))
	img (new BufferedImage (* scale dim)
		 (* scale dim)
		 (. BufferedImage TYPE_INT_ARGB))
	bg (. img (getGraphics))]
    ;; background
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))

    ;; places
;    (if lighting
      (dorun
       (for [x (range dim) y (range dim)]
	 (render-place bg (v (+ (* x dim) y)) x y)))
;      (dorun
;       (for [x (range dim) y (range dim)]
;	 (render-place-light-off bg (v (+ (* x dim) y)) x y))))

    
    (doto bg
      (.setColor (. Color blue))
      (.drawRect (* scale home-off) (* scale home-off)
		 (* scale nants-sqrt) (* scale nants-sqrt)))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(defn mouse-clicked-action [e]
  (let [e-x (.getX e)
	e-y (.getY e)
	x (int (/ e-x scale))
	y (int (/ e-y scale))
	p (place [x y])
	being (get-being @p)]
    (upd-s-pane-text-2 (str "["x","y"]"))
    (if (and being (being-assert being))
      (do
	(dosync (ref-set target-being being))
;;;	(upd-s-pane-text (string-me being (str "being at click:")))
	)
      (upd-s-pane-text "there's no being here"))))

(defn make-main-panel []
  (let [main-panel (proxy [JPanel MouseListener] []
		(paint [g] (render g))
		(mousePressed [e])
		(mouseEntered [e])
		(mouseExited [e]) 
		(mouseClicked [e] (mouse-clicked-action e))
		(mouseReleased [e]))]
    (.addMouseListener main-panel main-panel)
    (doto main-panel
      (.setPreferredSize
       (new Dimension (* scale dim) (* scale dim))))
    main-panel))

(defn make-h-panel []
  (let [h-panel	(proxy [JPanel MouseListener] []
		  (paint [g] (render-h g))
		  (mouseClicked [e] (mouse-clicked-action)))]
    (doto h-panel
      (.setPreferredSize
       (new Dimension (+ (* scale dim) 20) 20)))
    h-panel))

(defn make-v-panel []
  (let [panel	(proxy [JPanel MouseListener] []
		  (paint [g] (render-v g))
		  (mouseClicked [e] (mouse-clicked-action)))]
    (doto panel
      (.setPreferredSize
       (new Dimension 20 (* scale dim))))
    panel))

;; --------------------------------- main frame

(defn create-main-frame []
  (let [;graphics
	main-panel (make-main-panel) ; base-panel
	h-panel (make-h-panel) ; base-panel
	v-panel (make-v-panel) ; base-panel
	;controls & info
	base-panel (JPanel.) ; frame
	buttons-panel (make-tools-panel) ; frame
	text-panel (get-text-pane) ; frame
	;frame
	frame (JFrame.)
	frame-control (JFrame.)
	frame-text (JFrame.)] 

    (defn repaint-the-land []
      (. main-panel (repaint))
      (. h-panel (repaint))
      (. v-panel (repaint))
      )
    
    (doto base-panel
      (.setLayout (BorderLayout.))
      (.add h-panel BorderLayout/NORTH)
      (.add v-panel BorderLayout/WEST)
      (.add main-panel BorderLayout/CENTER))

    (doto frame
      (.add base-panel)
;      (.add buttons-panel)
;      (.add text-panel)
      (.setLayout (FlowLayout.)) .show .pack)
    (doto frame-control
;      (.add base-panel)
      (.add buttons-panel)
;;      (.add text-panel)
      (.setLocation 1000 0)
      (.setLayout (FlowLayout.)) .show .pack)
    (doto frame-text
 ;     (.add base-panel)
 ;     (.add buttons-panel)
      (.add text-panel)
      (.setLocation 1000 300)
      (.setLayout (FlowLayout.)) .show .pack)

    [frame frame-control frame-text]))