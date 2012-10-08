(ns elfs.elfs-defs)
(in-ns 'elfs.elfs-main)

(declare
 upd-s-pane-text ;gui
 get-gui-ant-sleep-ms
 get-gui-animation-sleep-ms
 gui-take-color

 ;elfs-main
 time-on 
 time-off 
 create-the-world
 re-create-the-world
 destroy-the-world
 push-the-light
 push-the-life

 ;; switches (time & light)
 switch-the-light
 gui-set-chb-light
 switch-the-time
 gui-set-chb-time
 
 start-the-world
 stop-the-world
 step-the-world
 pause-the-world
 pause-the-light
 switch-the-light-off
 switch-the-light-on
 push-the-light
 get-fov ;elfs-being
 being-ctor
 being-child-assert
 
 get-food-dir
 get-action-fn
 undergo-fn
 make-map
 conditions-map
 primitive-actions-list
 place
 delta-loc	 
 bound
 action-wander ;elfs-being-fns
 action-graze 
 action-forage
 go-forward
 penalty-sleep
 get-current-time ;utils
 )

;;FIXME: make all the parameters as a separate namespace?
(def dim 16) ;multiple of 4!
(def ants-scale 10)
(def ants-size (/ ants-scale 2)) ;;scale
(def scale (+ (* ants-scale 2) 1))
(def ants-head (- ants-size 1))
(def nants-sqrt 2)
(def food-range 100)
(def food-places 20)
(def food-scale 30.0)
(def beings-number 1)
(def text-panel-width 300)

(def pher-scale 20.0)

(def home-off 0)
(def home-range (range home-off (+ nants-sqrt home-off)))
(def animation-sleep-ms 500)
(def ant-sleep-ms 1000)

;;(def running true)
(def running false)
(def lighting true)
(def target-being (ref nil))

;; Being's initial conditions:
(def kk-being-ic-satiety 5)
(def kk-being-ic-energy 5)
;; k-* = constants
(def kk-f2e 1)
(def kk-being-max-energy 100)
(def kk-being-max-satiety 100)
(def kk-being-appeased-by 2)

;; class Mortal
;(defrecord Mortal [type name alive]) ;type
;(defprotocol MortalProtocol ;protocol
;  (alive? [this]))

;; getters of Being (overriden)
;(def mortal-type (fn [this] (str (:type this) " ! get-type-mortal")))
;; getters of Mortal
;(def mortal-alive? (fn [this] (:alive this)))
;(def mortal-getters-default
;  {:alive? mortal-alive?})



