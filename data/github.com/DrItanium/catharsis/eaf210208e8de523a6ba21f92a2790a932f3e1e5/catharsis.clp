; Entry point into catharsis
; Modify this to fit your system.
(defglobal MAIN 
           ?*catharsis-root* = (str-cat (get-shell-variable HOME) 
                                        "/dev/catharsis"))

(defmountpoint catharsis ?*catharsis-root*)

(batch* (proton: /lib/core.clp))
(batch* (proton: /lib/neutron.clp))
; begin catharsis
(defglobal MAIN
           ?*old-point-x* = 0
           ?*old-point-y* = 0)

(deftemplate pen-size
             (slot min 
                   (type INTEGER)
                   (range 1 ?VARIABLE)
                   (default 1))
             (slot max
                   (type INTEGER)
                   (range 1 ?VARIABLE)
                   (default 128))
             (slot current
                   (type INTEGER)
                   (range 1 ?VARIABLE)
                   (default 1)))

(defmethod on-resized
  "method to handle resizing of the window"
  ((?value SYMBOL (not (neq ?value FALSE TRUE))))
  (if (and ?value (< (getwindow) 0)) then
    (printout werror "ERROR: couldn't reattach to window" crlf)
    (exit))
  (bind ?two-thirds (/ 2 3))
  (bind ?width (screen/dimensions/width))
  (bind ?height (screen/dimensions/height))
  (bind ?point-dim (as-point:screen/dimensions))
  (bind ?f2/3-width (* ?width ?two-thirds))
  (bind ?f2/3-height (* ?height ?two-thirds))
  (bind ?f2/3-point (new Point ?f2/3-width ?f2/3-height))
  (bind ?p0height-point (new Point 0 ?height))
  (bind ?pwidth0-point (new Point ?width 0))
  (bind ?zpointer (send [ZP] get-pointer))
  (bind ?pipointer (send [pixel-image] get-pointer))
  (screen/draw-line ?zpointer
                    ?p0height-point
                    0
                    0
                    8 
                    ?pipointer
                    ?zpointer)
  (screen/draw-line ?p0height-point
                    ?point-dim
                    0
                    0
                    8 
                    ?pipointer
                    ?zpointer)
  (screen/draw-line ?point-dim
                    ?pwidth0-point
                    0
                    0
                    8 
                    ?pipointer
                    ?zpointer)
  (screen/draw-line ?zpointer
                    ?pwidth0-point
                    0
                    0
                    8 
                    ?pipointer
                    ?zpointer)
  (screen/draw-line (new Point 0 ?f2/3-height)
                    ?f2/3-point
                    0
                    0
                    8
                    ?pipointer
                    ?zpointer)
  (screen/draw-line (new Point ?f2/3-width 0)
                    (new Point ?f2/3-width ?height)
                    0
                    0
                    8
                    ?pipointer
                    ?zpointer)
  )

(definstances elements
              (menu1 of menu (menu-entries cut copy paste))
              (menu2 of menu (menu-entries eat sleep drink))
              (scratch-rect of rectangle (x 0) (y 0) (bx 0) (by 0))
              (pixel-image of image (rectangle [pixel])
                           (replicate TRUE)
                           (color (get-standard-color black))))

(deffacts query-operation
          (query input)
          (pen-size (min 1)
                    (max 128)
                    (current 1)))



(defrule query-input
         ?f <- (query input)
         =>
         (retract ?f)
         (send [mouse] query)
         (send [keyboard] query)
         (assert (check mouse)
                 (check keyboard)))

(defrule process-mouse-inputs
         (declare (salience -1))
         ?f <- (check mouse)
         =>
         (retract ?f)
         (assert (query mouse)))

(defrule process-mouse-inputs:button1
         ?f <- (check mouse)
         (object (is-a mouse)
                 (name [mouse])
                 (buttons button1)
                 (position ?x ?y))
         ;?rect <- (object (is-a rectangle)
         ;                 (name [scratch-rect]))
         (pen-size (current ?factor))
         =>
         (retract ?f)
         ;(modify-instance ?rect (x ?x) (y ?y)
         ;                 (bx (+ ?x ?factor)) 
         ;                 (by (+ ?y ?factor)))
         ;rebuild the native memory since we've made
         ; changes to the fields
         ;(send ?rect build-pointer)
         (screen/draw-line (new Point ?*old-point-x* ?*old-point-y*)
                           (new Point ?x ?y)
                           0
                           0
                           ?factor 
                           (send [pixel-image] get-pointer)
                           (send [ZP] get-pointer))
         (screen/draw-line (new Point ?*old-point-x* ?y)
                           (new Point ?x ?*old-point-y*)
                           0
                           0
                           ?factor 
                           (send [pixel-image] get-pointer)
                           (send [ZP] get-pointer))
         (screen/draw-line (new Point ?x ?y)
                           (new Point ?*old-point-x* ?*old-point-y*)
                           0
                           0
                           ?factor 
                           (send [pixel-image] get-pointer)
                           (send [ZP] get-pointer))
         (screen/draw-line (new Point ?x ?*old-point-y*)
                           (new Point ?*old-point-x* ?y)
                           0
                           0
                           ?factor 
                           (send [pixel-image] get-pointer)
                           (send [ZP] get-pointer))
         (bind ?*old-point-x* ?x)
         (bind ?*old-point-y* ?y)
         (assert (query mouse)))

;(screen/draw ?rect [pixel-image] [ZP])
;(assert (query mouse)))


(defrule process-mouse-inputs:menu1
         (declare (salience 1))
         ?f <- (check mouse)
         (object (is-a mouse)
                 (name [mouse])
                 (buttons button3))
         =>
         (retract ?f)
         ; Display a menu
         (printout t (send [menu1] show-menu 3) crlf)
         (assert (query mouse)))

(defrule process-mouse-inputs:menu2
         (declare (salience 1))
         ?f <- (check mouse)
         (object (is-a mouse)
                 (name [mouse])
                 (buttons button2)
                 (timestamp ?ts))
         =>
         (retract ?f)
         ; Display a menu
         (printout t (send [menu2] show-menu 2) crlf)
         (assert (query mouse)))


(defrule process-keyboard-inputs:quit
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys ESC))
         =>
         (retract ?f)
         (exit))
(defrule process-keyboard-inputs:left
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys LEFT))
         =>
         (retract ?f)
         (assert (query keyboard))
         (halt))
(defrule process-keyboard-inputs:nil
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys NIL))
         =>
         (retract ?f)
         (assert (query keyboard)))

(defrule process-keyboard-inputs:up
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys UP))
         ?pen <- (pen-size 
                   (current ?factor)
                   (max ?max))
         =>
         (if (< ?factor ?max) then
           (modify ?pen (current (+ ?factor 1))))
         (retract ?f)
         (assert (query keyboard)))

(defrule process-keyboard-inputs:down
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys DOWN))
         ?pen <- (pen-size (min ?min)
                           (current ?factor))
         =>
         (if (> ?factor ?min) then
           (modify ?pen (current (- ?factor 1))))
         (retract ?f)
         (assert (query keyboard)))
(defrule process-keyboard-inputs
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys ?b&~NIL))
         =>
         (retract ?f)
         (printout t "Pressed " ?b crlf)
         (assert (query keyboard)))

(defrule ready-to-query-input-again 
         ?f <- (query keyboard)
         ?f2 <- (query mouse)
         =>
         (retract ?f ?f2)
         (send [mouse] clear)
         (send [keyboard] clear)
         (assert (query input)))




(batch* (proton: /lib/reset-run-exit.clp))
