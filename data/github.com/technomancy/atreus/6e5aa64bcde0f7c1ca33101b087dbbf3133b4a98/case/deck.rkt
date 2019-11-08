#lang racket
;; Atreus 2 deck design
;; Copyright © 2019 Phil Hagelberg and contributors
;; released under the GPLv3 or later

;; TODO:
;; * port notch on bottom plate and spacer
;; * top layer

(require xml)

;; glowforge uses 96 dpi, 25.4 mm in an inch
(define scale (/ 96 25.4))
(define width 260)
(define height 232)


(define cols 6) ; per hand
(define rows 4)
(define angle (degrees->radians 10))
(define corner-radius 6.0)

(define alps-switch-width 15.887)
(define alps-switch-height 13.087)
(define cherry-switch-width 13.62)
(define cherry-switch-height 13.72)
(define cherry? true)
(define switch-height (if cherry? cherry-switch-height alps-switch-height))
(define switch-width (if cherry? cherry-switch-width alps-switch-width))

(define switch-spacing 19.0)

(define screw-radius 1.4) ; for M3 screws + kerf

(define side-screw-distance (* switch-spacing rows))
(define bottom-screw-distance (* switch-spacing cols))
(define left corner-radius)
(define bottom 95) ; outer bottom
(define left-top (+ left (* side-screw-distance (sin angle))))
(define top (- bottom (* side-screw-distance (cos angle))))
(define right (- width corner-radius))
(define right-top (- right (* side-screw-distance (sin angle))))
(define mid-bottom (+ bottom (* bottom-screw-distance (sin angle)) -3))
(define mid-offset 25)
(define mid-x (/ width 2))
(define mid-left (- mid-x mid-offset))
(define mid-right (+ mid-x mid-offset))

(define upper-height 75)
(define left-upper-top (+ left-top (* upper-height (sin angle))))
(define right-upper-top (- right-top (* upper-height (sin angle))))
(define upper-top (- top (* upper-height (cos angle))))

(define hull-coords (list (list right-upper-top upper-top)
                          (list right-top top)
                          (list right bottom)
                          (list mid-right mid-bottom)
                          (list mid-left mid-bottom)
                          (list left bottom)
                          (list left-top top)
                          (list left-upper-top upper-top)))

;;; screws
(define screws
  `(g () ,@(for/list ([s (append (take hull-coords 3)
                                 ;; the bottom middle has only one screw but
                                 ;; two hull positions
                                 (list (list (/ width 2) mid-bottom))
                                 (drop hull-coords 5))])
             `(circle ((r ,(number->string screw-radius))
                       (cx ,(number->string (first s)))
                       (cy ,(number->string (second s))))))))

;;; outline
(define outline-coords (append hull-coords (take hull-coords 2)))

(define (to-next-screw? theta current-screw)
  (let* ([current (list-ref outline-coords current-screw)]
         [cx (first current)] [cy (second current)]
         [next (list-ref outline-coords (add1 current-screw))]
         [nx (first next)] [ny (second next)]
         [dx (- nx cx)] [dy (- ny cy)]
         [next-theta (- (radians->degrees (atan dy dx)))])
    (= (floor (modulo (floor next-theta) 180))
       (floor (modulo (- theta 90) 180)))))

;; trace the outline by going from screw to screw until you've gone full-circle
(define (outline-points coords theta current-screw)
  (if (< -360 (- theta 90) 360)
      (let* ([current (list-ref outline-coords current-screw)]
             [sx (first current)] [sy (second current)]
             [x (+ sx (* (cos (degrees->radians theta)) corner-radius))]
             [y (- sy (* (sin (degrees->radians theta)) corner-radius))]
             [coords (cons (format "~s,~s" x y) coords)])
        (if (to-next-screw? theta current-screw)
            (begin (printf "~a~n" current)
                   (outline-points coords theta (add1 current-screw)))
            (outline-points coords (sub1 theta) current-screw)))
      coords))

(define port-depth 8)

(define port-curve (list (format "~s,~s" mid-right (- top corner-radius))
                         (format "~s,~s" (+ mid-x corner-radius)
                                 (+ top port-depth
                                    (- corner-radius)))
                         (format "~s,~s" (- mid-x corner-radius)
                                 (+ top port-depth
                                    (- corner-radius)))
                         (format "~s,~s" mid-left (- top corner-radius))))

(define (outline with-port?)
  `(polygon ((points ,(string-join (let ((noport (outline-points '() 90 0)))
                                     (if with-port?
                                         (append noport port-curve)
                                         noport)))))))

;;; switches

(define column-offsets `(8 5 0 6 11 ,(+ 8 switch-spacing switch-spacing)))

(define (switch row col)
  (let* ([x (* (+ 1 col) switch-spacing)]
         [y (+ (list-ref column-offsets col) (* switch-spacing row))])
    `(rect ((height ,(number->string switch-height))
            (width ,(number->string switch-width))
            (x ,(number->string x))
            (y ,(number->string y))))))

(define hand-height (+ (* switch-spacing rows) (- switch-spacing switch-height)
                       (list-ref column-offsets 0)))
(define switch-x-offset -6.5)
(define switch-y-offset (- bottom hand-height -3.5))

(define switches
  `(g ((transform ,(format "translate(~s, ~s) rotate(~s, ~s, ~s)"
                           switch-x-offset switch-y-offset
                           (radians->degrees angle)
                           0 hand-height)))
      ,@(for/list ([col (in-range cols)]
                   #:when true
                   [row (if (= 5 col) '(0 1) (in-range rows))])
          (switch row col))))

(define switches-right
  `(g ((transform ,(format "translate(~s,~s) scale(-1, 1)" width 0)))
      ,switches))

(define logo-doc (call-with-input-file "logo-fragment.svg" read-xml))

(define pcb-doc (call-with-input-file "pcb-fragment.svg" read-xml))

(define (layer plate)
  (document (prolog '() false '())
            (xexpr->xml
             `(svg ((xmlns:svg "http://www.w3.org/2000/svg")
                    (height ,(number->string (* height scale)))
                    (width ,(number->string (* width scale))))
                   ,@(if (eq? plate 'switch)
                         `((g ((transform ,(format "translate(436, ~s)"
                                                   (+ (* scale upper-height) 115)))
                               (stroke "red"))
                              ,(xml->xexpr (document-element logo-doc))))
                         '())
                   ,@(if (eq? plate 'spacer)
                         (list (xml->xexpr (document-element pcb-doc)))
                         (list))
                   (g ((transform ,(format "scale(~s, ~s) translate(0, ~s)"
                                           scale scale upper-height))
                       (stroke-width "1")
                       (stroke "black")
                       (fill-opacity "0"))
                      ,screws
                      ,(outline (not (eq? plate 'switch)))
                      ,@(if (eq? plate 'switch)
                            (list switches switches-right)
                            (list)))))
            '()))

;; to laser cut these, you have to open them in inkscape, then save,
;; and then upload; for some reason the glowforge crashes if you try to cut
;; them directly.

(define (write-out-layer layer-name)
  (call-with-output-file (format "deck-~a.svg" (symbol->string layer-name))
    (lambda (out)
      (display "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" out)
      (display-xml (layer layer-name) out))
    #:exists 'replace))

;; live-reload with:
;; qiv --watch deck-switch.svg

(write-out-layer 'switch)
;; (write-out-layer 'bottom)
;; (write-out-layer 'spacer)
