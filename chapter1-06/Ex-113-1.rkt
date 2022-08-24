#lang htdp/bsl

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

(define (checked-make-aim ufo tank)
  (cond [(and
          (posn? ufo)
          (tank? tank))
         (make-aim ufo tank)]
        [else (error "ufo should be UFO,tank should be Tank")]))

(define (checked-make-fired ufo tank missile)
  (cond [(and (posn? ufo)
              (tank? tank)
              (posn? missile))
         (make-fired ufo tank missile)]
        [else (error "ufo should be Posn, tank should be Tank, missile should be Posn")]))

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; Any -> Boolean
; is v an element of the SIGS collection
(define (SIGS? v) (cond [(and (aim? v) (posn? (aim-ufo v)) (tank? (aim-tank v))) #true]
                        [(and (fired? v) (posn? (fired-ufo v)) (tank? (fired-tank v)) (posn? (fired-missile v))) #true]
                        [else #false]))

(check-expect (SIGS? #false) #false)
(check-expect (SIGS? 1) #false)
(check-expect (SIGS? (make-aim (make-posn 1 4) (make-tank 5 4))) #true)
(check-expect (SIGS? (make-fired 1 2 4)) #false)
(check-expect (SIGS? (make-fired (make-posn 1 4) (make-tank 5 4) (make-posn 1 4))) #true)