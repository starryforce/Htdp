#lang htdp/bsl+

(require 2htdp/universe)
(require 2htdp/image)

; ExpectsToSee is one of:
; – AA
; – BB
; – DD 
; – ER

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)

(define lot (list (make-transition AA "a" BB)
                  (make-transition BB "b" BB)
                  (make-transition BB "c" BB)
                  (make-transition BB "d" DD)))

(define fsm-pattern (make-fsm AA lot DD))
(define fsm-pattern1 (make-fsm BB lot DD))
(define fsm-pattern2 (make-fsm DD lot DD))
(define fsm-pattern3 (make-fsm ER lot DD))

; FSM.v2 -> Image
; renders current world state as a text
(define (render-state-as-text an-fsm)
  (text (fsm-initial an-fsm)  16 "black"))

(check-expect (render-state-as-text fsm-pattern) (text "start, expect an 'a'" 16 "black"))

; FSM.v2 -> FSM.v2
; ; finds the next state from an-fsm and ke
(define (find-next-state an-fsm ke)
  (make-fsm (find (fsm-transitions an-fsm)
                  (fsm-initial an-fsm)
                  ke)
            (fsm-transitions an-fsm)
            (fsm-final an-fsm)))

(check-expect (find-next-state fsm-pattern "a") fsm-pattern1)
(check-expect (find-next-state fsm-pattern "q") fsm-pattern3)
(check-expect (find-next-state fsm-pattern1 "b") fsm-pattern1)
(check-expect (find-next-state fsm-pattern1 "c") fsm-pattern1)
(check-expect (find-next-state fsm-pattern1 "d") fsm-pattern2)
(check-expect (find-next-state fsm-pattern1 "q") fsm-pattern3)

; FSM.v2 FSM-State ke -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(define (find transitions current ke)
  (cond [(empty? transitions) ER]
        [else (if (and (string=? (transition-current (first transitions)) current)
                       (string=? (transition-key (first transitions)) ke))
                  (transition-next (first transitions))
                  (find (rest transitions) current ke))]))

(check-expect (find lot AA "a") BB)
(check-expect (find lot AA "q") ER)
(check-expect (find lot BB "b") BB)
(check-expect (find lot BB "c") BB)
(check-expect (find lot BB "d") DD)
(check-expect (find lot BB "q") ER)

; FSM.v2 -> Boolean
(define (terminate an-fsm)
  (cond [(string=? (fsm-initial an-fsm) DD) #true]
        [(string=? (fsm-initial an-fsm) ER) #true]
        [else #false]))

(check-expect (terminate fsm-pattern) #false)
(check-expect (terminate fsm-pattern1) #false)
(check-expect (terminate fsm-pattern2) #true)
(check-expect (terminate fsm-pattern3) #true)

(define (fsm-simulate init)
  (big-bang init
    [on-draw render-state-as-text]
    [on-key find-next-state]
    [stop-when terminate render-state-as-text]))