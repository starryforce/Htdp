#lang htdp/bsl+

(require 2htdp/universe)
(require 2htdp/image)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)


 
; FSM-State is a String.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; ExpectsToSee.v2 is one of:
; – AA
; – BB
; – DD 
; – ER

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

(define fsm-pattern
  (list (make-ktransition AA "a" BB)
        (make-ktransition BB "b" BB)
        (make-ktransition BB "c" BB)
        (make-ktransition BB "d" DD)))

(define-struct fs [fsm current])
; A SimulationState.v3 is a structure: 
;   (make-fs FSM FSM-State)

; FSM FSM-State -> SimulationState.v3 
; match the keys pressed with the given FSM 
(define (simulate.v3 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-input-tip]
    [on-key find-next-state]))

; SimulationState.v2 -> Image 
; renders current world state as a tip text.
 
(check-expect (state-as-input-tip
                (make-fs fsm-pattern AA))
              (text "start, expect an 'a'" 16 "black"))
 
(define (state-as-input-tip an-fsm)
  (text (fs-current an-fsm) 16 "black"))

(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm) ke)))

(check-expect
  (find-next-state (make-fs fsm-pattern AA) "a")
  (make-fs fsm-pattern BB))
(check-expect
  (find-next-state (make-fs fsm-pattern BB) "b")
  (make-fs fsm-pattern BB))
(check-expect
  (find-next-state (make-fs fsm-pattern BB) "c")
  (make-fs fsm-pattern BB))
(check-expect
  (find-next-state (make-fs fsm-pattern BB) "d")
  (make-fs fsm-pattern DD))

; FSM-State FSM-State -> Boolean
; check if s1 s2 are same
(define (state=? s1 s2) (string=? s1 s2))

(check-expect (state=? "red" "red") #true)
(check-expect (state=? "red" "green") #false)


; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(check-expect (find fsm-pattern AA "a") BB)
(check-expect (find fsm-pattern AA "c") ER)
(check-expect (find fsm-pattern BB "b") BB)
(check-expect (find fsm-pattern BB "c") BB)
(check-expect (find fsm-pattern BB "d") DD)
(check-expect (find fsm-pattern BB "e") ER)

(define (find transitions current ke)
  (cond [(empty? transitions) ER]
        [else (if (and (state=? (ktransition-current (first transitions)) current)
                       (string=? (ktransition-key (first transitions)) ke))
                  (ktransition-next (first transitions))
                  (find (rest transitions) current ke))]))