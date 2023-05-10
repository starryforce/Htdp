#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)

#| Exercise 380.
Reformulate the data definition for 1Transition
so that it is possible to restrict transitions to certain keystrokes.
Try to formulate the change so that find continues to work without change.
What else do you need to change to get the complete program to work?
Which part of the design recipe provides the answer(s)?
See exercise 229 for the original exercise statement. 
|#

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of three items:
;   (cons FSM-State (cons FSM-State (cons KeyEvent '())))
; An FSM-State is a String that specifies a color
 
; data examples 
(define fsm-traffic
  '(("red" "green" "g") ("green" "yellow" "y" ) ("yellow" "red" "r")))
 
; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay (text current 24 "black")
                 (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (local ((define trigger (query-trigger transitions current)))
          (if (string=? trigger key-event)
              (find transitions current)
              current)))]))

; [X Y KeyEvent] [List-of [List X Y KeyEvent] -> KeyEvent
; finds the matching KeyEvent for the given X in alist
(define (query-trigger alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (third fm) (error "not found"))))

(check-expect (query-trigger fsm-traffic "red") "g")
(check-expect (query-trigger fsm-traffic "green") "y")
(check-expect (query-trigger fsm-traffic "yellow") "r")
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")