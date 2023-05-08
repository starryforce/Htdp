#lang htdp/isl+

#| Exercise 382.
Formulate an XML configuration for the BW machine,
which switches from white to black and back for every key event.
Translate the XML configuration into an XMachine representation.
See exercise 227 for an implementation of the machine as a program.
|#

; <machine initial="black">
;   <action state="black" next="white" />
;   <action state="white" next="black" />
; </machine>

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State))  [List-of X1T]))
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

(define fsm-bw (cons 'machine
                     (cons '((initial "black"))
                           (list '(action ((state "black") (next "white")))
                                 '(action ((state "white") (next "black")))))))