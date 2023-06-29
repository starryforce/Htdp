#lang htdp/isl+

; Exercise 476

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
(check-expect (fsm-match? fsm-a-bc*-d "acbd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #t)
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "da") #f)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #f)
(check-expect (fsm-match? fsm-a-bc*-d "d") #f)
(define (fsm-match? an-fsm a-string)
  (local ((define initial (fsm-initial an-fsm))
          (define final (fsm-final an-fsm))
          (define transitions (fsm-transitions an-fsm))
          (define l (explode a-string))
          
          ; FSM-State [List-of 1String] -> Boolean
          ; state represents the current state of the finite state machine
          ; also represents the remaining list of 1Strings
          (define (match? state alos)
            (cond [(empty? alos) (string=? state "DD")]
                  [else (local ((define candidate (search-t (first alos) transitions)))
                          (cond [(boolean? candidate) #false]
                                [(string=? (transition-current candidate) state)
                                 (match? (transition-next candidate) (rest alos))]
                                [else #false]))]))

          ; [List-of 1Transition] -> [Maybe 1Transition]
          ; find the transition related to key in fsm
          (define (search-t key trans)
            (cond [(empty? trans) #false]
                  [else (if (string=? (transition-key (first trans)) key)
                            (first trans)
                            (search-t key (rest trans)))])))
    (match? initial l)))

