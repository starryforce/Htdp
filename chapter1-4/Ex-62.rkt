#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")
(define DELAY 3)
   

; A DoorState is one of:
; – LOCKED
; – CLOSED
; – OPEN

; DoorState -> DoorState
; if the DoorState is OPEN,
; change it to CLOSED during a tick
(define (door-closer ds)
  (cond
    [(string=? LOCKED ds) LOCKED]
    [(string=? CLOSED ds) CLOSED]
    [(string=? OPEN ds) CLOSED]
    ))

; given state    desired state
; LOCKED         LOCKED
; CLOSED         CLOSED
; OPEN           CLOSED
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)

; DoorState KeyEvent -> DoorState
; turns key event k into an action on state s
(define (door-action ds key) (cond
                               [(and (string=? LOCKED ds) (string=? "u" key)) CLOSED]
                               [(and (string=? CLOSED ds) (string=? " " key)) OPEN]
                               [(and (string=? CLOSED ds) (string=? "l" key)) LOCKED]
                               [else ds]))

; given state    given key event    desired state
; LOCKED         "u"                CLOSED
; CLOSED         " "                OPEN
; CLOSED         "l"                LOCKED
; OPEN           --                 OPEN
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)

; DoorState -> Image
; translates the state s into a large text image
(check-expect (door-render CLOSED)
              (text CLOSED 40 "red"))
(define (door-render s)
  (text s 40 "red"))

; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
    [on-tick door-closer DELAY]
    [on-key door-action]
    [to-draw door-render]))

(door-simulation LOCKED)