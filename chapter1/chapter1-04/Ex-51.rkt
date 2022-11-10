#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define LIGHT-RADIUS 500)
(define INTERVAL 5)

; A TrafficLight is one of the following string:
; - "red"
; - "green"
; - "yellow"
; interpretation the three strings represent the
; three possible states that a traffic light may assume


; TrafficLight -> Image
; generate an circle which color is tl
(define (render tl) (circle LIGHT-RADIUS "solid" tl))

; TrafficLight -> TrafficLight
; follow the rule to get next state of TrafficLight
; according current state tl
(define (tock tl) (traffic-light-next tl))

(check-expect (tock "red") "green")
(check-expect (tock "green") "yellow")
(check-expect (tock "yellow") "red")

; TrafficLight -> TrafficLight
; yields the next state given current state s

(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; TrafficLight -> TrafficLight
; start width a state of tl
(define (main tl)
  (big-bang tl
      [to-draw render]
      [on-tick tock INTERVAL]))

(main "red")