#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume


; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(define (tl-next cs)
  (cond
    [(string=? "red" cs) "green"]
    [(string=? "green" cs) "yellow"]
    [(string=? "yellow" cs) "red"]))

(check-expect (tl-next "red") "green")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")

 
; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (place-image (bulb "red" (bulb-style "red" current-state)) (* (/ 90 4) 2) (/ 30 2)
             (place-image (bulb "yellow" (bulb-style "yellow" current-state)) (* (/ 90 4) 1) (/ 30 2)
                          (place-image (bulb "green" (bulb-style "green" current-state)) (* (/ 90 4) 3) (/ 30 2)
                                       (empty-scene 90 30)))))

(check-expect (tl-render "red") (place-image (bulb "red" (bulb-style "red" "red")) (* (/ 90 4) 2) (/ 30 2)
             (place-image (bulb "yellow" (bulb-style "yellow" "red")) (* (/ 90 4) 1) (/ 30 2)
                          (place-image (bulb "green" (bulb-style "green" "red")) (* (/ 90 4) 3) (/ 30 2)
                                       (empty-scene 90 30)))))
(check-expect (tl-render "yellow") (place-image (bulb "red" (bulb-style "red" "yellow")) (* (/ 90 4) 2) (/ 30 2)
             (place-image (bulb "yellow" (bulb-style "yellow" "yellow")) (* (/ 90 4) 1) (/ 30 2)
                          (place-image (bulb "green" (bulb-style "green" "yellow")) (* (/ 90 4) 3) (/ 30 2)
                                       (empty-scene 90 30)))))

(check-expect (tl-render "green") (place-image (bulb "red" (bulb-style "red" "green")) (* (/ 90 4) 2) (/ 30 2)
             (place-image (bulb "yellow" (bulb-style "yellow" "green")) (* (/ 90 4) 1) (/ 30 2)
                          (place-image (bulb "green" (bulb-style "green" "green")) (* (/ 90 4) 3) (/ 30 2)
                                       (empty-scene 90 30)))))

; TrafficLight Style -> Image
; render a single bulb which color is tl,
; Style:
; - solid
; - outline
(define (bulb tl s)
  (circle 8 s tl))

(check-expect (bulb "red" "solid") (circle 8 "solid" "red"))
(check-expect (bulb "green" "outline") (circle 8 "outline" "green"))

; TrafficLight TrafficLight -> Style
; according to the current state of TrafficLight,
; decide if the bulb is "solid" or "outline"
; to represent if the bulb is turn on
(define (bulb-style current state)
  (if (string=? current state) "solid" "outline"))


; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))


