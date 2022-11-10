#lang htdp/bsl

(define base-people 120)
(define base-price 5.0)
(define base-effect 15)
(define base-effect-coefficient 0.1)
(define fixed-cost 180)
(define cost-person 0.04)


(define (attendees ticket-price)
  (- base-people (* (- ticket-price base-price) (/ base-effect base-effect-coefficient))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ fixed-cost (* cost-person (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
