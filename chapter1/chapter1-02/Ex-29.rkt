#lang htdp/bsl

(define base-people 120)
(define base-price 5.0)
(define base-effect 15)
(define base-effect-coefficient 0.1)
(define fixed-cost 0)
(define cost-person 1.5)


(define (attendees ticket-price)
  (- base-people (* (- ticket-price base-price) (/ base-effect base-effect-coefficient))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ fixed-cost (* cost-person (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))


(define (profit-single price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
        (* 1.5
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price))))))

(profit 3)
(profit-single 3)
(profit 4)
(profit-single 4)
(profit 5)
(profit-single 5)
