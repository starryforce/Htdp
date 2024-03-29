#lang htdp/bsl

(define-struct pair [left right])
; A ConsPair is a structure:
;   (make-pair Any Any).

; A ConsOrEmpty is one of:
; – '()
; – (make-pair Any ConsOrEmpty)
; interpretation ConsOrEmpty is the class of all lists

; Any Any -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "cons: second argument ...")]))


; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-left a-list)))

; ConsOrEmpty -> Any
; extracts the rest part of the given pair
(define (our-rest a-list)
  (if (empty? a-list)
      (error 'our-rest "...")
      (pair-right a-list)))