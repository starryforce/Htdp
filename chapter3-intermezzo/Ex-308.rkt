#lang htdp/isl+

(require 2htdp/abstraction)

(define-struct phone [area switch four])
; A Phone is a structure:
; (make-phone Number Number Number)
; interpretation (make-phone a s f) represents a phone record


(define ex (list (make-phone 713 664 9993) (make-phone 233 664 9993)))

; [List-of Phone] -> [List-of Phone]
; substitutes the area code 713 with 281 in alop
(define (replace alop)
  (for/list ([p alop])
    (match p
      [(phone 713 s f) (make-phone 281 s f)]
      [x x])))


(check-expect (replace ex)
              (list (make-phone 281 664 9993) (make-phone 233 664 9993)))