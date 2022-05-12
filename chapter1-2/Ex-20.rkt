#lang htdp/bsl

(define (string-delete str i)
  (if (> (string-length str) 0)
  (string-append (substring str 0 i) (substring str (+ i 1)))
  str))

(string-delete "abc" 2)
(string-delete "" 0)