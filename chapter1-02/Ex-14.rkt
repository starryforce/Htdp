#lang htdp/bsl

(define (string-last str)
  (string-ith str
              (- (string-length str) 1)))

(string-last "hello")