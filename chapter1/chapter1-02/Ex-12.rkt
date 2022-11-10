#lang htdp/bsl

(define (cvolume side-length)
  (* side-length side-length side-length))

(define (csurface side-length)
  (* side-length side-length 6))

(cvolume 10)
(csurface 6)