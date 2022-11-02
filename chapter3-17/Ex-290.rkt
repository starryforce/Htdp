#lang htdp/isl+

(require 2htdp/image)

; [List-of Item] [List-of Item] -> [List-of Item]
; concatenates the items in lx and ly.
(define (append-from-fold lx ly)
  (foldr ; String [List-of Item] -> [List-of Item]
   ; add s into alos
   (lambda (s alos)
     (cons s alos)) ly lx))

(check-expect (append-from-fold (list 1 2) (list 3 4))
              (list 1 2 3 4))

; if we replace foldr with foldl,the first part of the list
; will be in a wrong order

; [List-of Number] -> Number
; calc the sum of items in alon
(define (sum-from-fold alon)
  (foldr + 0 alon))

(check-expect (sum-from-fold (list 1 2 3)) 6)

; [List-of Number] -> Number
; calc the product of items in alon
(define (product-from-fold alon)
  (foldr * 1 alon))

(check-expect (product-from-fold (list 2 3 4)) 24)


(define SAMPLE (square 20 "solid" "red"))
(define ex (list SAMPLE SAMPLE SAMPLE))

; [List-of Image] -> Image
; horizontally composes a list of Images aloi
(define (h-from-fold aloi)
  (foldr beside empty-image aloi))


(check-expect (h-from-fold ex) (beside SAMPLE (beside SAMPLE SAMPLE)))

; [List-of Image] -> Image
; stacks a list of images vertically
(define (v-from-fold aloi)
  (foldr above empty-image aloi))

(check-expect (v-from-fold ex) (above SAMPLE (above SAMPLE SAMPLE)))

