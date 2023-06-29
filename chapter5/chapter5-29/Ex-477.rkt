#lang htdp/isl+

; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
      (foldr (lambda (item others)
               (local ((define without-item
                         (arrangements (remove item w)))
                       (define add-item-to-front
                         (map (lambda (a) (cons item a))
                              without-item)))
                 (append add-item-to-front others)))
        '()
        w)]))
 
; [List-of [List-of 1String]] -> Boolean
; are the words "rat", "art", and "tar" members of the given list?
(define (all-words-from-rat? w)
  (and (member (explode "rat") w)
       (member (explode "art") w)
       (member (explode "tar") w)))
 
(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)

; 1. when w is an empty list
; 2. the answer is '(())
; 3. the smaller question is arrangements that doesn't contain
; the item that foldr now processing.
; 4. append current item in front of all other arrangements
; generate all arrangements that starts with current item.
; join every list starts with each item in w generates the
; final result.
; termination: in every recursion, the input of the arragements is
; one size smaller than the past. so it will always reduce to an
; empty list.

; the contents are same, but the order is not guarenteed.