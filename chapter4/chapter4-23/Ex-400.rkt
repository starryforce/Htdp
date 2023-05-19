#lang htdp/isl+

#| Exercise 400.
Design the function DNAprefix.
The function takes two arguments, both lists of 'a, 'c, 'g, and 't, symbols that occur in DNA descriptions.
The first list is called a pattern, the second one a search string.
The function returns #true if the pattern is identical to the initial part of the search string;
otherwise it returns #false.
|#

; A DNADesc is one of:
; - 'a
; - 'c
; - 'g
; - 't

; [List-of DNADesc] [List-of DNADesc] -> Boolean
; determine if the pattern is identical to the initial part of searchstr
(define (DNAprefix pattern searchstr)
  (cond [(empty? pattern) #t]
        [(empty? searchstr) #f]
        [else
         (and (symbol=? (first pattern) (first searchstr))
              (DNAprefix (rest pattern) (rest searchstr)))]))

(check-expect (DNAprefix '() '()) #t)
(check-expect (DNAprefix '() '(a)) #t)
(check-expect (DNAprefix '(a) '()) #f)
(check-expect (DNAprefix '(a) '(a c)) #t)
(check-expect (DNAprefix '(a) '(t a)) #f)
(check-expect (DNAprefix '(c g) '(c g)) #t)
(check-expect (DNAprefix '(c g) '(a t)) #f)
(check-expect (DNAprefix '(a c g) '(t)) #f)

#|
Also design DNAdelta.
This function is like DNAprefix but returns the first item in the search string beyond the pattern.
If the lists are identical and there is no DNA letter beyond the pattern, the function signals an error.
If the pattern does not match the beginning of the search string, it returns #false.
The function must not traverse either of the lists more than once.

Can DNAprefix or DNAdelta be simplified?
|#

(define ERR "no DNA letter")

; [List-of DNADesc] [List-of DNADesc] -> [Maybe DNADesc]
; returns the first item in searchstr beyond the pattern
; If the lists are identical and there is no DNA letter beyond the pattern, the function signals an error
(define (DNAdelta pattern searchstr)
  (cond [(and (empty? pattern) (empty? searchstr)) (error ERR)]
        [(and (empty? pattern) (cons? searchstr)) (first searchstr)]
        [(and (cons? pattern) (empty? searchstr)) #f]
        [(and (cons? pattern) (cons? searchstr)) (if (symbol=? (first searchstr)  (first pattern))
                                                     (DNAdelta (rest pattern) (rest searchstr))
                                                     #f)]))

(check-error (DNAdelta '() '()) ERR)
(check-expect (DNAdelta '() '(a)) 'a)
(check-expect (DNAdelta '(a) '()) #f)
(check-expect (DNAdelta '(a) '(a c)) 'c)
(check-expect (DNAdelta '(a) '(t c)) #f)
(check-error (DNAdelta '(c g) '(c g)) ERR)
(check-expect (DNAdelta '(c g) '(c t)) #f)
(check-expect (DNAdelta '(a c g) '(a c)) #f)

; DNAprefix can be simplified