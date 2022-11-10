#lang htdp/bsl

; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name


(cons "a"
      (cons "b"
            (cons "c"
                  (cons "d"
                        (cons "e" '())))))


; '() is List-of-names, "2" is a string
; so (cons "2" '()) is also a List-of-names, "1" is a string
; (cons "1" (cons "2" '())) is List-of-names as well

; 2 is not string, it's a number