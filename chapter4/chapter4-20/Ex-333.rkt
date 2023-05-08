#lang htdp/isl+

(define-struct dir [name content])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

(define Text (make-dir "Text" (cons "part1" (cons "part2" (cons "part3" '())))))
(define Code (make-dir "Code" (cons "hang" (cons "draw" '()))))
(define Docs (make-dir "Docs" (cons "read!" '())))
(define Libs (make-dir "Libs" (cons Code (cons Docs '()))))
(define TS (make-dir "TS" (cons Text (cons "read!" (cons Libs '())))))


; Dir.v2 -> Number
; determines how many files does d contains
(define (how-many d)
  (how-many-lofd (dir-content d)))

(check-expect (how-many Text) 3)
(check-expect (how-many Code) 2)
(check-expect (how-many Docs) 1)
(check-expect (how-many Libs) 3)
(check-expect (how-many TS) 7)

; LOFD
(define (how-many-lofd alofd)
  (cond [(empty? alofd) 0]
        [else (+ (cond [(string? (first alofd)) 1]
                       [else (how-many (first alofd))])
                 (how-many-lofd (rest alofd)))]))

(check-expect (how-many-lofd (dir-content Text)) 3)
(check-expect (how-many-lofd (dir-content Code)) 2)
(check-expect (how-many-lofd (dir-content Docs)) 1)
(check-expect (how-many-lofd (dir-content Libs)) 3)
(check-expect (how-many-lofd (dir-content TS)) 7)

