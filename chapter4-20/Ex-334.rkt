#lang htdp/isl+

(define-struct dir [name content size readability])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD Number Boolean)
 
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