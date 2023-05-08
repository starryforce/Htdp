#lang htdp/isl+

(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)


(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)


(define part1 (make-file "part1" 99 ""))
(define part2 (make-file "part2" 52 ""))
(define part3 (make-file "part3" 17 ""))
(define hang (make-file "hang" 8 ""))
(define draw (make-file "draw" 2 ""))
(define read1 (make-file "read!" 19 ""))
(define read2 (make-file "read!" 10 ""))

(define Text (make-dir.v3 "Text" '() (cons part1 (cons part2 (cons part3 '())))))
(define Code (make-dir.v3 "Code" '() (cons hang (cons draw '()))))
(define Docs (make-dir.v3 "Docs" '() (cons read1 '())))
(define Libs (make-dir.v3 "Libs" (cons Code (cons Docs '())) '()))
(define TS (make-dir.v3 "TS" (cons Text (cons Libs '())) (cons read2 '())))