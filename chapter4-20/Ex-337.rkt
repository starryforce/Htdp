#lang htdp/isl+

(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)


(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String [List-of Dir.v3] [List-of File.v3])


(define part1 (make-file "part1" 99 ""))
(define part2 (make-file "part2" 52 ""))
(define part3 (make-file "part3" 17 ""))
(define hang (make-file "hang" 8 ""))
(define draw (make-file "draw" 2 ""))
(define read1 (make-file "read!" 19 ""))
(define read2 (make-file "read!" 10 ""))

(define Text (make-dir.v3 "Text" '() (list part1 part2 part3)))
(define Code (make-dir.v3 "Code" '() (list hang draw)))
(define Docs (make-dir.v3 "Docs" '() (list read1)))
(define Libs (make-dir.v3 "Libs" (list Code Docs) '()))
(define TS (make-dir.v3 "TS" (list Text Libs) (list read2)))




; [List-of File.v3] -> Number
; count the file's quantity
(define (how-many-files fl) (length fl))

(check-expect (how-many-files (dir.v3-files Text)) 3)
(check-expect (how-many-files (dir.v3-files Code)) 2)
(check-expect (how-many-files (dir.v3-files Docs)) 1)
(check-expect (how-many-files (dir.v3-files Libs)) 0)
(check-expect (how-many-files (dir.v3-files TS)) 1)


; [List-of Dir.v3] -> Number
; determines how many files in dl
(define (how-many-dirs dl)
  (foldl (lambda (cur prev) (+ (how-many cur) prev)) 0 dl))


(check-expect (how-many-dirs (dir.v3-dirs Text)) 0)
(check-expect (how-many-dirs (dir.v3-dirs Code)) 0)
(check-expect (how-many-dirs (dir.v3-dirs Docs)) 0)
(check-expect (how-many-dirs (dir.v3-dirs Libs)) 3)
(check-expect (how-many-dirs (dir.v3-dirs TS)) 6)



; Dir.v3 -> Number
; determines how many files d contains
(define (how-many d)
  (+ (how-many-dirs (dir.v3-dirs d))
     (how-many-files (dir.v3-files d))))

(check-expect (how-many Text) 3)
(check-expect (how-many Code) 2)
(check-expect (how-many Docs) 1)
(check-expect (how-many Libs) 3)
(check-expect (how-many TS) 7)