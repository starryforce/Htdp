#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

; A Path is [List-of String].
; interpretation directions into a directory tree

; File String -> Boolean
; determine if file's name is n
(define (file-match? file n)
  (string=? (file-name file) n))


(define part1 (make-file "part1" 99 0 ""))
(define part2 (make-file "part2" 52 0 ""))
(define part3 (make-file "part3" 17 0 ""))
(define hang (make-file "hang" 8 0 ""))
(define draw (make-file "draw" 2 0 ""))
(define read1 (make-file "read!" 19 0 ""))
(define read2 (make-file "read!" 10 0 ""))

(define Text (make-dir "Text" '() (list part1 part2 part3)))
(define Code (make-dir "Code" '() (list hang draw)))
(define Docs (make-dir "Docs" '() (list read1)))
(define Libs (make-dir "Libs" (list Code Docs) '()))
(define TS (make-dir "TS" (list Text Libs) (list read2)))



; Dir String -> [List-of Path]
; produces a path to a file with name f in d
; otherwise it produces #false.
(define (find d f)
  (map (lambda (p) (cons (dir-name d) p))
       (append (find-files (dir-files d) f)
               (find-dirs (dir-dirs d) f))))
          

(check-expect (find TS "read!") '(("TS" "read!") ("TS" "Libs" "Docs" "read!")))
(check-expect (find TS "draw") '(("TS" "Libs" "Code" "draw")))
(check-expect (find TS "dark.js") '())


; [List-of Dir] String -> [List-of [Maybe Path]]
; produces a list of items
; if exist: the item is a path to a file with name f in dirs
; otherwise it is #false.

(define (find-dirs dirs f)
  (foldl append
         '()
         (map (lambda (dir) (find dir f))
              (filter (lambda (dir) (not (empty? (find dir f)))) dirs))))

(check-expect (find-dirs (dir-dirs TS) "hang") '(("Libs" "Code" "hang")))
(check-expect (find-dirs (dir-dirs TS) "noop") '())


; [List-of File] String -> [List-of [Maybe Path]]
; produce f if file with name f exist in files
; otherwise produce '()
(define (find-files files f)
  (map (lambda (file) (list (file-name file)))
       (filter (lambda (file) (file-match? file f)) files)))

(check-expect (find-files (dir-files Docs) "read!") '(("read!")))
(check-expect (find-files (dir-files TS) "read!") '(("read!")))
(check-expect (find-files (dir-files Code) "draw") '(("draw")))
(check-expect (find-files (dir-files Code) "part1") '())


