#lang htdp/isl+

(require htdp/dir)


; A Path is [List-of String].
; interpretation directions into a directory tree


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

; Dir -> [List-of Path]
; lists the paths to all files contained in d
(define (ls-R d)
  (map (lambda (p) (cons (dir-name d) p))
       (append (ls-dirs (dir-dirs d))
               (ls-files (dir-files d)))))

(check-expect (ls-R TS) '(("TS" "Text" "part1")
                          ("TS" "Text" "part2")
                          ("TS" "Text" "part3")
                          ("TS" "Libs" "Code" "hang")
                          ("TS" "Libs" "Code" "draw")
                          ("TS" "Libs" "Docs" "read!")
                          ("TS" "read!")))

; [List-of Dir] -> [List-of Path]
; lists the paths to all files contained in dirs
(define (ls-dirs dirs)
  (foldl (lambda (cur prev)
           (append prev (ls-R cur)))
         '()
         dirs))

(check-expect (ls-dirs (list Text)) '(("Text" "part1")
                                      ("Text" "part2")
                                      ("Text" "part3")))
(check-expect (ls-dirs (dir-dirs Libs)) '(("Code" "hang")
                                          ("Code" "draw")
                                          ("Docs" "read!")))

; [List-of File] -> [List-of Path]
; lists the paths to all files contained in dirs
(define (ls-files files)
  (map (lambda (file) (list (file-name file))) files))

(check-expect (ls-files (dir-files Text)) '(("part1") ("part2") ("part3")))


; Dir String -> [List-of Path]
; produces a paths to a file with name f in d
(define (find-all d f)
  (filter (lambda (p) (string=? f (first (reverse p)))) (ls-R d)))


(check-expect (find-all TS "read!") '(("TS" "Libs" "Docs" "read!") ("TS" "read!") ))
(check-expect (find-all TS "draw") '(("TS" "Libs" "Code" "draw")))
(check-expect (find-all TS "dark.js") '())
