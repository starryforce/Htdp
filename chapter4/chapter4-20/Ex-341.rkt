#lang htdp/isl+

(require htdp/dir)

(define EX0 (make-dir ".vscode" '()
                      (list
                       (make-file "launch.json" 435 0 ""))))
(define EX (make-dir "algorithm"
                     (list EX0)
                     (list (make-file ".editorconfig" 227 0 "")
                           (make-file ".prettierrc.js" 230 0 "")
                           (make-file "app.js" 4415 0 "")
                           (make-file "canvas.html" 914 0 ""))))


; Dir -> Number
; computes the total size of all the files in d
(define (du d)
  (+ 1 
     (du-dirs (dir-dirs d))
     (du-files (dir-files d))))

(check-expect (du EX0) 436)
(check-expect (du EX) 6223)

; [List-of Dir] -> Number
; computes the total size of all the files in dirs
(define (du-dirs dirs)
  (cond [(empty? dirs) 0]
        [else (+ (du (first dirs)) 
                 (du-dirs (rest dirs)) )]))

(check-expect (du-dirs (dir-dirs EX)) 436)


; [List-of File] -> Number
; computes the total size of all the files in files
(define (du-files files)
  (cond [(empty? files) 0]
        [else (+ (file-size (first files))
                 (du-files (rest files)))]))

(check-expect (du-files (dir-files EX)) 5786)