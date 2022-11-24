#lang htdp/isl+

(require htdp/dir)

; A Path is [List-of String].
; interpretation directions into a directory tree
(define path1 '("algorithm" ".vscode" "launch.json"))

; File String -> Boolean
; determine if file's name is n
(define (file-match? file n)
  (string=? (file-name file) n))


(define EX0 (make-dir ".vscode" '()
                      (list
                       (make-file "launch.json" 435 0 ""))))
(define EX (make-dir "algorithm"
                     (list EX0)
                     (list (make-file ".editorconfig" 227 0 "")
                           (make-file ".prettierrc.js" 230 0 "")
                           (make-file "app.js" 4415 0 "")
                           (make-file "canvas.html" 914 0 ""))))

; Dir String -> [Maybe Path]
; produces a path to a file with name f in d
; otherwise it produces #false.
(define (find d f)
  ; both return #f, means not find, so return #f
  (if (and (false? (find-dirs (dir-dirs d) f))
           (false? (find-files (dir-files d) f)))
      #false
      ; one of the following have result
      (if (false? (find-files (dir-files d) f))
          (cons (dir-name d) (find-dirs (dir-dirs d) f))
          (list (dir-name d) (find-files (dir-files d) f)))))

(check-expect (find EX "launch.json") path1)
(check-expect (find EX "dark.js") #false)


; [List-of Dir] -> [Maybe Path]
; produces a path to a file with name f in d
; otherwise it produces #false.
(define (find-dirs dirs f)
  (cond [(empty? dirs) #false]
        [else (if (false? (find (first dirs) f))
                  (find-dirs (rest dirs) f)
                  (find (first dirs) f))]))

(check-expect (find-dirs (dir-dirs EX) "launch.json") '(".vscode" "launch.json"))
(check-expect (find-dirs (dir-dirs EX) "dark.js") #false)


; [List-of File] -> [Maybe String]
; produces a path to a file with name f in d
; otherwise it produces #false.
(define (find-files files f)
  (cond [(empty? files) #false]
        [else (if (file-match? (first files) f)
                  (file-name (first files))
                  (find-files (rest files) f) )]))

(check-expect (find-files (dir-files EX) "app.js") "app.js")
(check-expect (find-files (dir-files EX) "opq.js") #false)


