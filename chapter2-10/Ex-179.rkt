#lang htdp/bsl

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; Editor 1String -> Editor
; insert the 1String k between pre and post
(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
 
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))


; Editor -> Editor
; moves the cursor position one 1String left, 
; if possible 
(define (editor-lft ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))
                   (cons (first (editor-pre ed)) (editor-post ed)))))

(check-expect (editor-lft (make-editor '() '())) (make-editor '() '()))
(check-expect (editor-lft (make-editor (cons "a" '()) '())) (make-editor '() (cons "a" '())))
(check-expect (editor-lft (make-editor (cons "a" (cons "b" '())) (cons "c" (cons "d" '()))))
              (make-editor (cons "b" '()) (cons "a" (cons "c" (cons "d" '())))))


; Editor -> Editor
; moves the cursor position one 1String right, 
; if possible 
(define (editor-rgt ed)
  (if (empty? (editor-post ed))
      ed
      (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                   (rest (editor-post ed)))))

(check-expect (editor-rgt (make-editor '() '()))
              (make-editor '() '()))
(check-expect (editor-rgt (make-editor (cons "c" (cons "b" (cons "a" '()))) '()))
              (make-editor (cons "c" (cons "b" (cons "a" '()))) '()))
(check-expect (editor-rgt (make-editor '() (cons "c" (cons "b" (cons "a" '())))))
              (make-editor (cons "c" '()) (cons "b" (cons "a"  '()))))
(check-expect (editor-rgt (make-editor (cons "c" (cons "b" (cons "a" '()))) (cons "d" (cons "e" (cons "f" '())))))
              (make-editor (cons "d" (cons "c" (cons "b" (cons "a" '())))) (cons "e" (cons "f" '()))))
                                               

; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible 
(define (editor-del ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))
                   (editor-post ed))))


(check-expect (editor-del (make-editor '() '()))
              (make-editor '() '()))
(check-expect (editor-del (make-editor (cons "a" (cons "b" (cons "c" '()))) '()))
              (make-editor (cons "b" (cons "c" '())) '()))
(check-expect (editor-del (make-editor '() (cons "c" (cons "b" (cons "a" '())))))
              (make-editor '() (cons "c" (cons "b" (cons "a" '())))))
(check-expect (editor-del (make-editor (cons "a" (cons "b" '())) (cons "c" (cons "d" '()))))
              (make-editor (cons "b" '()) (cons "c" (cons "d" '()))))

