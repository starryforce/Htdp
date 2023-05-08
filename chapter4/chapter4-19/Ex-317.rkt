#lang htdp/isl+

; Any -> Boolean
; determine if a value is an Atom
; An Atom is one of: 
; – Number
; – String
; – Symbol 
(define (atom? s)
  (or (string? s)
      (number? s)
      (symbol? s)))

(check-expect (atom? "1") #t)
(check-expect (atom? 1) #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? (list 1)) #false)


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (main sexp sy)
  (local (; S-expr Symbol -> N 
          ; counts all occurrences of sy in sexp 
          (define (count sexp)
            (cond
              [(atom? sexp) (count-atom sexp)]
              [else (count-sl sexp)]))
 
          ; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl)) (count-sl (rest sl)))]))
 
          ; Atom Symbol -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (count sexp)))



(check-expect (main 'world 'hello) 0)
(check-expect (main '(world hello) 'hello) 1)
(check-expect (main '(((world) hello) hello) 'hello) 2)
