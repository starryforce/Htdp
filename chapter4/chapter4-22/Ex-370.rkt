#lang htdp/isl+

#| Exercise 370.
Make up three examples for XWords. Design word?,
which checks whether some ISL+ value is in XWord, and word-text,
which extracts the value of the only attribute of an instance of XWord. 
|#

; An XWord is '(word ((text String))).
(define w1 '(word ((text ""))))
(define w2 '(word ((text "nice"))))
(define w3 '(word ((text "dark"))))

; Any -> Boolean
; determine if v is in XWord
(define (word? v)
  (and (cons? v)
       (= (length v) 2)
       (local ((define name (first v))
               (define attrs (second v)))
         (and (equal? name 'word)
              (= (length attrs) 1)
              (local ((define attr (first attrs)))
                (and (= (length attr) 2)
                     (local ((define key (first attr))
                             (define value (second attr)))
                       (and (equal? key 'text)
                            (string? value)))))))))


(check-expect (word? 1) #false)
(check-expect (word? "abc") #false)
(check-expect (word? #false) #false)
(check-expect (word? '()) #false)
(check-expect (word? w1) #true)
(check-expect (word? w2) #true)
(check-expect (word? '("word" ((text "a")))) #false)
(check-expect (word? '(word (("text" "a")))) #false)
(check-expect (word? '(word ((text 1)))) #false)
(check-expect (word? '(hello ((text "a")))) #false)
(check-expect (word? '(hello ((text "a")))) #false)
(check-expect (word? '(word ((txt "abc")))) #false)

; XWord -> String
; extracts the value of the only attribute of xw
(define (word-text xw)
  (local ((define attrs (second xw))
          (define attr (first attrs))
          (define content (second attr)))
    content))

(check-expect (word-text w1) "")
(check-expect (word-text w2) "nice")
(check-expect (word-text w3) "dark")

