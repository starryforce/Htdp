#lang htdp/isl+

; Exercise 376.
; Design a program that counts all "hello"s in an instance of XEnum.v2.

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]

; Xexpr.v2 -> [List-of Xexpr.v2]
; retrieves the content of xe
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  (rest optional-loa+content)
                  optional-loa+content))])))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))
(define aloa '((color "red") (size "large")))

; An XWord is '(word ((text String))).

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

; XWord -> String
; extracts the value of the only attribute of xw
(define (word-text xw)
  (local ((define attrs (second xw))
          (define attr (first attrs))
          (define content (second attr)))
    content))

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
(define xi1 (list 'li '(word ((text "hello"))))) ; i1
(define xi2 (list 'li aloa '(word ((text "hello"))))) ; i2
(define xi3 (list 'li '(word ((text "world"))))) ; i1
(define xi4 (list 'li aloa '(word ((text "world"))))) ; i2

; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))
(define xe1 `(ul ,xi1 ,xi1 ,xi1)) ; e1
(define xe2 `(ul ,aloa ,xi2 ,xi2 ,xi2)) ;e2

(define xi5 (list 'li xe1)) ;i3
(define xi6 (list 'li aloa xe2)) ;i4

(define xe3 (list 'ul aloa xi1 xi2 xi3 xi4 xi5 xi6)) ;e2

(define xi7 `(ul ,xe3)) ;e1

; XEnum.v2 -> Number
; count how many "hello" in xe.
(define (count-enum xe)
  (local ((define content (xexpr-content xe))
          (define (count cur prev)
            (+ (count-item cur) prev)))
    (foldr count 0 content)))

(check-expect (count-enum xe1) 3)
(check-expect (count-enum xe2) 3)
(check-expect (count-enum xe3) 8)

; XItem.v2 -> Number
; count how may "hello" in xi.
(define (count-item xi)
  (local ((define content (first (xexpr-content xi))))
    (cond [(word? content)
           (if (string=? (word-text content) "hello") 1 0)]
          [else (count-enum content)])))

(check-expect (count-item xi1) 1)
(check-expect (count-item xi2) 1)
(check-expect (count-item xi3) 0)
(check-expect (count-item xi4) 0)
(check-expect (count-item xi5) 3)
(check-expect (count-item xi6) 3)
(check-expect (count-item xi7) 8)