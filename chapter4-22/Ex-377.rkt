#lang htdp/isl+

; Exercise 377.
; Design a program that replaces all "hello"s with "bye" in an enumeration.

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

(define r-xi1 (list 'li '(word ((text "bye")))))
(define r-xi2 (list 'li aloa '(word ((text "bye")))))

(define r-xe1 `(ul ,r-xi1 ,r-xi1 ,r-xi1))

(define r-xi5 (list 'li r-xe1))

(define r-xe2 `(ul ,aloa ,r-xi2 ,r-xi2 ,r-xi2))

(define r-xi6 (list 'li aloa r-xe2))

(define r-xe3 (list 'ul aloa r-xi1 r-xi2 xi3 xi4 r-xi5 r-xi6))

; A Container is one of:
; – (cons Symbol '())
; – (cons Symbol (cons [List-of Attribute] '()))
; Xexpr.v2 -> Container
(define (query-container xe)
  (local ((define maybe-attrs (second xe)))
    (if (list-of-attributes? maybe-attrs)
        (cons (first xe) (cons maybe-attrs '()))
        (cons (first xe) '()))))

(check-expect (query-container xi1) '(li))
(check-expect (query-container xi2) (list 'li aloa))
(check-expect (query-container xe1) '(ul))
(check-expect (query-container xe2) `(ul ,aloa))

; XEnum.v2 -> XEnum.v2
; replaces all "hello"s with "bye" in xe.
(define (replace-enum xe)
  (local ((define container (query-container xe))
          (define content (xexpr-content xe)))
    (append container
            (map replace-item content))))

(check-expect (replace-enum xe1) r-xe1)
(check-expect (replace-enum xe2) r-xe2)
(check-expect (replace-enum xe3) r-xe3)


; XItem.v2 -> XItem.v2
; replaces all "hello"s with "bye" in xi.
(define (replace-item xi)
  (local ((define container (query-container xi))
          (define content (first (xexpr-content xi))))
    (append container
            (list
             (cond [(word? content) (if (string=? (word-text content) "hello")
                                        '(word ((text "bye")))
                                        content)]
                   [else (replace-enum content)])))))

(check-expect (replace-item xi1) r-xi1)
(check-expect (replace-item xi2) r-xi2)
(check-expect (replace-item xi3) xi3)
(check-expect (replace-item xi4) xi4)
(check-expect (replace-item xi5) r-xi5)
(check-expect (replace-item xi6) r-xi6)
(check-expect (replace-item xi7) `(ul ,r-xe3))
