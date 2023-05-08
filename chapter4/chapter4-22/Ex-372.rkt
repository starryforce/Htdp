#lang htdp/isl+

(require 2htdp/image)

#| Exercise 372.
Before you read on, equip the definition of render-item1 with tests.
Make sure to formulate these tests in such a way that they don’t depend on the BT constant.
Then explain how the function works; keep in mind that the purpose statement explains what it does.
|#

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

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


; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))
(define xi1 '(li (word ((text "hello")))))
(define xi2 '(li ((color "red")) (word ((text "world")))))

(define BT (circle 2 "solid" "black"))

; XItem.v1 -> Image 
; renders an item as a "word" prefixed by a bullet
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

(check-expect (render-item1 xi1)
              (beside/align 'center BT (text "hello" 12 'black)))
(check-expect (render-item1 xi2)
              (beside/align 'center BT (text "world" 12 'black)))