#lang htdp/isl+

(require 2htdp/image)

#| Exercise 374.
The data definitions in figure 127 use list.
Rewrite them so they use cons.
Then use the recipe to design the rendering functions for XEnum.v2 and XItem.v2 from scratch.
You should come up with the same definitions as in figure 128. 
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

(define SIZE 12) ; font size 
(define COLOR "black") ; font color
(define BLACK "black") ; bullet color
(define BT ; a graphical constant 
  (beside (circle 1 'solid BLACK) (text " " SIZE COLOR)))
 
; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BT item))

; An XWord is '(word ((text String))).
(define xw1 '(word ((text "hello"))))
(define xw2 '(word ((text "world"))))

(define loa '((color "red") (size "big")))

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))

(define xi1 (list 'li xw1))
(define xi2 (list 'li loa xw2))

; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))
(define xe1 (list 'ul xi1 xi2))
(define xe2 (list 'ul loa xi1 xi2))

(define xi3 (list 'li xe1))
(define xi4 (list 'li loa xe1))

 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image 
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (draw cur prev)
            (above/align 'left (render-item cur) prev)))
    (foldr draw empty-image content)))

(check-expect (render-enum xe1) (above/align 'left
                                             (bulletize (text "hello" SIZE BLACK))
                                             (bulletize (text "world" SIZE BLACK))))
(check-expect (render-enum xe2) (above/align 'left
                                             (bulletize (text "hello" SIZE BLACK))
                                             (bulletize (text "world" SIZE BLACK))))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE BLACK)]
       [else (render-enum content)]))))

(check-expect (render-item xi1) (bulletize (text "hello" SIZE BLACK)))
(check-expect (render-item xi2) (bulletize (text "world" SIZE BLACK)))
(check-expect (render-item xi3) (bulletize  (above/align 'left
                                                         (bulletize (text "hello" SIZE BLACK))
                                                         (bulletize (text "world" SIZE BLACK)))))
(check-expect (render-item xi4) (bulletize  (above/align 'left
                                                         (bulletize (text "hello" SIZE BLACK))
                                                         (bulletize (text "world" SIZE BLACK)))))