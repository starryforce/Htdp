#lang htdp/isl+

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

#| Exercise 386. Here is the get function:

It assumes the existence of get-xexpr,
a function that searches an arbitrary Xexpr.v3 for the desired attribute and produces [Maybe String].
Formulate test cases that look for other values than "F" and that force get to signal an error.

Design get-xexpr. Derive functional examples for this function from those for get.
Generalize these examples so that you are confident get-xexpr can traverse an arbitrary Xexpr.v3.
Finally, formulate a test that uses the web data saved in exercise 385.

|#

; read-plain-xexpr/web 's result have no whitespace.

(define PREFIX "Https://www.google.com/finance?q=")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; interpretation A (make-data p d) represents the price of stock is p,
; and the change of it is delta

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


; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  '()))])))

; [List-of Attribute] Symbol -> [Maybe String]
; retrieves the value from aloa that associate with s
(define (find-attr aloa s)
  (local ((define item (assq s aloa)))
    (if (false? item) #false (second item) )))


; Xexpr.v3 Symbol -> [Maybe String]
(define (get-xexpr xe s)
  (local ((define attrs (xexpr-attr xe))
          (define key (find-attr attrs 'itemprop))
          (define value (find-attr attrs 'content)))
    (if (string=? key s) value #false)))

(check-expect
  (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
(check-expect
  (get-xexpr '(meta ((content "+1") (itemprop "F"))) "G")
  #false)

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
(check-error
  (get '(meta ((content "+1") (itemprop "F"))) "G")
  "not found")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))
 
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; retrieve the price & change of stock from web.
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image 
          (define (render-stock-data w)
            (local (; [StockWorld String -> String] -> Image
                    ; generate a picture to show the info of the stock
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))


