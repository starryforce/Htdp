#lang htdp/bsl+

(require 2htdp/web-io)

; String String -> ... deeply nested list ...
; produces a web page with given author and title
(define (my-first-web-page author title)
  `(html
     (head
       (title ,title)
       (meta ((http-equiv "content-type")
              (content "text-html"))))
     (body
       (h1 ,title)
       (p "I, " ,author ", made this page."))))


(define page (my-first-web-page "Matthias" "Hello World"))

(show-in-browser page)