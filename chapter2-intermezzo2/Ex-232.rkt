#lang htdp/bsl+

(require 2htdp/web-io)

(define q1 `(1 "a" 2 #false 3 "c"))

(define a11 (list `1 `"a" `2 `#false `3 `"c"))
(define a12 (list 1 "a" 2 #false 3 "c"))

(check-expect q1 a11)
(check-expect q1 a12)

(define q2 `(("alan" ,(* 2 500))
  ("barb" 2000)
  (,(string-append "carl" " , the great") 1500)
  ("dawn" 2300)))

(define a21 (list `("alan" ,(* 2 500))
                  `("barb" 2000)
                  `(,(string-append "carl" " , the great") 1500)
                  `("dawn" 2300)))
(define a22 (list (list `"alan" `,(* 2 500))
                 (list `"barb" `2000)
                 (list `,(string-append "carl" " , the great") `1500)
                 (list `"dawn" `2300)))
(define a23 (list (list "alan" 1000)
                  (list "barb" 2000)
                  (list "carl , the great" 1500)
                  (list "dawn" 2300)))

(check-expect q2 a21)
(check-expect q2 a22)
(check-expect q2 a23)


(define title "ratings")

(define q3 `(html
             (head
              (title ,title))
             (body
              (h1 ,title)
              (p "A second web page"))))

(define a31 (list `html
                  `(head
                    (title ,title))
                  `(body
                    (h1 ,title)
                    (p "A second web page"))))
(define a32 (list `html
                  (list `head
                        `(title ,title))
                  (list `body
                        `(h1 ,title)
                        `(p "A second web page"))))
(define a33 (list `html
                  (list `head
                        (list `title `,title))
                  (list `body
                        (list `h1 `,title)
                        (list `p `"A second web page"))))
(define a34 (list `html
                  (list `head
                        (list `title "ratings"))
                  (list `body
                        (list `h1 "ratings")
                        (list `p "A second web page"))))


(check-expect q3 a31)
(check-expect q3 a32)
(check-expect q3 a33)       
(check-expect q3 a34)

(show-in-browser q3)
