#lang htdp/bsl


(define-struct movie [title director year])
(define (f-movie m)
  (... (movie-title m) ...
   ... (movie-director m) ...
   ... (movie-year m) ...))

(define-struct pet [name number])
(define (f-pet p)
  (... (pet-name p) ...
       (pet-number p) ...))

(define-struct CD [artist title price])
(define (f-CD cd)
  (... (CD-artist cd) ...
       (CD-title cd) ...
       (CD-price cd) ...))

(define-struct sweater [material size color])
(define (f-sweater s)
  (... (sweater-material) ...
       (sweater-size) ...
       (sweater-color) ...))