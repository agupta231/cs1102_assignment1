#lang racket

;; #:transparent makes structures display a bit nicer
(define-struct graph (name vertices) #:transparent)
(define-struct node (name edges) #:transparent)

;; X (listof X) --> (listof X)
;; adds item to lst if it is not already there
;; if item is in lst, just returns lst
(define (add-unique item lst)
  (if (member item lst)
      lst
      (cons item lst)))
