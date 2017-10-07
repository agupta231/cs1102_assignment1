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

;;Macro 1

(define-syntax new
  (syntax-rules (graph)
    [(new graph str)
     (begin (define str 0)
            (set! str (make-graph (quote str)  '())))]))

(new graph g0)


;;Macro 2
(define-syntax vertex
  (syntax-rules (in)
    [(vertex v in g)
     (begin (define v 0)
            (set! v (make-node (quote v) '()))
            (set! g (make-graph (graph-name g) (add-unique (quote v) (graph-vertices g)))))]))

(vertex n0 in g0)
(vertex n1 in g0)
(vertex n2 in g0)

;; Macro 3
(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (begin (set! n0 (make-node (quote n0) (add-unique (quote n1) (node-edges n0)))))]))

;; Macro 4
(define-syntax edges
  (syntax-rules (-> <->)
    [(edges n0 -> n1 <-> n2 -> n3)
     (begin (edge n0 n1)
            (edge n1 n2)
            (edge n1 n2)
            (edge n2 n1)
            (edge n2 n3))]))
                                            