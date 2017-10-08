#lang racket
(require test-engine/racket-tests)

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
(new graph g1)


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
(vertex n3 in g1)


;; Macro 3
(define-syntax edge
  (syntax-rules ()
    [(edge n0 n1)
     (begin (set! n0 (make-node (quote n0) (add-unique (quote n1) (node-edges n0)))))]))

;;Macro 4

(define-syntax edges
  (syntax-rules (-> <->)
    [(edges n0 -> n1) (edge n0 n1)]
    [(edges n0 <-> n1)
     (begin (edge n0 n1)
            (edge n1 n0))]
    [(edges n0 -> n1 -> n2 ...)
     (begin (edges n0 -> n1)
            (edges n1 -> n2 ...))]
    [(edges n0 -> n1 <-> n2 ...)
     (begin (edges n0 -> n1)
            (edges n1 <-> n2 ...))]
    [(edges n0 <-> n1 -> n2 ...)
     (begin (edges n0 <-> n1)
            (edges n1 -> n2 ...))]
    [(edges n0 <-> n1 <-> n2 ...)
     (begin (edges n0 <-> n1)
            (edges n1 <-> n2 ...))]))

;;Some functions for check-expects



(edge n0 n1)
(edge n1 n0)
(edge n1 n2)

;; Macro 5
(check-expect (->? n0 n1) true)
(check-expect (->? n1 n2) true)
(check-expect (->? n2 n1) false)
(check-expect (->? n0 n2) false)

(define-syntax ->?
  (syntax-rules ()
    [(->? n0 n1)
     (if (empty? (filter (lambda (x) (equal? x (quote n1))) (node-edges n0)))
         false
         true)]))

;; Macro 6
(check-expect (<->? n0 n1) true)
(check-expect (<->? n1 n2) false)
(check-expect (<->? n2 n1) false)
(check-expect (<->? n0 n2) false)

(define-syntax <->?
  (syntax-rules ()
    [(<->? n0 n1)
     (and (->? n0 n1) (->? n1 n0))]))

;; Macro 7
(define (-->? checknodemaster check)
  (local [(define base checknodemaster)
          (define (helper checknode acclst)
            (cond
              [(->? (eval checknode) check) true]
              [(empty? (node-edges (eval checknode))) false]
              [(not (empty? (filter (lambda (x) (equal? x checknode)) acclst))) false]
              [else (if (empty? (filter (lambda (x) (not (false? x))) (map (lambda (y) (helper y (cons y acclst))) (node-edges (eval checknode)))))
                        false
                        true)]))] 
         (helper checknodemaster (list))))  


(test)