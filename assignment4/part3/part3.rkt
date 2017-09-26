;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)
(require 2htdp/image)

;; =========== Data Definitions =============

(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
; same as assignment #3, except no parts component

(define-struct bst (widget left right))
;; a BST is either
;;          false, or
;;          a (make-bst widget bst bst)


;; =========== Constants =============

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  ;; not sure if i should have this???  maybe good for testing???
(define TAB 5) ; someone senior told me this constant might help


;; =========== Functions =============

;; +++++++++ Taken from Part 1 ++++++++++

;; Natural -> String
;; creates a blank string of length equal to n

(check-expect (blanks 0) "")
(check-expect (blanks 3) "   ")

(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))

;;Natural Natural -> (listof Widget)
;;generates random widgets given a number of widgets to create and a range of possible numerical values for all parts of a widget

(define (random-widgets num range)
  (build-list num (lambda (x) (make-widget (number->string (random range)) (random range) (random range)))))

;; Widget Bst -> Bst
;; adds widget to existing bst

(define (insert w BST)
  (cond [(false?  BST) (make-bst w false false)]
        [else
         (if (string<=? (widget-name (bst-widget BST)) (widget-name w))
             (make-bst (bst-widget BST) (bst-left BST) (insert w (bst-right BST)))
             (make-bst (bst-widget BST) (insert w (bst-left BST)) (bst-right BST)))]))

;; (listof Widget) -> Bst
;; Given in a list of widgets, insert all of the widgets into the bst

(define (insert-all low BST)
  (cond [(empty? low) BST]
        [else
         (insert (first low) (insert-all (rest low) BST))]))

;; string bst -> widget | false
;; searches the inputted binary search and returns the widget whose name
;;    is the same as the inputted string.. else returns false

(define (find-name name bst)
  (cond [(false? bst) false]
        [else
         (local [(define wid-name (widget-name (bst-widget bst)))]
           (cond [(string=? wid-name name) (bst-widget bst)]
                 [(string>=? wid-name name)
                  (find-name name (bst-left bst))]
                 [else
                  (find-name name (bst-right bst))]))]))

;; BST -> Image
;; Given a BST element, will generate a tree showing left and right elements of the tree,
;;    with proper indenting properly show how the tree is made

(define (render bst)
  (local [(define (render-element bst prefix level)
            (cond [(false? bst) (square 0 "solid" "white")]
                  [else
                   (above/align
                    "left"
                    (text
                     (string-append (blanks (* level TAB)) prefix ": " (widget-name (bst-widget bst)))
                     TEXT-SIZE
                     TEXT-COLOR)
                    (render-element (bst-left bst) "L" (+ level 1))
                    (render-element (bst-right bst) "R" (+ level 1)))]))]
    
    (render-element bst "T" 0)))

;; --------- Taken from Part 1 -----------

;; +++++++++ Part 3 Functions Begin +++++++++++

;; Function given from assignment

(define (do-tests lst)
  (cond
    [(empty? lst) empty]
    [else
     (cons
      ((first lst) 5)
      (do-tests (rest lst)))]))

;; __________ Constants ______________

(define benchmarks (list 0.25 0.5 0.75 0)) ; Where in list values are taken from: 0 means not in list

(define null-wid (make-widget "Gregor, the myth, the lengend" 420 1738)) ; Create placeholder for empty list search

;; Build 10k 100k and 1M lists

(define list10k (random-widgets 10000 10000000))
(define bst10k (insert-all list10k false))
(define list100k (random-widgets 100000 10000000))
(define bst100k (insert-all list100k false))
(define list1M (random-widgets 1000000 10000000))
(define bst1M (insert-all list1M false))

;; (listof Widget) BST -> (listof func)
;; Given a list of widgets and a bst, will produce a list of functions that will execute
;;    timing tests for the different values scattered around the list and BST. 

(define (generate-test-code lst bst)
  (build-list
   (* (length benchmarks) 2)
   (lambda (list_pos)
     (local [(define (determine-wid val)
               (cond [(zero? (floor
                              (* (list-ref benchmarks (floor (/ val 2)))
                                 (length lst)))) 
                      null-wid] 
                     [else
                      (list-ref lst 
                                (floor
                                 (* (list-ref benchmarks (floor (/ val 2)))
                                    (length lst))))]))
             (define search-val (widget-name (determine-wid list_pos)))
             (define (gen-test index)
               (if (even? index)
                   (lambda (x)
                     (time (find-name search-val bst)))
                   (lambda (x)
                     (time (filter 
                            (lambda (y)
                              (string=? search-val (widget-name y)))
                            lst)))))]
       (gen-test list_pos)))))