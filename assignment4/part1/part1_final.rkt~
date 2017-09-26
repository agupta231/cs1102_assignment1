;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part1_final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)
(require 2htdp/image)

;; =========== Data Definitions ============

(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
; same as assignment #3, except no parts component

(define-struct bst (widget left right))
;; a BST is either
;;          false, or
;;          a (make-bst widget bst bst)


;; =========== Constants  ============

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  ;; not sure if i should have this???  maybe good for testing???
(define TAB 5) ; someone senior told me this constant might help

(define L1 (list
            (make-widget "1" 8 10)
            (make-widget "19" 19 0)
            (make-widget "3" 13 18)
            (make-widget "10" 14 2)
            (make-widget "13" 3 15)))

(define BST5  (make-bst (make-widget "9" 1 15) false false))
(define BST4  (make-bst (make-widget "4"  16  5) false false))
(define BST3  (make-bst (make-widget "7"   5 15) BST4  BST5 ))
(define BST2  (make-bst (make-widget "16"  7  8) BST3  false))
(define BST1  (make-bst (make-widget "3"   9 13) false BST2 ))
(define BST15 (make-bst (make-widget "13"  3 15) false false))
(define BST14 (make-bst (make-widget "10" 14  2) false BST15))
(define BST13 (make-bst (make-widget "3"  13 18) false false))
(define BST12 (make-bst (make-widget "9"   1 15) false BST14))
(define BST11 (make-bst (make-widget "4"  16  5) BST13 false))
(define BST10 (make-bst (make-widget "19" 19  0) false false))
(define BST9  (make-bst (make-widget "7"   5 15) BST11 BST12))
(define BST8  (make-bst (make-widget "16"  7  8) BST9  BST10))
(define BST7  (make-bst (make-widget "1"   8 10) false false))
(define BST6  (make-bst (make-widget "3"   9 13) BST7  BST8 ))


;; =========== Functions  ============

;;Natural Natural -> (listof Widget)
;;generates random widgets given a number of widgets to create and a range of possible numerical values for all parts of a widget
;(define (random-widgets num range) (list (make-widget "0" 0 0))) ;stub

(define (random-widgets num range)
  (build-list num (lambda (x) (make-widget (number->string (random range)) (random range) (random range)))))


;;Widget Bst -> Bst
;;adds widget to existing bst, based on lexigraphical sorting of widget names

(check-expect (insert (make-widget "1" 8 10) false) BST7)
(check-expect (insert (make-widget "1" 8 10) BST5)
              (make-bst (make-widget "9"   1 15) (make-bst (make-widget "1" 8 10) false false) false))
(check-expect (insert (make-widget "50" 8 10) BST4)
              (make-bst (make-widget "4" 16 5) false (make-bst (make-widget "50" 8 10) false false)))

(define (insert w BST)
  (cond [(false?  BST) (make-bst w false false)]
        [else
         (if (string<=? (widget-name w) (widget-name (bst-widget BST)))
             (make-bst (bst-widget BST) (insert w (bst-left BST)) (bst-right BST))
             (make-bst (bst-widget BST) (bst-left BST) (insert w (bst-right BST))))]))


;; (listof Widget) Bst -> Bst
;; Given in a list of widgets and a bst, insert all of the widgets into the bst

(check-expect (insert-all empty false) false)
(check-expect (insert-all (list (make-widget "1" 8 10)) BST5)
              (make-bst (make-widget "9"   1 15) (make-bst (make-widget "1" 8 10) false false) false))

(define (insert-all low BST)
  (cond [(empty? low) BST]
        [else
         (insert (first low) (insert-all (rest low) BST))]))

;; string bst -> widget | false
;; searches the inputted binary search and returns the widget whose name
;;    is the same as the inputted string.. else returns false

(check-expect (find-name "Gregor" false) false)
(check-expect (find-name "9" BST5) (make-widget "9" 1 15))
(check-expect (find-name "19" (insert-all L1 BST1)) (make-widget "19" 19 0))
(check-expect (find-name "459" BST6) false)

(define (find-name name bst)
  (cond [(false? bst) false]
        [else
         (local [(define wid-name (widget-name (bst-widget bst)))]
           (cond [(string=? wid-name name) (bst-widget bst)]
                 [(string<=? name wid-name)
                  (find-name name (bst-left bst))]
                 [else
                  (find-name name (bst-right bst))]))]))

;; Natural -> String
;; creates a blank string of length equal to n

(check-expect (blanks 0) "")
(check-expect (blanks 3) "   ")

(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))


;; BST -> Image
;; Given a BST element, will generate a tree showing left and right elements of the tree,
;;    with proper indenting properly show how the tree is made

(check-expect (render false) (square 0 "solid" "white"))
(check-expect (render (insert-all L1 false))
              (above/align
               "left"
               (text "T: 13" TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks (* TAB 1)) "L: 10") TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks (* TAB 2)) "L: 1") TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks (* TAB 1)) "R: 3") TEXT-SIZE TEXT-COLOR)
               (text (string-append (blanks (* TAB 2)) "L: 19") TEXT-SIZE TEXT-COLOR)))

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
