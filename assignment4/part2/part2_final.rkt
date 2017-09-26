;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2_final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =========== Data Definitions ============

(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
; same as assignment #3, except no parts component

(define-struct bst (widget left right))
;; a BST is either
;;          false, or
;;          a (make-bst widget bst bst)


;; =========== Constants  ============

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


; =============== Functions =============

;; (value BST -> Boolean) (value BST -> Boolean) value bst -> widget | false
;; Given a value, and BST, will return a widget whose value is the same as the one inputted.
;;    This is done by the equality function and the less than function to deteremine how the 
;;    tree should be traversed

(define eq_strs (lambda (x y) (string=? x (widget-name (bst-widget y)))))
(define lt_strs (lambda (x y) (string<=? x (widget-name (bst-widget y)))))
(define eq_num (lambda (x y) (= x (widget-quantity (bst-widget y)))))
(define lt_num (lambda (x y) (< x (widget-quantity (bst-widget y)))))

(check-expect (find eq_strs lt_strs "Gregor" false) false)
(check-expect (find eq_strs lt_strs "9" BST5) (make-widget "9" 1 15))
(check-expect (find eq_strs lt_strs "3" BST11) (make-widget "3" 13 18))
(check-expect (find eq_strs lt_strs "459" BST6) false)

(define (find eq lt value bst)
  (cond [(false? bst) false]
        [else
         (cond [(eq value bst) (bst-widget bst)]
               [(lt value bst)
                (find eq lt value (bst-left bst))]
               [else
                (find eq lt value (bst-right bst))])]))

;; (widget BST -> Boolean) Widget BST -> BST
;; Given a widget and BST, will return a BST with the widget inserted in the proper location
;;     as dicated by the inutted comparison function

(define comp_str (lambda (x y) (string<=? (widget-name x) (widget-name (bst-widget y)))))
(define comp_num (lambda (x y) (< (widget-quantity x) (widget-quantity (bst-widget y)))))

(check-expect (insert comp_str (make-widget "1" 8 10) false) BST7)
(check-expect (insert comp_str (make-widget "1" 8 10) BST5)
              (make-bst (make-widget "9" 1 15) (make-bst (make-widget "1" 8 10) false false) false))
(check-expect (insert comp_str (make-widget "50" 8 10) BST4)
              (make-bst (make-widget "4" 16 5) false (make-bst (make-widget "50" 8 10) false false)))
(check-expect (insert comp_num (make-widget "1" 8 10) BST5)
              (make-bst (make-widget "9" 1 15) false (make-bst (make-widget "1" 8 10) false false)))

(define (insert compare w BST)
  (cond [(false?  BST) (make-bst w false false)]
        [else
         (if (compare w BST)
             (make-bst (bst-widget BST) (insert compare w (bst-left BST)) (bst-right BST))
             (make-bst (bst-widget BST) (bst-left BST) (insert compare w (bst-right BST))))]))

;; (X X -> Boolean) (Widget -> X) BST -> BST
;; Given a list of widgets, will insert all of widgets into the provided BST. the values
;;      will be order by the inputted order function, in which the values inputted into the
;;      order function will given by the value function

(define (insert-all order value low bst)
  (foldr 
   (lambda (w b) 
     (local 
       [(define (order-abs wid bst) 
          (order (value wid) (value (bst-widget bst))))]
       (insert order-abs w b))) 
   bst 
   low))
