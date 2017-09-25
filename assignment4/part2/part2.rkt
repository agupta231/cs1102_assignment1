(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")
(define TAB 5) 

;; (value BST -> Boolean) (value BST -> Boolean) value bst -> widget | false
;; Given a value, and BST, will return a widget whose value is the same as the one inputted.
;;    This is done by the equality function and the less than function to deteremine how the 
;;    tree should be traversed

(define (find eq lt value bst)
  (cond [(false? bst) false]
        [else
         (cond [(eq value BST) (bst-widget bst)]
               [(lt value BST)
                (find eq lt value (bst-left bst))]
               [else
                (find eq lt value (bst-right bst))])]))

;; (widget BST -> Boolean) Widget BST -> BST
;; Given a widget and BST, will return a BST with the widget inserted in the proper location
;;     as dicated by the inutted comparison function

(define insert (compare w bst)
 (cond [(false?  BST) (make-bst w false false)]
        [else
         (if (compare w bst)
             (make-bst (bst-widget BST) (bst-left BST) (insert w (bst-right BST)))
             (make-bst (bst-widget BST) (insert w (bst-left BST)) (bst-right BST)))]))

;; (X X -> Boolean) (Widget -> X) BST -> BST
;; Given a list of widgets, will insert all of widgets into the provided BST. the values
;;      will be order by the inputted order function, in which the values inputted into the
;;      order function will given by the value function

(define insert-all (order value low bst)
  (foldr 
	(lambda 
	  (w b)
	  (insert 
		(lambda (x y)
		  ))) 
	bst 
	low))
