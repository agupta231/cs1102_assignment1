(require racket/list)
(require 2htdp/image)
(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
; same as assignment #3, except no parts component

(define-struct bst (widget left right))
;; a BST is either
;;          false, or
;;          a (make-bst widget bst bst)



;; sample code for Part 3

; As you look over the sample code a thought occurs to you and
; you confront The Head:  "Hey! You said we're not allowed to use
; the list template, how come you're allowed to use it?"

; "Because I don't need it.  However, in the restricted subset
; of Racket you are using there isn't an easy way to solve this
; problem with map, so I thought I'd give you an assist."

; (there is a function called eval that evaluates code, but
; it does not exist even in the advanced student language.)

(define (do-tests lst)
  (cond
    [(empty? lst) empty]
    [else
     (cons
      ((first lst) 5) ; 5 is a dummy value
      (do-tests (rest lst)))]))

; As The Head moves to leave you call out one last objection: "you
; made a typo and have an extra set of parens on the second to
; last line!"

; "That's not a typo.  That's the tricky part.  However, I'll also
; give you an example of how to use the code."

; an example of how your code should interact with do-tests
; list1m is a list of 1 million elements
; tree1m is the binary search tree constructed from list1m

;; (listof Widget) BST -> (listof time)

(define (generate-test-code lst bst)
  )
