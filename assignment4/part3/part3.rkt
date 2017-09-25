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


;; (listof Widget) BST -> (listof func)

(define benchmarks (list 0.25 0.5 0.75 0))

(define (generate-test-code lst bst)
  (build-list)
  (* (length benchmarks) 2)
  (lambda (index)
	(local [(define (determine-wid val)
			  (cond [(zero? (list-ref benchmarks val)) 
					 ("Gregor, the myth, the legend")] 
					[else
					  (list-ref lst 
								(floor
								  (* (list-ref benchmarks val)
									 (length benchmarks))))]))
			(define search-val (widget-name (determine-wid index)))
			(define (gen-test ))])
	(if (even? index)
	  (lambda (x)
		(time (find-name
				(determine-wid index)
				bst)))
	  (lambda (x)
		(time (filter 
				(lambda (y)
				  (string? y (determine-wid index)))
				lst))))))

(define (generate-test-code lst bst)
  (build-list 
	(* (length benchmarks) 2) 
	(lambda (pos)
	  (local [
			  ;; Number[Element from benchmarks list] -> String [Widget-name]
			  (define (gen_search_val p)
				(cond [(zero? p) "dummy"]
					  [else (widget-name (list-ref lst (floor (* p (length benchmarks)))))]))
			  
			  ;; ])))
