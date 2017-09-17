(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number ListOfWidget)

(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Cell (make-widget "Cell" 6 25 7 (list Buttons)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))

(define Glass (make-widget "Glass" 6 9 4 empty))
(define Beads (make-widget "Beads" 25 12 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 3 5 (list Beads)))
(define Chain (make-widget "Chain" 7 2 1 empty))
(define Pendant (make-widget "Pendant" 4 3 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3 (list Chain Pendant)))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry" 4 17 30 (list Rings Necklace Bracelet)))

;; ++++++++++++++ Question 4 +++++++++++++++

;; Widget Natural -> (listof Widget)
;; Given a widget and a cutoff, returns a list of all of the subwidgets whose quantity is less than
;;      the cutoff

(check-expect (find-widget-hard-make Wire 4) empty)
(check-expect (find-widget-hard-make Wire 1) empty)
(check-expect (find-widget-hard-make Cord 4) (list Wire))
(check-expect (find-widget-hard-make Telephone 4) (list Wire))
(check-expect (find-widget-hard-make Jewelry 5) (list  Necklace))
(check-expect (find-widget-hard-make Jewelry 10) (list  Necklace Bracelet Beads))

(define (find-widget-hard-make w c)
  (clean-up-list (find-widget-hard-make--low (widget-parts w) c)))

(define (clean-up-list low)
  (cond [(empty? low) empty]
        [(not (list? low)) empty]
        [(list? (first low))
         (append (first low) (clean-up-list (rest low)))]
        [else
         (clean-up-list (rest low))]))

(define (find-widget-hard-make--w w c)
  (cond [(empty? (widget-parts w))
         (< (widget-quantity w) c)]
        [(list? (find-widget-hard-make--low (widget-parts w) c))
         (cons w (find-widget-hard-make--low (widget-parts w) c))]
        [else
         (if (find-widget-hard-make--low (widget-parts w) c)
             (cons w empty)
             false)]))

(define (find-widget-hard-make--low low c)
  (cond [(empty? low) false]
        [(or (list? (find-widget-hard-make--w (first low) c)) (list? (find-widget-hard-make--low (rest low) c)))
             (cond [(and (list? (find-widget-hard-make--w (first low) c)) (not (list? (find-widget-hard-make--low (rest low) c))))
                    (find-widget-hard-make--w (first low) c)]
                   [(and (not (list? (find-widget-hard-make--w (first low) c))) (list? (find-widget-hard-make--low (rest low) c)))
                    (find-widget-hard-make--low (rest low) c)]
                   [else
                    (append (find-widget-hard-make--w (first low) c) (find-widget-hard-make--low (rest low) c))])]
        [else
         (or (find-widget-hard-make--w (first low) c) (find-widget-hard-make--low (rest low) c))]))


