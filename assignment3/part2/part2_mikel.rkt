;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Part 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Mikel Matticoli and Ankur Gupta
;; CS1102 HW 3 Part 1

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

;; Abstract template for widget
#;
(define (fn-for-widget widget)
  (local [(define (fn-for-widget--element w) 
            (... (widget-name w) 
                 (widget-quantity w)
                 (widget-time w) 
                 (widget-price w) 
                 (fn-for-widget--low 
                  (widget-parts w))))
          (define (fn-for-widget--low low) 
            (cond [(empty? low) ...] 
                  [else 
                   (... fn-for-widget--element (first low) 
                        (fn-for-widget--low (rest low)))]))]
    (fn-for-widget--element widget)))

;; widget (Function widget) -> (listof widget)
;; filters widgets using given boolean function
(define (filter-widgets widget filter)
  (local [(define (filter-widget-list low filter)
            (cond [(empty? low) empty]
                  [else 
                   (append (filter-widgets (first low) filter)
                           (filter-widget-list (rest low) filter))]))]
    (if (filter widget)
        (cons widget (filter-widget-list (widget-parts widget) filter))
        (filter-widget-list (widget-parts widget) filter))))
;; Test Cases
(check-expect (filter-widgets Wire (lambda (w) (> (string-length (widget-name w)) 5))) empty)
(check-expect (filter-widgets Chain (lambda (w) (< (widget-price w) 8))) (list Chain))
(check-expect (filter-widgets Cord (lambda (w) (< (widget-quantity w) 4))) (list Wire))
(check-expect (filter-widgets Jewelry (lambda (w) (> (string-length (widget-name w)) 5))) (list Jewelry Necklace Pendant Bracelet)) 

;; widget Natural -> (listof widget)
;; This function will examine the widget, as well as all of the subwidgets used to manufacture it, and return all whose name length is longer than the specified natural.
(define (find-widget-name-longer-than w len)
  (filter-widgets w (lambda (w) (> (string-length (widget-name w)) len))))

;; Test cases
(check-expect (find-widget-name-longer-than Wire 5) empty)
(check-expect (find-widget-name-longer-than Jewelry 5) (list Jewelry Necklace Pendant Bracelet))
(check-expect (find-widget-name-longer-than Jewelry 50) empty)
(check-expect (find-widget-name-longer-than Telephone 8) (list Telephone))

;; (listof widget) Natural -> (listof widget)
;; Runs find-widget-quantity-over recursively for all items in a list of widgets
(define (find-widget-quantity-over w qty)
  (filter-widgets w (lambda (w) (> (widget-quantity w) qty))))

;; Test cases
(check-expect (find-widget-quantity-over Wire 5) empty)
(check-expect (find-widget-quantity-over Jewelry 5) (list Rings Necklace Chain Beads Glass))
(check-expect (find-widget-quantity-over Chain 5) (list Chain))
(check-expect (find-widget-quantity-over Beads 5) (list Beads Glass))


;; (listof widget) Natural -> (listof widget)
;; Runs find-widget-cheaper-than recursively for all items in a list of widgets
(define (find-widget-cheaper-than w price)
  (filter-widgets w (lambda (w) (< (widget-price w) price))))

;; Test cases
(check-expect (find-widget-cheaper-than Wire 5) empty)
(check-expect (find-widget-cheaper-than Jewelry 5) (list Necklace Chain Pendant Glass))
(check-expect (find-widget-cheaper-than Chain 8) (list Chain))
(check-expect (find-widget-cheaper-than Beads 12) (list Beads Glass))

;; Widget Natural -> (listof Widget)
;; Finds all subwidgets who have at least one component widget whose quantity in stock
;; is less than the cutoff.
(define (find-widget-hard-make w cutoff)
  (filter-widgets w (lambda (w) (< (widget-quantity w) cutoff))))

;; Test cases
(check-expect (find-widget-hard-make Wire 4) (list Wire))
(check-expect (find-widget-hard-make Cord 4) (list Wire))
(check-expect (find-widget-hard-make Telephone 4) (list Wire))

;; Widget String -> Widget or false
;; Finds widget by name
(define (find-widget-name w name)
  (local [(define search
            (filter-widgets w (lambda (w) (string=? (widget-name w) name))))]
    (if (empty? search)
        false
        (first search))))

;; Test cases
(check-expect (find-widget-name Wire "Wire") Wire)
(check-expect (find-widget-name Cord "Wire") Wire)
(check-expect (find-widget-name Telephone "Necklace") false)
(check-expect (find-widget-name Cord "") false)

;; Parts 6 & 7 combined
;; X (X -> (listof X)) (X -> Y) -> (listof Y)
;; Function that will list all elements and subelements of a structure. The function is given an input of type X,
;;   a function which will get the list of subelements from X, and a data type selector function, which will 
;;   return the correct attribute of the functiont that should be included in the list.

(define rest-wid (lambda (x) (widget-parts x)))
(define name-widg (lambda (x) (widget-name x)))

(check-expect (list-all Beads rest-wid name-widg) (list (widget-name Beads) (widget-name Glass)))
(check-expect (list-all Jewelry rest-wid name-widg) (list "Jewelry" "Rings" "Necklace" "Chain" "Pendant" "Bracelet" "Beads" "Glass"))
(check-expect (list-all Chain rest-wid name-widg) (list "Chain"))
(check-expect (list-all Wire rest-wid name-widg) (list "Wire"))
(check-expect (list-all Wire rest-wid identity) (list Wire))
(check-expect (list-all Cord rest-wid identity) (list Cord Wire))

(define (list-all input rest-of selector)
  (local [(define (list-all--element e) 
            (cons (selector e) (list-all--low (rest-of e))))
          (define (list-all--low loe) 
            (cond [(empty? loe) empty] 
                  [else 
                   (append (list-all--element (first loe)) 
                           (list-all--low (rest loe)))]))]

    (list-all--element input)))


;; Sort function 

(check-expect (sort-widgets Wire (lambda (x y) (< (widget-quantity x) (widget-quantity y)))) (list Wire))
(check-expect (sort-widgets Cord (lambda (x y) (> (widget-quantity x) (widget-quantity y)))) (list Wire Cord))

(define (sort-widgets wid order)
  (local [(define (gen-list--e w)
            (cons w (gen-list--loe (widget-parts w))))
          (define (gen-list--loe loe)
            (cond [(empty? loe) empty]
                  [else
                   (append (gen-list--e (first loe)) (gen-list--loe (rest loe)))]))
          (define (sort-list loe)
            (cond [(empty? loe) empty]
                  [else
                   (local
                     [(define pivot (first loe))]
                     (append
                      (sort-list (find-elements order pivot (rest loe)))
                      (list pivot)
                      (sort-list (find-elements (lambda (x y) (not (order x y))) pivot (rest loe)))))]))
          (define (find-elements fn p loe)
            (cond [(empty? loe) empty]
                  [(fn p (first loe))
                   (cons (first loe) (find-elements fn p (rest loe)))]
                  [else
                   (find-elements fn p (rest loe))]))]

    (sort-list (gen-list--e wid))))