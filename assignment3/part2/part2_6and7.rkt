;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Parts 1 - 4 combined


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
