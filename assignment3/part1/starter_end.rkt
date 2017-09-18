;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname starter_ankur) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Template
(define (fn-for-widget w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-low (widget-parts w))))

(define (fn-for-low low)
  (cond [(empty? low) ...]
        [else 
         (... (fn-for-widget (first low))
              (fn-for-low (rest low)))]))

;; Widget Natural -> (listof Widget)
;; Finds all subwidgets who have at least one component widget whose quantity in stock
;;    is less than the cutoff.
;; !!! Change purpoise

;(define (find-widget-hard-make widget cutoff) (list Rings))

(check-expect (find-widget-hard-make Wire 4) (list Wire))
(check-expect (find-widget-hard-make Cord 4) (list Wire))
(check-expect (find-widget-hard-make Telephone 4) (list Wire))

(define (find-widget-hard-make widget cutoff)
  (cond [(< (widget-quantity widget) cutoff)
         (cons widget (find-widget-hard-make--low (widget-parts widget) cutoff))]
        [else
         (find-widget-hard-make--low (widget-parts widget) cutoff)]))


;; (listof Widget) Natural -> (listof Widget)
;; Searches through a list of widgets and will return a list of all the widgets whose
;;    quantity is less than the cutoff 
;; !!! Change Purpoise

(check-expect (find-widget-hard-make--low empty 3) empty)
(check-expect (find-widget-hard-make--low (list Wire) 5) (list Wire))
(check-expect (find-widget-hard-make--low (list Cord Buttons) 20) (list Cord Wire Buttons Numbers))

(define (find-widget-hard-make--low low cutoff)
  (cond [(empty? low) empty]
        [else 
         (append (find-widget-hard-make (first low) cutoff)
               (find-widget-hard-make--low (rest low) cutoff))]))

;; widget string -> widget | false
;; given widget and a name, searches the widget and all corresponding parts and will return a 
;;   widget with the same name. Else, it returns false

(check-expect (find-widget-name Cord "Cord") Cord)
(check-expect (find-widget-name Telephone "Wire") Wire)
(check-expect (find-widget-name Telephone "Gregor") false)

(define (find-widget-name widget name)
  (if (string=? (widget-name widget) name) 
	widget
	(find-widget-name-low (widget-parts widget) name)))

;; (listof Widget) String -> Widget | false
;; Given a list of widgets, it will search through the list and return the widget whose name is the same 
;;    as the inputted name. Else, it will return false

(check-expect (find-widget-name-low empty "Wire") false)
(check-expect (find-widget-name-low (list Wire) "Wire") Wire)
(check-expect (find-widget-name-low (list Cord Cell) "Numbers") Numbers)
(check-expect (find-widget-name-low (list Cord Cell) "Gregor") false)

(define (find-widget-name-low low name) 
  (cond [(empty? low) false]
        [else 
         (if (false? (find-widget-name (first low) name))
		   (find-widget-name-low (rest low) name)
		   (find-widget-name (first low) name))]))

;; Widget -> (listof Widget)
;; Given the main widget, the function will return a list of the widget and all of the sub widgets required to
;;    manufacture it

(check-expect (list-all-widgets Wire) (list Wire))
(check-expect (list-all-widgets Cord) (list Cord Wire))

(define (list-all-widgets widget) 
  (cons widget (list-all-widgets-low (widget-parts widget))))

;; (listof Widget) -> (listof Widget)
;; Given a list of widgets, will return the list of widgets as well as all of the subwidgets for each widget in
;;     the list

(check-expect (list-all-widgets-low empty) empty)
(check-expect (list-all-widgets-low (list Wire)) (list Wire))
(check-expect (list-all-widgets-low (list Cord)) (list Cord Wire))
(check-expect (list-all-widgets-low (list Cell Cord)) (list Cell Buttons Numbers Cord Wire))

(define (list-all-widgets-low low) 
  (cond [(empty? low) empty]
        [else 
         (append (list-all-widgets (first low)) (list-all-widgets-low (rest low)))]))
