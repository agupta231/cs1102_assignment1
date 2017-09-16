;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Templates
#;
(define (fn-for-widget w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-low
        (widget-parts w))))
#;
(define (fn-for-low low)
  (cond [(empty? low) ...]
        [else 
         (... fn-for-widget (first low)
              (fn-for-low (rest low)))]))

;; ++++++++++++++ Question 1 +++++++++++++++

;; widget Natural -> (listof widget)
;; This function will examine the widget, as well as all of the subwidgets used to manufacture it, and return all whose name length is longer than the specified natural.
(define (find-widget-name-longer-than w len)
  (cond [(> (string-length (widget-name w)) len)
             (cons w (find-widgets-name-longer-than (widget-parts w) len))]
        [else (find-widgets-name-longer-than (widget-parts w) len)]))

;; (listof widget) Natural -> (listof widget)
;; Runs find-widget-name-longer-than recursively for all items in a list of widgets
(define (find-widgets-name-longer-than low len)
  (cond [(empty? low) empty]
        [else 
         (append (find-widget-name-longer-than (first low) len)
                 (find-widgets-name-longer-than (rest low) len))]))

;; Test cases
(check-expect (find-widget-name-longer-than Wire 5) empty)
(check-expect (find-widget-name-longer-than Jewelry 5) (list Jewelry Necklace Pendant Bracelet))
(check-expect (find-widget-name-longer-than Jewelry 50) empty)
(check-expect (find-widget-name-longer-than Telephone 8) (list Telephone))


;; ++++++++++++++ Question 2 +++++++++++++++

;; widget Natural -> (listof widget)
;; This function will examine the widget, as well as all of the subwidgets used to manufacture it, and return all whose quantity is larger than the specified natural.
(define (find-widget-quantity-over w qty)
  (cond [(> (widget-quantity w) qty)
             (cons w (find-widgets-quantity-over (widget-parts w) qty))]
        [else (find-widgets-quantity-over (widget-parts w) qty)]))

;; (listof widget) Natural -> (listof widget)
;; Runs find-widget-quantuty-over recursively for all items in a list of widgets
(define (find-widgets-quantity-over low qty)
  (cond [(empty? low) empty]
        [else 
         (append (find-widget-quantity-over (first low) qty)
                 (find-widgets-quantity-over (rest low) qty))]))

;; Test cases
(check-expect (find-widget-quantity-over Wire 5) empty)
(check-expect (find-widget-quantity-over Jewelry 5) (list Rings Necklace Chain Beads Glass))
(check-expect (find-widget-quantity-over Chain 5) (list Chain))
(check-expect (find-widget-quantity-over Beads 5) (list Beads Glass))


;; ++++++++++++++ Question 3 +++++++++++++++

;; widget Natural -> (listof widget)
;; This function will examine the widget, as well as all of the subwidgets used to manufacture it, and return all whose price is lower than the specified natural.
(define (find-widget-cheaper-than w price)
  (cond [(< (widget-price w) price)
             (cons w (find-widgets-cheaper-than (widget-parts w) price))]
        [else (find-widgets-cheaper-than (widget-parts w) price)]))

;; (listof widget) Natural -> (listof widget)
;; Runs find-widget-cheaper-than recursively for all items in a list of widgets
(define (find-widgets-cheaper-than low price)
  (cond [(empty? low) empty]
        [else 
         (append (find-widget-cheaper-than (first low) price)
                 (find-widgets-cheaper-than (rest low) price))]))

;; Test cases
(check-expect (find-widget-cheaper-than Wire 5) empty)
(check-expect (find-widget-cheaper-than Jewelry 5) (list Necklace Chain Pendant Glass))
(check-expect (find-widget-cheaper-than Chain 8) (list Chain))
(check-expect (find-widget-cheaper-than Beads 12) (list Beads Glass))


;; ++++++++++++++ Question 4 +++++++++++++++

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


;; ++++++++++++++ Question 5 +++++++++++++++

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


;; ++++++++++++++ Question 6 +++++++++++++++

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


;; ++++++++++++++ Question 7 +++++++++++++++

;; widget Natural -> (listof widget)
;; This function will examine the widget, as well as all of the subwidgets used to manufacture it, and return all whose price is lower than the specified natural.
(define (list-all-widget-names w)
  (cons (widget-name w) (list-all-widgets-names (widget-parts w))))

;; (listof widget) Natural -> (listof widget)
;; Runs find-widget-cheaper-than recursively for all items in a list of widgets
(define (list-all-widgets-names low)
  (cond [(empty? low) empty]
        [else 
         (append (list-all-widget-names (first low))
                 (list-all-widgets-names (rest low)))]))

;; Test Cases
(check-expect (list-all-widget-names Beads ) (list (widget-name Beads) (widget-name Glass)))
(check-expect (list-all-widget-names Jewelry ) (list "Jewelry" "Rings" "Necklace" "Chain" "Pendant" "Bracelet" "Beads" "Glass"))
(check-expect (list-all-widget-names Chain) (list "Chain"))
(check-expect (list-all-widget-names Wire ) (list "Wire"))
