;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part1_ankur_rev) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Mikel Matticoli and Ankur Gupta
;; CS1102 HW 3 Part 1

;; ================
;; Data Defintions

(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number ListOfWidget)

#;
(define (fn-for-widget w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-low
        (widget-parts w))))

;; Template rules used
;; Compound: 4 fields
;; Reference: (widget-parts w) is a (listof Widget)

;; (listof Widget) is a list of widgets

#;
(define (fn-for-low low)
  (cond [(empty? low) ...]
        [else 
         (... fn-for-widget (first low)
              (fn-for-low (rest low)))]))

;; Template rules used:
;;    - one of: 2 cases
;;    - atomic distinct: empty
;;    - compund (listof Widget)
;;    - self-reference: (rest low) is a (listof Widget)
;;    - referance: (first low) is a widget

;; ================
;; Constants

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

;; =================
;; Functions


;; ++++++++++++++ Question 1 +++++++++++++++

;; widget Natural -> (listof widget)
;; Given a widget and a length, this function will return all (sub)widgets whose name is longer than the length specified

(check-expect (find-widget-name-longer-than Wire 10) empty)
(check-expect (find-widget-name-longer-than Wire 2) (list Wire))
(check-expect (find-widget-name-longer-than Cord 3) (list Cord Wire))
(check-expect (find-widget-name-longer-than Cell 5) (list Buttons Numbers))
(check-expect (find-widget-name-longer-than Jewelry 5) (list Jewelry Necklace Pendant Bracelet))

;; Template taken from Widget

(define (find-widget-name-longer-than w len)
  (cond [(> (string-length (widget-name w)) len)
         (cons w (find-widgets-name-longer-than (widget-parts w) len))]
        [else (find-widgets-name-longer-than (widget-parts w) len)]))


;; (listof widget) Natural -> (listof widget)
;; Given a (listof Widget) and a length, this function will return all (sub)widgets in the list whose name is longer than the length specified

(check-expect (find-widgets-name-longer-than empty 10) empty)
(check-expect (find-widgets-name-longer-than (list Wire) 10) empty)
(check-expect (find-widgets-name-longer-than (list Wire) 1) (list Wire))
(check-expect (find-widgets-name-longer-than (list Cord Buttons) 5) (list Buttons Numbers))
(check-expect (find-widgets-name-longer-than (list Jewelry Buttons) 5) (list Jewelry Necklace Pendant Bracelet Buttons Numbers))

;; Template taken from (listof Widget)

(define (find-widgets-name-longer-than low len)
  (cond [(empty? low) empty]
        [else 
         (append (find-widget-name-longer-than (first low) len)
                 (find-widgets-name-longer-than (rest low) len))]))


;; ++++++++++++++ Question 2 +++++++++++++++

;; widget Natural -> (listof widget)
;; Given a widget and an amount, will return all (sub)widgets whose quantity is less than the amount

(check-expect (find-widget-quantity-over Wire 5) empty)
(check-expect (find-widget-quantity-over Jewelry 5) (list Rings Necklace Chain Beads Glass))
(check-expect (find-widget-quantity-over Chain 5) (list Chain))
(check-expect (find-widget-quantity-over Beads 5) (list Beads Glass))

;; Template taken from Widget

(define (find-widget-quantity-over w qty)
  (cond [(> (widget-quantity w) qty)
         (cons w (find-widgets-quantity-over (widget-parts w) qty))]
        [else (find-widgets-quantity-over (widget-parts w) qty)]))

;; (listof widget) Natural -> (listof widget)
;; Given a (listof Widget) and an amount, will return all (sub)widgets in the list whose quantity is less than the amount

(check-expect (find-widgets-quantity-over empty 5) empty)
(check-expect (find-widgets-quantity-over (list Wire) 5) empty)
(check-expect (find-widgets-quantity-over (list Beads) 5) (list Beads Glass))
(check-expect (find-widgets-quantity-over (list Beads Chain) 5) (list Beads Glass Chain))
(check-expect (find-widget-quantity-over Jewelry 5) (list Rings Necklace Chain Beads Glass))

;; Template taken from (listof Widget)

(define (find-widgets-quantity-over low qty)
  (cond [(empty? low) empty]
        [else 
         (append (find-widget-quantity-over (first low) qty)
                 (find-widgets-quantity-over (rest low) qty))]))

;; ++++++++++++++ Question 3 +++++++++++++++

;; widget Natural -> (listof widget)
;; Takes in a widget and an amount, will returns all (sub)widgets whose price is less than the amount

(check-expect (find-widget-cheaper-than Wire 5) empty)
(check-expect (find-widget-cheaper-than Chain 8) (list Chain))
(check-expect (find-widget-cheaper-than Beads 12) (list Beads Glass))
(check-expect (find-widget-cheaper-than Jewelry 5) (list Necklace Chain Pendant Glass))

;; Template taken from widget

(define (find-widget-cheaper-than w price)
  (cond [(< (widget-price w) price)
         (cons w (find-widgets-cheaper-than (widget-parts w) price))]
        [else (find-widgets-cheaper-than (widget-parts w) price)]))

;; (listof widget) Natural -> (listof widget)
;; Given a (listof widget) and an amount, will return all (sub)widgets in the list whose price is less than the amount

(check-expect (find-widgets-cheaper-than empty 5) empty)
(check-expect (find-widgets-cheaper-than (list Wire) 5) empty)
(check-expect (find-widgets-cheaper-than (list Wire) 100) (list Wire))
(check-expect (find-widgets-cheaper-than (list Wire Cord) 15) (list Wire Cord Wire))
(check-expect (find-widgets-cheaper-than (list Jewelry Cord) 5) (list Necklace Chain Pendant Glass))

;; Template taken from (listof Widget)

(define (find-widgets-cheaper-than low price)
  (cond [(empty? low) empty]
        [else 
         (append (find-widget-cheaper-than (first low) price)
                 (find-widgets-cheaper-than (rest low) price))]))

;; ++++++++++++++ Question 4 +++++++++++++++

;; Widget natural -> (listof Widget)
;; Given a widget and a cutoff, will return all subwidgets who have >= one subwidget whose quanity is less than the cutoff

(check-expect (find-widget-hard-make Wire 5) empty)
(check-expect (find-widget-hard-make Wire 1) empty)
(check-expect (find-widget-hard-make Cord 5) empty)
(check-expect (find-widget-hard-make Cell 10) (list Buttons))
(check-expect (find-widget-hard-make Jewelry 5) (list  Necklace))
(check-expect (find-widget-hard-make Jewelry 10) (list  Necklace Bracelet Beads))

(define (find-widget-hard-make w c)
  (n-list--loe (widget-parts w) c))

;; widget natural -> (listof Widget)
;; given a widget and a cutoff, will deteremine if the current widget is "hard-to-make", and if so, return
;;     a list of the widget and all of the subwidgets that are hard-to-make

(check-expect (n-list--e Wire 5) empty)
(check-expect (n-list--e Cord 4) (list Cord))
(check-expect (n-list--e Cell 10) (list Cell Buttons))

;; Template taken from Widget

(define (n-list--e w c)
  (cond [(subs-hard-to-make?--loe (widget-parts w) c) (cons w (n-list--loe (widget-parts w) c))]
        [else
         (n-list--loe (widget-parts w) c)]))

;; (listof Widget) natural -> (listof Widget)
;; Given a list of widgets and a cutoff, function will return all widgets and subwidgets in the list that
;;     are "hard-to-make"

(check-expect (n-list--loe empty 5) empty)
(check-expect (n-list--loe (list Wire) 5) empty)
(check-expect (n-list--loe (list Cord) 4) (list Cord))
(check-expect (n-list--loe (list Cell Cord) 10) (list Cell Buttons Cord))

;; Template taken from (listof Widget)

(define (n-list--loe loe c)
  (cond [(empty? loe) empty]
        [else
         (append (n-list--e (first loe) c) (n-list--loe (rest loe) c))]))

;; widget natural -> boolean
;; Given a widget, will determine if a widgets subs (and by transistivity, the widget itself) are "hard-to-make".
;;     If so, returns true, otherwise, returns false)

(check-expect (subs-hard-to-make? Wire 1) false)
(check-expect (subs-hard-to-make? Wire 5) true)
(check-expect (subs-hard-to-make? Cord 4) true)
(check-expect (subs-hard-to-make? Jewelery 5) true)
 
;; Template taken from Widget

(define (subs-hard-to-make? w c)
  (cond [(empty? (widget-parts w)) (< (widget-quantity w) c)]
        [else
         (subs-hard-to-make?--loe (widget-parts w) c)]))

;; (listof Widget) natural -> boolean
;; Given a (listof Widget) and a cutoff, the function will determine if there is even a single (sub)widget in the 
;;    list that is "hard-to-make"... otherwise, it will return false

(check-expect (subs-hard-to-make?--loe empty 9) empty)
(check-expect (subs-hard-to-make?--loe (list Glass) 7) true)
(check-expect (subs-hard-to-make?--loe (list Receiver Buttons Cord) 2) false)
(check-expect (subs-hard-to-make?--loe (list Receiver Buttons Cord) 4) true)

;; Template taken from (listof Widget)

(define (subs-hard-to-make?--loe low c)
  (cond [(empty? low) false]
        [else
         (or (subs-hard-to-make? (first low) c) (subs-hard-to-make?--loe (rest low) c))]))

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

;; widget -> (listof Widget)
;; Given a widget, will return the name of the widget and the subwidgets used to manufacture it

(check-expect (list-all-widget-names Beads) (list (widget-name Beads) (widget-name Glass)))
(check-expect (list-all-widget-names Jewelry) (list "Jewelry" "Rings" "Necklace" "Chain" "Pendant" "Bracelet" "Beads" "Glass"))
(check-expect (list-all-widget-names Chain) (list "Chain"))
(check-expect (list-all-widget-names Wire ) (list "Wire"))

;; Template taken from Widget

(define (list-all-widget-names w)
  (cons (widget-name w) (list-all-widgets-names (widget-parts w))))

;; (listof Widget) -> (listof Widget)
;; Given a (listof Widget), this function will go through the list and and return a list of all of the names of the widgets
;;   as well as the subwidgetsin the list

(check-expect (list-all-widgets-names empty) empty)
(check-expect (list-all-widgets-names (list Glass)) (list "Glass"))
(check-expect (list-all-widgets-names (list Cord)) (list "Cord" "Wire"))
(check-expect (list-all-widgets-names (list Cell Bracelet)) (list "Cell" "Buttons" "Numbers" "Bracelet" "Beads" "Glass"))

;; Template taken from (listof Widget)

(define (list-all-widgets-names low)
  (cond [(empty? low) empty]
        [else 
         (append (list-all-widget-names (first low))
                 (list-all-widgets-names (rest low)))]))
