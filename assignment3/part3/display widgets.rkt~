;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |display widgets|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")
(define TAB 5)

;; Natural -> String
;; creates a blank string of length equal to n
(check-expect (blanks 0) "")
(check-expect (blanks 3) "   ")

(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))

;; widget fn -> Image
;; Given a widget an a function, this function will create a hierachy in which the sub widgets are indented in and the image colors are those according to function inputted

(check-expect (simple-render Glass (lambda (x) TEXT-COLOR)) (text "Glass : 6 @ $9" TEXT-SIZE TEXT-COLOR))
(check-expect (simple-render Beads (lambda (x) 
									 (if (< (widget-quantity x) 10) "purple" TEXT-COLOR))) (above/align
									  "left"
									  (text "Beads : 25 @ $12" TEXT-SIZE TEXT-COLOR)
									  (text (string-append (blanks TAB) "Glass : 6 @ $9") TEXT-SIZE "purple"))
(check-expect (simple-render Necklace (lambda (x) 
										(if (> (widget-price x) 2) "green" TEXT-COLOR))) (above/align
									  "left"
									  (text "Necklace : 10 @ $7" TEXT-SIZE TEXT-COLOR)
									  (text (string-append (blanks TAB) "Chain : 7 @ $2") TEXT-SIZE TEXT-COLOR)
									  (text (string-append (blanks TAB) "Pendant : 4 @ $3") TEXT-SIZE "green"))
(check-expect (simple-render Jewelry (lambda (x)
									   (if (< (string-length (widget-name w)) 6)
										 "blue"
										 TEXT-COLOR)))
			  (above/align
				"left"
				(text "Jewelry : 4 @ 17" TEXT-SIZE TEXT-COLOR)
				(text (string-append (blanks TAB) "Rings : 15 @ 8" TEXT-SIZE "blue"))
				(text (string-append (blanks TAB) "Necklace : 10 @ 7" TEXT-SIZE TEXT-COLOR))
				(text (string-append (blanks (* 2 TAB)) "Chain : 7 @ $2") TEXT-SIZE "blue") 
				(text (string-append (blanks (* 2 TAB)) "Pendant : 4 @ $3") TEXT-SIZE TEXT-COLOR)
				(text (string-append (blanks TAB) "Bracelet : 5 @ 3" TEXT-SIZE TEXT-COLOR))))

(define (render widget fn)
  (local [(define (render--w wid n)
            (above/align 
             "left"
             (text (string-append 
                    (blanks (* TAB n)) 
                    (widget-name wid) 
                    " : "
                    (number->string (widget-quantity wid))
                    " @ $"
                    (number->string (widget-price wid)))
                   TEXT-SIZE
                   (fn wid))
             (render--low (widget-parts wid) (+ n 1))))
          (define (render--low low n)
            (cond [(empty? low) (square 0 "solid" "white")]
                  [else
                   (above/align
                    "left"
                    (render--w (first low) n)
                    (render--low (rest low) n))]))]
    (render--w widget 0)))

;; Widget -> Image
;; Given a widget, outputs an image in which subwidgets are indented a tab inwards from the master widget

(check-expect (simple-render Glass) (text "Glass : 6 @ $9" TEXT-SIZE TEXT-COLOR))
(check-expect (simple-render Beads) (above/align
									  "left"
									  (text "Beads : 25 @ $12" TEXT-SIZE TEXT-COLOR)
									  (text (string-append (blanks TAB) "Glass : 6 @ $9") TEXT-SIZE TEXT-COLOR))
(check-expect (simple-render Necklace) (above/align
									  "left"
									  (text "Necklace : 10 @ $7" TEXT-SIZE TEXT-COLOR)
									  (text (string-append (blanks TAB) "Chain : 7 @ $2") TEXT-SIZE TEXT-COLOR)
									  (text (string-append (blanks TAB) "Pendant : 4 @ $3") TEXT-SIZE TEXT-COLOR))
(check-expect (simple-render Jewelry)
			  (above/align
				"left"
				(text "Jewelry : 4 @ 17" TEXT-SIZE TEXT-COLOR)
				(text (string-append (blanks TAB) "Rings : 15 @ 8" TEXT-SIZE TEXT-COLOR))
				(text (string-append (blanks TAB) "Necklace : 10 @ 7" TEXT-SIZE TEXT-COLOR))
				(text (string-append (blanks (* 2 TAB)) "Chain : 7 @ $2") TEXT-SIZE TEXT-COLOR) 
				(text (string-append (blanks (* 2 TAB)) "Pendant : 4 @ $3") TEXT-SIZE TEXT-COLOR)
				(text (string-append (blanks TAB) "Bracelet : 5 @ 3" TEXT-SIZE TEXT-COLOR))))

(define (simple-render widget)
  (render widget (lambda (x) TEXT-COLOR)))
