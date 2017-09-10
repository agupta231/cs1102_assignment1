;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fireworks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ===================
;; Data Definitions

(define-struct firework (x y dx dy color fuse height expl))
;; Firework is (make-firework Natural[0,WIDTH] Natural[0,HEIGHT] Integer Integer String Natural Natural Natural
;; intrep. (make-firework (x y dx dy color fuse height exp)) is each independent firework object, containing
;;         the position, velocity, color, fuse, height, and explosion type data
;;     x is in screen pixels
;;     y is in screen pixels
;;     dx is in screen pixels per tick
;;     dy is in screen pixels per tick
;;     color is a string
;;     fuse is in screen ticks
;;     height is in screen ticks
;;     expl is an integer selecting which explosion to execute

(define fw1 (make-firework 10 10 10 10 "blue" 10 10 1)) ; Firework at (10,10), moving (10, 10) pixels/sec, and is blue, and will explode at after (28 - EXPLSN-LEN) ticks, with the explosive object being 10 pixels and style being expl selector 1
(define fw2 (make-firework -10 -10 -10 10 "red" 20 100 2)) ; Firework at (-10,-10), moving (-10, 10) pixels/sec, and is red, and will explode at after (20 - EXPLSN-LEN) ticks, with the explosive object being 100 pixels and style being expl selector 2

#;
(define (fn-for-fw fw)
  (... (firework-x fw)
	   (firework-y fw)
	   (firework-dx fw)
	   (firework-dy fw)
	   (firework-color fw)
	   (firework-fuse fw)
	   (firework-height fw)
	   (firework-expl fw)))

;; Template rules used:
;;  - compound: 7 fields

;; ListOfFirework is one of:
;;   - empty
;;   - (cons Firework ListOfFirework
;; interp. a list of fireworks

(define lof1 empty)
(define lof2 (cons fw1 empty))
(define lof3 (list fw1 fw2))

#; 
(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
		[else
		  ((fn-for-fw (first lof))
		   (fn-for-lof (rest lof)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Firework ListOfFirework)
;;  - self-referance: (rest lof) is a ListOfFirework
;;  - referance: (first lof) is a Firework

(define-struct ws (lof color height expl))
;; World State (make-ws ListOfFireworks String Integer Integer
;; intrep. (make-ws (lof color height fuse) is a world state with containing the list of fireworks
;;         the current color, height of firework, and explosion choice of the user
;;    lof is a ListOfFireworks
;;    color is a string
;;    height is in screen pixels
;;    exp is in screen pixels

(define ws1 (make-ws empty "blue" 200 9))   ; World State has no fireworks, blue as the color, height as 200, and has explosion choice 9
(define ws1 (make-ws lof3 "blue" 200 9))   ; World State has three fireworks, blue as the chosen color, height as 200, and has explosion choice 9

#; 
(define (fn-for-ws ws)
  (... (fn-for-lof (ws-lof ws))
	   (ws-color ws)
	   (ws-height ws)
	   (ws-expl ws)))

;; Template rules used:
;;  - compound: 4 fields
;;  - reference: (ws-lof ws) is a ListOfFirework


;; ===================
;; Constants

(define WIDTH 600)
(define HEIGHT 900)
(define MTS (empty-scene WIDTH HEIGHT "black"))

(define EXPLSN-LEN 28)
()
;; ===================
;; Functions
