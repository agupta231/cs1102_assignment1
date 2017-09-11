;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fireworks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ===================
;; Data Definitions

(define-struct firework (x y dx dy color fuse height))
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

(define fw1 (make-firework 10 10 10 10 "blue" 100 10)) ; Firework at (10,10), moving (10, 10) pixels/sec, and is blue, and will explode at after (28 - EXPLSN-LEN) ticks, with the explosive object being 10 pixels and style being expl selector 1
(define fw2 (make-firework 100 100 -10 10 "red" 100 100)) ; Firework at (-10,-10), moving (-10, 10) pixels/sec, and is red, and will explode at after (20 - EXPLSN-LEN) ticks, with the explosive object being 100 pixels and style being expl selector 2

#;
(define (fn-for-fw fw)
  (... (firework-x fw)
       (firework-y fw)
       (firework-dx fw)
       (firework-dy fw)
       (firework-color fw)
       (firework-fuse fw)
       (firework-height fw)))

;; Template rules used:
;;  - compound: 6 fields

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
;;    height is an integer
;;    expl is the image function given in by the user

(define ws1 (make-ws empty "blue" 9 3))   ; World State has no fireworks, blue as the color, height as 9 
(define ws2 (make-ws lof3 "blue" 5 3))   ; World State has three fireworks, blue as the chosen color, height as 5 

#; 
(define (fn-for-ws ws)
  (... (fn-for-lof (ws-lof ws))
       (ws-color ws)
       (ws-height ws)
       (ws-expl ws)))

;; Template rules used:
;;  - compound: 4 fields
;;  - reference: (ws-lof ws) is a ListOfFirework

(define-struct ws_init (color height))
;; IntialWorldState (make ws_init (String Natural))
;; interp. (make-ws (color height) starts the world off with the given height and color and no fireworks. This sub structure
;;         allows for the user to input in their own functions
;;    color is a String
;;    height is a Natural

#;
(define (fn-for-wsi wsi)
  (... (ws_init-color wsi)
       (ws_init-height wsi)))

;; Template rules used:
;;   - compound: 2 fields


;; ===================
;; Constants

(define WIDTH 600)
(define HEIGHT 900)
(define MTS (empty-scene WIDTH HEIGHT "black"))

(define FW-RADIUS 4)
(define FW-FUSE-MIN 75)
(define FW-FUSE-MAX 100)
(define FW-DX-MIN -3)
(define FW-DX-MAX 3)
(define FW-DY-MIN 10)
(define FW-DY-MAX 15)

(define EXPLSN-LEN 56)

;; ===================
;; Functions

;; WorldState -> WorldState
;; Called to begin the fireworks function: Start with (main START1) or (main START2)
;; No test for the main function
(define (main wsi explsn_func)
  (big-bang (make-ws
             empty
             (ws_init-color wsi)
             (ws_init-height wsi)
             explsn_func)
            (on-tick update-fireworks)
            (to-draw render)
            (on-key handle-key)
            (on-mouse handle-mouse)))

;; WorldState -> WorldState
;; Updates the status and positions of all of the active fireworks

; (define (update-fireworks ws) ws)

(define (update-fireworks ws)
  (make-ws (update-lof (ws-lof ws))
           (ws-color ws)
           (ws-height ws) 
           (ws-expl ws)))

;; Template taken from ws

;; ListOfFirework -> ListOfFirework
;; Updates the ListOfFirework inputted, and removes any that have have completed their cycle

(check-expect (update-lof empty) empty)

; (define (update-lof lof) lof)

(define (update-lof lof)
  (cond [(empty? lof) empty]
        [else
         (append (update-firework (first lof)) (update-lof (rest lof)))]))

;; Firework -> ListOfFirework
;; Updates the tick cycle in the firework, and if the lifespan of the firework is up, it returns false 

;(define (update-firework fw) fw)

(define (update-firework fw)
  (cond [(<= (- (firework-fuse fw) 1) 0) empty]
        [else
         (list (make-firework (+ (firework-x fw) (firework-dx fw)) 
                              (+ (firework-y fw) (firework-dy fw)) 
                              (firework-dx fw) 
                              (firework-dy fw) 
                              (firework-color fw) 
                              (- (firework-fuse fw) 1) 
                              (firework-height fw)))]))

;; WorldState -> Image
;; Generates the scene based off of the current world state 

; (define (render ws) ws)

(define (render ws)
  (gen-img-lof (ws-lof ws) (ws-expl ws)))

;; ListOfFirework -> Image
;; Iterates through a ListOfFirework, generating the image overlaying on top of the previous images

(check-expect (gen-img-lof empty shape1) MTS)
(check-expect (gen-img-lof (cons fw1 empty) shape1) (place-image (circle FW-RADIUS "solid" "blue") 10 10 MTS))
(check-expect (gen-img-lof (cons fw1 (cons fw2 empty)) shape1) (place-image (circle FW-RADIUS "solid" "red")
                                                                            100
                                                                            100
                                                                            (place-image (circle FW-RADIUS "solid" "blue")
                                                                                         10
                                                                                         10
                                                                                         MTS)))

(define (gen-img-lof lof shape) 
  (cond [(empty? lof) MTS]
        [else
         (place-image 
          (select-img (first lof) shape) 
          (firework-x (first lof)) 
          (firework-y (first lof)) 
          (gen-img-lof (rest lof) shape))]))

;; Firework -> Image
;; Based on the lifespan and the current tick of the firework, render out the correct image for the firework 

; (define (select-img fw shape) MTS)

(define (select-img fw shape)
  (cond [(<= (- (firework-fuse fw) EXPLSN-LEN) 0) (shape (firework-height fw) (firework-color fw))]
        [else
         (circle FW-RADIUS "solid" (firework-color fw))]))

;; WorldState KeyEvent -> WorldState
;; Modifies the settings of future fireworks based on which key the user presses 

; (define (handle-key ws ke) ws)

(define (handle-key ws ke)
  (cond [(key=? ke "r") 
         (make-ws (ws-lof ws) 
                  "red" 
                  (ws-height ws) 
                  (ws-expl ws))]
        [(key=? ke "g") 
         (make-ws (ws-lof ws) 
                  "green" 
                  (ws-height ws) 
                  (ws-expl ws))]
        [(key=? ke "b") 
         (make-ws (ws-lof ws) 
                  "blue" 
                  (ws-height ws) 
                  (ws-expl ws))]
        [(key=? ke "w") 
         (make-ws (ws-lof ws) 
                  "white" 
                  (ws-height ws) 
                  (ws-expl ws))]
        [(key=? ke "1") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  1
                  (ws-expl ws))]
        [(key=? ke "2") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  2
                  (ws-expl ws))]
        [(key=? ke "3") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  3
                  (ws-expl ws))]
        [(key=? ke "4") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  4
                  (ws-expl ws))]
        [(key=? ke "5") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  5
                  (ws-expl ws))]
        [(key=? ke "6") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  6
                  (ws-expl ws))]
        [(key=? ke "7") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  7
                  (ws-expl ws))]
        [(key=? ke "8") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  8
                  (ws-expl ws))]
        [(key=? ke "9") 
         (make-ws (ws-lof ws) 
                  (ws-color ws)
                  9
                  (ws-expl ws))]
        [else
         ws]))
		
;; WorldState MouseEvent -> WorldState
;; Modifies the current world state based on where the user clicks with the mouse 

; (define (handle-mouse ws x y me) ws) 

(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-up") 
         (make-ws 
          (append
           (list
            (make-firework x
                           y
                           (+ (random (- FW-DX-MAX FW-DX-MIN)) FW-DX-MIN)
                           (- (+ (random (- FW-DY-MAX FW-DY-MIN)) FW-DY-MIN))
                           (ws-color ws)
                           (+ (random (- FW-FUSE-MAX FW-FUSE-MIN)) FW-FUSE-MIN)
                           (ws-height ws))
            )
           (ws-lof ws))
          (ws-color ws)
          (ws-height ws)
          (ws-expl ws))]
        [else
         ws]))


;;; HELPER FUNCTIONS
(define (shape1 size color)
  (rectangle (* size 10) (* size 10) "solid" color))

(define START
  (make-ws_init "white" 1))