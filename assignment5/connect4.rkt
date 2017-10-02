;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname connect4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; do not modify the constants
(define RED 1) 
(define BLACK 2)
(define BLANK 0)
(define ROWS 10)  
(define COLUMNS 10) 
(define DEFAULT-DEPTH 1)

;; state --> state
;; you will implement this function to make a move
(define (computer-moves state)
  (local [(define moves (legal-next-moves state))]
    (make-move state (get-nth (random (length moves)) moves))))

;; state --> Number
;; you will implement this function to determine the value of a state (terminal or non-terminal)
(define (evaluation-function state)
  (random 10))

;; State -> Number
;; Given a state, will output a number representating the state of the board 

#;
(define (evaluate_board_state state) 
  (sum_chains (gen_chains state)))

;; State -> (listof Chains)
;; Given a state, will return a list of all of the chains
(define (gen_chains state) 
  (get_chains (get_perimeter_pieces (world-state-position state))))

(define-struct piece (row col color))
(define-struct pos (row col))

;; State -> list
;; Given a state, will return a list of the positions of all of the game pieces that are on the perimeter
;;     of the game board
(define (get_perimeter_pieces state) 
  (local [(define (next_row p)
            (cond [(>= (add1 (pos-row p)) ROWS)
                   (make-pos 0 (add1 (pos-col p)))]
                  [else
                   (make-pos (add1 (pos-row p)) (pos-col p))]))
          
          (define (get_pieces pos lop)
            (local [(define row (pos-row pos))
                    (define col (pos-col pos))
                    (define current_piece (piece-at state row col))
                    (define right_piece (piece-at state row (add1 col)))
                    (define left_piece (piece-at state row (sub1 col)))] 
                     (cond
                       [(= col COLUMNS) lop]
                       [(not (= current_piece 0))
                        (if (or (or 
                                  (= right_piece 0) 
                                  (= right_piece -1)) 
                                 (or 
                                  (= left_piece 0)
                                  (= left_piece -1)))
                            
                                   (get_pieces 
                                    (next_row pos)
                                    (cons (make-piece row col current_piece) lop))

                                   (get_pieces
                                    (make-pos 0 (add1 col))
                                    (cons (make-piece row col current_piece) lop)))]
                       [else
                        (get_pieces
                         (next_row pos)
                         lop)])))]
    (get_pieces (make-pos 0 0) empty)))

;; list -> (listOf Chains)
;; Given game peices, determine the chains that each piece is a part of 
; !!!
(define (get_chains pieces) empty)

;; you must implement the above two functions as part of the asignment, but may create additional
;; helper functions

(define-struct world-state (position whose-turn settings other-info))
;; position is what checkers are on the board
;; whose-turn is the player who is about to move
;; settings and other-info are future functionality


;; returns the checker color (RED, BLACK, or BLANK) of the specified position
;; on the board
(define (piece-at board row column)
  (cond [(or (< row 0) (< column 0) (>= column COLUMNS) (>= row ROWS)) -1] ; piece outside of board
        [else (get-nth row (get-nth column board))]))

;; Natural List --> Element
;; returns the nth element of a list.  for former LISP programmers learning Racket :-)
(define (get-nth n alist)
  (list-ref alist n))

(define (main state)
  (local 
    [(define board 
       (make-list COLUMNS
                  (make-list ROWS 0)))
     
     (define PIECE-SIZE 30)
     
     (define RED-CHECKER (circle PIECE-SIZE "solid" "red"))
     (define BLACK-CHECKER (circle PIECE-SIZE "solid" "black"))
     (define BLANK-CHECKER (circle PIECE-SIZE "solid" "white"))
     
     (define OFFSET (/ PIECE-SIZE .66))
     (define WIDTH
       (+ (* COLUMNS 2.5 PIECE-SIZE) (* 0.5 PIECE-SIZE)))
     (define HEIGHT
       (+ (* ROWS 2.5 PIECE-SIZE) (* 0.5 PIECE-SIZE)))
     
     (define MTS 
       (rectangle WIDTH HEIGHT "solid" "yellow"))
     (define (place-checker state x y mouse-event)
       (local
         [(define move (map-coordinates x y))
          (define next-state (make-move state move))]
         (cond
           [(and (string=? mouse-event "button-down")
                 (member move (legal-next-moves state)))
            (if (check-win? next-state)  
                (cond
                  [(= (world-state-whose-turn state) RED)
                   "RED WINS"]
                  [(= (world-state-whose-turn state) BLACK)
                   "BLACK WINS"])
                (local [(define result (computer-moves next-state))]
                  (if (check-win? result)
                      (cond
                        [(= (world-state-whose-turn next-state) RED)
                         "RED WINS"]
                        [(= (world-state-whose-turn next-state) BLACK)
                         "BLACK WINS"])
                      result)))]
           [else state])))
     (define (display-column2 column x-offset y-offset image)
       x-offset)
     (define (display-column column x-offset y-offset image)
       (cond
         [(empty? column) image]
         [else
          (place-image
           (cond 
             [(= (first column) RED) RED-CHECKER]
             [(= (first column) BLACK) BLACK-CHECKER]
             [(= (first column) BLANK) BLANK-CHECKER])
           x-offset y-offset 
           (display-column (rest column) x-offset (+ y-offset (* 2.5 PIECE-SIZE)) image))]))
     
     (define (display-board-helper position x-offset image)
       (cond 
         [(empty? position) image]
         [else
          (display-board-helper
           (rest position)
           (+ x-offset (* 2.5 PIECE-SIZE))
           (display-column (first position)
                           x-offset
                           OFFSET image))]))
     
     (define (display-board position)
       (display-board-helper position OFFSET MTS))
     (define (render state)
       (display-board (world-state-position state)))
     
     (define (map-coordinate lower upper click pos)
       (cond
         [(and (> click lower) (< click upper)) pos]
         [(> pos (max ROWS COLUMNS)) -1]
         [else
          (map-coordinate (+ lower (* 2.5 PIECE-SIZE)) (+ upper (* 2.5 PIECE-SIZE)) click (+ 1 pos))]))
     
     (define (map-coordinates x y) 
       (list (map-coordinate (/ PIECE-SIZE 2) (+  (/ PIECE-SIZE 2) (* 2 PIECE-SIZE)) x 0)
             (map-coordinate (/ PIECE-SIZE 2) (+  (/ PIECE-SIZE 2) (* 2 PIECE-SIZE)) y 0)))]
    
    (big-bang state 
              (on-mouse place-checker) 
              (to-draw render))))

;; *** this function permits you to make both legal and illegal moves
;; *** you do not need to use this function and probably should not.  someone thought of a reason
;; *** for it to exist and so I included it.  to be clear, your program is only permitted to 
;; *** make legal moves.
(define (make-hypothetical-move state move)
  (local [(define (update-column turn column current move)
            (cond
              [(empty? column) empty]
              [else
               (cons
                (cond
                  [(= current move)
                   turn]
                  [else (first column)])
                (update-column turn (rest column) (+ 1 current) move))]))
          
          (define (do-move board turn move-x move-y current-x)
            (cond
              [(empty? board) empty]
              [else
               (cons
                (cond
                  [(= move-x current-x) (update-column turn (first board) 0 move-y)]
                  [else (first board)])
                (do-move (rest board) turn move-x move-y (+ 1 current-x)))]))]
    (make-world-state
     (do-move (world-state-position state)
              (world-state-whose-turn state) 
              (first move) (second move) 0)
     (cond
       [(= (world-state-whose-turn state) RED) BLACK]
       [(= (world-state-whose-turn state) BLACK) RED])
     (world-state-settings state)
     (world-state-other-info state))))

;; you will use this function.  it takes as input the move you will make, represented as a list of X Y coordinates
(define (make-move state move)
  (if (member move (legal-next-moves state))
      (make-hypothetical-move state move)
      state))

;; world-state --> list
;; returns all of the legal moves for the current position
(define (legal-next-moves state)
  (local [
          (define (first-blank column pos)
            (cond
              [(empty? column) (- pos 1)]
              [(not (= (first column) BLANK))
               (- pos 1)]
              [else (first-blank (rest column) (+ 1 pos))]))
          (define (get-moves board-state column)
            (cond
              [(empty? board-state) empty]
              [else
               (local [(define blank (first-blank (first board-state) 0))]
                 (if (< blank 0)
                     (get-moves (rest board-state) (+ 1 column))
                     (cons
                      (list column (first-blank (first board-state) 0))
                      (get-moves (rest board-state) (+ 1 column)))))]))]
    (get-moves (world-state-position state)
               0)))


;; check-win:  world-state --> boolean
;; determines whether the game has ended with a victory for whoever just moved
(define (check-win? state)
  (local [
          ;; !!! should go back and fix these functions with piece-at function
          (define (up-column board color x y)
            (if (< y 3)
                false
                (local [(define column (get-nth x board))]
                  (= (get-nth  (- y 1) column)
                     (get-nth  (- y 2) column)
                     (get-nth  (- y 3) column)
                     color))))
          
          (define (right-row board color x y)
            (if (>= x (- COLUMNS 3))
                false
                (= (get-nth y (get-nth (+ 1 x) board))
                   (get-nth y (get-nth (+ 2 x) board))
                   (get-nth y (get-nth (+ 3 x) board))
                   color)))
          
          (define (up-right board color x y)
            (if (or (< y 3)
                    (>= x (- COLUMNS 3)))
                false
                (= (get-nth (- y 1) (get-nth ( + x 1) board))
                   (get-nth (- y 2) (get-nth ( + x 2) board))
                   (get-nth (- y 3) (get-nth ( + x 3) board))
                   color)))
          
          (define (down-right board color x y)
            (if (or (>= y (- ROWS 3))
                    (>= x (- COLUMNS 3)))
                false
                (= (get-nth (+ y 1) (get-nth ( + x 1) board))
                   (get-nth (+ y 2) (get-nth ( + x 2) board))
                   (get-nth (+ y 3) (get-nth ( + x 3) board))
                   color)))
          
          (define (victory? board x y)
            (let
                ([color (get-nth y (get-nth x board))])
              (if (= color BLANK)
                  false
                  (or
                   (up-column board color x y)
                   (right-row board color x y)
                   (up-right board color x y)
                   (down-right board color x y)))))
          
          (define (walk-column board col row)
            (cond
              [(= row ROWS) false]
              [else
               (or
                (victory? board col row)
                (walk-column board col (+ 1 row)))]))
          
          (define (walk-board board col)
            (cond
              [(= col COLUMNS) false]
              [else
               (or (walk-column board col 0)
                   (walk-board board (+ 1 col)))]))]
    (walk-board (world-state-position state) 0)))


(define START-BOARD
  (make-list COLUMNS
             (make-list ROWS BLANK)))
(define start-state
  (make-world-state START-BOARD RED 5 empty))

(define test-board
  (list
   (list 1 2 3 4 5 6 7 8 9)
   (list 11 12 13 14 15 16 17 18 19)
   (list 21 22 23 24 25 26 27 28 29)
   (list 31 32 33 34 35 36 37 38 39)
   (list 0 0 0 0 0 0 0 0 0)
   (list 0 0 0 0 0 0 0 0 0)
   (list 0 0 0 0 0 0 0 0 0)
   (list 0 0 0 0 0 0 0 0 0)))

(define test-board-2
  (list
   ;     ROW
   ;     0 1 2 3 4 5 6 7 8 9 
   (list 0 0 0 0 0 0 0 0 0 0) ;0   COLMUN
   (list 0 0 0 0 0 0 0 1 1 1) ;1
   (list 0 0 0 0 0 0 0 0 2 1) ;2
   (list 0 0 0 0 0 0 0 0 1 2) ;3
   (list 0 0 0 0 0 0 0 0 0 0) ;4
   (list 0 0 0 0 0 0 0 0 0 2) ;5
   (list 0 0 0 0 0 0 0 0 0 0) ;6
   (list 0 0 0 0 0 0 0 0 2 1) ;7
   (list 0 0 0 0 0 0 0 0 0 1) ;8
   (list 0 0 0 0 0 0 0 0 0 0))) ;9

(define test-state (make-world-state test-board RED 5 empty))
