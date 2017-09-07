;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname upcase_custom) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Ankur Gupta
;; Sept 2, 2017
;; agupta4@wpi.edu
;; github.com/agupta231

;; String -> String
;; Converts String to Uppercase
(check-expect (upcase-string "abcde!") "ABCDE!")
(check-expect (upcase-string "aCbE!") "ACBE!")
(check-expect (upcase-string "") "")

;(define (upcase-string str) "") ;stub

;(define (upcase-string str) ;template
;    (... str))

(define (upcase-string str) 
  (cond
    [(= (string-length str) 0) ""]
    [else (string-append
           (string (char-upcase (string-ref str 0)))
           (upcase-string (substring str 1)))]))
