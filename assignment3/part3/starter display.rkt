;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |starter display|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  ;; not sure if i should have this???  maybe good for testing???
(define TAB 5) ; someone senior told me this constant might help

;; so much easier in ISL-lambda than BSL (just don't tell the boss)
;; Natural -> String
;; creates a blank string of length equal to n
(check-expect (blanks 0) "")
(check-expect (blanks 3) "   ")
(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))