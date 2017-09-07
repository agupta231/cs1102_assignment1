;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname prob6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define STR1 "hello, world.")
(define STR2 "how now brown cow")
(define STR3 "third string in the list!")
(define LOS (list STR1 STR2 STR3))

(check-expect (concat-strings LOS)
              (cons "hello, world." (cons "how now brown cow" (cons "THIRD STRING IN THE LIST!" empty))))

;; helper function for upcase-string
(define (upcase-helper loc loc2)
  (cond
    [(empty? loc) (reverse loc2)]
    [else
     (upcase-helper 
      (rest loc)
      (cons
       (char-upcase (first loc)) loc2))]))
    

;String -> String
;converts a string to uppercase
;; start the accumulator and some string hacking
(define (upcase-string str)
  (list->string (upcase-helper (string->list str) empty)))

(check-expect (upcase-string "abCDe!") "ABCDE!")

;; ListOfString is one of:
;;   - empty
;;   - (cons String ListOfString)
;; interp. a list of strings

(define los1 empty)
(define los2 (cons "string" empty))
(define los3 (cons "strin1" (cons "sting2" empty)))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (rest los))]))

;; Template rules used:
;; - one of: 2 cases
;; - atmoic distinct: empty
;; - compound: (cons String ListOfString)

;; ListOfString -> ListOfString
;; Return same list of strings, but all the ones that end in "!" are capitalized

;(check-expect (concat-strings "abCDe!") "ABCDE!")

;(define (concat-string los) "Our Lord and Savoir Gregor") ;Stub

(define (concat-strings los)
  (cond [(empty? los) empty]
        [else
         (if (string=? (substring (first los) (- (string-length (first los)) 1)) "!")
             (cons (upcase-string (first los)) (concat-strings (rest los)))
             (cons (first los) (concat-strings (rest los))))]))