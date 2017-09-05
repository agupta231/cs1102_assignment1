;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname prob4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define IMG1 (triangle  10  "outline" "green"))
(define IMG2 (ellipse 80 45 "solid" "blue"))

;; Image Image -> Image
;; Returns overlayed image with minimal scaling

(define (scale-image i1 i2) 
	(if (< (abs (- (image-width i1) (image-width i2))) (abs (- (image-height i1) (image-height i2))))
		(overlay ;If the width is the smallest factor
			(scale
				(/ (image-width i2) (image-width i1))
				i1
			)
			i2
		)
		(overlay ;If the height is the smallest factor
			(scale 
				(/ (image-height i2) (image-height i1))
				i1
			)
			i2
		)
	)
)

