(define (make-segment a b) (cons a b))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))


(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))


(define (make-rect ab cd) (cons ab cd))

(define (a-point p) (car (car p)))

(define (b-point p) (cdr (car p)))

(define (c-point p) (car (cdr p)))

(define (d-point p) (cdr (cdr p)))

(define (square r) (x-point (a-point r)) (a-point r))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s) 
	(make-point 
		(/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
		(/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)
	))

; (print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 1 1)))
; (define mp (midpoint-segment (make-segment (make-point 0 0 ) (make-point 1 1))))
; (print-point (midpoint-segment (make-segment (make-point 0 10 ) (make-point 1 1))))
