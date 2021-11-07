(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)))))

(define one (add-1 zero))

(define two (add-1 one))

(define (add a b) 
	(lambda (f) (lambda (x)  ((b f) ((a f) x)))) )




(define (pone x) (+ x 1))

(define (calc-number n) 
	((n pone) 0))

(add one two)

(calc-number (add (add two two) two))