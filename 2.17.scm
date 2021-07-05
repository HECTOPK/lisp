(define (last-pair l) 
	(if (null? (cdr l))
		(car l)
		(last-pair (cdr l))))

(last-pair (list 23 72 149 34))

(define (reverse l)
	(reverse-iter (cdr l) (list (car l))))


(define (reverse-iter l n) 
	(newline)
	(display l)
	(display n)
	(newline)
	(if (null? l)
		n
		(reverse-iter (cdr l) (cons (car l) n))))

(reverse (list 1 4 9 16 25))