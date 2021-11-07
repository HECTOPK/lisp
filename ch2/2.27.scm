(define (deep-reverse l)
	(define (iter l r)
		(cond ((null? l) r)
			((not (pair? (car l))) (iter (cdr l) (cons (car l) r)))
			(else (iter (cdr l) (cons (deep-reverse (car l)) r)))))
	(iter l '()))

(deep-reverse (list (list 1 2) (list 3 4))) ; ((4 3) (2 1))