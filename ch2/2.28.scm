(define (reverse l)
(define (iter l r)
	(if (null? l)
		r
		(iter (cdr l) (cons (car l) r))))
(iter l '()))

(define (fringe l)
	(define (iter l r)
		(cond ((null? l) r)
			((not (pair? (car l))) (iter (cdr l) (cons (car l) r)))
			(else (iter (cdr l) (append r (reverse (fringe (car l))))))))
	 (iter l '()))

(define x (list (list 1 2) (list 3 4)))

(fringe x) ; (1 2 3 4)