(define (accumulate op init seq)
	(if (null? seq)
		init	
		(op (car seq) (accumulate op init (cdr seq)))))

(define (count-leaves t)
	(newline)
	(display t)
	(accumulate (lambda (x y) (+ y (if (pair? x)
		(count-leaves x) 1))) 0 t))

(count-leaves (list (list 1 2 3) 2 (list 1 2))) ; 6