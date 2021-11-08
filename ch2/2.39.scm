(define nil '())

(define (fold-right op init seq)
	(if (null? seq)
		init	
		(op (car seq) (fold-right op init (cdr seq)))))


(define (fold-left op init  seq)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest)) (cdr rest))))
	(iter init seq))

(define (reverse seq)
	(fold-right (lambda (x y) (append y (list x))) nil seq))

(reverse (list 1 2 3 4)) ; (4 3 2 1)

(define (reverse seq)
	(fold-left (lambda (x y ) (cons y x)) nil seq))

(reverse (list 1 2 3 4)) ; (4 3 2 1)