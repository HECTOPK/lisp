(define x (list 1 ( list 2 ( list 3 4) 5) (list 6 7)))

(define (square-tree tree)
	(cond (
		(null? tree) '())
		((not (pair? tree)) (* tree tree))
		(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(square-tree x) ; (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
	(map 
		(lambda (tree)
			(if (pair? tree) (square-tree tree) (* tree tree)))
		tree))

(square-tree x) ; (1 (4 (9 16) 25) (36 49))