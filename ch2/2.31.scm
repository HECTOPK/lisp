(define x (list 1 ( list 2 ( list 3 4) 5) (list 6 7)))

(define (tree-map f tr)
	(map 
		(lambda (tr)
			(if (pair? tr) (tree-map f tr) (f tr)))
		tr))

(define (square-tree tree)
	(tree-map square tree))

(square-tree x) ; (1 (4 (9 16) 25) (36 49))