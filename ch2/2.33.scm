(define (accumulate op init seq)
	(if (null? seq)
		init	
		(op (car seq) (accumulate op init (cdr seq)))))

(define (map p seq)
	(accumulate (lambda (x y) (cons (p x) y)) '() seq))

(define (subsets s)
	(if (null? s)
		(list '())
		(let ((rest (subsets (cdr s))))
			(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define (append seq1 seq2)
	(accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)

(define (len seq)
	(accumulate (lambda (x y) (+ y 1)) 0 seq))

(len (list 1 2 3 4 5)) ; 5