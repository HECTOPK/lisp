(define nil '())

(define (accumulate op init seq)
	(if (null? seq)
		init	
		(op (car seq) (accumulate op init (cdr seq)))))

(define (firsts seqs)
	(map car seqs))

(define (no-firsts seqs)
	(map cdr seqs))

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		'()
		(cons (accumulate op init (firsts seqs))
					(accumulate-n op init (no-firsts seqs)))))

(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map dot-product (map (lambda (x) v) m) m))

(define (transpose mat)
	(accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda (x) (matrix-*-vector cols x)) m)))


(define m (list
(list 1 2 3)
(list 4 5 6)
(list 7 8 9)))

(define v (list 1 1 1))


(dot-product v v) ; 3

(matrix-*-vector m v) ; (6 15 24)

(transpose m) ; ((1 4 7) (2 5 8) (3 6 9))

(matrix-*-matrix m m) ; ((30 36 42) (66 81 96) (102 126 150))