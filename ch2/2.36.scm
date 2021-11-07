(define (accumulate op init seq)
	(if (null? seq)
		init	
		(op (car seq) (accumulate op init (cdr seq)))))

(define (reverse l)
	(define (iter l r)
		(if (null? l)
			r
			(iter (cdr l) (cons (car l) r))))
	(iter l '()))

(define (firsts seqs)
	(map car seqs))

(define (no-firsts seqs)
	(map cdr seqs))

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		'()
		(cons (accumulate op init (firsts seqs))
					(accumulate-n op init (no-firsts seqs)))))



(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) ; (22 26 30)