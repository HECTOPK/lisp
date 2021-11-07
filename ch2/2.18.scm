(define (reverse l)
	(define (iter l r)
    (if (null? l)
      r
      (iter (cdr l) (cons (car l) r))))
  (iter l '()))

(reverse (list 1 4 9 16 25))