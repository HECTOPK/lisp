(define (reverse l)
	(define (iter l r)
    (if (null? l)
      r
      (iter (cdr l) (cons (car l) r))))
  (iter l '()))

(define (odd? n) (= (remainder n 2) 1))
(define (even? n) (= (remainder n 2) 0))

(define (same-parity n . l)
  (define check? (if (odd? n) odd? even?))
  (define (iter l r)
    (if (null? l)
      r
      (if (check? (car l))
        (iter (cdr l) (cons (car l) r))
        (iter (cdr l) r))))
  (reverse (iter l (list n))))

(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7) ; (2 4 6)
