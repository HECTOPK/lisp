
(define (reverse l)
	(reverse-iter (cdr l) (list (car l))))


(define (reverse-iter l n) 
	(newline)
	(display l)
	(display n)
	(newline)
	(if (null? l)
		n
		(reverse-iter (cdr l) (cons (car l) n))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))



(define (odd? n) (= (gcd n 2) 1))
(define (even? n) (= (gcd n 2) 2))

(define (same-parity n . l)
	(if (odd? n)
		(reverse (same-parity-iter odd? l (list n)))
		(reverse (same-parity-iter even? l (list n)))))


(define (same-parity-iter checker l r) 
	(newline)
	(display l)
	(display r)
	(newline)
	(if (null? l)
		r
		(if (checker (car l))
			(same-parity-iter checker (cdr l) (cons (car l) r))
			(same-parity-iter checker (cdr l) r))))


(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
