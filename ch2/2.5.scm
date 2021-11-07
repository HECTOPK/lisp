(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (cons x y)
	(* (cons-iter x 2 1) (cons-iter y 3 1)))

(define (cons-iter n m r) 
	(if (= n 0)
		r
		(cons-iter(- n 1) m (* r m))))

(define (car a)
	(car-iter a 2 0))

(define (cdr a)
	(car-iter a 3 0))

(define (car-iter a m r)
	(if (= (gcd a m) m)
		(car-iter (/ a m) m (+ r 1))
		r))


(cdr (cons 10 20))
(car (cons 10 20))