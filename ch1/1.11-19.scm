; 1.11
(define (fib-r n)
	(cond
		((< n 3) n)
		(else
			(+
				(fib-r (- n 1))
				(fib-r (- n 2))
				(fib-r (- n 3))
			)
		)
	)
)

(define (fib-i n)

)

(define (fib-iter a b c count)
	(if (= count 0)
		c
		(fib-iter b c (+ a b c) (- count 1) )
	)
)

; 1.12
; a - строка, b - номер в строке
(define (pascal a b)
	(if
		(or (= a b) (= b 1))
		1
		(+
			(pascal (- a 1) (- b 1))
			(pascal (- a 1) b)
		)
	)
)

; 1.16
(define (fexp b n)
	(fexp-i b n 1)
)

(define (fexp-i b n a)
	(if
		(= n 0)
		a
		(if
			(even? n)
			(fexp-i (* b b) (/ n 2) a)
			(fexp-i b (- n 1) (* a b))
		)
	)
)

; 1.17
(define (fmul-r a b)
	(if (= b 1)
		a
		(if
			(even? b)
			(double fmul(a halve(b)))
			(+ a fmul(a (- b 1))
		)
	)
)

; 1.18
(define (fmul a b)
	fmul-1(a b 0)
)

(define (fmul-i a b x)
	(if (= b 0)
		x
		(if
			(even? b)
			fmul-i((double a) (half b) x)
			fmul-i(a (- b 1) (+ x a))
		)

	)

)

; 1.19
(define (fib n)
	(fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q count)
	(cond
		((= count 0) b)
		((even? count) (fib-iter
			a
			b
			(+ (* p p) (* q q))
			(+ (* q q) (* 2 p q))
			(/ count 2))
	)
	(else (fib-iter
		(+ (* b q) (* a q) (* a p))
		(+ (* b p) (* a q))
		p
		q
		(- count 1))
	)
)
