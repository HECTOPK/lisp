(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
	(let (
		(is-prime (start-prime-test n (runtime)))
	))
	(display n)
	(newline)
	(is-prime))


(define (start-prime-test n start-time)
	(let (
		(is-prime (prime? n))
	))
	(if is-prime
		(report-prime (- (runtime) start-time))
		(report-not-prime)))
	(is-prime)

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (report-not-prime)
  (display " not prime "))


(define (find-next n e)
	(if (> e 0)
		(
			(if (timed-prime-test n))
				(find-next (+ n 1) (- e 1))
				(find-next (+ n 1) e)
			)
		)
	)

)

(define (find-next-primes n)
	(find-next n 3)

)



(find-next-primes 100)
