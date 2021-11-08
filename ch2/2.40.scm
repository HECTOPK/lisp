(define nil '())

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


(define (enumerate-interval a b)
	(define (iter a b res)
		(if (= a b)
			(cons a res)
			(iter a (- b 1) (cons b res))))
	(if (< b a)
		(error "bad interval")
		(iter a b nil)))

(define (accumulate op init seq)
	(if (null? seq)
		init	
		(op (car seq) (accumulate op init (cdr seq)))))


(define (prime-sum? pair)

(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (flatmap proc seq)
	(accumulate append nil (map proc seq)))

(define (unique-pairs n)
	(flatmap 
		(lambda (i) (map
			(lambda (j) (list j i))
			(enumerate-interval 1 (- i 1))
		))
		(enumerate-interval 2 n)
	))

(define (prime-sum-pairs n)
	(map make-pair-sum 
		(filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6) ; ((1 2 3) (2 3 5) (1 4 5) (3 4 7) (2 5 7) (1 6 7) (5 6 11))