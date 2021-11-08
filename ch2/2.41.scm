(define nil '())

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


(define (flatmap proc seq)
	(accumulate append nil (map proc seq)))



(define (find-nums n s)
	(define (nums-filter l)
		(let ((i (car l)) (j (cadr l)) (k (caddr l)))
			(and 
				(not (or (= i j) (= j k) (= i k))) 
				(> k 0) 
				(< k n)))
		
		)
	(filter nums-filter
	(flatmap 
		(lambda (i) (map 
	 		(lambda (j) (list i j (- s i j)))
	 		(enumerate-interval 1 n))) 
	 	(enumerate-interval 1 n))
	)

)

(find-nums 6 6) ; ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))