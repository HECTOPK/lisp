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


(define (adjoin-position row col rest)
	(cons (cons row col) rest))

(define empty-board nil)

(define (on-fire? q1 q2)
	(or 
		(= (car q1) (car q2))
		(= (cdr q1) (cdr q2))
		(= (abs (- (car q1) (car q2))) (abs (- (cdr q1) (cdr q2))))
	))

(define (safe? k positions)
	(define (iter new pos)
		(if (null? pos)
			true
			(if (on-fire? new (car pos))
				false	
				(iter new (cdr pos)))))
	(iter (car positions) (cdr positions)))

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map (lambda (new-row)
									(adjoin-position new-row k rest-of-queens))
								(enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))


(queens 4) ; (((3 . 4) (1 . 3) (4 . 2) (2 . 1)) ((2 . 4) (4 . 3) (1 . 2) (3 . 1)))