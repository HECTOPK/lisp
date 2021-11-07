(define (first l) (car l))
(define (second l) (cadr l))

(define (make-mobile left right)
	(list left right))

(define (make-branch length structure)
	(list length structure))

(define (left-branch mobile) (first mobile))
(define (right-branch mobile) (second mobile))

(define (branch-length branch) (first branch))
(define (branch-structure branch) (second branch))

(define (total-weight mobile) 
	(define (structre-weight structure)
		(if (pair? structure)
			(total-weight structure)
			structure))
	(+ (structre-weight (branch-structure (left-branch mobile)))
		 (structre-weight (branch-structure (right-branch mobile)))))

(define (balanced? mobile)
	(define (iter mobile)
		(define (balanced-structure structure)
			(if (pair? structure)
				(iter structure)
				(cons true structure)))
		(let (
			(leftb (left-branch mobile))
			(rightb (right-branch mobile)))
			(let (
				(left-balanced (balanced-structure (branch-structure leftb)))
				(right-balanced (balanced-structure (branch-structure rightb))))
				(if (or (not (car left-balanced)) (not (car right-balanced)))
					(cons false 0)
					(if (= 
							(* (branch-length leftb) (cdr left-balanced))
							(* (branch-length rightb) (cdr right-balanced)))
						(cons true (+ (cdr right-balanced) (cdr left-balanced)))
						(cons false 0))
				))))
	(car (iter mobile)))

(define m (make-mobile (make-branch 1 (make-mobile (make-branch 1 1) (make-branch 1 1))) (make-branch 2 1)))

(total-weight m) ; 4

(balanced? m) ; true