(define (element-of-set? x set)
(cond ((null? set) false)
      ((= x (car set)) true)
      ((< x (car set)) false)
      (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
(if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (define (iter x added set result)
    (cond 
      ((null? set) (if added result (cons x result)))
      ((and (not added) (< x (car set))) (iter x #t set (cons x result)))
      (else (iter x added (cdr set) (cons (car set) result)))))
  (if (element-of-set? x set)
    set
    (reverse (iter x #f set '()))))



(define (union-set set1 set2)
  (define (iter set1 set2 result)
    (cond 
      ((and (null? set1) (null? set2)) result)
      ((null? set1) (iter set1 (cdr set2) (cons (car set2) result)))
      ((null? set2) (iter set2 (cdr set1) (cons (car set1) result)))
      ((= (car set1) (car set2)) (iter (cdr set1) (cdr set2) (cons (car set1) result)))
      ((> (car set1) (car set2)) (iter set1 (cdr set2) (cons (car set2) result)))
      (else (iter (cdr set1) set2 (cons (car set1) result)))
    ))
  (reverse (iter set1 set2 '())))

(adjoin-set 5 '(1 2 5 7)) ; (1 2 5 7)
(adjoin-set 5 '(1 2 7)) ; (1 2 5 7)
(adjoin-set 5 '(6 7)) ; (5 6 7)
(adjoin-set 5 '(1 2)) ; (1 2 5)
(adjoin-set 5 '()) ; 

(union-set '(1 2 3 4) '(3 4 5 6)) ; (1 2 3 4 5 6)
(union-set '(3 4 5 6) '(1 2 3 4)) ; (1 2 3 4 5 6)