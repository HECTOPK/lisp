(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
(list entry left right))

(define (partial-tree elts n)
(if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))


(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define (intersection-ordered-list-set set1 set2)

(if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-ordered-list-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-ordered-list-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-ordered-list-set set1 (cdr set2)))))))


(define (union-ordered-list-set set1 set2)
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

(define (intersection-set set1 set2)
  (list->tree (intersection-ordered-list-set (tree->list set1) (tree->list set2))))

(define (union-set set1 set2)
  (list->tree (union-ordered-list-set (tree->list set1) (tree->list set2))))

(define set1 (list->tree '(1 2 3 4 5 6 7 8)))

(define set2 (list->tree '(5 6 7 8 9 10 11 12)))


(tree->list (intersection-set set1 set2)) ; (5 6 7 8)

(tree->list (union-set set1 set2)) ; (1 2 3 4 5 6 7 8 9 10 11 12)