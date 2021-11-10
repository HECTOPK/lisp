(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (make-list entry) (make-tree entry '() '()))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define t1 (make-tree 7 (make-tree 3 (make-list 1) (make-list 5)) (make-tree 9 '() (make-list 11))))
(define t2 (make-tree 3 (make-list 1) (make-tree 7 (make-list 5) (make-tree 9 '() (make-list 11)))))

(tree->list-1 t1) ; (1 3 5 7 9 11)
(tree->list-2 t1) ; (1 3 5 7 9 11)

(tree->list-1 t2) ; (1 3 5 7 9 11)
(tree->list-2 t2) ; (1 3 5 7 9 11)

(quotient 5 2)

