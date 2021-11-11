(define (encode message tree)
(if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (define (iter node)
    (if (leaf? node)
      (cons '() (eq? (symbol-leaf node) s))
      (let 
        ((left-result (iter (left-branch node)))
        (right-result (iter (right-branch node))))  
        (cond 
          ((cdr left-result) (cons (cons 0 (car left-result)) #t))
          ((cdr right-result) (cons (cons 1 (car right-result)) #t))
          (else (cons '() #f))))))
  (let ((res (iter tree)))
    (if (cdr res) 
      (car res) 
      (error "no symbol in tree"))))



(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(encode '(A D A B B C A) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)