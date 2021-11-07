(define (for-each f l)
  (define (iter l)
    (if (null? l)
      true
      ((lambda (x) (f x) (iter (cdr l))) (car l))))
  (iter l))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))