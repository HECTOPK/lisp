(define (equal? a b)
  (newline)
  (display a)
  (display "\t\t")
  (display b)
  (if (and (pair? a) (pair? b))
    (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
    (if (and (not (pair? a)) (not (pair? b)))
      (eq? a b)
      false)))

(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f
