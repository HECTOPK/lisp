(define (inc x) (+ x 1))

(define (double proc) (lambda (x) (proc (proc x))))

((double inc) 0)

(((double (double double)) inc) 5)