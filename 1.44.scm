(define (square x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated-iter f n result)
  (if (= n 0)
    result
    (repeated-iter f (- n 1) (compose f result))))

(define (repeated f n) 
  (repeated-iter f (- n 1) f))

(define dx 0.001)

(define (smooth f) (lambda (x) (/ (+ (f (- x dx)) (f (+ x dx))) 2)))

(define (nsmooth f n) 
  (repeated smooth n))