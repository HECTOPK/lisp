(define (square x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated-iter f n result)
  (if (<= n 0)
    result
    (repeated-iter f (- n 1) (compose f result))))

(define (repeated f n) 
  (repeated-iter f (- n 1) f))


(define tolerance 0.001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average-damp-n f n)
  ((repeated average-damp 2) f))


(define (pow x n) ((repeated (lambda (y) (* x y)) n) 1))

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))



(define (root x n) 
  (fixed-point (average-damp-n (lambda (y) (/ x (pow y (- n 1))))  (/ (log n) (log 2))) 1.0))

(define power 26)

(define result (root 2 power))

result

(pow  result power)

((repeated (lambda (x) (+ x 1)) 2) 0)