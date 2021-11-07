(define (iterative-improve good-enough? improve) 
  (define (iter x next) 
    (if (good-enough? x next)
      next
      (iter next (improve next))))
  (lambda (x) (iter x (improve x))))

(define (make-improve x)
  (lambda (guess)
    (average guess (/ x guess))))

(define (average x y)
  (/ (+ x y) 2))

(define (make-good-enough x)
  (lambda (prev guess) 
    (< (abs (- (square guess) x)) 0.001)))

(define (sqrt x)
  ((iterative-improve (make-good-enough x) (make-improve x)) 1.0))

(sqrt 2) ; 1.41




(define (close-enough x y)
    (< (abs (- x y)) 0.0001))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough f) first-guess))

(fixed-point cos 1.0) ; 0.739