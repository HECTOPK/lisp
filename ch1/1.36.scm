(define tolerance 0.1)

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

(fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0)

(fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) 2.0)