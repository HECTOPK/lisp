(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (id x)
  x)

(define (inc x)
  (+ x 1))

(define (factorial n)
  (product id 1 inc n))

(factorial 5)




(define (term x)
  (/ (* x x) (* (- x 1) (+ x 1))))

(define (plus2 x)
  (+ x 2))


(define (pi n)
  (* (/ 8 3) (product term 4 plus2 n)))

(pi 50)

