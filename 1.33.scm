(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (filtered-accumulate combiner null-value filter term a next b)
(define (iter a result)
  (if (> a b)
    result
    (if (filter a)
      (iter (next a) (combiner result (term a)))
      (iter (next a) result))))
(iter a null-value))

(define (square x)
  (* x x))


(define (inc x)
  (+ x 1))

(define (sum-prime-squares a b)
  (filtered-accumulate + 0 prime? square a inc b))

(sum-prime-squares 1 5)


(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (id x) x)

(define (func n)
  (define (gcdn a) (= (gcd a n) 1 ))
  (filtered-accumulate * 1 gcdn id 1 inc n))


(func 5)