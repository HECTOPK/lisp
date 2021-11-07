(define (summ f x d max sum)
  (if (> x max)
    sum
    (summ f (+ x d) d max (+ sum (f x)))))


(define (integr f a b n h)
  (* 
    (/ h 3.0) 
    (+ 
      (f a) 
      (f b) 
      (* 4 (summ f (+ a h) (+ h h) b 0))
      (* 2 (summ f (+ a h h) (+ h h) (- b h) 0)))))

(define (integral-simp f a b n)
  (if (= (remainder n 2) 1)
    (integral-simp f a b (+ n 1))
    (integr f a b n (/ (- b a) n))))


(define (cube x)
  (* x x x))


(integral-simp cube 0.0 1.0 20) ; 1/4