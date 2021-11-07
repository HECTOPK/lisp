(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))       

(define (test cur num)
  (if (= cur 1)
    1
    (if (= (expmod cur num num) cur)
      (test (- cur 1) num)
      0)))

(define (karm-test num)
  (test (- num 1) num))

(karm-test 100)
(karm-test 561)
(karm-test 1105)
(karm-test 1729)
(karm-test 2465)
(karm-test 2821)
(karm-test 6601)