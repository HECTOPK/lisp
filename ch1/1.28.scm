(define (even? n) (= (remainder n 2) 0))


(define (expmod base exp n result)
  (newline)
  (display base)
  (newline)
  (display exp)
  (newline)
  (display n)
  (newline)
  (display result)
  (newline)
  ((lambda (base result)
    (if (= exp 2)
      (remainder (* base base result) n)
      (if (even? exp)
        ((lambda (sq) 
          (if (and (= (remainder sq n) 1) (or (= base 1) (= base (- n 1))))
            0
            (expmod (square base) (/ exp 2) n result))) (square base))
        (expmod base (- exp 1) n (* result base))))) (remainder base n) (remainder result n)))

;; (define (expmod base exp m)
;; (newline)
;; (display base)
;; (newline)
;; (display exp)
;; (newline)
;; (display m)
;; (cond ((= exp 0) 1)
;;       ((even? exp)
;;         (expmod base (/ exp 2) m)
;;         ((lambda (res) 
;;           (if (and (= res 1) (or (= base 1) (= base (- m 1))))
;;             0
;;             res)
;;           ) (remainder (square (expmod base (/ exp 2) m)) m))
;;         (remainder (square (expmod base (/ exp 2) m)) m))
;;       (else
;;         (remainder (* base (expmod base (- exp 1) m)) m))))


(define (miller-rabin-test n) 
  (define (iter i n)
    (newline)
    (display i)
    (newline)
    (display n)
    (newline)
    (if (> i n)
      1
      ((lambda (exp) 
        (newline)
        (display "(expmod ")
        (display i)
        (display " ")
        (display (- n 1))
        (display " ")
        (display n)
        (display " 1) = ")
        (display exp)
        (if (= exp 0)
          0
          (iter (+ i 2) n))) (expmod i (- n 1) n 1))))
  (if (even? n)
    0
    (iter 2 n)))

(miller-rabin-test 97)



;; (expmod 6 11 12 1)