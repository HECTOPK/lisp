(define (cont-frac-iter n d k div)
  (newline)
  (display k)
  (newline)
  (display div)
  (newline)
  (display (d k))
  (newline)
  (if (= k 0)
    div
    (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) div)))))

(define (cont-frac n d k)
  (cont-frac-iter n d (- k 1) (/ (n k) (d k))))

(define (tan-cf x k)
  (/ x (- 1 (cont-frac (lambda (i) (* i i)) (lambda (i) (+ 1 (* i 2))) k))))


(tan-cf (/ 3.14 4) 100)