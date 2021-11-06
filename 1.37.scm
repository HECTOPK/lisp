(define (cont-frac-iter n d k div)
  (newline)
  (display k)
  (newline)
  (display div)
  (newline)
  (if (= k 0)
    div
    (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) div)))))

(define (cont-frac n d k)
  (cont-frac-iter n d (- k 1) (/ (n k) (d k))))


(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13))