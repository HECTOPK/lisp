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

(define (eseq i) 
  (if (= (remainder (+ i 1) 3) 0)
    (* (/ (+ i 1) 3) 2)
    1))

(+ 2 (cont-frac (lambda (i) 1.0) eseq 10))
