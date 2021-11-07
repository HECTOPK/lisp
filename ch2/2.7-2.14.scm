(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (cross-zero? x) 
 (and (>= (upper-bound x) 0) (<= (lower-bound x) 0)))

(define (div-interval x y)
  (if (cross-zero? y)
      (error "cross zero interval " y)
      (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (sub-interval x y) 
  (make-interval (- (lower-bound y) (upper-bound x)) 
                 (- (upper-bound y) (lower-bound x))))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent x) 
  (* 100 (/ (width x) (center x))))


(define (make-center-percent x r) (make-center-width x (* x (/ r 100))))


(define A (make-center-percent 100 1))
(define B (make-center-percent 200 2))

(define AdivB (div-interval A B))
(define AdivA (div-interval A A))

AdivA
AdivB

(center AdivB)
(percent AdivB)

(center AdivA)
(percent AdivA)