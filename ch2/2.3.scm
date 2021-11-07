(define (make-rect a b c d l) (cons (cons (cons a b) (cons c d)) l))

(define (sq n) (* n n))

(define (width-rect r) 
  (let (
    (a (car (car (car r))))
    (b (cdr (car (car r))))
    (c (car (cdr (car r))))
    (d (cdr (cdr (car r))))
    )
    (sqrt (+ (sq (- c a)) (sq (- d b))))
  ))

(define (heigh-rect r) (cdr r))

(define (square r) (* (width-rect r) (heigh-rect r)))

(define (perimeter r) (* 2 (+ (width-rect r) (heigh-rect r))))


(square (make-rect 1 1 9 7 5)) ; 50
(perimeter (make-rect 1 1 9 7 5)) ; 30
