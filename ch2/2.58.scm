(define (accumulate op initial sequence)
(if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))



(define (deriv exp var)
(newline)
(display exp)
(cond ((number? exp) 0)
      ((variable? exp)
       (if (same-variable? exp var) 1 0))
      ((sum? exp)
       (make-sum (deriv (addend exp) var)
                 (deriv (augend exp) var)))
      ((product? exp)
       (make-sum
         (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
         (make-product (deriv (multiplier exp) var)
                       (multiplicand exp))))
      (else
       (error "unknown expression type -- DERIV" exp))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (has-symbol? l s)
  (accumulate (lambda (x y) (or (same-variable? x s) y)) #f l))

(define (sum? x)
  (has-symbol? x '+))

(define (addend s)
  (define (iter l result)
      (if (same-variable? (car l) '+)
        result
        (iter (cdr l) (cons (car l) result))))
  (debrackets (reverse (iter s '()))))

(define (debrackets l)
  (if (pair? l)
    (if (null? (cdr l))
      (car l)
      l)
    l))

(define (augend s)
  (define (iter l founded result)
    (if (null? l) result
    (if founded (iter (cdr l) founded (cons (car l) result))
    (if (same-variable? (car l) '+)
      (iter (cdr l) #t '())
      (iter (cdr l) #f '())))))
  (debrackets (reverse (iter s #f '()))))

(define (product? x)
  (and (has-symbol? x '*)
  (not (has-symbol? x '+))))

(define (multiplier p) 
  (car p))

(define (multiplicand p) 
  (caddr p))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2)) (+ a1 a2))
      (else (list a1 '+ a2))))

(define (=number? exp num)
(and (number? exp) (= exp num)))

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      (else (list m1 '* m2))))


(deriv '(x + x * x) 'x) ; (1 + (x + x))
(deriv '(x + x * x + x) 'x) ; (1 + ((x + x) + 1))
