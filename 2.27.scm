(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse-iter x r)
	(display x)
	(display r)
	(newline)
	(cond ((null? x) x)
		  (else (append (deep-reverse (cdr x))  (deep-reverse (car x))))))

(define (deep-reverse x) 
	(display x)
	(newline)
	(cond ((null? x) x)
		  ((not (pair? x))  x)
		  ((null? (cdr x)) (list (deep-reverse (car x))))
	      (else (cons (deep-reverse (cdr x))  (deep-reverse (car x))))))
	; (if (not (pair? x))
	; 	x
	; 	((deep-reverse-iter x (list)))))

(deep-reverse x)
(count-leaves x)