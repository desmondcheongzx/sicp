#lang sicp

(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (derivative exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (derivative (addend exp) var)
		   (derivative (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(derivative (multiplicand exp) var))
	  (make-product (multiplicand exp)
			(derivative (multiplier exp) var))))
	(else (display ":("))))

(define (variable? e) (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
	((=number? b 0) a)
	((and (number? a) (number? b)) (+ a b))
	(else (list a '+ b))))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
	((=number? a 1) b)
	((=number? b 1) a)
	((and (number? a) (number? b)) (* a b))
	(else (list a '* b))))

(define (=number? x num)
  (and (number? x) (= x num)))

(define (sum? x)
  (define (iter cur right)
    (cond ((eq? cur '+) #t)
	  ((pair? right) (iter (car right)
			       (cdr right)))
	  (else #f)))
  (and (pair? x) (iter (car x) (cdr x))))

(define (strip exp)
  (if (not (pair? (cdr exp)))
      (car exp)
      exp))

(define (addend s)
  (define (iter left cur right)
    (if (eq? cur '+)
	left
	(iter (append left (list cur))
	      (car right) (cdr right))))
  (strip (iter '()  (car s) (cdr s))))
  

(define (augend s)
  (define (iter cur right)
    (if (eq? cur '+)
	right
	(iter (car right) (cdr right))))
  (strip (iter (car s) (cdr s))))

(define (product? x)
  (define (iter cur right)
    (cond ((eq? cur '*) #t)
	  ((pair? right) (iter (car right)
			       (cdr right)))
	  (else #f)))
  (and (pair? x) (iter (car x) (cdr x))))

(define (multiplier p)
  (define (iter left cur right)
    (if (eq? cur '*)
	left
	(iter (append left (list cur))
	      (car right) (cdr right))))
  (strip (iter '()  (car p) (cdr p))))

(define (multiplicand p)
  (define (iter cur right)
    (if (eq? cur '*)
	right
	(iter (car right) (cdr right))))
  (strip (iter (car p) (cdr p))))
