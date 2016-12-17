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
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation
			 (base exp)
			 (make-sum
			  (exponent exp)
			  -1)))
	  (derivative (base exp) var)))
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
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
	((=number? n 1) x)
	((and (number? x) (number? n)) (expt x n))
	(else (list '** x n))))
