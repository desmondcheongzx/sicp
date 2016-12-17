#lang sicp

(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (member? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else (member? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set a b)
  (cond ((null? a) b)
	((null? b) a)
	((= (car a) (car b))
	 (cons (car a) (union-set (cdr a) (cdr b))))
	((< (car a) (car b))
	 (cons (car a) (union-set (cdr a) b)))
	((< (car b) (car a))
	   (cons (car b) (union-set a (cdr b)))))))
