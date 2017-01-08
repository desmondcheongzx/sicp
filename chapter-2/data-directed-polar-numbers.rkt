#lang sicp

(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)


(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (sqrt (+ (square (real-part z))
				 (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;generic operations

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
	 (proc (get op type-tags)))
    (if proc
	(apply proc (map content args))
	#f)))

(define (attach-tag tag x)
  (cons tag x))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      #f))

(define (content datum)
  (if (pair? datum)
      (cdr datum)
      #f))

;Operation table implemented via message passing

(define operation-table (make-table))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))

(define (assoc key records)
  (cond
   ((null? records) #f)
   ((equal? key (caar records)) (car records))
   (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list 'table)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key1 key2 val)
      (let ((subtable (assoc key1 (cdr local-table))))
	(if subtable
	    (let ((pos (assoc key2 (cdr subtable))))
	      (if pos
		  (set-cdr! pos val)
		  (set-cdr! subtable
			    (cons (cons key2 val)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key1 (cons key2 val))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (case m
	('lookup lookup)
	('insert! insert!)))
    dispatch))

;math

(define (square x) (* x x))
