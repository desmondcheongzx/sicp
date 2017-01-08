#lang sicp

(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

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
