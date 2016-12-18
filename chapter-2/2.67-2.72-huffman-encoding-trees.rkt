#lang sicp

(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;ex 2.67: message is '(A D A B B C A)

(define rock-song-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
;........wtf

(define rock-message '(GET A JOB
			   SHA NA NA NA NA NA NA NA NA
			   GET A JOB
			   SHA NA NA NA NA NA NA NA NA
			   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
			   SHA BOOM))
;...................................-_-
;ex 2.70: It takes 84 bits to encode using a prefix code. With a fixed-length code it would've taken 108 bits

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge
       (adjoin-set (make-code-tree (first leaf-set)
				   (second leaf-set))
		   (cddr leaf-set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (next-bit branch)
    (cond ((member symbol (symbols (left-branch branch))) 0)
	  ((member symbol (symbols (right-branch branch))) 1)
	  (else (display "ERROR symbol not inside tree"))))
  (define (next-branch bit branch)
    (if (= bit 0)
	(left-branch branch)
	(right-branch branch)))
  (define (iter result branch)
    (if (and (leaf? branch) (eq? (leaf-symbol branch) symbol))
	result
	(let* ((bit (next-bit branch))
	       (next (next-branch bit branch)))
	  (iter (append result (list bit)) next))))
  (iter '() tree))

(define (decode code tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
	  ((= bit 1) (right-branch branch))
	  (else (display "ERROR bad bit in code"))))
  (define (iter result bits branch)
    (cond ((leaf? branch) (iter
			   (append result (symbols branch))
			   bits
			   tree))
	  ((null? bits) result)
	  (else (iter result
		      (cdr bits)
		      (choose-branch (car bits) branch)))))
  (iter '() code tree))
;The problem with this is that it's prone to bugging because of the
;counter-intuitive swap with the null-check (which usually comes first) and
;the leaf? check. It might be wiser to do it as in the book

(define (decode code tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
	  ((= bit 1) (right-branch branch))
	  (else (display "ERROR bad bit in code"))))
  (define (iter result bits branch)
    (if (null? bits)
	result
	(let ((next (choose-branch (car bits) branch)))
	  (if (leaf? next)
	      (iter (append result (symbols next))
		    (cdr bits)
		    tree)
	      (iter result (cdr bits) next)))))
  (iter '() code tree))

(define (make-code-tree left right)
  (list left right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (leaf-symbol tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (leaf-weight tree)
      (cadddr tree)))

(define (make-leaf symbol freq)
  (list 'leaf symbol freq))

(define (leaf? x)
  (and (pair? x) (eq? (car x) 'leaf)))

(define (leaf-symbol leaf) (cadr leaf))

(define (leaf-weight leaf) (caddr leaf))

(define (member x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (member x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define (first l) (car l))

(define (second l) (cadr l))