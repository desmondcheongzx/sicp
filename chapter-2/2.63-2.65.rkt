#lang sicp

(#%require (only racket/base print-as-expression print-mpair-curly-braces))
(print-as-expression #f)
(print-mpair-curly-braces #f)

;ex 2.63a: The two procedures produce the same lists, ie ordered lists
;ex 2.63b: For practical purposes.. the order of growth is the same
;          however, since tree->list-1 uses append, it performs additional
;          cons n/2 times at every level of the binary tree. But well,
;          log(n)/2..... I wouldn't hold it against the procedure

;ex 2.64a: At every node, partial-tree calculates the size of its left
;          and right branches. The left branch has a size of
;          (quotient (- n 1) 2) because one element is used as the value 
;          of the node, and the remaining elements are split as evenly as
;          possible, with the left branch being smaller if need be. If the
;          size of a branch is 0, then, obviously, its value is nil.
;
;          The left branch is derived first. After which, the first
;          remaining element is the smallest, so it's used as the value of
;          the current node, ensuring that the nodes on its right branch
;          have larger values. The process used on the left branch is now
;          carried out for the right branch.
;
;          This tree produced is either the desired tree, or the left branch
;          of a larger tree. In the latter case, the process is repeated
;          until all elements are exhausted.

;tree produced:
;
;    5
;   / \
;  1   9
;   \  /\
;   3 7 11

;ex 2.64b: Order of growth is ~O(n)

;ex 2.65: The question is, why can't we just convert them to ordered lists
;         since we already have O(n) operations for those....
;         Cheeky youngster style
;
;         I suppose there are many ways to do this. This one isn't
;         particularly efficient though...

(define (union-set a b)
  (define (iter elements set)
    (cond ((null? elements) set)
	  ((member (car elements) set) (iter (cdr elements) set))
	  (else (iter (cdr elements) (adjoin-set (car elements) set)))))
  (list-to-tree
   (tree-to-list
    (iter (tree-to-list a) b))))

(define (intersect-set a b)
  (define (iter elements set)
    (cond ((null? elements) '())
	  ((member (car elements) set)
	   (cons (car elements) (iter (cdr elements) set)))
	  (else (iter (cdr elements) set))))
  (list-to-tree
   (iter (tree-to-list a) b)))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set)) (make-tree (entry set)
				      (adjoin-set x (left-branch set))
				      (right-branch set)))
	((> x (entry set)) (make-tree (entry set)
				      (left-branch set)
				      (adjoin-set x (right-branch set))))))

(define (member x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set)) (member x (left-branch set)))
	((> x (entry set)) (member x (right-branch set)))))

(define (list-to-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
  (if (= n 0)
      (cons '() elements)
      (let* ((left-size (quotient (- n 1) 2))
	     (left-branch (partial-tree elements left-size))
	     (remaining (cdr left-branch))
	     (entry (car remaining))
	     (right-size (- n left-size 1))
	     (right-branch (partial-tree (cdr remaining) right-size)))
	(cons (make-tree entry (car left-branch) (car right-branch))
	      (cdr right-branch)))))

(define (tree-to-list tree)
  (if (null? tree)
      '()
      (append (tree-to-list (left-branch tree))
	      (cons (entry tree)
		    (tree-to-list (right-branch tree))))))

(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))
