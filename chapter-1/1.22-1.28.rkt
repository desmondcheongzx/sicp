#lang sicp
(#%require (only racket/base random))

(define (divisor? x a)
  (= (remainder x a) 0))

(define (even? x)
  (divisor? x 2))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (define (find-divisor a)
    (cond ((> (square a) n) n)
	  ((divisor? n a) a)
	  (else (find-divisor (+ a 2)))))
  (if (even? n)
      2
      (find-divisor 3)))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-test f n)
  (newline)
  (display n)
  (define (print-results elapsed-time)
    (display " *** ")
    (display elapsed-time)
    (newline)
    #t)
  (define (start-test start-time)
    (if (f n)
	(print-results (- (runtime) start-time))
	#f))
  (start-test (runtime)))

(define (search-for-primes pos count)
  (if (> count 0)
      (if (timed-test prime? pos)
	  (search-for-primes (inc pos) (dec count))
	  (search-for-primes (inc pos) count))))
;yes, runtime increases very approximately by a factor of sqrt n
;but that's only obvious for larger n of course, where the overheads
;are less significant

;ex 1.23: It's faster, but not by such a larger factor
;There's still overheads and fixed costs I suppose

(define (expmod base exp mod)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) mod)) mod))
	(else (remainder (* base (expmod base (dec exp) mod)) mod))))

(define (fermat-test n)
  (define (test a)
    (= (expmod a n n) a))
  (test (inc (random (dec n)))))

(define (fast-prime? n)
  (define (test count)
    (cond ((= count 0) #t)
	  ((fermat-test n) (test (dec count)))
	  (else #f)))
  (test 20))

;ex 1.24: yes it's approximately log n growth

;ex 1.25: the intermediate numbers are huge... that can't be good for anyone

;ex 1.26: fast exponentiation relies on the idea that whenever you hit a
;fork with two identical branches, you just compute the branch once and square
;it. For an approximately binary tree, this gives us a log n number of branches
;to compute -- one for each level of depth in the tree -- rather than n number
;of nodes to compute. If you're going to evaluate every node anyway, then you
;have to make n computations.

(define (carmichael-test n)
  (define (test a)
    (cond ((= a n) #t)
	  ((= (expmod a n n) a) (test (inc a)))
	  (else #f)))
  (test 1))
;yes, they're carmichael numbers

(define (mr-expmod base exp mod)
  (cond ((= exp 0) 1)
	((even? exp)
	 (let* ((val (mr-expmod base (/ exp 2) mod))
		(result (remainder (square val) mod)))
	   (if (and (not (= val 1)) (not (= val (dec mod)))
		    (= result 1))
	       0
	       result)))
	(else (remainder (* base (mr-expmod base (dec exp) mod)) mod))))

(define (miller-rabin-test n)
  (define (test a)
    (= (mr-expmod a (dec n) n) 1))
  (test (inc (random (dec n)))))

(define (mr-fast-prime? n)
  (define (test count)
    (cond ((= count 0) #t)
	  ((miller-rabin-test n) (test (dec count)))
	  (else #f)))
  (test 20))
