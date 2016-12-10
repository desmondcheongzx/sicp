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
