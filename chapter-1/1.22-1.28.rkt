#lang sicp

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
