#lang sicp

(define (even? x)
  (= (remainder x 2) 0))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (identity x)
  x)

(define (sum f next a b)
  (if (> a b)
      0
      (+ (f a)
	 (sum f next (next a) b))))

(define (integral f dx a b)
  (* (sum f (lambda (x) (+ x dx)) (+ a (/ dx 2)) b)
     dx))

(define (simpson-integral f n a b)
  (define h (/ (- b a) n))
  (/ (* (+ (sum (lambda (x) (* (f (+ a (* x h)))
			       (if (even? x) 2 4)))
		inc 1 (- n 1))
	   (f a)
	   (f b))
      h) 3))
;ex 1.29: wow it really is a lot more accurate....

(define (iter-sum f next a b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (f a)))))
  (iter a 0))

(define (product f next a b)
  (if (> a b)
      1
      (* (f a)
	 (product f next (next a) b))))

(define (iter-product f next a b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (f a)))))
  (iter a 1))

(define (factorial n)
  (product identity inc 1 n))

(define (pi-approx n)
  (* (/ (product (lambda (x) (square x))
		 (lambda (x) (+ x 2))
		 4 n)
	(* (product (lambda (x) (square x))
		    (lambda (x) (+ x 2))
		    3 n)
	   (+ n 1)))
     8.0))
			
(define (accumulate combiner null-value f next a b)
  (if (> a b)
      null-value
      (combiner (f a)
		(accumulate combiner null-value f next (next a) b))))

(define (iter-accumulate combiner null-value f next a b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (f a)))))
  (iter a null-value))

(define (nsum f next a b)
  (accumulate + 0 f next a b))

(define (nproduct f next a b)
  (accumulate * 1 f next a b))

(define (divides? x a)
  (= (remainder x a) 0))

(define (smallest-divisor x)
  (define (iter a)
    (cond ((> (square a) x) x)
	  ((divides? x a) a)
	  (else (iter (+ a 2)))))
  (if (even? x)
      2
      (iter 3)))

(define (prime? x)
  (and (= (smallest-divisor x) x)
       (> x 1)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (coprime? a b)
  (= (gcd a b) 1))
	
(define (filtered-accumulate filter combiner null-value f next a b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (f a) null-value)
		(filtered-accumulate filter combiner null-value f next (next a) b))))

(define (sum-primes a b)
  (filtered-accumulate prime? + 0 (lambda (x) (square x)) inc a b))

(define (sum-of-coprimes n)
  (filtered-accumulate (lambda (x) (coprime? x n)) * 1 identity inc 1 (- n 1)))
