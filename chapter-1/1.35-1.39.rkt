#lang sicp

(define tolerance 0.00001)

(define (average a b)
  (/ (+ a b) 2.0))

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;ex 1.35: 1.6180365296803654

(define (average-damp f)
  (lambda (x) (average x (f x))))

;ex 1.36: given 5 as an initial guess, it took 28 guesses without average
;damping as opposed to 8 guesses with damping

(define (cont-frac n d k)
  (define (term x)
    (if (= x k)
	(/ (n k) (d k))
	(/ (n x) (+ (d x) (term (+ x 1))))))
  (term 1))

(define (cont-frac-iter n d k)
  (define (iter x val)
    (if (= x 0)
	val
	(iter (- x 1)
	      (/ (n x) (+ (d x) val)))))
  (iter k 0))

;ex 1.37: it takes k = 11

(define e
  (+ 2
     (cont-frac-iter
      (lambda (i) 1.0)
      (lambda (i) 
	(let ((var (- i 2)))
	  (cond ((= i 1) 1)
		((= i 2) 2)
		((= (remainder var 3) 0)
		 (+ 2 (* 2 (/ var 3))))
		(else 1))))
      20)))

;ex 1.38: ~2.718281828459045

(define (tan-cf x k)
  (/ (cont-frac-iter
      (lambda (i) (- (* x x)))
      (lambda (i) (- (* i 2) 1))
      k)
     (- x)))



