#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define dx 0.00001)

(define (derivative g)
  (lambda (x)
    (/ (- (g (+ x dx))
	  (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derivative g) x)))))

(define (newton-method g first-guess)
  (fixed-point (newton-transform g) first-guess))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;ex 1.40: x^3 + 2x^2 + 3x + 4 = 0 -> x ~= -1.6506291914330982

(define (double f)
  (lambda (x)
    (f (f x))))
;ex 1.41: 21. Apply (double double) to (double double), ie f^(4*4) = f^16

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (f ((repeated f (- n 1)) x)))))
;alternatively
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (lambda (x) (/ (+ (f x)
		    (f (+ x dx))
		    (f (- x dx)))
		 3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

(define (average a b)
  (/ (+ a b) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y))))
	       5.0))

(define (log2 x)
  (/ (log x) (log 2)))

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (n-root x n)
  (fixed-point ((repeated average-damp (ceiling (log2 n))) ;ceiling for less risks
		(lambda (y) (/ x (power y (- n 1)))))
	       5.0))
;ex 1.45: 19th root of 87 ~= 1.264974176552685

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  (lambda (x) (try x)))

(define (sqrt x)
  (define (close-enough? guess)
    (< (abs (- x (square guess))) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (close-enough? x)
    (< (abs (- x (f x))) tolerance))
  ((iterative-improve close-enough? f) 1.0))
