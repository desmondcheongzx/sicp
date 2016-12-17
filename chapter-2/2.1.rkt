#lang sicp

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
	       (* (numer b) (denom a)))
	    (* (denom a) (denom b))))

(define (sub-rat a b)
  (make-rat (- (* (numer a) (denom b))
	       (* (numer b) (denom a)))
	    (* (denom a) (denom b))))

(define (mul-rat a b)
  (make-rat (* (numer a) (numer b))
	    (* (denom a) (denom b))))

(define (div-rat a b)
  (make-rat (* (numer a) (denom b))
	    (* (denom a) (numer b))))

(define (equal-rat? a b)
  (= (* (numer a) (denom b))
     (* (numer b) (denom a))))

(define (make-rat numer denom)
  (let ((divisor (gcd numer denom)))
    (cons (/ numer divisor (if (< denom 0) -1 1))
	  (abs (/ denom divisor)))))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
