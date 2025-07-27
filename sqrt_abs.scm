#lang scheme

#|
Implementation of square root procedure
which relies on initial guess and improving
the guess
|#

(define (sqrt x)
  (define (sqrt-iter guess x)  ; nested function which does the work
    (begin (display guess) (newline)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x))))

  (define (good-enough? guess x) ; relies on pre-determined tolerance
    (< (abs (- (* guess guess) x)) 0.001))

  (define (improve guess x) ; from Newton's method
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (sqrt-iter 1.0 x))


(define (square x)
  (* x x))

; function to computer percent error after squaring
(define (percent-err guess x)
  (/ (abs (- (square guess) x)) x))

#| run a test with init, and each subsequent
 value divided by 10, for n iterations;
 this will show that the sqrt procedure performs terribly
for small inputs. |#
(define (sqrt-test test-val n)
  (if (< n 0)
      0
      (begin (display "testing: ") (display test-val) (display " : ")  (newline)
             (display (percent-err (sqrt test-val) test-val)) (newline)
             (sqrt-test (/ test-val 10) (- n 1)))))

(sqrt-test 10 10)


