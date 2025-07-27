#lang scheme

#| second implementation of square root;
instead of using predetermined absolute tolerance,
this one compares relative changes in the
guess, and stops when the difference is
small enough.|#

(define (good-enough? new-guess old-guess)
  (< (abs (/ (- new-guess old-guess) old-guess)) 0.00001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve-guess guess x)
  (average guess (/ x guess)))
      
(define (sqrt-iter guess old-guess x)
  (begin (display guess) (display ", ") (newline)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve-guess guess x) guess x))))


(define (sqrt x)
  (sqrt-iter 1.0 0.5 x))


(define (square x)
  (* x x))

; function to computer percent error after squaring
(define (percent-err guess x)
  (/ (abs (- (square guess) x)) x))

#| run a test with init, and each subsequent
 value divided by 10, for n iterations;
 hopefully will see better results than the
previous algorithm which relied on pre-determined
tolerance to end. |#
(define (sqrt-test test-val n)
  (if (< n 0)
      0
      (begin (display "testing: ") (display test-val) (display " : ")  (newline)
             (display (percent-err (sqrt test-val) test-val)) (newline)
             (sqrt-test (/ test-val 10) (- n 1)))))

(sqrt-test 10 10)