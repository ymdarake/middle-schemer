#lang racket

(require rackunit rackunit/text-ui)

;; 解答: step-01

(define (pair-head p)
  (car p))

(define (pair-tail p)
  (cdr p))

(define (prepend x xs)
  (cons x xs))

(define tests
  (test-suite
   "step-01-solution"
   (test-case "pair and list basics"
     (check-equal? (car (cons 1 2)) 1)
     (check-equal? (cdr (cons 1 2)) 2)
     (check-equal? (pair-head (cons 'a 'b)) 'a)
     (check-equal? (pair-tail (cons 'a 'b)) 'b)
     (check-equal? (prepend 0 '(1 2 3)) '(0 1 2 3))
     (check-true (pair? (cons 1 2)))
     (check-true (list? '(1 2 3))))))

(module+ main
  (displayln "=== step-01 solution ===")
  (run-tests tests))
