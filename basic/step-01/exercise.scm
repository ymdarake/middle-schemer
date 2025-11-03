#lang racket

(require rackunit rackunit/text-ui)

;; 目的: cons/pair/list の基本操作を体験

;; TODO: 実装してください
(define (pair-head p)
  (error "TODO: pair-head を実装してください"))

(define (pair-tail p)
  (error "TODO: pair-tail を実装してください"))

(define (prepend x xs)
  (error "TODO: prepend を実装してください"))


(define tests
  (test-suite
   "step-01"
   (test-case "pair and list basics"
     (check-equal? (car (cons 1 2)) 1)
     (check-equal? (cdr (cons 1 2)) 2)
     (check-equal? (pair-head (cons 'a 'b)) 'a)
     (check-equal? (pair-tail (cons 'a 'b)) 'b)
     (check-equal? (prepend 0 '(1 2 3)) '(0 1 2 3))
     (check-true (pair? (cons 1 2)))
     (check-true (list? '(1 2 3))))))

(module+ main
  (displayln "=== step-01: cons/list 基本 ===")
  (run-tests tests))


