#lang racket

(require rackunit rackunit/text-ui)

;; 解答: step-00

(define (square x)
  (* x x))

;; テストでは例外が期待されているので、仕様どおりに残す
(define (unimplemented)
  (error "TODO: 実装してください"))

(define tests
  (test-suite
   "step-00-solution"
   (test-case "square"
     (check-equal? (square 3) 9))
   (test-case "unimplemented"
     (check-exn exn:fail? (λ () (unimplemented))))))

(module+ main
  (displayln "=== step-00 solution ===")
  (run-tests tests))
