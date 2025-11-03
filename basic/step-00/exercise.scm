#lang racket

(require rackunit rackunit/text-ui)

;; 目的: 実行環境と rackunit の使い方を確認

;; 練習: 関数定義とテスト
(define (square x)
  (* x x))

(define (unimplemented)
  (error "TODO: 実装してください"))


(define tests
  (test-suite
   "step-00"
   (test-case "square"
     (check-equal? (square 3) 9))
   (test-case "unimplemented"
     (check-exn exn:fail? (λ () (unimplemented))))))

(module+ main
  (displayln "=== step-00: rackunit動作確認 ===")
  (run-tests tests))


