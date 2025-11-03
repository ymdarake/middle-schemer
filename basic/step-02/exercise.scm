#lang racket

(require rackunit rackunit/text-ui)

;; 目的: リストユーティリティの実装練習（length, append, map, filter, foldl）

;; TODO: 実装してください（末尾再帰/名前付きlet 推奨）
(define (my-length xs)
  (error "TODO: my-length を実装してください"))

(define (my-append xs ys)
  (error "TODO: my-append を実装してください"))

(define (my-map f xs)
  (error "TODO: my-map を実装してください"))

(define (my-filter p xs)
  (error "TODO: my-filter を実装してください"))

(define (my-foldl f init xs)
  (error "TODO: my-foldl を実装してください"))


(define tests
  (test-suite
   "step-02"
   (test-case "list utilities"
     (check-equal? (my-length '()) 0)
     (check-equal? (my-length '(a b c)) 3)
     (check-equal? (my-append '(1 2) '(3 4)) '(1 2 3 4))
     (check-equal? (my-map add1 '(1 2 3)) '(2 3 4))
     (check-equal? (my-filter even? '(1 2 3 4)) '(2 4))
     (check-equal? (my-foldl + 0 '(1 2 3 4)) 10))))

(module+ main
  (displayln "=== step-02: リストユーティリティ ===")
  (run-tests tests))


