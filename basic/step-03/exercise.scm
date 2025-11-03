#lang racket

(require rackunit rackunit/text-ui)

;; 目的: 文字列ユーティリティの実装練習

;; TODO: 実装してください
(define (take-str s n)
  (error "TODO: take-str を実装してください"))

(define (drop-str s n)
  (error "TODO: drop-str を実装してください"))

(define (find-char s ch)
  (error "TODO: find-char を実装してください"))

(define (join-with sep ss)
  (error "TODO: join-with を実装してください"))


(define tests
  (test-suite
   "step-03"
   (test-case "string utils"
     (check-equal? (string-length "hello") 5)
     (check-equal? (take-str "hello" 2) "he")
     (check-equal? (drop-str "hello" 2) "llo")
     (check-equal? (find-char "hello" #\l) 2)
     (check-equal? (find-char "hello" #\z) #f)
     (check-equal? (join-with "-" '("a" "b" "c")) "a-b-c"))))

(module+ main
  (displayln "=== step-03: 文字列ユーティリティ ===")
  (run-tests tests))


