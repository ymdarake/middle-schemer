#lang racket

(require "lexer-exercise.scm")

;; 簡易ベンチマーク（time）

(define (make-numstr n)
  (define s (build-string n (λ (i) (integer->char (+ 48 (remainder i 10))))))
  (string-append s "abc"))

(define inputs
  (for/list ([n '(8 16 32 64 128 256 512 1024)])
    (make-numstr n)))

(define (bench name f)
  (displayln (format "== ~a ==" name))
  (collect-garbage)
  ;; ウォームアップ
  (for ([s inputs]) (f s 0))
  (collect-garbage)
  (time
   (for* ([iter 1000]
          [s inputs])
     (f s 0))))

;; 実行
(bench "read-number (reverse方式)" read-number)
(bench "read-number-substring (substring方式)" read-number-substring)


