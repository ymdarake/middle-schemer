#lang racket

;; サンプルプログラム: フィボナッチ数列
;; step-09の統合インタプリタで実行できる例

;; 使用例:
;; (interpret "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
;; (interpret "(fib 10)")  ; => 55

;; フィボナッチ関数の定義（文字列として）
(define fibonacci-definition
  "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")

;; 使用例
(define example-usage
  "(fib 10)")

;; 説明
(display "=== フィボナッチ数列のサンプル ===\n\n")
(display "このサンプルでは、再帰関数を使ってフィボナッチ数を計算します。\n")
(display "\n")
(display "定義:\n")
(display fibonacci-definition)
(display "\n\n")
(display "使用例:\n")
(display example-usage)
(display "\n\n")
(display "step-09の統合インタプリタで実行する場合:\n")
(display "  (load \"../step-09/interpreter.scm\")\n")
(display "  (interpret \"")
(display fibonacci-definition)
(display "\")\n")
(display "  (interpret \"")
(display example-usage)
(display "\")\n")
(display "\n")

