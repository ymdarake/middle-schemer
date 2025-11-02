#lang racket

;; サンプルプログラム: 階乗の計算
;; step-09の統合インタプリタで実行できる例

;; 使用例:
;; (interpret "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))")
;; (interpret "(factorial 5)")  ; => 120

;; 階乗関数の定義（文字列として）
(define factorial-definition
  "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))")

;; 使用例
(define example-usage
  "(factorial 5)")

;; 説明
(display "=== 階乗の計算サンプル ===\n\n")
(display "このサンプルでは、再帰関数を使って階乗を計算します。\n")
(display "\n")
(display "定義:\n")
(display factorial-definition)
(display "\n\n")
(display "使用例:\n")
(display example-usage)
(display "\n\n")
(display "step-09の統合インタプリタで実行する場合:\n")
(display "  (load \"../step-09/interpreter.scm\")\n")
(display "  (interpret \"")
(display factorial-definition)
(display "\")\n")
(display "  (interpret \"")
(display example-usage)
(display "\")\n")
(display "\n")

