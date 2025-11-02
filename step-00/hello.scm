#lang racket

;; ステップ0: 開発環境の動作確認
;; このファイルを実行して、Racket環境が正しく動作するか確認します

(display "=== Racket開発環境の動作確認 ===\n\n")

;; テスト1: 基本的な出力
(display "Hello, Scheme!\n")
(display "\n")

;; テスト2: 基本的な計算
(display "1 + 2 = ")
(display (+ 1 2))
(display "\n\n")

;; テスト3: 関数定義
(define (test-function x)
  (* x 2))

(display "関数テスト: (test-function 5) = ")
(display (test-function 5))
(display "\n\n")

;; テスト4: リスト操作
(define test-list '(1 2 3 4 5))
(display "リストテスト: ")
(display test-list)
(display "\n")
(display "car: ")
(display (car test-list))
(display ", cdr: ")
(display (cdr test-list))
(display "\n\n")

;; テスト5: 再帰
(define (sum n)
  (if (= n 0)
      0
      (+ n (sum (- n 1)))))

(display "再帰テスト: (sum 4) = ")
(display (sum 4))
(display "\n\n")

;; テスト6: lambda式
(define multiply (lambda (x y) (* x y)))
(display "lambdaテスト: (multiply 3 4) = ")
(display (multiply 3 4))
(display "\n\n")

;; テスト7: 条件分岐
(display "条件分岐テスト: ")
(if (> 5 3)
    (display "5 > 3 は真\n")
    (display "5 > 3 は偽\n"))
(display "\n")

;; すべてのテストが成功したことを確認
(display "✓ すべてのテストが成功しました\n")
(display "\n")
(display "開発環境の構築は完了です！\n")
(display "次のステップ（step-01）に進みましょう。\n")

