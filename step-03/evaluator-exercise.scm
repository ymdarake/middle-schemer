#lang racket

;; ステップ3: 基本的な評価器 - 穴埋め式練習問題
;; 一部の関数が未実装です。??? の部分を実装してください。

(define (eval-expr expr env)
  "式を評価する"
  (cond
   ;; TODO: 数値の場合の処理を実装してください
   ((number? expr)
    ???)
   
   ;; TODO: シンボルの場合の処理を実装してください
   ;; ヒント: assoc を使って環境から値を取得します
   ((symbol? expr)
    ???)
   
   ;; リストは関数適用として評価
   ((list? expr)
    (if (null? expr)
        '()
        (let ((operator (car expr))
              (operands (cdr expr)))
          (cond
           ;; TODO: 加算の場合の処理を実装してください
           ((eq? operator '+)
            ???)
           ;; TODO: 減算の場合の処理を実装してください
           ((eq? operator '-)
            ???)
           ;; TODO: 乗算の場合の処理を実装してください
           ((eq? operator '*)
            ???)
           ;; TODO: 除算の場合の処理を実装してください
           ((eq? operator '/)
            ???)
           (else
            (error "Unknown operator:" operator))))))
   
   (else
    (error "Cannot evaluate:" expr))))

;; TODO: 加算関数を実装してください
(define (apply-+ args)
  "加算を実行"
  ???)

;; TODO: 減算関数を実装してください
(define (apply-- args)
  "減算を実行"
  ???)

;; TODO: 乗算関数を実装してください
(define (apply-* args)
  "乗算を実行"
  ???)

;; TODO: 除算関数を実装してください
(define (apply-/ args)
  "除算を実行"
  ???)

;; テスト
(define (test-exercise)
  (display "=== 穴埋め式練習問題のテスト ===\n\n")
  
  (let ((empty-env '())
        (test-cases '(((+ 1 2) . 3)
                      ((- 10 3) . 7)
                      ((* 3 4) . 12)
                      ((/ 12 3) . 4))))
    (for-each
     (lambda (test-case)
       (let* ((expr (car test-case))
              (expected (cdr test-case))
              (result (eval-expr expr empty-env)))
         (display "式: ")
         (display expr)
         (display "\n結果: ")
         (display result)
         (display "\n期待: ")
         (display expected)
         (display "\n")
         (if (equal? result expected)
             (display "✓ OK\n\n")
             (display "✗ NG - 実装を確認してください\n\n"))))
     test-cases)))

;; 実行
(test-exercise)

