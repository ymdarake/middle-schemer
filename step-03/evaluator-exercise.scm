#lang racket

(require rackunit rackunit/text-ui)

;; ステップ3: 基本的な評価器 - 穴埋め式練習問題
;; 一部の関数が未実装です。??? の部分を実装してください。
;; 
;; テストの実行方法:
;;   racket step-03/evaluator-exercise.scm                    # すべてのテストを実行
;;   racket step-03/evaluator-exercise.scm eval-expr          # 個別のテストを実行
;; 
;; すべてのテストが通れば、実装は正しいです！

(define (eval-expr expr env)
  "式を評価する"
  (cond
   ;; TODO: 数値の場合の処理を実装してください
   ((number? expr)
    expr)
   
   ;; TODO: シンボルの場合の処理を実装してください
   ;; ヒント: assoc を使って環境から値を取得します
   ((symbol? expr)
    (error "シンボルの処理はまだ実装されていません"))
   
   ;; リストは関数適用として評価
   ((list? expr)
    (if (null? expr)
        '()
        (let ((operator (car expr))
              (operands (cdr expr)))
          (cond
           ;; TODO: 加算の場合の処理を実装してください
           ((eq? operator '+)
            (apply-+ (map (lambda (e) (eval-expr e env)) operands)))
           ;; TODO: 減算の場合の処理を実装してください
           ((eq? operator '-)
            (apply-- (map (lambda (e) (eval-expr e env)) operands)))
           ;; TODO: 乗算の場合の処理を実装してください
           ((eq? operator '*)
            (apply-* (map (lambda (e) (eval-expr e env)) operands)))
           ;; TODO: 除算の場合の処理を実装してください
           ((eq? operator '/)
            (apply-/ (map (lambda (e) (eval-expr e env)) operands)))
           (else
            (error "Unknown operator:" operator))))))
   
   (else
    (error "Cannot evaluate:" expr))))

;; TODO: 加算関数を実装してください
(define (apply-+ args)
  "加算を実行"
  (error "apply-+ はまだ実装されていません"))

;; TODO: 減算関数を実装してください
(define (apply-- args)
  "減算を実行"
  (error "apply-- はまだ実装されていません"))

;; TODO: 乗算関数を実装してください
(define (apply-* args)
  "乗算を実行"
  (error "apply-* はまだ実装されていません"))

;; TODO: 除算関数を実装してください
(define (apply-/ args)
  "除算を実行"
  (error "apply-/ はまだ実装されていません"))

;; ============================================
;; ユニットテストスイート
;; ============================================

(define empty-env '())

;; テスト結果を蓄積するためのグローバル変数
(define test-results '())

;; ANSIカラーコード
(define ansi-reset "\x1b[0m")
(define ansi-green "\x1b[32m")
(define ansi-red "\x1b[31m")

;; テーブル風の表示用ヘルパー関数（色付け対応）
(define (format-test-table results)
  (let ((max-name-length 50))
    ;; ヘッダーを表示
    (display (format "+~a+----------+\n" (make-string max-name-length #\-)))
    (display (format "| ~a~a | 結果     |\n" 
                     "テスト名" 
                     (make-string (- max-name-length 6) #\space)))
    (display (format "+~a+----------+\n" (make-string max-name-length #\-)))
    ;; 各テスト結果を表示
    (for-each
     (lambda (result)
       (let* ((test-name (car result))
              (status (cdr result))
              (name-len (string-length test-name))
              (padding (make-string (max 0 (- max-name-length name-len 3)) #\space))
              ;; ステータスに応じて色を設定
              (color-code (if (string=? status "SUCCESS") ansi-green ansi-red)))
         (display (format "| ~a~a~a~a | ~a~a~a |\n" 
                         color-code
                         test-name 
                         ansi-reset
                         padding 
                         color-code 
                         status 
                         ansi-reset))))
     (reverse results))
    ;; フッターを表示
    (display (format "+~a+----------+\n" (make-string max-name-length #\-)))))

;; 成功したテストケースも表示するカスタムtest-caseマクロ（テーブル風表示）
(define-syntax-rule (test-case-verbose name body ...)
  (test-case name
    (with-handlers ([exn:fail? 
                     (lambda (e)
                       ;; 失敗時に結果を記録
                       (set! test-results (cons (cons name "FAILURE") test-results))
                       (raise e))])
      (begin
        body ...
        ;; 成功時に結果を記録
        (set! test-results (cons (cons name "SUCCESS") test-results))))))

;; テスト1: eval-expr 関数（数値）
(define-test-suite test-eval-expr-number
  "eval-expr関数のテスト（数値）"
  (test-case-verbose "数値をそのまま返す"
    (check-equal? (eval-expr 42 empty-env) 42)
    (check-equal? (eval-expr 0 empty-env) 0)
    (check-equal? (eval-expr -10 empty-env) -10)))

;; テスト2: eval-expr 関数（シンボル）
(define-test-suite test-eval-expr-symbol
  "eval-expr関数のテスト（シンボル）"
  (test-case-verbose "環境から値を取得"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "シンボルの処理はまだ実装されていません"))])
      (let ((env '((x . 10) (y . 20))))
        (check-equal? (eval-expr 'x env) 10)
        (check-equal? (eval-expr 'y env) 20)))))

;; テスト3: eval-expr 関数（演算）
(define-test-suite test-eval-expr-operations
  "eval-expr関数のテスト（演算）"
  (test-case-verbose "基本的な演算"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "演算の処理はまだ実装されていません"))])
      (check-equal? (eval-expr '(+ 1 2) empty-env) 3)
      (check-equal? (eval-expr '(- 10 3) empty-env) 7)
      (check-equal? (eval-expr '(* 3 4) empty-env) 12)
      (check-equal? (eval-expr '(/ 12 3) empty-env) 4))))

;; テスト4: apply-+ 関数
(define-test-suite test-apply-+
  "apply-+関数のテスト"
  (test-case-verbose "加算を実行"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "apply-+ はまだ実装されていません"))])
      (check-equal? (apply-+ '(1 2 3)) 6)
      (check-equal? (apply-+ '()) 0)
      (check-equal? (apply-+ '(5)) 5))))

;; テスト5: apply-- 関数
(define-test-suite test-apply--
  "apply--関数のテスト"
  (test-case-verbose "減算を実行"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "apply-- はまだ実装されていません"))])
      (check-equal? (apply-- '(10 3)) 7)
      (check-equal? (apply-- '(10 3 2)) 5)
      (check-equal? (apply-- '(5)) -5))))

;; テスト6: apply-* 関数
(define-test-suite test-apply-*
  "apply-*関数のテスト"
  (test-case-verbose "乗算を実行"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "apply-* はまだ実装されていません"))])
      (check-equal? (apply-* '(3 4)) 12)
      (check-equal? (apply-* '(2 3 4)) 24)
      (check-equal? (apply-* '()) 1))))

;; テスト7: apply-/ 関数
(define-test-suite test-apply-/
  "apply-/関数のテスト"
  (test-case-verbose "除算を実行"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "apply-/ はまだ実装されていません"))])
      (check-equal? (apply-/ '(12 3)) 4)
      (check-equal? (apply-/ '(24 2 3)) 4)
      (check-exn exn:fail? (lambda () (apply-/ '(10 0)))))))

;; すべてのテストスイートをまとめる
(define all-tests
  (test-suite
   "評価器のすべてのテスト"
   test-eval-expr-number
   test-eval-expr-symbol
   test-eval-expr-operations
   test-apply-+
   test-apply--
   test-apply-*
   test-apply-/))

;; 成功したテストケースも表示するカスタムrun-tests（テーブル風表示）
(define (run-tests-with-success-display suite)
  ;; テスト結果をリセット
  (set! test-results '())
  ;; 標準のrun-testsを実行（各テストケースがtest-resultsに結果を記録）
  (run-tests suite)
  ;; すべてのテストが終了した後にテーブルを表示
  (display "\n")
  (display "=== テスト結果サマリー ===\n")
  (format-test-table test-results)
  (display "\n"))

;; テストを実行（コマンドライン引数で個別のテストを指定可能）
(define (run-selected-tests)
  (let ((args (vector->list (current-command-line-arguments))))
    (if (null? args)
        ;; 引数がない場合はすべてのテストを実行
        (begin
          (display "=== 評価器のユニットテスト（すべて） ===\n\n")
          (run-tests-with-success-display all-tests))
        ;; 引数がある場合は個別のテストを実行
        (for-each
         (lambda (test-name)
           (case (string->symbol test-name)
             ((eval-expr-number number)
              (display "=== eval-expr 関数のテスト（数値） ===\n\n")
              (run-tests-with-success-display test-eval-expr-number))
             ((eval-expr-symbol symbol)
              (display "=== eval-expr 関数のテスト（シンボル） ===\n\n")
              (run-tests-with-success-display test-eval-expr-symbol))
             ((eval-expr-operations operations)
              (display "=== eval-expr 関数のテスト（演算） ===\n\n")
              (run-tests-with-success-display test-eval-expr-operations))
             ((apply-+ apply-plus plus)
              (display "=== apply-+ 関数のテスト ===\n\n")
              (run-tests-with-success-display test-apply-+))
             ((apply-- apply-minus minus)
              (display "=== apply-- 関数のテスト ===\n\n")
              (run-tests-with-success-display test-apply--))
             ((apply-* apply-times times multiply)
              (display "=== apply-* 関数のテスト ===\n\n")
              (run-tests-with-success-display test-apply-*))
             ((apply-/ apply-div divide div)
              (display "=== apply-/ 関数のテスト ===\n\n")
              (run-tests-with-success-display test-apply-/))
             ((all)
              (display "=== 評価器のユニットテスト（すべて） ===\n\n")
              (run-tests-with-success-display all-tests))
             (else
              (display (format "未知のテスト: ~a\n" test-name))
              (display "利用可能なテスト: eval-expr-number, eval-expr-symbol, eval-expr-operations, apply-+, apply--, apply-*, apply-/, all\n"))))
         args))))

;; テストを実行
(run-selected-tests)

;; 使い方の例:
;;   racket step-03/evaluator-exercise.scm                    # すべてのテストを実行
;;   racket step-03/evaluator-exercise.scm eval-expr-number   # 数値のテストだけを実行
;;   racket step-03/evaluator-exercise.scm apply-+           # apply-+だけを実行
;;   racket step-03/evaluator-exercise.scm all                # すべてのテストを実行

