#lang racket

(require rackunit rackunit/text-ui)

;; ステップ1: レキサー（字句解析） - 穴埋め式練習問題
;; 一部の関数が未実装です。??? の部分を実装してください
;; 
;; テストの実行方法:
;;   racket step-01/lexer-exercise.scm
;; 
;; すべてのテストが通れば、実装は正しいです！。

(define (whitespace? c)
  "空白文字かどうかを判定"
  (member c '(#\space #\tab #\newline #\return)))

(define (skip-whitespace str start)
  "空白文字をスキップして、次の非空白文字の位置を返す"
  ;; TODO: ここを実装してください
  ;; ヒント: loop を使って i を進めていきます
  ;; 空白文字なら i+1、そうでなければ i を返します
  (error "skip-whitespace はまだ実装されていません"))

(define (read-number str start)
  "数値トークンを読み取る。数値の終端位置と値のペアを返す"
  ;; TODO: ここを実装してください
  ;; ヒント: loop を使って数字を読み取り、最後に数値に変換します
  ;; 戻り値は (終端位置 数値) のリストです
  (let loop ((i start)
             (result '()))
    (if #f  ; 未実装
        '()
        (error "read-number はまだ実装されていません"))))

(define (read-identifier str start)
  "識別子トークンを読み取る。識別子の終端位置と値のペアを返す"
  (define (identifier-char? c)
    ;; TODO: ここを実装してください
    ;; 英字、数字、または記号（+, -, *, /, <, >, =, !, ?）なら真を返します
    (error "identifier-char? はまだ実装されていません"))
  
  ;; TODO: read-number と同様に、ここを実装してください
  (error "read-identifier はまだ実装されていません"))

(define (tokenize str)
  "文字列をトークンのリストに分割する"
  (define (tokenize-helper str start tokens)
    (if (>= start (string-length str))
        (reverse tokens)
        (let ((pos (skip-whitespace str start)))
          (if (>= pos (string-length str))
              (reverse tokens)
              (let ((c (string-ref str pos)))
                (cond
                 ;; TODO: 開き括弧の場合の処理を実装してください
                 ((char=? c #\()
                  (tokenize-helper str (+ pos 1) (cons "(" tokens)))
                 ;; TODO: 閉じ括弧の場合の処理を実装してください
                 ((char=? c #\))
                  (tokenize-helper str (+ pos 1) (cons ")" tokens)))
                 ;; TODO: 数字の場合の処理を実装してください
                 ((char-numeric? c)
                  (error "数字の処理はまだ実装されていません"))
                 ;; TODO: 識別子の場合の処理を実装してください
                 (else
                  (error "識別子の処理はまだ実装されていません"))))))))
  
  (tokenize-helper str 0 '()))

;; ============================================
;; ユニットテストスイート
;; ============================================


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

;; テスト1: whitespace? 関数
(define-test-suite test-whitespace?
  "whitespace?関数のテスト"
  (test-case-verbose "空白文字を正しく判定"
    ;; memberはリストまたは#fを返すので、リストが返れば真値として扱われる
    (check-not-false (whitespace? #\space))
    (check-not-false (whitespace? #\tab))
    (check-not-false (whitespace? #\newline))
    (check-false (whitespace? #\a))
    (check-false (whitespace? #\1))))

;; テスト2: skip-whitespace 関数
(define-test-suite test-skip-whitespace
  "skip-whitespace関数のテスト"
  (test-case-verbose "空白文字をスキップ"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "skip-whitespace はまだ実装されていません"))])
      (check-equal? (skip-whitespace "  abc" 0) 2)
      (check-equal? (skip-whitespace "abc" 0) 0)
      (check-equal? (skip-whitespace "   " 0) 3)
      (check-equal? (skip-whitespace "  abc  def" 5) 7))))

;; テスト3: read-number 関数
(define-test-suite test-read-number
  "read-number関数のテスト"
  (test-case-verbose "数値を正しく読み取る"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "read-number はまだ実装されていません"))])
      (let ((result1 (read-number "123abc" 0)))
        (check-equal? (car result1) 3)
        (check-equal? (cadr result1) 123))
      (let ((result2 (read-number "42 " 0)))
        (check-equal? (car result2) 2)
        (check-equal? (cadr result2) 42))
      (let ((result3 (read-number "abc123" 3)))
        (check-equal? (car result3) 6)
        (check-equal? (cadr result3) 123)))))

;; テスト4: read-identifier 関数
(define-test-suite test-read-identifier
  "read-identifier関数のテスト"
  (test-case-verbose "識別子を正しく読み取る"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "read-identifier はまだ実装されていません"))])
      (let ((result1 (read-identifier "abc123" 0)))
        (check-equal? (car result1) 6)
        (check-equal? (cadr result1) "abc123"))
      (let ((result2 (read-identifier "define " 0)))
        (check-equal? (car result2) 6)
        (check-equal? (cadr result2) "define"))
      (let ((result3 (read-identifier "+-*/" 0)))
        (check-equal? (car result3) 4)
        (check-equal? (cadr result3) "+-*/")))))

;; テスト5: tokenize 関数（統合テスト）
(define-test-suite test-tokenize
  "tokenize関数のテスト"
  (test-case-verbose "基本的なトークン分割"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "tokenize はまだ実装されていません"))])
      (check-equal? (tokenize "(+ 1 2)")
                    '("(" "+" "1" "2" ")"))
      (check-equal? (tokenize "(define x 42)")
                    '("(" "define" "x" "42" ")"))
      (check-equal? (tokenize "(* (+ 1 2) 3)")
                    '("(" "*" "(" "+" "1" "2" ")" "3" ")"))
      (check-equal? (tokenize "(lambda (x) (+ x 1))")
                    '("(" "lambda" "(" "x" ")" "(" "+" "x" "1" ")" ")")))))

;; すべてのテストスイートをまとめる
(define all-tests
  (test-suite
   "レキサーのすべてのテスト"
   test-whitespace?
   test-skip-whitespace
   test-read-number
   test-read-identifier
   test-tokenize))

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
          (display "=== レキサーのユニットテスト（すべて） ===\n\n")
          (run-tests-with-success-display all-tests))
        ;; 引数がある場合は個別のテストを実行
        (for-each
         (lambda (test-name)
           (case (string->symbol test-name)
             ((whitespace? whitespace)
              (display "=== whitespace? 関数のテスト ===\n\n")
              (run-tests-with-success-display test-whitespace?))
             ((skip-whitespace skip)
              (display "=== skip-whitespace 関数のテスト ===\n\n")
              (run-tests-with-success-display test-skip-whitespace))
             ((read-number number)
              (display "=== read-number 関数のテスト ===\n\n")
              (run-tests-with-success-display test-read-number))
             ((read-identifier identifier)
              (display "=== read-identifier 関数のテスト ===\n\n")
              (run-tests-with-success-display test-read-identifier))
             ((tokenize)
              (display "=== tokenize 関数のテスト ===\n\n")
              (run-tests-with-success-display test-tokenize))
             ((all)
              (display "=== レキサーのユニットテスト（すべて） ===\n\n")
              (run-tests-with-success-display all-tests))
             (else
              (display (format "未知のテスト: ~a\n" test-name))
              (display "利用可能なテスト: whitespace?, skip-whitespace, read-number, read-identifier, tokenize, all\n"))))
         args))))

;; テストを実行
(run-selected-tests)

;; 使い方の例:
;;   racket step-01/lexer-exercise.scm                    # すべてのテストを実行
;;   racket step-01/lexer-exercise.scm whitespace?        # whitespace?だけを実行
;;   racket step-01/lexer-exercise.scm skip-whitespace   # skip-whitespaceだけを実行
;;   racket step-01/lexer-exercise.scm all                # すべてのテストを実行

