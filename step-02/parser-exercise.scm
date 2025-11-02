#lang racket

(require rackunit rackunit/text-ui)

;; ステップ2: パーサー（構文解析） - 穴埋め式練習問題
;; 一部の関数が未実装です。??? の部分を実装してください。
;; 
;; テストの実行方法:
;;   racket step-02/parser-exercise.scm                    # すべてのテストを実行
;;   racket step-02/parser-exercise.scm string->symbol-safe # 個別のテストを実行
;; 
;; すべてのテストが通れば、実装は正しいです！

(define (string->symbol-safe str)
  "文字列をシンボルに変換する"
  ;; TODO: 数値の場合は数値に、それ以外はシンボルに変換してください
  (error "string->symbol-safe はまだ実装されていません"))

(define (parse tokens)
  "トークンのリストを構文木に変換する"
  (define (parse-expression tokens pos)
    "式を解析する。構文木と次の位置のペアを返す"
    (if (>= pos (length tokens))
        (error "Unexpected end of tokens")
        (let ((token (list-ref tokens pos)))
          (cond
           ;; TODO: 開き括弧の場合の処理を実装してください
           ((string=? token "(")
            (error "開き括弧の処理はまだ実装されていません"))
           ;; TODO: 閉じ括弧が予期しない場所にある場合のエラー処理
           ((string=? token ")")
            (error "閉じ括弧の処理はまだ実装されていません"))
           ;; TODO: アトムの場合の処理を実装してください
           (else
            (error "アトムの処理はまだ実装されていません")))))))
  
  (define (parse-list tokens start-pos)
    "リストを解析する。構文木と次の位置のペアを返す"
    (let loop ((pos start-pos)
               (result '()))
      (if (>= pos (length tokens))
          (error "Unclosed parenthesis")
          (let ((token (list-ref tokens pos)))
            (cond
             ;; TODO: 閉じ括弧の場合の処理を実装してください
             ((string=? token ")")
              (error "リストの閉じ括弧の処理はまだ実装されていません"))
             ;; TODO: ネストしたリストの場合の処理を実装してください
             ((string=? token "(")
              (error "ネストしたリストの処理はまだ実装されていません"))
             ;; TODO: アトムの場合の処理を実装してください
             (else
              (error "リスト内のアトムの処理はまだ実装されていません")))))))
  
  (let* ((result (parse-expression tokens 0))
         (ast (car result))
         (final-pos (cadr result)))
    ;; TODO: 余分なトークンがある場合のエラーチェックを実装してください
    (if (< final-pos (length tokens))
        (error "余分なトークンのチェックはまだ実装されていません")
        ast)))

;; 簡易版レキサー（テスト用）
(define (tokenize str)
  "文字列をトークンのリストに分割する（簡易版）"
  (define (whitespace? c)
    (member c '(#\space #\tab #\newline #\return)))
  (define (skip-whitespace str start)
    (let loop ((i start))
      (if (and (< i (string-length str))
               (whitespace? (string-ref str i)))
          (loop (+ i 1))
          i)))
  (define (read-number str start)
    (let loop ((i start) (result '()))
      (if (and (< i (string-length str))
               (char-numeric? (string-ref str i)))
          (loop (+ i 1) (cons (string-ref str i) result))
          (list i (string->number (list->string (reverse result)))))))
  (define (read-identifier str start)
    (define (identifier-char? c)
      (or (char-alphabetic? c)
          (char-numeric? c)
          (member c '(#\+ #\- #\* #\/ #\< #\> #\= #\! #\?))))
    (let loop ((i start) (result '()))
      (if (and (< i (string-length str))
               (identifier-char? (string-ref str i)))
          (loop (+ i 1) (cons (string-ref str i) result))
          (list i (list->string (reverse result))))))
  (define (tokenize-helper str start tokens)
    (if (>= start (string-length str))
        (reverse tokens)
        (let ((pos (skip-whitespace str start)))
          (if (>= pos (string-length str))
              (reverse tokens)
              (let ((c (string-ref str pos)))
                (cond
                 ((char=? c #\()
                  (tokenize-helper str (+ pos 1) (cons "(" tokens)))
                 ((char=? c #\))
                  (tokenize-helper str (+ pos 1) (cons ")" tokens)))
                 ((char-numeric? c)
                  (let* ((num-result (read-number str pos))
                         (end-pos (car num-result))
                         (num-value (cadr num-result)))
                    (tokenize-helper str end-pos
                                    (cons (number->string num-value) tokens))))
                 (else
                  (let* ((id-result (read-identifier str pos))
                         (end-pos (car id-result))
                         (id-value (cadr id-result)))
                    (tokenize-helper str end-pos
                                    (cons id-value tokens)))))))))
  (tokenize-helper str 0 '()))

;; ============================================
;; ユニットテストスイート
;; ============================================

;; テスト1: string->symbol-safe 関数
(define-test-suite test-string->symbol-safe
  "string->symbol-safe関数のテスト"
  (test-case "文字列をシンボルまたは数値に変換"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "string->symbol-safe はまだ実装されていません"))])
      (check-equal? (string->symbol-safe "abc") 'abc)
      (check-equal? (string->symbol-safe "123") 123)
      (check-equal? (string->symbol-safe "42") 42))))

;; テスト2: parse 関数（統合テスト）
(define-test-suite test-parse
  "parse関数のテスト"
  (test-case "基本的な構文解析"
    ;; TODO: 実装してからテストが通るようにしてください
    (with-handlers ([exn:fail? (lambda (e) 
                                  (fail-check "parse はまだ実装されていません"))])
      (check-equal? (parse '("(" "+" "1" "2" ")")) '(+ 1 2))
      (check-equal? (parse '("(" "define" "x" "42" ")")) '(define x 42))
      (check-equal? (parse '("(" "*" "(" "+" "1" "2" ")" "3" ")"))
                    '(* (+ 1 2) 3)))))

;; すべてのテストスイートをまとめる
(define all-tests
  (test-suite
   "パーサーのすべてのテスト"
   test-string->symbol-safe
   test-parse))

;; テストを実行（コマンドライン引数で個別のテストを指定可能）
(define (run-selected-tests)
  (let ((args (vector->list (current-command-line-arguments))))
    (if (null? args)
        ;; 引数がない場合はすべてのテストを実行
        (begin
          (display "=== パーサーのユニットテスト（すべて） ===\n\n")
          (run-tests all-tests))
        ;; 引数がある場合は個別のテストを実行
        (for-each
         (lambda (test-name)
           (case (string->symbol test-name)
             ((string->symbol-safe symbol-safe)
              (display "=== string->symbol-safe 関数のテスト ===\n\n")
              (run-tests test-string->symbol-safe))
             ((parse)
              (display "=== parse 関数のテスト ===\n\n")
              (run-tests test-parse))
             ((all)
              (display "=== パーサーのユニットテスト（すべて） ===\n\n")
              (run-tests all-tests))
             (else
              (display (format "未知のテスト: ~a\n" test-name))
              (display "利用可能なテスト: string->symbol-safe, parse, all\n"))))
         args))))

;; テストを実行
(run-selected-tests)

;; 使い方の例:
;;   racket step-02/parser-exercise.scm                    # すべてのテストを実行
;;   racket step-02/parser-exercise.scm string->symbol-safe # string->symbol-safeだけを実行
;;   racket step-02/parser-exercise.scm parse              # parseだけを実行
;;   racket step-02/parser-exercise.scm all                # すべてのテストを実行

