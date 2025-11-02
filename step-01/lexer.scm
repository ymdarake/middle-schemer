#lang racket

(require rackunit rackunit/text-ui)

;; ステップ1: レキサー（字句解析）
;; 文字列をトークンのリストに分割する

(define (whitespace? c)
  "空白文字かどうかを判定"
  (member c '(#\space #\tab #\newline #\return)))

(define (skip-whitespace str start)
  "空白文字をスキップして、次の非空白文字の位置を返す
  
  SICPの用語では：
  - パターン1（現在の実装）: 末尾再帰による反復的プロセス（iterative process）
    → 再帰的手続きで反復的プロセスを実現（SICPの「iterパターン」）
  
  参考実装（命令型スタイルの反復）:
  (let ((i start))
    (let loop ()
      (if (and (< i (string-length str))
               (whitespace? (string-ref str i)))
          (begin
            (set! i (+ i 1))  ; 副作用を使う（関数型スタイルではない）
            (loop))
          i)))
  
  参考実装（do構文）:
  (do ((i start (+ i 1)))
      ((or (>= i (string-length str))
           (not (whitespace? (string-ref str i))))
       i))
  "
  ;; パターン1: 末尾再帰（現在の実装）
  (let loop ((i start))
    (if (and (< i (string-length str))
             (whitespace? (string-ref str i)))
        (loop (+ i 1))
        i)))

(define (read-number str start)
  "数値トークンを読み取る。数値の終端位置と値のペアを返す"
  (let loop ((i start)
             (result '()))
    (if (and (< i (string-length str))
             (char-numeric? (string-ref str i)))
        (loop (+ i 1)
              (cons (string-ref str i) result))
        (list i (string->number (list->string (reverse result)))))))

(define (read-identifier str start)
  "識別子トークンを読み取る。識別子の終端位置と値のペアを返す"
  (define (identifier-char? c)
    (or (char-alphabetic? c)
        (char-numeric? c)
        (member c '(#\+ #\- #\* #\/ #\< #\> #\= #\! #\?))))
  
  (let loop ((i start)
             (result '()))
    (if (and (< i (string-length str))
             (identifier-char? (string-ref str i)))
        (loop (+ i 1)
              (cons (string-ref str i) result))
        (list i (list->string (reverse result))))))

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
                 ((char=? c #\()
                  (tokenize-helper str (+ pos 1)
                                  (cons "(" tokens)))
                 ((char=? c #\))
                  (tokenize-helper str (+ pos 1)
                                  (cons ")" tokens)))
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
                                    (cons id-value tokens))))))))))

  (tokenize-helper str 0 '()))

;; ============================================
;; ユニットテスト（rackunit使用）
;; ============================================

;; whitespace?のテスト
(define-test-suite whitespace-tests
  (test-case "空白文字を判定"
    (check-true (whitespace? #\space))
    (check-true (whitespace? #\tab))
    (check-true (whitespace? #\newline))
    (check-false (whitespace? #\a))
    (check-false (whitespace? #\1))))

;; skip-whitespaceのテスト
(define-test-suite skip-whitespace-tests
  (test-case "空白文字をスキップ"
    (check-equal? (skip-whitespace "  hello" 0) 2)
    (check-equal? (skip-whitespace "hello" 0) 0)
    (check-equal? (skip-whitespace "  " 0) 2)
    (check-equal? (skip-whitespace "a  b" 1) 3)))

;; read-numberのテスト
(define-test-suite read-number-tests
  (test-case "数値を読み取る"
    (check-equal? (read-number "123abc" 0) '(3 123))
    (check-equal? (read-number "42" 0) '(2 42))
    (check-equal? (read-number "abc123" 3) '(6 123))))

;; read-identifierのテスト
(define-test-suite read-identifier-tests
  (test-case "識別子を読み取る"
    (check-equal? (read-identifier "hello world" 0) '(5 "hello"))
    (check-equal? (read-identifier "+-*/" 0) '(4 "+-*/"))
    (check-equal? (read-identifier "x123" 0) '(4 "x123"))))

;; tokenizeのテスト
(define-test-suite tokenize-tests
  (test-case "文字列をトークンに分割"
    (check-equal? (tokenize "(+ 1 2)") '("(" "+" "1" "2" ")"))
    (check-equal? (tokenize "(define x 42)") '("(" "define" "x" "42" ")"))
    (check-equal? (tokenize "(* (+ 1 2) 3)") '("(" "*" "(" "+" "1" "2" ")" "3" ")"))
    (check-equal? (tokenize "(lambda (x) (+ x 1))") 
                  '("(" "lambda" "(" "x" ")" "(" "+" "x" "1" ")" ")"))))

;; すべてのテストスイート
(define-test-suite lexer-tests
  whitespace-tests
  skip-whitespace-tests
  read-number-tests
  read-identifier-tests
  tokenize-tests)

;; テスト実行（メイン）
(module+ main
  (display "=== レキサーのユニットテスト ===\n\n")
  (run-tests lexer-tests))

;; テスト実行（モジュールとしてロードされた場合も実行）
(module+ test
  (run-tests lexer-tests))

