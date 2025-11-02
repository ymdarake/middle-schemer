#lang racket

;; ステップ1: レキサー（字句解析） - 穴埋め式練習問題
;; 一部の関数が未実装です。??? の部分を実装してください。

(define (whitespace? c)
  "空白文字かどうかを判定"
  (member c '(#\space #\tab #\newline #\return)))

(define (skip-whitespace str start)
  "空白文字をスキップして、次の非空白文字の位置を返す"
  ;; TODO: ここを実装してください
  ;; ヒント: loop を使って i を進めていきます
  ;; 空白文字なら i+1、そうでなければ i を返します
  ???)

(define (read-number str start)
  "数値トークンを読み取る。数値の終端位置と値のペアを返す"
  ;; TODO: ここを実装してください
  ;; ヒント: loop を使って数字を読み取り、最後に数値に変換します
  ;; 戻り値は (終端位置 数値) のリストです
  (let loop ((i start)
             (result '()))
    ???))

(define (read-identifier str start)
  "識別子トークンを読み取る。識別子の終端位置と値のペアを返す"
  (define (identifier-char? c)
    ;; TODO: ここを実装してください
    ;; 英字、数字、または記号（+, -, *, /, <, >, =, !, ?）なら真を返します
    ???)
  
  ;; TODO: read-number と同様に、ここを実装してください
  ???)

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
                  ???)
                 ;; TODO: 閉じ括弧の場合の処理を実装してください
                 ((char=? c #\))
                  ???)
                 ;; TODO: 数字の場合の処理を実装してください
                 ((char-numeric? c)
                  ???)
                 ;; TODO: 識別子の場合の処理を実装してください
                 (else
                  ???)))))))
  
  (tokenize-helper str 0 '()))

;; テスト
(define (test-exercise)
  (display "=== 穴埋め式練習問題のテスト ===\n\n")
  
  (let ((test-cases '(("(+ 1 2)" . ("(" "+" "1" "2" ")"))
                      ("(define x 42)" . ("(" "define" "x" "42" ")")))))
    (for-each
     (lambda (test-case)
       (let* ((input (car test-case))
              (expected (cdr test-case))
              (result (tokenize input)))
         (display "入力: ")
         (display input)
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

