#lang racket

;; ステップ1: レキサー（字句解析）
;; 文字列をトークンのリストに分割する

(define (whitespace? c)
  "空白文字かどうかを判定"
  (member c '(#\space #\tab #\newline #\return)))

(define (skip-whitespace str start)
  "空白文字をスキップして、次の非空白文字の位置を返す"
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
                                    (cons id-value tokens)))))))))
  
  (tokenize-helper str 0 '()))

;; テスト用のヘルパー関数
(define (test-lexer)
  (display "=== レキサーのテスト ===\n")
  
  (let ((test-cases '(("(+ 1 2)" . ("(" "+" "1" "2" ")"))
                      ("(define x 42)" . ("(" "define" "x" "42" ")"))
                      ("(* (+ 1 2) 3)" . ("(" "*" "(" "+" "1" "2" ")" "3" ")"))
                      ("(lambda (x) (+ x 1))" . ("(" "lambda" "(" "x" ")" "(" "+" "x" "1" ")" ")")))))
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
             (display "✗ NG\n\n"))))
     test-cases)))

;; 実行
(test-lexer)

