#lang racket

;; ステップ2: パーサー（構文解析）
;; トークンのリストから構文木（AST）を構築する

(define (string->symbol-safe str)
  "文字列をシンボルに変換する"
  (if (string->number str)
      (string->number str)
      (string->symbol str)))

(define (parse tokens)
  "トークンのリストを構文木に変換する"
  (define (parse-expression tokens pos)
    "式を解析する。構文木と次の位置のペアを返す"
    (if (>= pos (length tokens))
        (error "Unexpected end of tokens")
        (let ((token (list-ref tokens pos)))
          (cond
           ((string=? token "(")
            ;; リストの開始
            (parse-list tokens (+ pos 1)))
           ((string=? token ")")
            (error "Unexpected closing parenthesis"))
           (else
            ;; アトム（数値または識別子）
            (list (string->symbol-safe token)
                  (+ pos 1)))))))
  
  (define (parse-list tokens start-pos)
    "リストを解析する。構文木と次の位置のペアを返す"
    (let loop ((pos start-pos)
               (result '()))
      (if (>= pos (length tokens))
          (error "Unclosed parenthesis")
          (let ((token (list-ref tokens pos)))
            (cond
             ((string=? token ")")
              ;; リストの終了
              (list (reverse result) (+ pos 1)))
             ((string=? token "(")
              ;; ネストしたリスト
              (let* ((nested-result (parse-list tokens (+ pos 1)))
                     (nested-expr (car nested-result))
                     (next-pos (cadr nested-result)))
                (loop next-pos (cons nested-expr result))))
             (else
              ;; アトム
              (let* ((atom-result (parse-expression tokens pos))
                     (atom-expr (car atom-result))
                     (next-pos (cadr atom-result)))
                (loop next-pos (cons atom-expr result)))))))))
  
  (let* ((result (parse-expression tokens 0))
         (ast (car result))
         (final-pos (cadr result)))
    (if (< final-pos (length tokens))
        (error "Extra tokens after expression")
        ast)))

;; テスト用のヘルパー関数
(define (test-parser)
  (display "=== パーサーのテスト ===\n")
  
  (let ((test-cases '(("(+ 1 2)" . (+ 1 2))
                      ("(define x 42)" . (define x 42))
                      ("(* (+ 1 2) 3)" . (* (+ 1 2) 3))
                      ("(lambda (x) (+ x 1))" . (lambda (x) (+ x 1))))))
    (for-each
     (lambda (test-case)
       (let* ((input-str (car test-case))
              (expected (cdr test-case))
              ;; 簡易的なトークン化（step-01のlexerを利用）
              (tokens (tokenize input-str))
              (result (parse tokens)))
         (display "入力: ")
         (display input-str)
         (display "\nトークン: ")
         (display tokens)
         (display "\n構文木: ")
         (display result)
         (display "\n期待: ")
         (display expected)
         (display "\n")
         (if (equal? result expected)
             (display "✓ OK\n\n")
             (begin
               (display "✗ NG\n")
               (display "  詳細: ")
               (display "結果=")
               (display result)
               (display ", 期待=")
               (display expected)
               (display "\n\n")))))
     test-cases)))

;; step-01のlexerを使用するため、簡易版を定義
;; （完全版はstep-01/lexer.scmを参照）
(define (whitespace? c)
  (member c '(#\space #\tab #\newline #\return)))

(define (skip-whitespace str start)
  (let loop ((i start))
    (if (and (< i (string-length str))
             (whitespace? (string-ref str i)))
        (loop (+ i 1))
        i)))

(define (read-number str start)
  (let loop ((i start)
             (result '()))
    (if (and (< i (string-length str))
             (char-numeric? (string-ref str i)))
        (loop (+ i 1)
              (cons (string-ref str i) result))
        (list i (string->number (list->string (reverse result)))))))

(define (read-identifier str start)
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

;; 実行
(test-parser)

