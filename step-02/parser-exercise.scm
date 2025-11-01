;; ステップ2: パーサー（構文解析） - 穴埋め式練習問題
;; 一部の関数が未実装です。??? の部分を実装してください。

(define (string->symbol-safe str)
  "文字列をシンボルに変換する"
  ;; TODO: 数値の場合は数値に、それ以外はシンボルに変換してください
  ???)

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
            ???)
           ;; TODO: 閉じ括弧が予期しない場所にある場合のエラー処理
           ((string=? token ")")
            ???)
           ;; TODO: アトムの場合の処理を実装してください
           (else
            ???))))))
  
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
              ???)
             ;; TODO: ネストしたリストの場合の処理を実装してください
             ((string=? token "(")
              ???)
             ;; TODO: アトムの場合の処理を実装してください
             (else
              ???)))))))
  
  (let* ((result (parse-expression tokens 0))
         (ast (car result))
         (final-pos (cadr result)))
    ;; TODO: 余分なトークンがある場合のエラーチェックを実装してください
    ???))

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

;; テスト
(define (test-exercise)
  (display "=== 穴埋め式練習問題のテスト ===\n\n")
  
  (let ((test-cases '(("(+ 1 2)" . (+ 1 2))
                      ("(define x 42)" . (define x 42)))))
    (for-each
     (lambda (test-case)
       (let* ((input-str (car test-case))
              (expected (cdr test-case))
              (tokens (tokenize input-str))
              (result (parse tokens)))
         (display "入力: ")
         (display input-str)
         (display "\n構文木: ")
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

