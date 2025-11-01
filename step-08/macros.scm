;; ステップ8: マクロとメタプログラミング
;; quote, quasiquote, unquote と基本的なマクロ展開器を実装する

;; ============================================
;; 基本的な評価器（step-04をベースに拡張）
;; ============================================

(define (eval-expr expr env)
  "式を評価する（マクロ対応版）"
  (cond
   ;; 数値はそのまま返す
   ((number? expr)
    expr)
   
   ;; シンボルは環境から値を取得
   ((symbol? expr)
    (lookup-symbol expr env))
   
   ;; リストは特別な形式または関数適用として評価
   ((list? expr)
    (if (null? expr)
        '()
        (let ((operator (car expr))
              (operands (cdr expr)))
          (cond
           ;; quote: 式を評価せずにそのまま返す
           ((eq? operator 'quote)
            (eval-quote operands env))
           
           ;; quasiquote: 部分的に評価できるquote
           ((eq? operator 'quasiquote)
            (eval-quasiquote operands env))
           
           ;; unquote: quasiquote内でのみ使用可能
           ((eq? operator 'unquote)
            (error "unquote can only be used inside quasiquote"))
           
           ;; define: 変数・関数を定義
           ((eq? operator 'define)
            (eval-define operands env))
           
           ;; lambda: 関数を定義
           ((eq? operator 'lambda)
            (eval-lambda operands env))
           
           ;; 関数適用（その他）
           (else
            (eval-application operator operands env))))))
   
   (else
    (error "Cannot evaluate:" expr))))

(define (lookup-symbol sym env)
  "環境からシンボルの値を取得"
  (if (null? env)
      (error "Undefined variable:" sym)
      (let ((binding (car env)))
        (if (eq? (car binding) sym)
            (cdr binding)
            (lookup-symbol sym (cdr env))))))

(define (extend-env sym value env)
  "環境に新しい束縛を追加"
  (cons (cons sym value) env))

(define (eval-define args env)
  "define を評価する"
  (if (< (length args) 2)
      (error "define requires at least 2 arguments"))
  
  (let ((name (car args))
        (value-expr (cadr args)))
    (if (not (symbol? name))
        (error "define: first argument must be a symbol"))
    
    (let ((value (eval-expr value-expr env)))
      (extend-env name value env)
      value)))

(define (eval-lambda args env)
  "lambda を評価してクロージャを作成"
  (if (< (length args) 2)
      (error "lambda requires at least 2 arguments"))
  
  (let ((params (car args))
        (body (cadr args)))
    ;; クロージャ: (closure params body env)
    (list 'closure params body env)))

(define (eval-application operator operands env)
  "関数適用を評価する"
  (let ((func (eval-expr operator env)))
    (cond
     ;; 組み込み演算
     ((eq? func '+)
      (apply-+ (map (lambda (e) (eval-expr e env)) operands)))
     
     ((eq? func '-)
      (apply-- (map (lambda (e) (eval-expr e env)) operands)))
     
     ((eq? func '*)
      (apply-* (map (lambda (e) (eval-expr e env)) operands)))
     
     ((eq? func '/)
      (apply-/ (map (lambda (e) (eval-expr e env)) operands)))
     
     ;; ユーザー定義関数（クロージャ）
     ((and (list? func) (eq? (car func) 'closure))
      (apply-closure func operands env))
     
     (else
      (error "Not a function:" func)))))

(define (apply-closure closure args env)
  "クロージャを適用する"
  (let ((params (cadr closure))
        (body (caddr closure))
        (closure-env (cadddr closure)))
    ;; 引数を評価
    (let ((arg-values (map (lambda (e) (eval-expr e env)) args)))
      ;; パラメータと引数を束縛した新しい環境を作成
      (let ((new-env (bind-params params arg-values closure-env)))
        ;; 関数本体を評価
        (eval-expr body new-env)))))

(define (bind-params params arg-values env)
  "パラメータと引数値を環境に束縛"
  (if (null? params)
      (if (null? arg-values)
          env
          (error "Too many arguments"))
      (if (null? arg-values)
          (error "Too few arguments")
          (let ((param (car params))
                (arg-value (car arg-values)))
            (extend-env param arg-value
                        (bind-params (cdr params) (cdr arg-values) env))))))

;; ============================================
;; quote の実装
;; ============================================

(define (eval-quote args env)
  "quote を評価する：式を評価せずにそのまま返す"
  (if (null? args)
      (error "quote requires one argument")
      (car args)))  ; 環境は使わず、そのまま返す

;; ============================================
;; quasiquote と unquote の実装
;; ============================================

(define (eval-quasiquote args env)
  "quasiquote を評価する：部分的に評価できるquote"
  (if (null? args)
      (error "quasiquote requires one argument")
      (expand-quasiquote (car args) env)))

(define (expand-quasiquote expr env)
  "quasiquote内の式を展開する"
  (cond
   ;; unquote を見つけたら、その引数を評価
   ((and (list? expr)
         (not (null? expr))
         (eq? (car expr) 'unquote))
    (if (< (length expr) 2)
        (error "unquote requires one argument")
        (eval-expr (cadr expr) env)))
   
   ;; リストの場合は再帰的に展開
   ((list? expr)
    (map (lambda (e) (expand-quasiquote e env)) expr))
   
   ;; それ以外はそのまま
   (else
    expr)))

;; ============================================
;; 組み込み演算関数
;; ============================================

(define (apply-+ args)
  (if (null? args)
      0
      (let loop ((sum 0) (rest args))
        (if (null? rest)
            sum
            (loop (+ sum (car rest)) (cdr rest))))))

(define (apply-- args)
  (if (null? args)
      0
      (let ((first (car args)) (rest (cdr args)))
        (if (null? rest)
            (- first)
            (let loop ((result first) (rest rest))
              (if (null? rest)
                  result
                  (loop (- result (car rest)) (cdr rest))))))))

(define (apply-* args)
  (if (null? args)
      1
      (let loop ((product 1) (rest args))
        (if (null? rest)
            product
            (loop (* product (car rest)) (cdr rest))))))

(define (apply-/ args)
  (if (null? args)
      (error "Division requires at least one argument")
      (let ((first (car args)) (rest (cdr args)))
        (if (null? rest)
            (/ 1 first)
            (let loop ((result first) (rest rest))
              (if (null? rest)
                  result
                  (let ((divisor (car rest)))
                    (if (= divisor 0)
                        (error "Division by zero")
                        (loop (/ result divisor) (cdr rest))))))))))

;; ============================================
;; テスト用のヘルパー関数
;; ============================================

(define (test-macros)
  (display "=== マクロとメタプログラミングのテスト ===\n\n")
  
  (let ((env '()))
    ;; 組み込み演算を環境に追加
    (set! env (extend-env '+ '+ env))
    (set! env (extend-env '- '- env))
    (set! env (extend-env '* '* env))
    (set! env (extend-env '/ '/ env))
    
    ;; テスト1: quote
    (display "--- テスト1: quote ---\n")
    (let* ((expr '(quote (+ 1 2)))
           (result (eval-expr expr env)))
      (display "式: ")
      (display expr)
      (display "\n結果: ")
      (display result)
      (display "\n期待: (+ 1 2)\n")
      (if (equal? result '(+ 1 2))
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト2: quasiquote（単純なケース）
    (display "--- テスト2: quasiquote（単純） ---\n")
    (let* ((expr '(quasiquote (1 2 3)))
           (result (eval-expr expr env)))
      (display "式: ")
      (display expr)
      (display "\n結果: ")
      (display result)
      (display "\n期待: (1 2 3)\n")
      (if (equal? result '(1 2 3))
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト3: quasiquote + unquote（値の展開）
    (display "--- テスト3: quasiquote + unquote ---\n")
    (let* ((expr '(quasiquote (1 (unquote (+ 2 3)) 4)))
           (result (eval-expr expr env)))
      (display "式: ")
      (display expr)
      (display "\n結果: ")
      (display result)
      (display "\n期待: (1 5 4)\n")
      (if (equal? result '(1 5 4))
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト4: quote で変数を保持
    (display "--- テスト4: quote で変数シンボルを保持 ---\n")
    (let* ((expr '(quote x))
           (result (eval-expr expr env)))
      (display "式: ")
      (display expr)
      (display "\n結果: ")
      (display result)
      (display "\n期待: x\n")
      (if (equal? result 'x)
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト5: quasiquote + unquote（ネスト）
    (display "--- テスト5: quasiquote + unquote（ネスト） ---\n")
    (let* ((expr '(quasiquote ((unquote (* 2 3)) (unquote (+ 1 2)))))
           (result (eval-expr expr env)))
      (display "式: ")
      (display expr)
      (display "\n結果: ")
      (display result)
      (display "\n期待: (6 3)\n")
      (if (equal? result '(6 3))
          (display "✓ OK\n\n")
          (display "✗ NG\n\n"))))
  
  (display "=== マクロの概念 ===\n")
  (display "quote: 式を評価せずにそのまま返す\n")
  (display "  (quote (+ 1 2)) => (+ 1 2)  ; 評価されない\n")
  (display "\n")
  (display "quasiquote: 部分的に評価できるquote\n")
  (display "  (quasiquote (1 (unquote (+ 2 3)) 4)) => (1 5 4)\n")
  (display "  このように、quasiquote内でunquoteを使うと、\n")
  (display "  その部分だけが評価されます。\n")
  (display "\n")
  (display "これはマクロ展開の基礎です。\n")
  (display "実際のマクロシステムでは、マクロ展開時にコードを変換します。\n"))

;; 実行
(test-macros)

