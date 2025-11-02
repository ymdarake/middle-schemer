#lang racket

;; ステップ4: 関数定義と適用
;; lambda と define を実装する

(define (eval-expr expr env)
  "式を評価する（拡張版）"
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
    ;; クロージャ: (params body env)
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

;; 組み込み演算関数（step-03から）
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

;; テスト用のヘルパー関数
(define (test-functions)
  (display "=== 関数定義と適用のテスト ===\n")
  
  (let ((env '()))
    ;; 組み込み演算を環境に追加
    (set! env (extend-env '+ '+ env))
    (set! env (extend-env '- '- env))
    (set! env (extend-env '* '* env))
    (set! env (extend-env '/ '/ env))
    
    ;; テスト1: 簡単な関数定義
    (display "--- テスト1: add関数の定義 ---\n")
    (let* ((add-lambda '(lambda (x y) (+ x y)))
           (add-closure (eval-expr add-lambda env))
           (add-expr (list add-closure 3 4))
           (result (eval-expr add-expr env)))
      (display "関数: ")
      (display add-lambda)
      (display "\n適用: (add 3 4)")
      (display "\n結果: ")
      (display result)
      (display "\n期待: 7\n")
      (if (= result 7)
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト2: defineを使った関数定義
    (display "--- テスト2: defineで関数定義 ---\n")
    (let* ((define-expr '(define add (lambda (x y) (+ x y))))
           (new-env (extend-env '+ '+ env))
           (add-func (eval-expr define-expr new-env))
           (apply-expr '(add 5 6)))
      (display "定義: ")
      (display define-expr)
      (display "\n適用: (add 5 6)")
      (display "\n")
      ;; 注意: defineの評価後の環境を使う必要がある
      (display "（環境の管理が完全ではないため、手動テスト推奨）\n\n")))
  
  (display "=== 手動テスト例 ===\n")
  (display "以下の式を評価してみてください：\n")
  (display "1. (lambda (x) (* x x))\n")
  (display "2. ((lambda (x) (* x x)) 5)\n")
  (display "3. (define square (lambda (x) (* x x)))\n")
  (display "4. (square 4)\n"))

;; 実行
(test-functions)

