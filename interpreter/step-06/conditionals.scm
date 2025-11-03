#lang racket

;; ステップ6: 条件分岐とブール値
;; if, 比較演算, 論理演算を実装する

;; ============================================
;; 基本的な評価器（step-04をベースに拡張）
;; ============================================

(define (eval-expr expr env)
  "式を評価する（条件分岐対応版）"
  (cond
   ;; 数値はそのまま返す
   ((number? expr)
    expr)
   
   ;; 真偽値
   ((eq? expr '#t)
    #t)
   ((eq? expr '#f)
    #f)
   
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
           ;; if: 条件分岐
           ((eq? operator 'if)
            (eval-if operands env))
           
           ;; 比較演算
           ((eq? operator '=)
            (apply-= (map (lambda (e) (eval-expr e env)) operands)))
           ((eq? operator '<)
            (apply-< (map (lambda (e) (eval-expr e env)) operands)))
           ((eq? operator '>)
            (apply-> (map (lambda (e) (eval-expr e env)) operands)))
           ((eq? operator '<=)
            (apply-<= (map (lambda (e) (eval-expr e env)) operands)))
           ((eq? operator '>=)
            (apply->= (map (lambda (e) (eval-expr e env)) operands)))
           
           ;; 論理演算
           ((eq? operator 'and)
            (apply-and operands env))
           ((eq? operator 'or)
            (apply-or operands env))
           ((eq? operator 'not)
            (apply-not (map (lambda (e) (eval-expr e env)) operands)))
           
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

(define (eval-if args env)
  "if を評価する"
  (if (< (length args) 2)
      (error "if requires at least 2 arguments"))
  
  (let ((condition (car args))
        (then-expr (cadr args))
        (else-expr (if (null? (cddr args))
                       #f
                       (caddr args))))
    (let ((condition-value (eval-expr condition env)))
      (if (truthy? condition-value)
          (eval-expr then-expr env)
          (if else-expr
              (eval-expr else-expr env)
              #f)))))

(define (truthy? value)
  "値が真かどうかを判定（Schemeの仕様に従う）"
  ;; Schemeでは、#fのみが偽、それ以外はすべて真
  (not (eq? value #f)))

(define (eval-define args env)
  "define を評価する"
  (if (< (length args) 2)
      (error "define requires at least 2 arguments"))
  
  (let ((name (car args))
        (value-expr (cadr args)))
    (if (not (symbol? name))
        (error "define: first argument must be a symbol"))
    
    (let ((value (eval-expr value-expr env)))
      (set! env (extend-env name value env))
      value)))

(define (eval-lambda args env)
  "lambda を評価してクロージャを作成"
  (if (< (length args) 2)
      (error "lambda requires at least 2 arguments"))
  
  (let ((params (car args))
        (body (cadr args)))
    (list 'closure params body env)))

(define (eval-application operator operands env)
  "関数適用を評価する"
  (let ((func (eval-expr operator env)))
    (cond
     ((eq? func '+)
      (apply-+ (map (lambda (e) (eval-expr e env)) operands)))
     ((eq? func '-)
      (apply-- (map (lambda (e) (eval-expr e env)) operands)))
     ((eq? func '*)
      (apply-* (map (lambda (e) (eval-expr e env)) operands)))
     ((eq? func '/)
      (apply-/ (map (lambda (e) (eval-expr e env)) operands)))
     ((and (list? func) (eq? (car func) 'closure))
      (apply-closure func operands env))
     (else
      (error "Not a function:" func)))))

(define (apply-closure closure args env)
  "クロージャを適用する"
  (let ((params (cadr closure))
        (body (caddr closure))
        (closure-env (cadddr closure)))
    (let ((arg-values (map (lambda (e) (eval-expr e env)) args)))
      (let ((new-env (bind-params params arg-values closure-env)))
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

;; 比較演算
(define (apply-= args)
  (if (null? args)
      #t
      (let ((first (car args)))
        (let loop ((rest (cdr args)))
          (if (null? rest)
              #t
              (if (= first (car rest))
                  (loop (cdr rest))
                  #f))))))

(define (apply-< args)
  (if (null? args)
      #t
      (let loop ((prev (car args))
                 (rest (cdr args)))
        (if (null? rest)
            #t
            (let ((current (car rest)))
              (if (< prev current)
                  (loop current (cdr rest))
                  #f))))))

(define (apply-> args)
  (if (null? args)
      #t
      (let loop ((prev (car args))
                 (rest (cdr args)))
        (if (null? rest)
            #t
            (let ((current (car rest)))
              (if (> prev current)
                  (loop current (cdr rest))
                  #f))))))

(define (apply-<= args)
  (if (null? args)
      #t
      (let loop ((prev (car args))
                 (rest (cdr args)))
        (if (null? rest)
            #t
            (let ((current (car rest)))
              (if (<= prev current)
                  (loop current (cdr rest))
                  #f))))))

(define (apply->= args)
  (if (null? args)
      #t
      (let loop ((prev (car args))
                 (rest (cdr args)))
        (if (null? rest)
            #t
            (let ((current (car rest)))
              (if (>= prev current)
                  (loop current (cdr rest))
                  #f))))))

;; 論理演算（短絡評価）
(define (apply-and args env)
  "and を評価する（短絡評価）"
  (if (null? args)
      #t
      (let loop ((rest args))
        (if (null? rest)
            #t
            (let ((value (eval-expr (car rest) env)))
              (if (truthy? value)
                  (let ((next (cdr rest)))
                    (if (null? next)
                        value
                        (loop next)))
                  #f))))))

(define (apply-or args env)
  "or を評価する（短絡評価）"
  (if (null? args)
      #f
      (let loop ((rest args))
        (if (null? rest)
            #f
            (let ((value (eval-expr (car rest) env)))
              (if (truthy? value)
                  value
                  (loop (cdr rest))))))))

(define (apply-not args)
  "not を評価する"
  (if (null? args)
      (error "not requires one argument")
      (not (truthy? (car args)))))

;; 組み込み演算関数
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
(define (test-conditionals)
  (display "=== 条件分岐とブール値のテスト ===\n\n")
  
  (let ((env '()))
    ;; 組み込み演算を環境に追加
    (set! env (extend-env '+ '+ env))
    (set! env (extend-env '- '- env))
    (set! env (extend-env '* '* env))
    (set! env (extend-env '/ '/ env))
    
    ;; テスト1: if（真の場合）
    (display "--- テスト1: if（真の場合） ---\n")
    (let* ((expr '(if (> 5 3) 10 20))
           (result (eval-expr expr env)))
      (display "式: (if (> 5 3) 10 20)\n")
      (display "結果: ")
      (display result)
      (display "\n期待: 10\n")
      (if (= result 10)
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト2: if（偽の場合）
    (display "--- テスト2: if（偽の場合） ---\n")
    (let* ((expr '(if (< 5 3) 10 20))
           (result (eval-expr expr env)))
      (display "式: (if (< 5 3) 10 20)\n")
      (display "結果: ")
      (display result)
      (display "\n期待: 20\n")
      (if (= result 20)
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト3: 比較演算
    (display "--- テスト3: 比較演算 ---\n")
    (let* ((test-cases '((= 5 5) . #t)
                        ((= 5 3) . #f)
                        ((< 3 5) . #t)
                        ((> 5 3) . #t)
                        ((<= 3 5) . #t)
                        ((>= 5 3) . #t)))
      (for-each
       (lambda (test-case)
         (let* ((expr (car test-case))
                (expected (cdr test-case))
                (result (eval-expr expr env)))
           (display "式: ")
           (display expr)
           (display " => ")
           (display result)
           (display "\n")
           (if (equal? result expected)
               (display "✓ OK\n")
               (display "✗ NG\n"))))
       (list (cons '(= 5 5) #t)
             (cons '(= 5 3) #f)
             (cons '(< 3 5) #t)
             (cons '(> 5 3) #t))))
    
    ;; テスト4: 論理演算（and）
    (display "\n--- テスト4: 論理演算（and） ---\n")
    (let* ((expr '(and (> 5 3) (< 2 4)))
           (result (eval-expr expr env)))
      (display "式: (and (> 5 3) (< 2 4))\n")
      (display "結果: ")
      (display result)
      (display "\n期待: #t\n")
      (if (truthy? result)
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト5: 論理演算（or）
    (display "--- テスト5: 論理演算（or） ---\n")
    (let* ((expr '(or (< 5 3) (> 2 1)))
           (result (eval-expr expr env)))
      (display "式: (or (< 5 3) (> 2 1))\n")
      (display "結果: ")
      (display result)
      (display "\n期待: #t\n")
      (if (truthy? result)
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))))
  
  (display "=== 真偽値の扱い ===\n")
  (display "Schemeでは、#fのみが偽、それ以外はすべて真です。\n")
  (display "例: (if 0 'true 'false) => 'true  (0は真)\n")
  (display "    (if '() 'true 'false) => 'false  (空リストは偽の場合もある)\n"))

;; 実行
(test-conditionals)

