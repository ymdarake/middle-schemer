;; ステップ5: クロージャとスコープ
;; レキシカルスコープとクロージャの正しい実装を確認する

;; ============================================
;; 基本的な評価器（step-04をベースに）
;; ============================================

(define (eval-expr expr env)
  "式を評価する（クロージャ対応版）"
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
  "環境からシンボルの値を取得（レキシカルスコープ）"
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
  "define を評価する（環境を変更する）"
  (if (< (length args) 2)
      (error "define requires at least 2 arguments"))
  
  (let ((name (car args))
        (value-expr (cadr args)))
    (if (not (symbol? name))
        (error "define: first argument must be a symbol"))
    
    (let ((value (eval-expr value-expr env)))
      ;; 環境を拡張（環境は変更される）
      (set! env (extend-env name value env))
      value)))

(define (eval-lambda args env)
  "lambda を評価してクロージャを作成（環境を保存）"
  (if (< (length args) 2)
      (error "lambda requires at least 2 arguments"))
  
  (let ((params (car args))
        (body (cadr args)))
    ;; クロージャ: (closure params body env)
    ;; 重要: 現在の環境を保存する（レキシカルスコープ）
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
  "クロージャを適用する（レキシカルスコープ）"
  (let ((params (cadr closure))
        (body (caddr closure))
        (closure-env (cadddr closure)))  ; クロージャが保存した環境
    ;; 引数を評価
    (let ((arg-values (map (lambda (e) (eval-expr e env)) args)))
      ;; パラメータと引数を束縛した新しい環境を作成
      ;; 重要: クロージャの環境（closure-env）を継承する
      (let ((new-env (bind-params params arg-values closure-env)))
        ;; 関数本体を評価（クロージャの環境を継承）
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

;; ============================================
;; テスト用のヘルパー関数
;; ============================================

(define (test-closures)
  (display "=== クロージャとスコープのテスト ===\n\n")
  
  (let ((env '()))
    ;; 組み込み演算を環境に追加
    (set! env (extend-env '+ '+ env))
    (set! env (extend-env '- '- env))
    (set! env (extend-env '* '* env))
    (set! env (extend-env '/ '/ env))
    
    ;; テスト1: 基本的なクロージャ（自由変数なし）
    (display "--- テスト1: 基本的なクロージャ ---\n")
    (let* ((lambda-expr '(lambda (x) (* x x)))
           (closure (eval-expr lambda-expr env))
           (apply-expr (list closure 5))
           (result (eval-expr apply-expr env)))
      (display "関数: ")
      (display lambda-expr)
      (display "\n適用: ((lambda (x) (* x x)) 5)")
      (display "\n結果: ")
      (display result)
      (display "\n期待: 25\n")
      (if (= result 25)
          (display "✓ OK\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト2: クロージャが外側の変数を参照（レキシカルスコープ）
    (display "--- テスト2: クロージャが外側の変数を参照 ---\n")
    (let* ((outer-var (cons 'x 10))
           (new-env (cons outer-var env))
           (lambda-expr '(lambda (y) (+ x y)))  ; xは外側の変数
           (closure (eval-expr lambda-expr new-env))
           (apply-expr (list closure 5))
           (result (eval-expr apply-expr new-env)))
      (display "外側の変数 x = 10\n")
      (display "関数: (lambda (y) (+ x y))\n")
      (display "適用: ((lambda (y) (+ x y)) 5)\n")
      (display "結果: ")
      (display result)
      (display "\n期待: 15 (10 + 5)\n")
      (if (= result 15)
          (display "✓ OK（レキシカルスコープが正しく動作）\n\n")
          (display "✗ NG\n\n")))
    
    ;; テスト3: 高階関数（関数を返す関数）
    (display "--- テスト3: 高階関数（make-adder） ---\n")
    (let* ((make-adder '(lambda (n) (lambda (x) (+ x n))))
           (make-adder-closure (eval-expr make-adder env))
           (add10-expr (list make-adder-closure 10))
           (add10-closure (eval-expr add10-expr env))
           (apply-expr (list add10-closure 5))
           (result (eval-expr apply-expr env)))
      (display "make-adder: (lambda (n) (lambda (x) (+ x n)))\n")
      (display "add10 = (make-adder 10)\n")
      (display "適用: (add10 5)\n")
      (display "結果: ")
      (display result)
      (display "\n期待: 15 (5 + 10)\n")
      (if (= result 15)
          (display "✓ OK（クロージャが外側のnを正しく参照）\n\n")
          (display "✗ NG\n\n"))))
  
  (display "=== レキシカルスコープの説明 ===\n")
  (display "レキシカルスコープでは、関数が定義された時点の環境が保存されます。\n")
  (display "これにより、関数内で外側の変数を参照できます。\n")
  (display "\n")
  (display "例:\n")
  (display "  (define n 10)\n")
  (display "  (define make-adder (lambda (n) (lambda (x) (+ x n))))\n")
  (display "  (define add10 (make-adder 10))\n")
  (display "  (add10 5)  ; => 15\n")
  (display "\n")
  (display "ここで、add10はn=10の値を保持したクロージャです。\n"))

;; 実行
(test-closures)

