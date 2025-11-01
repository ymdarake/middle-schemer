;; ステップ8: マクロとメタプログラミング
;; quote, quasiquote, unquote と define-syntax, syntax-rules を実装する

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
           
           ;; define-syntax: マクロを定義
           ((eq? operator 'define-syntax)
            (eval-define-syntax operands env))
           
           ;; lambda: 関数を定義
           ((eq? operator 'lambda)
            (eval-lambda operands env))
           
           ;; 関数適用（その他）
           (else
            ;; マクロ展開を試みる
            (let ((maybe-expanded (expand-macro expr env)))
              (if (eq? maybe-expanded expr)
                  ;; マクロでない場合は通常の関数適用
                  (eval-application operator operands env)
                  ;; マクロ展開された場合は再帰的に評価
                  (eval-expr maybe-expanded env))))))))
   
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

(define (eval-define-syntax args env)
  "define-syntax を評価する：マクロを定義"
  (if (< (length args) 2)
      (error "define-syntax requires at least 2 arguments"))
  
  (let ((name (car args))
        (transformer (cadr args)))
    (if (not (symbol? name))
        (error "define-syntax: first argument must be a symbol"))
    
    ;; マクロはsyntax-rulesで定義される
    (if (not (list? transformer))
        (error "define-syntax: second argument must be a syntax-rules form"))
    
    (if (not (eq? (car transformer) 'syntax-rules))
        (error "define-syntax: currently only syntax-rules is supported"))
    
    ;; マクロトランスフォーマーをマクロ環境に追加
    (set! *macro-env* (cons (cons name (cons 'macro transformer)) *macro-env*))
    #t)))  ; define-syntax自体は値を返さない

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
;; define-syntax と syntax-rules の実装
;; ============================================

;; マクロ環境（グローバル環境とは別に管理）
(define *macro-env* '())

(define (expand-macro expr env)
  "マクロ展開を試みる。マクロでない場合はそのまま返す"
  (if (not (list? expr))
      expr
      (if (null? expr)
          '()
          (let ((operator (car expr)))
            (if (symbol? operator)
                (let ((macro-binding (assoc operator *macro-env*)))
                  (if (and macro-binding
                           (pair? (cdr macro-binding))
                           (eq? (cadr macro-binding) 'macro))
                      ;; マクロが見つかった
                      (let ((transformer (cddr macro-binding)))
                        (apply-syntax-rules transformer
                                           (cdr expr)
                                           env))
                      ;; マクロでない
                      expr))
                expr)))))

(define (apply-syntax-rules transformer args env)
  "syntax-rules を使ってマクロ展開を実行（簡易版）"
  ;; transformer の形式: (syntax-rules (literal ...) ((pattern template) ...))
  ;; 注意: これは簡易実装です。実際のsyntax-rulesはもっと複雑です。
  (if (< (length transformer) 2)
      (error "syntax-rules: invalid form"))
  
  (let ((literals (if (null? (cadr transformer))
                      '()
                      (cadr transformer)))
        (rules (cddr transformer)))
    ;; 引数をパターンに合わせて展開（argsのcarを削除してpatternとマッチ）
    ;; 実際の実装では、パターンの最初の要素が_の場合は無視する
    (let ((pattern-args (if (and (not (null? args))
                                  (symbol? (car args)))
                            (cdr args)
                            args)))
      ;; 最初にマッチするルールを見つける
      (let loop ((rest-rules rules))
        (if (null? rest-rules)
            (error "syntax-rules: no matching pattern")
            (let ((rule (car rest-rules)))
              (if (not (and (list? rule)
                            (= (length rule) 2)))
                  (error "syntax-rules: invalid rule format")
                  (let ((pattern (car rule))
                        (template (cadr rule)))
                    ;; パターンマッチング（簡易版）
                    ;; パターンの最初の要素（通常は_）をスキップ
                    (let ((pattern-to-match (if (and (list? pattern)
                                                      (not (null? pattern)))
                                                (cdr pattern)
                                                pattern)))
                      (let ((match-result (match-pattern pattern-to-match pattern-args literals)))
                        (if match-result
                            ;; マッチした場合はテンプレートを展開
                            (expand-template template match-result)
                            ;; マッチしなかった場合は次のルールを試す
                            (loop (cdr rest-rules)))))))))))))

(define (match-pattern pattern input literals)
  "パターンマッチング（簡易版）。マッチした場合は束縛のリストを返す"
  ;; 注意: これは非常に簡易版です。実際のsyntax-rulesはもっと複雑です。
  (cond
   ((symbol? pattern)
    ;; リテラルでないシンボルは変数として扱う
    (if (member pattern literals)
        ;; リテラルの場合は完全一致が必要
        (if (and (symbol? input) (eq? pattern input))
            '()
            #f)
        ;; 変数の場合は任意の値とマッチ
        (list (cons pattern input))))
   ((list? pattern)
    ;; リストの場合は再帰的にマッチ
    (if (not (list? input))
        #f
        (let ((pattern-len (length pattern))
              (input-len (length input)))
          (if (= pattern-len input-len)
              (let loop ((p-rest pattern)
                         (i-rest input)
                         (bindings '()))
                (if (null? p-rest)
                    bindings
                    (let ((p-item (car p-rest))
                          (i-item (car i-rest)))
                      (let ((match (match-pattern p-item i-item literals)))
                        (if match
                            (loop (cdr p-rest)
                                  (cdr i-rest)
                                  (append match bindings))
                            #f)))))
              #f))))
   (else
    ;; それ以外は完全一致が必要
    (if (equal? pattern input)
        '()
        #f))))

(define (expand-template template bindings)
  "テンプレートを展開する"
  (cond
   ((symbol? template)
    ;; シンボルの場合は束縛を探す
    (let ((binding (assoc template bindings)))
      (if binding
          (cdr binding)
          template)))
   ((list? template)
    ;; リストの場合は再帰的に展開
    (map (lambda (t) (expand-template t bindings)) template))
   (else
    ;; それ以外はそのまま
    template)))

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
  
  ;; マクロ環境を初期化
  (set! *macro-env* '())
  
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
  (display "実際のマクロシステムでは、マクロ展開時にコードを変換します。\n")
  (display "\n")
  (display "=== define-syntax と syntax-rules ===\n")
  (display "define-syntax と syntax-rules を使うと、より高度なマクロを定義できます。\n")
  (display "例:\n")
  (display "  (define-syntax unless\n")
  (display "    (syntax-rules ()\n")
  (display "      ((_ condition body)\n")
  (display "       (if (not condition) body))))\n")
  (display "\n")
  (display "注意: この実装は簡易版で、完全なsyntax-rulesの実装ではありません。\n")
  (display "実際のSchemeでは、より高度なパターンマッチングが可能です。\n"))

;; 実行
(test-macros)

