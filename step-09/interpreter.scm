;; ステップ9: 統合インタプリタ
;; レキサー → パーサー → 評価器を統合した完全なインタプリタ

;; ============================================
;; レキサー（step-01から）
;; ============================================

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

;; ============================================
;; パーサー（step-02から）
;; ============================================

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
            (parse-list tokens (+ pos 1)))
           ((string=? token ")")
            (error "Unexpected closing parenthesis"))
           (else
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
              (list (reverse result) (+ pos 1)))
             ((string=? token "(")
              (let* ((nested-result (parse-list tokens (+ pos 1)))
                     (nested-expr (car nested-result))
                     (next-pos (cadr nested-result)))
                (loop next-pos (cons nested-expr result))))
             (else
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

;; ============================================
;; 評価器（step-06をベースに拡張、step-08のマクロ機能も含む）
;; ============================================

;; グローバル環境
(define *global-env* '())

;; マクロ環境（グローバル環境とは別に管理）
(define *macro-env* '())

(define (init-global-env)
  "グローバル環境を初期化"
  (set! *global-env* '())
  (set! *global-env* (extend-env '+ '+ *global-env*))
  (set! *global-env* (extend-env '- '- *global-env*))
  (set! *global-env* (extend-env '* '* *global-env*))
  (set! *global-env* (extend-env '/ '/ *global-env*))
  (set! *global-env* (extend-env '= '= *global-env*))
  (set! *global-env* (extend-env '< '< *global-env*))
  (set! *global-env* (extend-env '> '> *global-env*))
  (set! *global-env* (extend-env '<= '<= *global-env*))
  (set! *global-env* (extend-env '>= '>= *global-env*))
  ;; マクロ環境も初期化
  (set! *macro-env* '())
  *global-env*)

(define (eval-expr expr env)
  "式を評価する（統合版）"
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
           ;; quote: 式を評価せずにそのまま返す
           ((eq? operator 'quote)
            (eval-quote operands env))
           
           ;; quasiquote: 部分的に評価できるquote
           ((eq? operator 'quasiquote)
            (eval-quasiquote operands env))
           
           ;; if: 条件分岐
           ((eq? operator 'if)
            (eval-if operands env))
           
           ;; let: 局所変数の定義
           ((eq? operator 'let)
            (eval-let operands env))
           
           ;; letrec: 相互再帰を含む局所定義
           ((eq? operator 'letrec)
            (eval-letrec operands env))
           
           ;; define: 変数・関数を定義
           ((eq? operator 'define)
            (eval-define operands env))
           
           ;; define-syntax: マクロを定義
           ((eq? operator 'define-syntax)
            (eval-define-syntax operands env))
           
           ;; lambda: 関数を定義
           ((eq? operator 'lambda)
            (eval-lambda operands env))
           
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

(define (truthy? value)
  "値が真かどうかを判定"
  (not (eq? value #f)))

;; quote, quasiquote (step-08から)
(define (eval-quote args env)
  "quote を評価する"
  (if (null? args)
      (error "quote requires one argument")
      (car args)))

(define (eval-quasiquote args env)
  "quasiquote を評価する"
  (if (null? args)
      (error "quasiquote requires one argument")
      (expand-quasiquote (car args) env)))

(define (expand-quasiquote expr env)
  "quasiquote内の式を展開する"
  (cond
   ((and (list? expr)
         (not (null? expr))
         (eq? (car expr) 'unquote))
    (if (< (length expr) 2)
        (error "unquote requires one argument")
        (eval-expr (cadr expr) env)))
   ((list? expr)
    (map (lambda (e) (expand-quasiquote e env)) expr))
   (else
    expr)))

;; if (step-06から)
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

;; let, letrec (step-07から)
(define (eval-let args env)
  "let を評価する"
  (if (< (length args) 2)
      (error "let requires at least 2 arguments"))
  
  (let ((bindings (car args))
        (body (cadr args)))
    (let ((new-env (add-bindings bindings env env)))
      (eval-expr body new-env))))

(define (add-bindings bindings eval-env new-env)
  "bindingsを新しい環境に追加"
  (if (null? bindings)
      new-env
      (let ((binding (car bindings)))
        (if (not (list? binding))
            (error "let binding must be a list")
            (let ((var (car binding))
                  (value-expr (cadr binding)))
              (let ((value (eval-expr value-expr eval-env)))
                (add-bindings (cdr bindings)
                             eval-env
                             (extend-env var value new-env))))))))

(define (eval-letrec args env)
  "letrec を評価する（簡易版）"
  (if (< (length args) 2)
      (error "letrec requires at least 2 arguments"))
  
  ;; 簡易実装：letと同等に扱う
  ;; （完全な実装は複雑なため、ここでは簡易版）
  (eval-let args env))

(define (eval-define args env)
  "define を評価する（グローバル環境を更新）"
  (if (< (length args) 2)
      (error "define requires at least 2 arguments"))
  
  (let ((name (car args))
        (value-expr (cadr args)))
    (if (not (symbol? name))
        (error "define: first argument must be a symbol"))
    
    (let ((value (eval-expr value-expr env)))
      (set! *global-env* (extend-env name value *global-env*))
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

;; 論理演算
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

;; ============================================
;; define-syntax と syntax-rules の実装（step-08から）
;; ============================================

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
;; インタプリタ（統合インターフェース）
;; ============================================

(define (interpret code-str)
  "文字列のプログラムコードを実行する"
  (let ((env (init-global-env)))
    (let* ((tokens (tokenize code-str))
           (ast (parse tokens))
           (result (eval-expr ast env)))
      result)))

(define (repl)
  "REPL（Read-Eval-Print Loop）を起動"
  (init-global-env)
  (display "Middle Schemer REPL\n")
  (display "Type 'exit' to quit\n")
  (display "Note: REPL機能は処理系によって異なります。\n")
  (display "基本的には (interpret \"コード\") を使用してください。\n\n"))

;; ============================================
;; テスト
;; ============================================

(define (test-interpreter)
  (display "=== 統合インタプリタのテスト ===\n\n")
  
  (let ((test-cases '(("(+ 1 2)" . 3)
                      ("(* 3 4)" . 12)
                      ("(if (> 5 3) 10 20)" . 10)
                      ("(if (< 5 3) 10 20)" . 20)
                      ("(and (> 5 3) (< 2 4))" . #t))))
    (for-each
     (lambda (test-case)
       (let* ((code (car test-case))
              (expected (cdr test-case))
              (result (interpret code)))
         (display "コード: ")
         (display code)
         (display "\n結果: ")
         (display result)
         (display "\n期待: ")
         (display expected)
         (display "\n")
         (if (equal? result expected)
             (display "✓ OK\n\n")
             (display "✗ NG\n\n"))))
     test-cases))
  
  (display "=== REPLを起動しますか？ ===\n")
  (display "テストを終了してREPLを起動する場合は、以下のコマンドを実行してください：\n")
  (display "  (repl)\n\n"))

;; 実行（テストのみ。REPLは処理系依存のため簡易版）
(test-interpreter)

;; 使用例:
;; (interpret "(+ 1 2)")  ; => 3
;; (interpret "(define x 10)")
;; (interpret "(+ x 5)")  ; => 15
;; 
;; マクロの使用例:
;; (interpret "(define-syntax unless (syntax-rules () ((_ condition body) (if (not condition) body))))")
;; (interpret "(unless #f (display \"Hello\"))")

