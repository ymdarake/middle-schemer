#lang racket

;; ステップ3: 基本的な評価器
;; 構文木を評価して値を得る

(define (eval-expr expr env)
  "式を評価する"
  (cond
   ;; 数値はそのまま返す
   ((number? expr)
    expr)
   
   ;; シンボルは環境から値を取得
   ((symbol? expr)
    (let ((value (assoc expr env)))
      (if value
          (cdr value)
          (error "Undefined variable:" expr))))
   
   ;; リストは関数適用として評価
   ((list? expr)
    (if (null? expr)
        '()
        (let ((operator (car expr))
              (operands (cdr expr)))
          (cond
           ;; 加算
           ((eq? operator '+)
            (apply-+ (map (lambda (e) (eval-expr e env)) operands)))
           
           ;; 減算
           ((eq? operator '-)
            (apply-- (map (lambda (e) (eval-expr e env)) operands)))
           
           ;; 乗算
           ((eq? operator '*)
            (apply-* (map (lambda (e) (eval-expr e env)) operands)))
           
           ;; 除算
           ((eq? operator '/)
            (apply-/ (map (lambda (e) (eval-expr e env)) operands)))
           
           (else
            (error "Unknown operator:" operator))))))
   
   (else
    (error "Cannot evaluate:" expr))))

;; 演算関数
(define (apply-+ args)
  "加算を実行"
  (if (null? args)
      0
      (let loop ((sum 0)
                 (rest args))
        (if (null? rest)
            sum
            (loop (+ sum (car rest)) (cdr rest))))))

(define (apply-- args)
  "減算を実行"
  (if (null? args)
      0
      (let ((first (car args))
            (rest (cdr args)))
        (if (null? rest)
            (- first)
            (let loop ((result first)
                       (rest rest))
              (if (null? rest)
                  result
                  (loop (- result (car rest)) (cdr rest))))))))

(define (apply-* args)
  "乗算を実行"
  (if (null? args)
      1
      (let loop ((product 1)
                 (rest args))
        (if (null? rest)
            product
            (loop (* product (car rest)) (cdr rest))))))

(define (apply-/ args)
  "除算を実行"
  (if (null? args)
      (error "Division requires at least one argument")
      (let ((first (car args))
            (rest (cdr args)))
        (if (null? rest)
            (/ 1 first)
            (let loop ((result first)
                       (rest rest))
              (if (null? rest)
                  result
                  (let ((divisor (car rest)))
                    (if (= divisor 0)
                        (error "Division by zero")
                        (loop (/ result divisor) (cdr rest))))))))))

;; テスト用のヘルパー関数
(define (test-evaluator)
  (display "=== 評価器のテスト ===\n")
  
  (let ((empty-env '())
        (test-cases '(((+ 1 2) . 3)
                      ((- 10 3) . 7)
                      ((* 3 4) . 12)
                      ((/ 12 3) . 4)
                      ((+ 1 2 3 4) . 10)
                      ((- 10 3 2) . 5)
                      ((* 2 3 4) . 24)
                      ((/ 24 2 3) . 4)
                      ((+ (+ 1 2) 3) . 6)
                      ((- (* 3 4) 5) . 7))))
    (for-each
     (lambda (test-case)
       (let* ((expr (car test-case))
              (expected (cdr test-case))
              (result (eval-expr expr empty-env)))
         (display "式: ")
         (display expr)
         (display "\n評価結果: ")
         (display result)
         (display "\n期待値: ")
         (display expected)
         (display "\n")
         (if (equal? result expected)
             (display "✓ OK\n\n")
             (begin
               (display "✗ NG\n\n")))))
     test-cases)))

;; 実行
(test-evaluator)

