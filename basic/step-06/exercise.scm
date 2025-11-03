#lang racket

(require rackunit rackunit/text-ui)

;; 目的: マクロ入門（構文テンプレートで小さな糖衣構文を作る）

;; TODO: 実装してください
;; sum: (sum 1 2 3) => (+ 1 2 3)
(define-syntax-rule (sum . xs)
  (error "TODO: sum マクロを実装してください"))

;; when-let: (when-let (x expr) body...) => (let ([x expr]) (when x body...))
(define-syntax-rule (when-let (name expr) body ...)
  (error "TODO: when-let マクロを実装してください"))

;; ->（スレッディング）: (-> x (f a) (g b)) => (g (f x a) b)
(define-syntax (-> stx)
  (syntax-case stx ()
    [(_ x)
     #'x]
    [(_ x (f args ...) more ...)
     (error "TODO: -> マクロを実装してください")]))


(define tests
  (test-suite
   "step-06"
   (test-case "sum"
     (check-equal? (sum) 0)
     (check-equal? (sum 1) 1)
     (check-equal? (sum 1 2 3 4) 10))

   (test-case "when-let"
     (define (pos? x) (and x (> x 0)))
     (check-equal?
      (when-let (x 10)
        (and (pos? x) (* x 2)))
      #t)
     (check-equal?
      (when-let (x #f)
        (error "not reached"))
      (void)))

   (test-case "-> threading"
     (define (f x a) (+ x a))
     (define (g x b) (* x b))
     (check-equal? (-> 1 (f 2) (g 3)) 9)
     (check-equal? (-> "a" (string-append "b") (string-append "c")) "abc"))))

(module+ main
  (displayln "=== step-06: マクロ入門 ===")
  (run-tests tests))



