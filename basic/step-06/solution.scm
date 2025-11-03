#lang racket

(require rackunit rackunit/text-ui)

;; 解答: step-06

;; sum: (sum 1 2 3) => (+ 1 2 3), 空は0
(define-syntax-rule (sum)
  0)
(define-syntax-rule (sum xs ...)
  (+ xs ...))

;; when-let: (when-let (x expr) body...) => (let ([x expr]) (when x body...))
(define-syntax-rule (when-let (name expr) body ...)
  (let ([name expr])
    (when name body ...)))

;; ->: (-> x (f a) (g b)) => (g (f x a) b)
(define-syntax (-> stx)
  (syntax-case stx ()
    [(_ x)
     #'x]
    [(_ x (f args ...) more ...)
     #'(-> (f x args ...) more ...)]))

(define tests
  (test-suite
   "step-06-solution"
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
  (displayln "=== step-06 solution ===")
  (run-tests tests))
