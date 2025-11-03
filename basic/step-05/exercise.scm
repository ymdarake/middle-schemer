#lang racket

(require rackunit rackunit/text-ui)

;; 目的: クォートと準クォート（'  `  ,  ,@）の基本とテンプレート構築

;; TODO: 実装してください
(define (quote-list a b c)
  ;; 例: (quote-list 'x 'y 'z) => '(x y z)
  (error "TODO: quote-list を実装してください"))

(define (make-call sym args)
  ;; 例: (make-call '+ '(1 2 3)) => '(+ 1 2 3)
  (error "TODO: make-call を実装してください"))

(define (let-binding name expr body)
  ;; 例: (let-binding 'x 10 '(* x x)) => '(let ((x 10)) (* x x))
  (error "TODO: let-binding を実装してください"))

(define (alist->hash-expr pairs)
  ;; 例: '((a 1) (b 2)) => '(hash 'a 1 'b 2)
  (error "TODO: alist->hash-expr を実装してください"))


(define tests
  (test-suite
   "step-05"
   (test-case "quote basics"
     (check-equal? (quote-list 'x 'y 'z) '(x y z))
     (check-equal? (quote 'a) 'a)
     (check-equal? '(1 2 3) (list 1 2 3)))

   (test-case "quasiquote call"
     (check-equal? (make-call '+ '(1 2 3)) '(+ 1 2 3))
     (check-equal? (make-call 'string-append '("a" "b")) '(string-append "a" "b")))

   (test-case "quasiquote let"
     (check-equal? (let-binding 'x 10 '(* x x)) '(let ((x 10)) (* x x)))
     (check-equal? (let-binding 's '(1 2) '(length s)) '(let ((s (1 2))) (length s))))

   (test-case "unquote-splicing"
     (check-equal? (alist->hash-expr '((a 1) (b 2))) '(hash 'a 1 'b 2))
     (check-equal? (alist->hash-expr '()) '(hash)))) )

(module+ main
  (displayln "=== step-05: クォート/準クォート ===")
  (run-tests tests))



