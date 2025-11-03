#lang racket

(require rackunit rackunit/text-ui)

;; 解答: step-05

(define (quote-list a b c)
  (list a b c))

(define (make-call sym args)
  `(,sym ,@args))

(define (let-binding name expr body)
  `(let ((,name ,expr)) ,body))

(define (alist->hash-expr pairs)
  (define flat
    (apply append
           (map (λ (p) (list (list 'quote (car p)) (cadr p))) pairs)))
  `(hash ,@flat))

(define tests
  (test-suite
   "step-05-solution"
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
     (check-equal? (alist->hash-expr '()) '(hash)))))

(module+ main
  (displayln "=== step-05 solution ===")
  (run-tests tests))
