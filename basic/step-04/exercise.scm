#lang racket

(require rackunit rackunit/text-ui)

;; 目的: 関数と高階関数の基本（compose / partial / curry / flip / apply-n）

;; TODO: 実装してください
(define (compose f g)
  (error "TODO: compose を実装してください"))

(define (partial f . args)
  (error "TODO: partial を実装してください"))

(define (curry2 f)
  (error "TODO: curry2 を実装してください"))

(define (flip f)
  (error "TODO: flip を実装してください"))

(define (apply-n f n)
  (error "TODO: apply-n を実装してください"))


(define tests
  (test-suite
   "step-04"
   (test-case "compose"
     (define inc (λ (x) (+ x 1)))
     (define double (λ (x) (* x 2)))
     (check-equal? ((compose inc double) 3) 7)
     (check-equal? ((compose double inc) 3) 8))

   (test-case "partial"
     (define add3 (partial + 3))
     (check-equal? (add3 7) 10)
     (define join-dash (partial string-join #:separator "-"))
     (check-equal? (join-dash '("a" "b" "c")) "a-b-c"))

   (test-case "curry2"
     (define curried+ (curry2 +))
     (check-equal? ((curried+ 2) 5) 7)
     (define curried-str (curry2 string-append))
     (check-equal? ((curried-str "a") "b") "ab"))

   (test-case "flip"
     (define sub (λ (a b) (- a b)))
     (define rsub (flip sub))
     (check-equal? (sub 10 3) 7)
     (check-equal? (rsub 10 3) -7))

   (test-case "apply-n"
     (define inc (λ (x) (+ x 1)))
     (check-equal? ((apply-n inc 0) 10) 10)
     (check-equal? ((apply-n inc 3) 10) 13)
     (define double (λ (x) (* x 2)))
     (check-equal? ((apply-n double 4) 1) 16))))

(module+ main
  (displayln "=== step-04: 関数と高階関数 ===")
  (run-tests tests))



