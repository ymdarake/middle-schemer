#lang racket

(require rackunit rackunit/text-ui)

;; 解答: step-04

(define (compose f g)
  (λ (x) (f (g x))))

(define (partial f . bound)
  (λ args (apply f (append bound args))))

(define (curry2 f)
  (λ (a) (λ (b) (f a b))))

(define (flip f)
  (λ (a b) (f b a)))

(define (apply-n f n)
  (cond
    [(zero? n) (λ (x) x)]
    [else (let loop ([i n] [h (λ (x) x)])
            (if (zero? i)
                h
                (loop (sub1 i) (λ (x) (h (f x))))))]))

(define tests
  (test-suite
   "step-04-solution"
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
  (displayln "=== step-04 solution ===")
  (run-tests tests))
