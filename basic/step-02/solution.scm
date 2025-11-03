#lang racket

(require rackunit rackunit/text-ui)

;; 解答: step-02

(define (my-length xs)
  (let loop ([rest xs] [n 0])
    (if (null? rest) n (loop (cdr rest) (add1 n)))))

(define (my-append xs ys)
  (if (null? xs) ys (cons (car xs) (my-append (cdr xs) ys))))

(define (my-map f xs)
  (let loop ([rest xs] [acc '()])
    (if (null? rest)
        (reverse acc)
        (loop (cdr rest) (cons (f (car rest)) acc)))))

(define (my-filter p xs)
  (let loop ([rest xs] [acc '()])
    (cond
      [(null? rest) (reverse acc)]
      [(p (car rest)) (loop (cdr rest) (cons (car rest) acc))]
      [else (loop (cdr rest) acc)])))

(define (my-foldl f init xs)
  (let loop ([acc init] [rest xs])
    (if (null? rest)
        acc
        (loop (f acc (car rest)) (cdr rest)))))

(define tests
  (test-suite
   "step-02-solution"
   (test-case "list utilities"
     (check-equal? (my-length '()) 0)
     (check-equal? (my-length '(a b c)) 3)
     (check-equal? (my-append '(1 2) '(3 4)) '(1 2 3 4))
     (check-equal? (my-map add1 '(1 2 3)) '(2 3 4))
     (check-equal? (my-filter even? '(1 2 3 4)) '(2 4))
     (check-equal? (my-foldl + 0 '(1 2 3 4)) 10))))

(module+ main
  (displayln "=== step-02 solution ===")
  (run-tests tests))
